extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{Ident, TokenTree};
use quote::{quote, ToTokens};
use syn::{
    group::parse_parens,
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    spanned::Spanned,
    Attribute, Data, DeriveInput, Expr, Field, Fields, Lit, LitStr, Meta, MetaNameValue, Token, Type,
};

#[proc_macro_derive(Metrics, attributes(metric))]
pub fn derive_metrics(item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as DeriveInput);
    expand_metrics(input).unwrap_or_else(|error| error.to_compile_error().into())
}

enum MetricType {
    IntCounter,
    IntCounterVec,
    HistogramVec,
}

#[derive(Default)]
struct MetricArgs {
    desc: Option<LitStr>,
    dims: Option<Punctuated<MetricDim, Token![,]>>,
}

struct MetricDim {
    label: LitStr,
    typ: Option<Type>,
    expr: Option<Expr>,
}

impl MetricDim {
    fn expr(&self, label: &Ident) -> proc_macro2::TokenStream {
        if let Some(ref expr) = self.expr {
            expr.to_token_stream()
                .into_iter()
                .flat_map(|item| {
                    match item {
                        TokenTree::Ident(ident) if ident.to_string() == "_" => label.to_token_stream(),
                        other => quote! {#other},
                    }
                })
                .collect()
        } else {
            quote! {#label.as_ref()}
        }
    }
}

impl Parse for MetricDim {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let label = input
            .parse::<Ident>()
            .map(|ident| LitStr::new(&ident.to_string(), ident.span()))
            .or_else(|_| input.parse::<LitStr>())?;
        let (typ, expr) = if input.parse::<Token![:]>().is_ok() {
            (
                Some(input.parse::<Type>()?),
                if input.parse::<Token![=]>().is_ok() {
                    Some(input.parse::<Expr>()?)
                } else {
                    None
                },
            )
        } else {
            (None, None)
        };

        Ok(Self { label, typ, expr })
    }
}

impl Parse for MetricArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut desc = None;
        let mut dims = None;

        while !input.is_empty() {
            let arg_name = input.parse::<Ident>()?;
            match &*arg_name.to_string() {
                "desc" => {
                    let _ = input.parse::<Token![=]>()?;
                    desc = Some(input.parse()?);
                },
                "labels" => {
                    let group = parse_parens(input)?.content;
                    dims = Some(group.parse_terminated(MetricDim::parse)?);
                },
                _ => return Err(syn::Error::new_spanned(arg_name, "unsupported metric attribute")),
            }

            let _ = input.parse::<Token![,]>();
        }

        Ok(Self { desc, dims })
    }
}

impl<'a> TryFrom<&'a Type> for MetricType {
    type Error = syn::Error;
    fn try_from(value: &'a Type) -> Result<Self, Self::Error> {
        let type_path = match value {
            Type::Path(path) => &path.path,
            _ => return Err(syn::Error::new(value.span(), "unsupported type")),
        };

        if type_path.is_ident("IntCounterVec") {
            Ok(Self::IntCounterVec)
        } else if type_path.is_ident("HistogramVec") {
            Ok(Self::HistogramVec)
        } else if type_path.is_ident("IntCounter") {
            Ok(Self::IntCounter)
        } else {
            Err(syn::Error::new(value.span(), "unsupported metric type"))
        }
    }
}

fn expand_metrics(input: DeriveInput) -> Result<TokenStream, syn::Error> {
    let span = input.span();
    let name = input.ident;

    let data = match input.data {
        Data::Struct(struct_) => struct_,
        _ => return Err(syn::Error::new(span, "derive(Metrics) is available for structs only")),
    };

    let fields = match data.fields {
        Fields::Named(fields) => fields.named,
        _ => {
            return Err(syn::Error::new(
                span,
                "derive(Metrics) is not available for unit or tuple structs",
            ))
        },
    };

    let mut methods = Vec::new();
    let mut initializers = Vec::new();

    for field in fields {
        let method_name = field.ident.clone().expect("method name");
        let metric_name = LitStr::new(&method_name.to_string(), method_name.span());
        let metric_type = MetricType::try_from(&field.ty)?;
        let MetricArgs {
            desc: metric_desc,
            dims: metric_dims,
        } = get_attr_args(&field, "metric").unwrap_or_default();
        let metric_desc = metric_desc
            .or_else(|| get_docs(&field).next())
            .unwrap_or_else(|| metric_name.clone());
        let metric_dims = metric_dims.unwrap_or_default();

        let metric_args: Vec<_> = metric_dims
            .iter()
            .enumerate()
            .map(|(num, label)| {
                let name = Ident::new(&format!("label_{}", num), label.label.span());
                let typ = label
                    .typ
                    .as_ref()
                    .map_or_else(|| quote! {impl AsRef<str>}, Type::to_token_stream);
                quote! {#name: #typ}
            })
            .collect();

        let metric_vars: Vec<_> = metric_dims
            .iter()
            .enumerate()
            .map(|(num, label)| {
                let name = Ident::new(&format!("label_{}", num), label.label.span());
                label.expr(&name)
            })
            .collect();

        let metric_labels = metric_dims.iter().map(|label| &label.label);

        let (method, initializer) = match metric_type {
            MetricType::IntCounterVec => (
                quote! {
                    pub fn #method_name(&self, #(#metric_args,)*) {
                        self.#method_name.with_label_values(&[#(#metric_vars,)*]).inc();
                    }
                },
                quote! {#method_name: ::prometheus_fire::register_int_counter_vec!(::prometheus_fire::opts!(#metric_name, #metric_desc), &[#(#metric_labels,)*])?,},
            ),
            MetricType::IntCounter => {
                if metric_dims.len() > 0 {
                    return Err(syn::Error::new(span, "IntCounter metric does not support labels"));
                }

                (
                    quote! {
                        pub fn #method_name(&self) {
                            self.#method_name.inc();
                        }
                    },
                    quote! {#method_name: ::prometheus_fire::register_int_counter!(#metric_name, #metric_desc)?,},
                )
            },
            MetricType::HistogramVec => {
                let start_method_name = Ident::new(&format!("start_{method_name}"), field.span());
                let observe_method_name = Ident::new(&format!("observe_{method_name}"), field.span());
                (
                    quote! {
                        pub fn #start_method_name(&self, #(#metric_args,)*) -> ::prometheus_fire::HistogramTimer {
                            self.#method_name.with_label_values(&[#(#metric_vars,)*]).start_timer()
                        }

                        pub fn #observe_method_name(&self, #(#metric_args,)* time: f64) {
                            self.#method_name.with_label_values(&[#(#metric_vars,)*]).observe(time);
                        }
                    },
                    quote! {#method_name: ::prometheus_fire::register_histogram_vec!(::prometheus_fire::histogram_opts!(#metric_name, #metric_desc), &[#(#metric_labels,)*])?,},
                )
            }, //_ => return Err(syn::Error::new(span, "unsupported field type")),
        };

        methods.push(method);
        initializers.push(initializer);
    }

    let tokens = quote! {
        impl #name {
            pub fn new() -> ::std::result::Result<Self, ::prometheus_fire::Error> {
                Ok(Self {
                    #(#initializers)*
                })
            }

            #(#methods)*
        }

        impl ::prometheus_fire::MetricsService for #name {}

        ::prometheus_fire::lazy_static! {
            pub static ref METRICS: #name = #name::new().expect("Can't create a metric");
        }

        pub fn metrics() -> &'static #name {
            &*METRICS
        }
    };

    Ok(tokens.into())
}

fn find_attr<'a, 'b>(field: &'a Field, name: &'b str) -> Option<&'a Attribute> {
    field.attrs.iter().find(|attr| attr.path.is_ident(name))
}

fn get_attr_args<T: Parse>(field: &Field, name: &str) -> Option<T> {
    find_attr(field, name).and_then(|attr| attr.parse_args::<T>().ok())
}

fn get_docs<'a>(field: &'a Field) -> impl Iterator<Item = LitStr> + 'a {
    field
        .attrs
        .iter()
        .filter(|attr| attr.path.is_ident("doc"))
        .filter_map(|attr| attr.parse_meta().ok())
        .filter_map(|meta| match meta {
            Meta::NameValue(MetaNameValue {
                lit: Lit::Str(ref lit), ..
            }) => Some(LitStr::new(&lit.value().trim(), meta.span())),
            _ => None,
        })
}
