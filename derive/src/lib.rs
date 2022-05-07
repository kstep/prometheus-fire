extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::{
    group::parse_brackets,
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    spanned::Spanned,
    Attribute, Data, DeriveInput, Field, Fields, Lit, LitStr, Meta, MetaNameValue, Token, Type,
};

#[proc_macro_derive(Metrics, attributes(metric))]
pub fn derive_metrics(item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as DeriveInput);
    expand_metrics(input).unwrap_or_else(|error| error.to_compile_error().into())
}

enum MetricType {
    IntCounterVec,
    HistogramVec,
}

#[derive(Default)]
struct MetricInfo {
    desc: Option<LitStr>,
    labels: Option<Punctuated<Lit, Token![,]>>,
}

impl Parse for MetricInfo {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut desc = None;
        let mut labels = None;

        while !input.is_empty() {
            let arg_name = input.parse::<Ident>()?;
            match &*arg_name.to_string() {
                "desc" => {
                    let _ = input.parse::<Token![=]>()?;
                    desc = Some(input.parse()?);
                },
                "labels" => {
                    let _ = input.parse::<Token![=]>()?;
                    let group = parse_brackets(input)?.content;
                    labels = Some(group.parse_terminated(Lit::parse)?);
                },
                _ => return Err(syn::Error::new_spanned(arg_name, "unsupported metric attribute")),
            }

            let _ = input.parse::<Token![,]>();
        }

        Ok(Self { desc, labels })
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
        let MetricInfo {
            desc: metric_desc,
            labels: metric_labels,
        } = get_attr_args(&field, "metric").unwrap_or_default();
        let metric_desc = metric_desc
            .or_else(|| get_docs(&field).next())
            .unwrap_or_else(|| metric_name.clone());
        let metric_labels_num = metric_labels.as_ref().map_or(0, Punctuated::len);
        let metric_labels = metric_labels.unwrap_or_default().into_iter();
        let metric_args: Vec<_> = (0..metric_labels_num)
            .map(|num| Ident::new(&format!("label_{}", num), span))
            .collect();

        let (method, initializer) = match metric_type {
            MetricType::IntCounterVec => (
                quote! {
                    pub fn #method_name(&self, #(#metric_args: impl AsRef<str>,)*) {
                        self.#method_name.with_label_values(&[#(#metric_args.as_ref(),)*]).inc();
                    }
                },
                quote! {#method_name: ::prometheus::register_int_counter_vec!(::prometheus::opts!(#metric_name, #metric_desc), &[#(#metric_labels,)*])?,},
            ),
            MetricType::HistogramVec => {
                let start_method_name = Ident::new(&format!("start_{method_name}"), field.span());
                let observe_method_name = Ident::new(&format!("observe_{method_name}"), field.span());
                (
                    quote! {
                        pub fn #start_method_name(&self, #(#metric_args: impl AsRef<str>,)*) -> ::prometheus::HistogramTimer {
                            self.#method_name.with_label_values(&[#(#metric_args.as_ref(),)*]).start_timer()
                        }

                        pub fn #observe_method_name(&self, #(#metric_args: impl AsRef<str>,)* time: f64) {
                            self.#method_name.with_label_values(&[#(#metric_args.as_ref(),)*]).observe(time);
                        }
                    },
                    quote! {#method_name: ::prometheus::register_histogram_vec!(::prometheus::histogram_opts!(#metric_name, #metric_desc), &[#(#metric_labels,)*])?,},
                )
            }, //_ => return Err(syn::Error::new(span, "unsupported field type")),
        };

        methods.push(method);
        initializers.push(initializer);
    }

    let tokens = quote! {
        impl #name {
            pub fn new() -> ::std::result::Result<Self, ::prometheus::Error> {
                Self {
                    #(#initializers)*
                }
            }

            #(#methods)*
        }

        impl ::prometheus_fire::MetricsService for #name {}
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
