extern crate core;
extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{Ident, Literal, TokenTree};
use quote::{quote, ToTokens, TokenStreamExt};
use std::collections::HashMap;
use syn::{
    group::{parse_brackets, parse_parens},
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    spanned::Spanned,
    Attribute, Data, DeriveInput, Expr, Fields, Lit, LitFloat, LitInt, LitStr, Meta, MetaNameValue, Token, Type,
};

static UNKNOWN_ATTR: &str = "unsupported metric attribute";

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
struct MetricGlobalArgs {
    global: Option<Ident>,
    func: Option<Ident>,
    labels: Option<Punctuated<MetricStaticDim, Token![,]>>,
    subsystem: Option<LitStr>,
    namespace: Option<LitStr>,
}

struct MetricStaticDim {
    label: LitStr,
    value: LitStr,
}

enum HistBuckets {
    List(Punctuated<LitNumber, Token![,]>),
    Lin(LitNumber, LitNumber, LitInt),
    Exp(LitNumber, LitNumber, LitInt),
}

#[derive(Default)]
struct MetricArgs {
    desc: Option<LitStr>,
    dims: Option<Punctuated<MetricDim, Token![,]>>,
    buckets: Option<HistBuckets>,
}

struct MetricDim {
    label: LitStr,
    typ: Option<Type>,
    expr: Option<Expr>,
    enum_def: Option<Punctuated<Ident, Token![|]>>,
}

enum LitNumber {
    Float(LitFloat),
    Int(LitInt),
}

impl Parse for LitNumber {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lit = input.parse::<Lit>()?;
        match lit {
            Lit::Float(value) => Ok(Self::Float(value)),
            Lit::Int(value) => Ok(Self::Int(value)),
            _ => Err(syn::Error::new_spanned(lit, "a number expected")),
        }
    }
}

impl ToTokens for LitNumber {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Self::Float(token) => token.to_tokens(tokens),
            Self::Int(token) => tokens.append(Literal::f64_suffixed(token.base10_parse::<f64>().unwrap())),
        }
    }
}

impl Parse for HistBuckets {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek2(Token![..]) {
            let start = input.parse::<LitNumber>()?;
            let _ = input.parse::<Token![..]>()?;
            let width = input.parse::<LitNumber>()?;
            let step = input.parse::<Token![;]>().and_then(|_| input.parse::<LitInt>())?;
            Ok(Self::Lin(start, width, step))
        } else if input.peek2(Token![::]) {
            let start = input.parse::<LitNumber>()?;
            let _ = input.parse::<Token![::]>()?;
            let width = input.parse::<LitNumber>()?;
            let step = input.parse::<Token![;]>().and_then(|_| input.parse::<LitInt>())?;
            Ok(Self::Exp(start, width, step))
        } else {
            input.parse_terminated(LitNumber::parse).map(Self::List)
        }
    }
}

impl ToTokens for HistBuckets {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Self::Lin(start, width, count) => {
                tokens.extend(quote! {::prometheus_fire::linear_buckets(#start, #width, #count)?})
            },
            Self::Exp(start, factor, count) => {
                tokens.extend(quote! {::prometheus_fire::exponential_buckets(#start, #factor, #count)?})
            },
            Self::List(list) => tokens.extend(quote! {vec![#list]}),
        }
    }
}

impl Parse for MetricStaticDim {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let label = input
            .parse::<Ident>()
            .map(|ident| LitStr::new(&ident.to_string(), ident.span()))
            .or_else(|_| input.parse::<LitStr>())?;

        let _ = input.parse::<Token![=]>()?;

        let value = input.parse::<LitStr>()?;

        Ok(Self { label, value })
    }
}

impl Parse for MetricGlobalArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut global = None;
        let mut func = None;
        let mut labels = None;
        let mut namespace = None;
        let mut subsystem = None;

        while !input.is_empty() {
            let arg_name = input.parse::<Ident>()?;

            match &*arg_name.to_string() {
                "global" => {
                    let _ = input.parse::<Token![=]>()?;
                    let value = input.parse::<LitStr>()?;
                    global = Some(Ident::new(&value.value(), value.span()));
                },
                "func" => {
                    let _ = input.parse::<Token![=]>()?;
                    let value = input.parse::<LitStr>()?;
                    func = Some(Ident::new(&value.value(), value.span()));
                },
                "labels" => {
                    let group = parse_parens(input)?.content;
                    labels = Some(group.parse_terminated(MetricStaticDim::parse)?);
                },
                "namespace" => {
                    let _ = input.parse::<Token![=]>()?;
                    namespace = Some(input.parse()?);
                },
                "subsystem" => {
                    let _ = input.parse::<Token![=]>()?;
                    subsystem = Some(input.parse()?);
                },
                _ => return Err(syn::Error::new_spanned(arg_name, UNKNOWN_ATTR)),
            }

            let _ = input.parse::<Token![,]>();
        }

        Ok(Self {
            global,
            func,
            labels,
            namespace,
            subsystem,
        })
    }
}

impl MetricDim {
    fn expr(&self, label: &Ident) -> proc_macro2::TokenStream {
        if let Some(ref expr) = self.expr {
            expr.to_token_stream()
                .into_iter()
                .flat_map(|item| match item {
                    TokenTree::Ident(ident) if ident == "_" => label.to_token_stream(),
                    other => quote! {#other},
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
        let (typ, enum_def, expr) = if input.parse::<Token![:]>().is_ok() {
            (
                Some(input.parse::<Type>()?),
                parse_brackets(&input)
                    .ok()
                    .map(|group| group.content.parse_terminated(Ident::parse))
                    .transpose()?,
                input
                    .parse::<Token![=]>()
                    .ok()
                    .map(|_| input.parse::<Expr>())
                    .transpose()?,
            )
        } else {
            (None, None, None)
        };

        Ok(Self {
            label,
            typ,
            enum_def,
            expr,
        })
    }
}

impl Parse for MetricArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut desc = None;
        let mut dims = None;
        let mut buckets = None;

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
                "buckets" => {
                    let group = parse_parens(input)?.content;
                    buckets = Some(group.parse()?);
                },
                _ => return Err(syn::Error::new_spanned(arg_name, UNKNOWN_ATTR)),
            }

            let _ = input.parse::<Token![,]>();
        }

        Ok(Self { desc, dims, buckets })
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
    let MetricGlobalArgs {
        global: global_name,
        func: func_name,
        labels: const_labels,
        namespace,
        subsystem,
    } = get_attr_args(&input.attrs, "metric")?.unwrap_or_default();

    let namespace = namespace.map(|name| {
        quote! {.namespace(#name)}
    });

    let subsystem = subsystem.map(|name| {
        quote! {.subsystem(#name)}
    });

    let (const_labels_def, const_labels) = const_labels.map_or((None, None), |labels| {
        let labels = labels
            .iter()
            .map(|MetricStaticDim { label, value }| quote! {(#label.to_string(), #value.to_string())});
        (
            Some(quote! {
                let const_labels = {
                    let mut labels = ::std::collections::HashMap::<String, String>::new();
                    labels.extend([#(#labels,)*]);
                    labels
                };
            }),
            Some(quote! {.const_labels(const_labels.clone())}),
        )
    });

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
    let mut typedefs = HashMap::new();

    for field in fields {
        let method_name = field.ident.clone().expect("method name");
        let metric_name = LitStr::new(&method_name.to_string(), method_name.span());
        let metric_type = MetricType::try_from(&field.ty)?;
        let MetricArgs {
            desc: metric_desc,
            dims: metric_dims,
            buckets: hist_buckets,
        } = get_attr_args(&field.attrs, "metric")?.unwrap_or_default();
        let metric_desc = metric_desc
            .or_else(|| get_docs(&field.attrs).next())
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

        typedefs.extend(
            metric_dims
                .iter()
                .filter_map(|dim| dim.enum_def.as_ref().zip(dim.typ.as_ref()))
                .map(|(enum_def, typ)| {
                    let enum_name = match typ {
                        Type::Path(path) => path.path.get_ident().unwrap().clone(),
                        _ => unimplemented!("enum derivation does not work for complex types"),
                    };
                    let enum_match: Vec<_> = enum_def
                        .iter()
                        .map(|ident| {
                            let name = LitStr::new(&to_snake_case(&ident.to_string()), ident.span());
                            quote! {Self::#ident => #name}
                        })
                        .collect();
                    let enum_def = enum_def.into_iter();

                    (enum_name.clone(), quote! {
                        #[derive(Debug, PartialEq, Eq, Copy, Clone)]
                        pub enum #enum_name {
                            #(#enum_def,)*
                        }

                        impl AsRef<str> for #typ {
                            fn as_ref(&self) -> &str {
                                match self {
                                    #(#enum_match,)*
                                }
                            }
                        }
                    })
                }),
        );

        let metric_labels = metric_dims.iter().map(|label| &label.label);

        let (method, initializer) = match metric_type {
            MetricType::IntCounterVec => {
                if hist_buckets.is_some() {
                    return Err(syn::Error::new_spanned(
                        field,
                        "IntCounterVec metric does not support buckets",
                    ));
                }
                (
                    quote! {
                        pub fn #method_name(&self, #(#metric_args,)*) {
                            self.#method_name.with_label_values(&[#(#metric_vars,)*]).inc();
                        }
                    },
                    quote! {#method_name: ::prometheus_fire::register_int_counter_vec!(::prometheus_fire::opts!(#metric_name, #metric_desc) #const_labels #namespace #subsystem, &[#(#metric_labels,)*])?,},
                )
            },
            MetricType::IntCounter => {
                if metric_dims.len() > 0 {
                    return Err(syn::Error::new_spanned(
                        field,
                        "IntCounter metric does not support labels",
                    ));
                }
                if hist_buckets.is_some() {
                    return Err(syn::Error::new_spanned(
                        field,
                        "IntCounter metric does not support buckets",
                    ));
                }

                (
                    quote! {
                        pub fn #method_name(&self) {
                            self.#method_name.inc();
                        }
                    },
                    quote! {#method_name: ::prometheus_fire::register_int_counter!(::prometheus_fire::opts!(#metric_name, #metric_desc) #namespace #subsystem)?,},
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
                    quote! {#method_name: ::prometheus_fire::register_histogram_vec!(::prometheus_fire::histogram_opts!(#metric_name, #metric_desc, #hist_buckets) #const_labels #namespace #subsystem, &[#(#metric_labels,)*])?,},
                )
            }, //_ => return Err(syn::Error::new_spanned(field, "unsupported field type")),
        };

        methods.push(method);
        initializers.push(initializer);
    }

    let typedefs: Vec<_> = typedefs.into_values().collect();

    let global_def = global_name.map(|ref_name| {
        quote! {
            ::prometheus_fire::lazy_static! {
                pub static ref #ref_name: #name = #name::new().expect("Can't create a metric");
            }
        }
    });

    let func_def = func_name.map(|f_name| {
        quote! {
            pub fn #f_name() -> &'static #name {
                &*METRICS
            }
        }
    });

    let tokens = quote! {
        impl #name {
            pub fn new() -> ::std::result::Result<Self, ::prometheus_fire::Error> {
                #const_labels_def

                Ok(Self {
                    #(#initializers)*
                })
            }

            #(#methods)*
        }

        impl ::prometheus_fire::MetricsService for #name {}

        #(#typedefs)*

        #global_def

        #func_def
    };

    Ok(tokens.into())
}

fn find_attr<'a, 'b>(attrs: &'a [Attribute], name: &'b str) -> Option<&'a Attribute> {
    attrs.iter().find(|attr| attr.path.is_ident(name))
}

fn get_attr_args<T: Parse>(attrs: &[Attribute], name: &str) -> Result<Option<T>, syn::Error> {
    find_attr(attrs, name).map(|attr| attr.parse_args::<T>()).transpose()
}

fn get_docs<'a>(attrs: &'a [Attribute]) -> impl Iterator<Item = LitStr> + 'a {
    attrs
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

fn to_snake_case(value: &str) -> String {
    let mut start = true;
    struct Iter(bool, bool, char);
    impl Iter {
        fn new(uscore: bool, ch: char) -> Self {
            Self(uscore, true, ch.to_ascii_lowercase())
        }
    }
    impl Iterator for Iter {
        type Item = char;

        fn next(&mut self) -> Option<Self::Item> {
            if self.0 {
                self.0 = false;
                Some('_')
            } else if self.1 {
                self.1 = false;
                Some(self.2)
            } else {
                None
            }
        }
    }

    value
        .chars()
        .flat_map(|ch| {
            if start {
                start = false;
                Iter::new(false, ch)
            } else {
                Iter::new(ch.is_ascii_uppercase(), ch)
            }
        })
        .collect::<String>()
}

#[test]
fn test_to_snake_case() {
    assert_eq!(to_snake_case(""), "");
    assert_eq!(to_snake_case("HelloWorldOfCamelCase"), "hello_world_of_camel_case");
}
