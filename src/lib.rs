use proc_macro::TokenStream;
use quote::quote;
use syn::{Data::Struct, Fields};

fn get_fields(data: &syn::Data) -> syn::punctuated::Punctuated<syn::Field, syn::token::Comma> {
    let input = if let Struct(d) = data {
        match &d.fields {
            Fields::Named(f) => f.named.clone(),
            Fields::Unnamed(f) => f.unnamed.clone(),
            Fields::Unit => panic!("Unit structs not supported"),
        }
    } else {
        panic!("only structs are supported")
    };
    input
}

#[proc_macro_derive(FromInner)]
pub fn from_inner_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(input as syn::DeriveInput);

    let self_ident = &ast.ident;

    let fields = get_fields(&ast.data);
    let fields_count = fields.iter().count();

    let types = fields.iter().map(|f| &f.ty);
    let names = fields.iter().map(|f| &f.ident);
    let counted_names = (0..fields_count).map(|i| syn::Index::from(i));

    let inner = if fields_count == 1 {
        quote! {
            (#(#types),*)
        }
    } else {
        quote! {
            (#(#types,)*)
        }
    };

    let struct_data = if let Struct(d) = &ast.data {
        d
    } else {
        panic!("only structs are supported");
    };

    if let Fields::Unit = struct_data.fields {
        panic!("unit structs have no inner, so cannot implement From<inner> and Into<inner>");
    }

    let self_from_inner = {
        let names = names.clone();
        let counted_names = counted_names.clone();

        if fields_count == 1 {
            match &struct_data.fields {
                Fields::Named(_) => {
                    quote! {
                        Self {
                            #(#names: value,)*
                        }
                    }
                }
                Fields::Unnamed(_) => {
                    quote! {
                        Self(value)
                    }
                }
                Fields::Unit => unreachable!(),
            }
        } else {
            match &struct_data.fields {
                Fields::Named(_) => {
                    quote! {
                        Self {
                            #(#names: value.#counted_names),*
                        }
                    }
                }
                Fields::Unnamed(_) => {
                    quote! {
                        Self(#(value.#counted_names),*)
                    }
                }
                Fields::Unit => unreachable!(),
            }
        }
    };

    let inner_from_self = {
        if fields_count == 1 {
            match &struct_data.fields {
                Fields::Named(_) => {
                    quote! {
                        (#(outer.#names),*)
                    }
                }
                Fields::Unnamed(_) => {
                    quote! {
                        (outer.0)
                    }
                }
                Fields::Unit => unreachable!(),
            }
        } else {
            match &struct_data.fields {
                Fields::Named(_) => {
                    quote! {
                        (#(outer.#names,)*)
                    }
                }
                Fields::Unnamed(_) => {
                    quote! {
                        (#(outer.#counted_names,)*)
                    }
                }
                Fields::Unit => unreachable!(),
            }
        }
    };

    return quote! {
        impl ::std::convert::From<#inner> for #self_ident {
            fn from(value: #inner) -> Self {
                #self_from_inner
            }
        }

        impl ::std::convert::From<#self_ident> for #inner {
            fn from(outer: #self_ident) -> Self {
                #inner_from_self
            }
        }
    }
    .into();
}
