use proc_macro::{Delimiter, Group, Ident, Punct, Spacing, TokenStream, TokenTree};
use proc_macro_error::proc_macro_error;
use quote::{spanned::Spanned, ToTokens};
use syn::{parse_macro_input, Attribute, Item, ItemFn};

#[proc_macro_error]
#[proc_macro_attribute]
pub fn isotest(attrs: TokenStream, input: TokenStream) -> TokenStream {
    let mut it = attrs.into_iter();
    let mut small: Option<Ident> = None;
    let mut big: Option<Ident> = None;

    if let Some(TokenTree::Ident(i)) = it.next() {
        small = Some(i);
    } else {
        panic!("First attribute argument must be the name of the replacement fn")
    }
    let _comma = it.next().expect("minitest must have two arguments");

    if let Some(TokenTree::Ident(i)) = it.next() {
        big = Some(i);
    } else {
        panic!("Second attribute argument must be the name of the real struct")
    }

    let small = small.expect("minitest must have two arguments").to_string();
    let big = big.expect("minitest must have two arguments");

    fn replace(ts: TokenStream, small: String, replacement: TokenTree) -> TokenStream {
        ts.into_iter()
            .map(|tt| match tt {
                TokenTree::Ident(ref i) if i.to_string() == small => replacement.clone(),
                TokenTree::Group(g) => TokenTree::Group(Group::new(
                    g.delimiter(),
                    replace(g.stream(), small.clone(), replacement.clone()),
                )),
                other => other,
            })
            .collect()
    }

    let input = parse_macro_input!(input as Item);

    if let Item::Fn(f) = input {
        let mut fa = f.clone();
        let mut fb = f.clone();
        // let id = quote::quote!(std::convert::identity).into();
        // let test_body = replace(
        //     f.block.clone().into_token_stream().into(),
        //     small.clone(),
        //     TokenTree::Group(Group::new(Delimiter::Parenthesis, id)),
        // );
        // let newname = Group::new(
        //     Delimiter::Parenthesis,
        //     [
        //         TokenTree::Ident(big.clone()),
        //         TokenTree::Punct(Punct::new(':', Spacing::Joint)),
        //         TokenTree::Punct(Punct::new(':', Spacing::Joint)),
        //         TokenTree::Ident(Ident::new("from", big.span())),
        //     ]
        //     .into_iter()
        //     .collect(),
        // );
        // let real_body = replace(
        //     f.block.into_token_stream().into(),
        //     small,
        //     TokenTree::Group(newname),
        // );
        let ident = f.sig.ident;
        fa.sig.ident = syn::Ident::new(&format!("{}_small", ident.to_string()), ident.span());
        fb.sig.ident = syn::Ident::new(&format!("{}_big", ident.to_string()), ident.span());

        // fa.block = Box::new(syn::parse2(test_body.into()).expect("could not parse small body"));
        // fb.block = Box::new(syn::parse2(real_body.into()).expect("could not parse big body"));
        let out = quote::quote! {
            #[test]
            #fa

            #[test]
            #fb
        };

        out.into()
    } else {
        panic!("#[isotest] must be applied to a function");
    }
}
