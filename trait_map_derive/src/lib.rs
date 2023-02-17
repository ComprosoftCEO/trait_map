use ctxt::Ctxt;
use quote::{quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{parse_macro_input, parse_quote, Attribute, DeriveInput, GenericParam, Generics, Meta, NestedMeta, Path};

mod ctxt;

#[proc_macro_derive(TraitMapEntry, attributes(trait_map))]
pub fn derive_trait_map_entry(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let derive_input = parse_macro_input!(input as DeriveInput);

  let name = derive_input.ident;

  let generics = add_trait_bounds(derive_input.generics);
  let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

  let mut ctx = Ctxt::new();
  let traits_to_include: Vec<_> = parse_attributes(&mut ctx, derive_input.attrs)
    .into_iter()
    .filter_map(|attr| parse_attribute_trait(&mut ctx, attr))
    .collect();

  let functions: Vec<_> = traits_to_include
    .into_iter()
    .map(|t| {
      let span = t.span();
      quote_spanned!(span => .add_trait::<dyn #t>())
    })
    .collect();

  if let Err(errors) = ctx.check() {
    let compile_errors = errors.iter().map(syn::Error::to_compile_error);
    return quote!(#(#compile_errors)*).into();
  }

  quote! {
    impl #impl_generics trait_map::TraitMapEntry for #name #ty_generics #where_clause {
      fn on_create<'a>(&mut self, context: trait_map::Context<'a>) {
        context.downcast::<Self>() #(#functions)* ;
      }
    }
  }
  .into()
}

// Add a bound `T: 'static` to every type parameter T.
fn add_trait_bounds(mut generics: Generics) -> Generics {
  for param in &mut generics.params {
    if let GenericParam::Type(ref mut type_param) = *param {
      type_param.bounds.push(parse_quote!('static));
    }
  }
  generics
}

/// Get all `#[trait_map(...)]` attributes
fn parse_attributes(ctx: &mut Ctxt, attributes: Vec<Attribute>) -> Vec<NestedMeta> {
  attributes
    .into_iter()
    // We only want attributes that look like
    //  #[trait_map(...)]
    .filter(|attr| attr.path.is_ident("trait_map"))
    .map(|attr| match attr.parse_meta() {
      // Valid form:
      //   #[trait_map(TraitOne, some::path::TraitTwo, ...)]
      Ok(Meta::List(meta)) => meta.nested.into_iter().collect::<Vec<_>>(),

      // Invalid form:
      //   #[trait_map = "value"]
      Ok(other) => {
        ctx.error_spanned_by(other, "expected #[trait_map(...)]");
        Vec::new()
      },

      Err(err) => {
        ctx.syn_error(err);
        Vec::new()
      },
    })
    .flatten()
    .collect()
}

/// Parse all attributes that look like:
///
/// ```
/// #[trait_map(TraitOne, some::path::TraitTwo)]
/// ```
fn parse_attribute_trait(ctx: &mut Ctxt, attr: NestedMeta) -> Option<Path> {
  match attr {
    // Valid form:
    //   trait_map(TraitOne)
    //   trait_map(some::path::TraitTwo)
    NestedMeta::Meta(Meta::Path(trait_path)) => Some(trait_path),

    // Invalid forms:
    //   trait_map("literal")
    //   trait_map(Key = Value)
    other => {
      ctx.error_spanned_by(other, "unexpected attribute, please specify a valid trait");
      None
    },
  }
}
