use std::fmt::Display;
use std::thread;

use quote::ToTokens;
use syn;

/// A type to collect errors together and format them.
///
/// Dropping this object will cause a panic. It must be consumed using `check`.
///
/// This code is based on the implementation in [Serde Derive](https://github.com/serde-rs/serde/blob/f85c4f2fa99c7f3b103e6e2580e75eb9313b00d0/serde_derive/src/internals/ctxt.rs).
#[derive(Debug)]
pub struct Ctxt {
  // The contents will be set to `None` during checking so it won't panic when this object is dropped.
  errors: Option<Vec<syn::Error>>,
}

impl Ctxt {
  /// Create a new context object.
  ///
  /// This object contains no errors, but will still trigger a panic if it is not `check`ed.
  pub fn new() -> Self {
    Ctxt {
      errors: Some(Vec::new()),
    }
  }

  /// Add an error to the context object with a tokenenizable object.
  ///
  /// The object is used for spanning in error messages.
  pub fn error_spanned_by<A: ToTokens, T: Display>(&mut self, obj: A, msg: T) {
    self
      .errors
      .as_mut()
      .unwrap()
      .push(syn::Error::new_spanned(obj.into_token_stream(), msg));
  }

  /// Add one of Syn's parse errors.
  pub fn syn_error(&mut self, err: syn::Error) {
    self.errors.as_mut().unwrap().push(err);
  }

  /// Consume this object, returning a list of found errors
  pub fn check(mut self) -> Result<(), Vec<syn::Error>> {
    let errors = self.errors.take().unwrap();
    match errors.len() {
      0 => Ok(()),
      _ => Err(errors),
    }
  }
}

impl Drop for Ctxt {
  fn drop(&mut self) {
    if !thread::panicking() && self.errors.is_some() {
      panic!("forgot to check for errors");
    }
  }
}
