use rustc_version::{version_meta, Channel};

fn main() {
  match version_meta().unwrap().channel {
    // Set "nightly" cfg flag if building on the nightly channel
    Channel::Nightly => {
      println!("cargo:rustc-cfg=nightly");
    },

    // Do nothing for the other channels
    Channel::Stable => {},
    Channel::Beta => {},
    Channel::Dev => {},
  }
}
