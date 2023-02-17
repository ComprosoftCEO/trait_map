use trait_map::TraitMapEntry;

// Allowed but will give a compiler warning on nightly
#[derive(TraitMapEntry)]
struct MyStruct {
  value: u32,
}

fn main() {}
