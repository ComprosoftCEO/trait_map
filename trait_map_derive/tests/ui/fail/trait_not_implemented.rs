use trait_map_derive::TraitMapEntry;

trait MyTrait {
  fn test(&self) -> u32;
}

#[derive(TraitMapEntry)]
#[trait_map(MyTrait)]
struct MyStruct {
  value: u32,
}

fn main() {}
