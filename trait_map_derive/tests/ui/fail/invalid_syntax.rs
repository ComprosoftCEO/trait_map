use trait_map_derive::TraitMapEntry;

trait MyTrait {
  fn test(&self) -> u32;
}

#[derive(TraitMapEntry)]
#[trait_map]
#[trait_map(MyTrait, "literal", true, value = "5")]
struct MyStruct {
  value: u32,
}

impl MyTrait for MyStruct {
  fn test(&self) -> u32 {
    self.value
  }
}

fn main() {}
