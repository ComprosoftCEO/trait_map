use trait_map_derive::TraitMapEntry;

#[derive(TraitMapEntry)]
#[trait_map(MyTrait)]
struct MyStruct {
  value: u32,
}

fn main() {}
