use trait_map::{TraitMap, TraitMapEntry};

trait MyTrait {
  fn test(&self) -> u32;
}

// Allowed but will give a compiler warning on nightly
#[derive(TraitMapEntry)]
#[trait_map(MyTrait, MyTrait)]
#[trait_map(MyTrait)]
struct MyStruct {
  value: u32,
}

impl MyTrait for MyStruct {
  fn test(&self) -> u32 {
    self.value
  }
}

fn main() {
  let mut trait_map = TraitMap::new();
  trait_map.add_entry(MyStruct { value: 5 });

  let entries = trait_map.get_entries::<dyn MyTrait>();
  assert_eq!(entries.len(), 1);
  assert_eq!(entries.values().next().unwrap().test(), 5);
}
