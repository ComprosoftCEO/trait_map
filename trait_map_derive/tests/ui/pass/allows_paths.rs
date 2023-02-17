use std::fmt::Debug;

use trait_map::{TraitMap, TraitMapEntry};

mod nested_mod {
  pub trait MyTrait {
    fn test(&self) -> u32;
  }
}

#[derive(TraitMapEntry, Debug)]
#[trait_map(nested_mod::MyTrait, Debug)]
struct MyStruct {
  value: u32,
}

impl nested_mod::MyTrait for MyStruct {
  fn test(&self) -> u32 {
    self.value
  }
}

fn main() {
  let mut trait_map = TraitMap::new();
  trait_map.add_entry(MyStruct { value: 5 });

  let entries = trait_map.get_entries::<dyn nested_mod::MyTrait>();
  assert_eq!(entries.len(), 1);
  assert_eq!(entries.values().next().unwrap().test(), 5);
}
