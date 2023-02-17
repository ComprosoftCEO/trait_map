use std::fmt::Debug;

use trait_map::{TraitMap, TraitMapEntry};

trait MyTrait {
  fn test(&self) -> u32;
}

#[derive(TraitMapEntry)]
#[trait_map(MyTrait)]
struct MyStruct<T: Debug>
where
  T: Clone,
{
  value: u32,
  internal: T,
}

impl<T> MyTrait for MyStruct<T>
where
  T: Debug + Clone,
{
  fn test(&self) -> u32 {
    self.value
  }
}

fn main() {
  let mut trait_map = TraitMap::new();
  trait_map.add_entry(MyStruct::<u64> {
    value: 5,
    internal: 500,
  });

  let entries = trait_map.get_entries::<dyn MyTrait>();
  assert_eq!(entries.len(), 1);
  assert_eq!(entries.values().next().unwrap().test(), 5);
}
