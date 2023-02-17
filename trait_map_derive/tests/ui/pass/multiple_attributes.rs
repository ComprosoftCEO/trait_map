use trait_map::{TraitMap, TraitMapEntry};

trait MyTraitOne {
  fn test_one(&self) -> u32;
}

trait MyTraitTwo {
  fn test_two(&self) -> u32;
}

trait MyTraitThree {
  fn test_three(&self) -> u32;
}

#[derive(TraitMapEntry)]
#[trait_map(MyTraitOne)]
#[trait_map(MyTraitTwo, MyTraitThree)]
struct MyStruct {
  value: u32,
}

impl MyTraitOne for MyStruct {
  fn test_one(&self) -> u32 {
    self.value
  }
}

impl MyTraitTwo for MyStruct {
  fn test_two(&self) -> u32 {
    self.value * self.value
  }
}

impl MyTraitThree for MyStruct {
  fn test_three(&self) -> u32 {
    self.value / 2
  }
}

fn main() {
  let mut trait_map = TraitMap::new();
  trait_map.add_entry(MyStruct { value: 5 });

  let entries = trait_map.get_entries::<dyn MyTraitOne>();
  assert_eq!(entries.len(), 1);
  assert_eq!(entries.values().next().unwrap().test_one(), 5);

  let entries = trait_map.get_entries::<dyn MyTraitTwo>();
  assert_eq!(entries.len(), 1);
  assert_eq!(entries.values().next().unwrap().test_two(), 25);

  let entries = trait_map.get_entries::<dyn MyTraitThree>();
  assert_eq!(entries.len(), 1);
  assert_eq!(entries.values().next().unwrap().test_three(), 2);
}
