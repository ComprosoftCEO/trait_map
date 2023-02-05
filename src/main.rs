#![feature(ptr_metadata)]
#![feature(unsize)]
mod trait_map;

use std::os::unix::prelude::DirEntryExt;

use trait_map::*;

trait MyTrait {
  fn test(&self, map: &TraitMap, recurse: bool) -> u32;
}

trait Collider {
  fn is_colliding_with(&self, point: (f32, f32)) -> bool;
}

trait OtherTrait {
  fn hello(&self);
}

struct MyEntity {
  val: u32,
}

impl TraitMapEntry for MyEntity {
  fn on_create<'a>(&mut self, context: Context<'a>) {
    context
      .downcast::<Self>()
      .add_trait::<dyn MyTrait>()
      .add_trait::<dyn Collider>()
      .add_trait::<dyn MyTrait>(); // Duplicates are ignored

    // .add_trait::<dyn OtherTrait>() -- Compile-time error
  }
}

impl MyTrait for MyEntity {
  fn test(&self, map: &TraitMap, recurse: bool) -> u32 {
    if recurse {
      for (_, test) in map.search_entities::<dyn MyTrait>() {
        println!("Recurse: {}", test.test(map, false));
      }
    }
    self.val
  }
}

impl Collider for MyEntity {
  fn is_colliding_with(&self, point: (f32, f32)) -> bool {
    false
  }
}

struct MyEntityTwo {
  a: u128,
}

impl TraitMapEntry for MyEntityTwo {
  fn on_create<'a>(&mut self, mut context: Context<'a>) {
    context.downcast::<Self>().add_trait::<dyn MyTrait>();
  }
}

impl MyTrait for MyEntityTwo {
  fn test(&self, map: &TraitMap, recurse: bool) -> u32 {
    5959509
  }
}

fn main() {
  let mut map = TraitMap::new();
  map.add_entry(MyEntity { val: 5 });
  map.add_entry(MyEntity { val: 6 });
  map.add_entry(MyEntity { val: 7 });
  map.add_entry(MyEntityTwo { a: 9 });

  let mut first = true;
  for (entry_id, test) in map.all_entries_mut() {
    println!("{:?}", entry_id);
  }

  for (_, test) in map.search_entities_mut::<dyn TraitMapEntry>() {
    // println!("{}", test.test(&map, true));
  }
}
