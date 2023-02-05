#![feature(ptr_metadata)]
#![feature(unsize)]
mod game_trait_map;

use game_trait_map::*;

trait MyTrait {
  fn test(&self, map: &GameTraitMap, recurse: bool) -> u32;
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

impl Entity for MyEntity {}

impl ConstructableEntity for MyEntity {
  fn on_create<'a>(&mut self, mut context: Context<'a, Self>) {
    context
      .add_trait::<dyn MyTrait>()
      .add_trait::<dyn Collider>()
      .add_trait::<dyn MyTrait>(); // Duplicates are ignored
                                   // .add_trait::<dyn OtherTrait>() -- Compile-time error
  }
}

impl MyTrait for MyEntity {
  fn test(&self, map: &GameTraitMap, recurse: bool) -> u32 {
    if recurse {
      for test in map.get_entities::<dyn MyTrait>() {
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

impl Entity for MyEntityTwo {}

impl ConstructableEntity for MyEntityTwo {
  fn on_create<'a>(&mut self, mut context: Context<'a, Self>) {
    context.add_trait::<dyn MyTrait>();
  }
}

impl MyTrait for MyEntityTwo {
  fn test(&self, map: &GameTraitMap, recurse: bool) -> u32 {
    5959509
  }
}

fn main() {
  let mut map = GameTraitMap::new();
  map.add_entity(MyEntity { val: 5 });
  map.add_entity(MyEntity { val: 6 });
  map.add_entity(MyEntity { val: 7 });
  map.add_entity(MyEntityTwo { a: 9 });

  for test in map.get_entities::<dyn MyTrait>() {
    println!("{}", test.test(&map, true));
  }
}
