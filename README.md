# Trait Map

Rust map for dynamic trait storage and references

**Note: this library must be compiled on Rust Nightly**.
It uses the [`ptr_metadata`](https://rust-lang.github.io/rfcs/2580-ptr-meta.html) and [`unsize`](https://doc.rust-lang.org/beta/unstable-book/library-features/unsize.html) features.

## About

This crate allows you to dynamically search for traits inside a map.
For example, assume we have some traits and structs that implement those traits:

```rust
trait ExampleTrait {
  fn do_something(&self) -> u32;
  fn do_another_thing(&mut self);
}

trait ExampleTraitTwo {
  fn test_method(&self);
}

struct MyStruct {
  // ...
}

struct AnotherStruct {
  // ...
}

impl ExampleTrait for MyStruct {
  fn do_something(&self) -> u32 { /* Code */ }
  fn do_another_thing(&mut self) { /* Code */ }
}

impl ExampleTrait for AnotherStruct {
  fn do_something(&self) -> u32 { /* Code */ }
  fn do_another_thing(&mut self) { /* Code */ }
}

impl ExampleTraitTwo for AnotherStruct{
  fn test_method(&self) { /* Code */ }
}
```

We can use `TraitMap` to have iterators over the `dyn Trait` types as follows:

```rust
use trait_map::{TraitMap, TraitMapEntry, Context};

impl TraitMapEntry for MyStruct {
  fn on_create<'a>(&mut self, context: Context<'a>) {
    // Must explicitly list which traits to expose
    context
      .downcast::<Self>()
      .add_trait::<dyn ExampleTrait>();
  }

  // Can be overridden to update the exposed traits in the map
  fn on_update<'a>(&mut self, context: Context<'a>) {
    context
      .downcast::<Self>()
      .remove_trait::<dyn ExampleTrait>();
  }
}

impl TraitMapEntry for AnotherStruct {
  fn on_create<'a>(&mut self, context: Context<'a>) {
    // Must explicitly list which traits to expose
    context
      .downcast::<Self>()
      .add_trait::<dyn ExampleTrait>()
      .add_trait::<dyn ExampleTraitTwo>();
  }
}

fn main() {
  let mut map = TraitMap::new();
  map.add_entry(MyStruct { /* ... */ });
  map.add_entry(AnotherStruct { /* ... */ });

  // Can iterate over all types that implement ExampleTrait
  //  Notice that entry is "&dyn mut ExampleTrait"
  for (entry_id, entry) in map.get_entries_mut::<dyn ExampleTrait>() {
    entry.do_another_thing();
  }

  // Can iterate over all types that implement ExampleTraitTwo
  //  Notice that entry is "&dyn ExampleTraitTwo"
  for (entry_id, entry) in map.get_entries::<dyn ExampleTraitTwo>() {
    entry.test_method();
  }
}
```

## Deriving

If you enable the `derive` feature flag, you can automatically derive `TraitMapEntry`.
You need to use one or more `#[trait_map(...)]` macros to specify which traits to register with the TraitMap.
It uses the [`proc_macro_diagnostic`](https://doc.rust-lang.org/beta/unstable-book/library-features/proc-macro-diagnostic.html) feature to emit helpful warnings when compiling on nightly.

```rust
use trait_map::TraitMapEntry;

// ...

#[derive(Debug, TraitMapEntry)]
#[trait_map(ExampleTrait, ExampleTraitTwo)]
#[trait_map(std::fmt::Debug)]
struct DerivedStruct {
  // ...
}

impl ExampleTrait for DerivedStruct {
  fn do_something(&self) -> u32 { /* Code */ }
  fn do_another_thing(&mut self) { /* Code */ }
}

impl ExampleTraitTwo for DerivedStruct{
  fn test_method(&self) { /* Code */ }
}
```
