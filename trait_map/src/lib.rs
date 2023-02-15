//! # Trait Map
//!
//! `trait_map` provides the [TraitMap] data structure, which can store variable data types
//! and expose traits on those types. Types must implement the [TraitMapEntry] trait, which provides
//! [`on_create()`](TraitMapEntry::on_create) and [`on_update()`](TraitMapEntry::on_update) hooks
//! for specifying which traits should be exposed in the map.
//!
//! **Warning: This crate must be compiled using Rust Nightly.**
//! It uses the [`ptr_metadata`](https://rust-lang.github.io/rfcs/2580-ptr-meta.html) and [`unsize`](https://doc.rust-lang.org/beta/unstable-book/library-features/unsize.html)
//! features for working with raw pointers.
//!
//! # Usage
//!
//! Assume we have some custom structs and traits defined:
//!
//! ```
//! trait ExampleTrait {
//!   fn do_something(&self) -> u32;
//!   fn do_another_thing(&mut self);
//! }
//!
//! trait ExampleTraitTwo {
//!   fn test_method(&self);
//! }
//!
//! struct MyStruct {
//!   // ...
//! }
//!
//! struct AnotherStruct {
//!   // ...
//! }
//!
//! impl ExampleTrait for MyStruct {
//!   fn do_something(&self) -> u32 { /* Code */ }
//!   fn do_another_thing(&mut self) { /* Code */ }
//! }
//!
//! impl ExampleTrait for AnotherStruct {
//!   fn do_something(&self) -> u32 { /* Code */ }
//!   fn do_another_thing(&mut self) { /* Code */ }
//! }
//!
//! impl ExampleTraitTwo for AnotherStruct{
//!   fn test_method(&self) { /* Code */ }
//! }
//! ```
//!
//! We can specify that we want to allow our struct types to work with the trait map by implementing the [TraitMapEntry] trait:
//!
//! ```
//! impl TraitMapEntry for MyStruct {
//!   fn on_create<'a>(&mut self, context: Context<'a>) {
//!     // Must explicitly list which traits to expose
//!     context
//!       .downcast::<Self>()
//!       .add_trait::<dyn ExampleTrait>();
//!   }
//!
//!   // Can be overridden to update the exposed traits in the map
//!   fn on_update<'a>(&mut self, context: Context<'a>) {
//!     context
//!       .downcast::<Self>()
//!       .remove_trait::<dyn ExampleTrait>();
//!   }
//! }
//!
//! impl TraitMapEntry for AnotherStruct {
//!   fn on_create<'a>(&mut self, context: Context<'a>) {
//!     context
//!       .downcast::<Self>()
//!       .add_trait::<dyn ExampleTrait>()
//!       .add_trait::<dyn ExampleTraitTwo>();
//!   }
//! }
//! ```
//!
//! Once this is done, we can store instances of these concrete types inside [TraitMap] and query them by trait.
//! For example:
//!
//! ```
//! fn main() {
//!   let mut map = TraitMap::new();
//!   map.add_entry(MyStruct { /* ... */ });
//!   map.add_entry(AnotherStruct { /* ... */ });
//!
//!   // Can iterate over all types that implement ExampleTrait
//!   //  Notice that entry is "&dyn mut ExampleTrait"
//!   for (entry_id, entry) in map.get_entries_mut::<dyn ExampleTrait>() {
//!     entry.do_another_thing();
//!   }
//!
//!   // Can iterate over all types that implement ExampleTraitTwo
//!   //  Notice that entry is "&dyn ExampleTraitTwo"
//!   for (entry_id, entry) in map.get_entries::<dyn ExampleTraitTwo>() {
//!     entry.test_method();
//!   }
//! }
//! ```
#![feature(ptr_metadata)]
#![feature(unsize)]

use std::any::TypeId;
use std::cell::{RefCell, RefMut};
use std::collections::HashMap;
use std::marker::Unsize;
use std::mem::transmute;
use std::ptr::{self, DynMetadata, NonNull, Pointee};

#[cfg(any(trait_map_derive, test))]
#[allow(unused_imports)]
use trait_map_derive::TraitMapEntry;

/// Rust type that can be stored inside of a [TraitMap].
pub trait TraitMapEntry: 'static {
  /// Called when the type is first added to the [TraitMap].
  /// This should be use to specify which implemented traits are exposed to the map.
  ///
  /// # Examples
  ///
  /// ```
  /// impl TraitMapEntry for MyStruct {
  ///   fn on_create<'a>(&mut self, context: Context<'a>) {
  ///     context
  ///      .downcast::<Self>()
  ///      .add_trait::<dyn ExampleTrait>()
  ///      .add_trait::<dyn ExampleTraitTwo>();
  ///   }
  /// }
  /// ```
  fn on_create<'a>(&mut self, context: Context<'a>);

  /// Hook that allows exposed traits to be dynamically updated inside the map.
  /// It is called by the [`update_entry()`](TraitMap::update_entry) method.
  /// The default implementation does nothing.
  ///
  /// # Examples:
  ///
  /// ```
  /// impl TraitMapEntry for MyStruct {
  ///   // ...
  ///
  ///   fn on_update<'a>(&mut self, context: Context<'a>) {
  ///     context
  ///      .downcast::<Self>()
  ///      .remove_trait::<dyn ExampleTrait>()
  ///      .add_trait::<dyn ExampleTraitTwo>();
  ///   }
  /// }
  ///
  /// fn main() {
  ///   let mut map = TraitMap::new();
  ///   let entry_id = map.add_entry(MyStruct { /* ... */ });
  ///   // ...
  ///   map.update_entry(entry_id);
  /// }
  /// ```
  #[allow(unused_variables)]
  fn on_update<'a>(&mut self, context: Context<'a>) {}
}

/// Opaque ID type for each entry in the trait map.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct EntryID(u64);

/// Map structure that allows types to be dynamically queries by trait.
///
/// Values must implement the [TraitMapEntry] trait to be added to the map.
/// The [`on_create()`](TraitMapEntry::on_create) method should be used to specify which traits are exposed to the map.
#[derive(Debug, Default)]
pub struct TraitMap {
  unique_entry_id: u64,
  traits: RefCell<HashMap<TypeId, HashMap<EntryID, PointerWithMetadata>>>,
  concrete_types: HashMap<EntryID, TypeId>,
}

/// Stores type information about an entry inside of a [TraitMap]
///
/// Must be cast to a [TypedContext] using the [`.downcast()`](Context::downcast) or [`.try_downcast()`](Context::try_downcast)
/// methods for adding or removing traits from the map.
#[derive(Debug)]
pub struct Context<'a> {
  entry_id: EntryID,
  pointer: NonNull<()>,
  type_id: TypeId,
  traits: RefMut<'a, HashMap<TypeId, HashMap<EntryID, PointerWithMetadata>>>,
}

/// Stores concrete type for an entry inside a [TraitMap]
///
/// It can be upcast to an untyped [Context] using the [`.upcast()`](TypedContext::upcast) method.
#[derive(Debug)]
pub struct TypedContext<'a, E: ?Sized> {
  entry_id: EntryID,
  pointer: NonNull<E>,
  traits: RefMut<'a, HashMap<TypeId, HashMap<EntryID, PointerWithMetadata>>>,
}

impl TraitMap {
  pub fn new() -> Self {
    Self::default()
  }

  /// Add an entry to the map.
  pub fn add_entry<Entry: TraitMapEntry + 'static>(&mut self, entry: Entry) -> EntryID {
    let entry_ref = Box::leak(Box::new(entry));

    // Generate the EntryID
    let entry_id = EntryID(self.unique_entry_id);
    self.unique_entry_id += 1;

    // Save the concrete type for downcasting later
    self.concrete_types.insert(entry_id, TypeId::of::<Entry>());

    // All entities get the dyn entity trait by default
    //  This stores the unique ownership of the object
    let mut context = TypedContext {
      entry_id,
      pointer: entry_ref.into(),
      traits: self.traits.borrow_mut(),
    };
    context.add_trait::<dyn TraitMapEntry>();

    // Add any other traits as required
    entry_ref.on_create(context.upcast());

    entry_id
  }

  /// Remove an entry from the map using its unique ID.
  /// Any cleanup should be handled by the `Drop` trait.
  ///
  /// Returns `true` if the entry was removed, or `false` otherwise.
  pub fn remove_entry(&mut self, entry_id: EntryID) -> bool {
    let mut removed = false;

    // Drop the entry from the map
    if let Some(pointer) = self
      .traits
      .borrow_mut()
      .get_mut(&TypeId::of::<dyn TraitMapEntry>())
      .and_then(|traits| traits.remove(&entry_id))
    {
      drop(unsafe { pointer.into_boxed::<dyn TraitMapEntry>() });
      removed = true;
    }

    // Drop the entry from the concrete types list
    self.concrete_types.remove(&entry_id);

    // Also remove any trait references to the entry
    for traits in self.traits.borrow_mut().values_mut() {
      traits.remove(&entry_id);
    }

    removed
  }

  /// Call the [`on_update()`](TraitMapEntry::on_update) handler for an entry.
  ///
  /// Returns `true` if the entry exists and was updated, or `false` otherwise.
  pub fn update_entry(&mut self, entry_id: EntryID) -> bool {
    (|| {
      let type_id = self.concrete_types.get(&entry_id).cloned()?;
      let (pointer, entry) = self
        .traits
        .borrow()
        .get(&TypeId::of::<dyn TraitMapEntry>())
        .and_then(|traits| traits.get(&entry_id))
        .map(|pointer| unsafe {
          (
            NonNull::new_unchecked(pointer.pointer),
            pointer.reconstruct_mut::<dyn TraitMapEntry>(),
          )
        })?;

      entry.on_update(Context {
        entry_id,
        pointer,
        type_id,
        traits: self.traits.borrow_mut(),
      });
      Some(())
    })()
    .is_some()
  }

  /// Get the concrete type for an entry in the map
  pub fn get_entry_type(&self, entry_id: EntryID) -> Option<TypeId> {
    self.concrete_types.get(&entry_id).cloned()
  }

  /// Get the list of all entries as an immutable reference
  pub fn all_entries(&self) -> HashMap<EntryID, &dyn TraitMapEntry> {
    self.get_entities()
  }

  /// Get the list of all entries as a mutable reference
  pub fn all_entries_mut(&mut self) -> HashMap<EntryID, &mut dyn TraitMapEntry> {
    self.get_entities_mut()
  }

  /// Returns all entries that are registered with a specific trait.
  /// Returns an immutable reference.
  ///
  /// # Examples
  ///
  /// ```
  /// for (entry_id, entry) in map.get_entries::<dyn MyTrait>() {
  ///   entry.trait_method(1, "hello");
  /// }
  /// ```
  pub fn get_entities<Trait>(&self) -> HashMap<EntryID, &Trait>
  where
    // Ensure that Trait is a valid "dyn Trait" object
    Trait: ?Sized + Pointee<Metadata = DynMetadata<Trait>> + 'static,
  {
    self
      .traits
      .borrow()
      .get(&TypeId::of::<Trait>())
      .map(|traits| {
        traits
          .iter()
          .map(|(entry_id, pointer)| (*entry_id, unsafe { pointer.reconstruct_ref() }))
          .collect()
      })
      .unwrap_or_default()
  }

  /// Returns all entries that are registered with a specific trait.
  /// Returns a mutable reference.
  ///
  /// # Examples
  ///
  /// ```
  /// for (entry_id, entry) in map.get_entries_mut::<dyn MyTrait>() {
  ///   entry.trait_method_mut("hello");
  /// }
  /// ```
  pub fn get_entities_mut<Trait>(&mut self) -> HashMap<EntryID, &mut Trait>
  where
    // Ensure that Trait is a valid "dyn Trait" object
    Trait: ?Sized + Pointee<Metadata = DynMetadata<Trait>> + 'static,
  {
    self
      .traits
      .borrow()
      .get(&TypeId::of::<Trait>())
      .map(|traits| {
        traits
          .iter()
          .map(|(entry_id, pointer)| (*entry_id, unsafe { pointer.reconstruct_mut() }))
          .collect()
      })
      .unwrap_or_default()
  }

  /// Get a specific entry that implements a trait.
  /// Returns an immutable reference.
  ///
  /// # Examples
  ///
  /// ```
  /// let my_ref: Option<&dyn MyTrait> = map.get_entry::<dyn MyTrait>(entry_id);
  /// ```
  pub fn get_entry<Trait>(&self, entry_id: EntryID) -> Option<&Trait>
  where
    // Ensure that Trait is a valid "dyn Trait" object
    Trait: ?Sized + Pointee<Metadata = DynMetadata<Trait>> + 'static,
  {
    self
      .traits
      .borrow()
      .get(&TypeId::of::<Trait>())
      .and_then(|traits| traits.get(&entry_id))
      .map(|pointer| unsafe { pointer.reconstruct_ref() })
  }

  /// Get a specific entry that implements a trait.
  /// Returns a mutable reference.
  ///
  /// # Errors
  /// Returns `None` if the entry no longer exists in the map.
  ///
  /// # Examples
  ///
  /// ```
  /// let my_mut_ref: Option<&mut dyn MyTrait> = map.get_entry_mut::<dyn MyTrait>(entry_id);
  /// ```
  pub fn get_entry_mut<Trait>(&mut self, entry_id: EntryID) -> Option<&mut Trait>
  where
    // Ensure that Trait is a valid "dyn Trait" object
    Trait: ?Sized + Pointee<Metadata = DynMetadata<Trait>> + 'static,
  {
    self
      .traits
      .borrow()
      .get(&TypeId::of::<Trait>())
      .and_then(|traits| traits.get(&entry_id))
      .map(|pointer| unsafe { pointer.reconstruct_mut() })
  }

  /// Get a specific entry and downcast to an immutable reference of its concrete type.
  ///
  /// # Errors
  /// Returns `None` if the entry no longer exists in the map.
  ///
  /// # Panics
  /// This method panics if the type parameter `T` does not match the concrete type.
  ///
  /// # Examples
  /// ```
  /// let entry_id = map.add_entry(MyStruct { /* ... */ });
  /// // ...
  /// let my_struct: Option<&MyStruct> = map.get_entry_downcast::<MyStruct>(entry_id);
  /// ```
  pub fn get_entry_downcast<T: TraitMapEntry + 'static>(&self, entry_id: EntryID) -> Option<&T> {
    self
      .try_get_entry_downcast(entry_id)
      .map(|entry| entry.expect("Invalid downcast"))
  }

  /// Get a specific entry and downcast to a mutable reference of its concrete type.
  ///
  /// # Errors
  /// Returns `None` if the entry no longer exists in the map.
  ///
  /// # Panics
  /// This method panics if the type parameter `T` does not match the concrete type.
  ///
  /// # Examples
  /// ```
  /// let entry_id = map.add_entry(MyStruct { /* ... */ });
  /// // ...
  /// let my_struct: Option<&mut MyStruct> = map.get_entry_downcast_mut::<MyStruct>(entry_id);
  /// ```
  pub fn get_entry_downcast_mut<T: TraitMapEntry + 'static>(&mut self, entry_id: EntryID) -> Option<&mut T> {
    self
      .try_get_entry_downcast_mut(entry_id)
      .map(|entry| entry.expect("Invalid downcast"))
  }

  /// Remove an entry from the map as its concrete type.
  ///
  /// # Errors
  /// Returns `None` if the entry no longer exists in the map.
  ///
  /// # Panics
  /// This method panics if the type parameter `T` does not match the concrete type.
  ///
  /// # Examples
  /// ```
  /// let entry_id = map.add_entry(MyStruct { /* ... */ });
  /// // ...
  /// let my_struct: Option<MyStruct> = map.take_entry_downcast::<MyStruct>(entry_id);
  pub fn take_entry_downcast<T: TraitMapEntry + 'static>(&mut self, entry_id: EntryID) -> Option<T> {
    self
      .try_take_entry_downcast(entry_id)
      .map(|entry| entry.expect("Invalid downcast"))
  }

  /// Get a specific entry and downcast to an immutable reference of its concrete type.
  ///
  /// # Errors
  /// Returns `None` if the entry no longer exists in the map.
  ///
  /// Returns `Some(None)` if the type parameter `T` does not match the concrete type.
  ///
  /// # Examples
  /// ```
  /// let entry_id = map.add_entry(MyStruct { /* ... */ });
  /// // ...
  /// let my_struct: Option<Option<&MyStruct>> = map.try_get_entry_downcast::<MyStruct>(entry_id);
  pub fn try_get_entry_downcast<T: TraitMapEntry + 'static>(&self, entry_id: EntryID) -> Option<Option<&T>> {
    // Make sure the downcast is valid
    if self.get_entry_type(entry_id)? != TypeId::of::<T>() {
      return Some(None);
    }

    Some(self.get_entry::<dyn TraitMapEntry>(entry_id).map(|entry| {
      let (pointer, _) = (entry as *const dyn TraitMapEntry).to_raw_parts();
      unsafe { &*(pointer as *const T) }
    }))
  }

  /// Get a specific entry and downcast to a mutable reference of its concrete type.
  ///
  /// # Errors
  /// Returns `None` if the entry no longer exists in the map.
  ///
  /// Returns `Some(None)` if the type parameter `T` does not match the concrete type.
  ///
  /// # Examples
  /// ```
  /// let entry_id = map.add_entry(MyStruct { /* ... */ });
  /// // ...
  /// let my_struct: Option<Option<&mut MyStruct>> = map.try_get_entry_downcast_mut::<MyStruct>(entry_id);
  pub fn try_get_entry_downcast_mut<T: TraitMapEntry + 'static>(
    &mut self,
    entry_id: EntryID,
  ) -> Option<Option<&mut T>> {
    // Make sure the downcast is valid
    if self.get_entry_type(entry_id)? != TypeId::of::<T>() {
      return Some(None);
    }

    Some(self.get_entry_mut::<dyn TraitMapEntry>(entry_id).map(|entry| {
      let (pointer, _) = (entry as *mut dyn TraitMapEntry).to_raw_parts();
      unsafe { &mut *(pointer as *mut T) }
    }))
  }

  /// Remove an entry from the map as its concrete type.
  /// If the downcast is invalid, the entry will not be removed from the map.
  ///
  /// # Errors
  /// Returns `None` if the entry no longer exists in the map.
  ///
  /// Returns `Some(None)` if the type parameter `T` does not match the concrete type.
  /// If this happens the type will **not** be removed from the map.
  ///
  /// # Examples
  /// ```
  /// let entry_id = map.add_entry(MyStruct { /* ... */ });
  /// // ...
  /// let my_struct: Option<Option<MyStruct>> = map.try_take_entry_downcast::<MyStruct>(entry_id);
  pub fn try_take_entry_downcast<T: TraitMapEntry + 'static>(&mut self, entry_id: EntryID) -> Option<Option<T>> {
    // Make sure the downcast is valid
    if self.get_entry_type(entry_id)? != TypeId::of::<T>() {
      return Some(None);
    }

    let entry = self
      .traits
      .borrow_mut()
      .get_mut(&TypeId::of::<dyn TraitMapEntry>())
      .and_then(|traits| traits.remove_entry(&entry_id))
      .map(|(_, pointer)| *unsafe { Box::from_raw(pointer.pointer as *mut T) })?;

    // Safe: we already removed the entry from <dyn TraitMapEntry> so it won't be double freed
    self.remove_entry(entry_id);

    Some(Some(entry))
  }
}

impl Drop for TraitMap {
  fn drop(&mut self) {
    if let Some(traits) = self.traits.borrow().get(&TypeId::of::<dyn TraitMapEntry>()) {
      for pointer in traits.values() {
        drop(unsafe { pointer.into_boxed::<dyn TraitMapEntry>() })
      }
    }
  }
}

/// Stores a "*mut dyn Trait" inside a fixed-size struct
#[derive(Debug)]
struct PointerWithMetadata {
  pointer: *mut (),
  boxed_metadata: Box<*const ()>,
}

impl PointerWithMetadata {
  /// Construct from a raw data pointer and BoxedMetadata
  #[inline]
  pub fn new(pointer: *mut (), boxed_metadata: Box<*const ()>) -> Self {
    Self {
      pointer,
      boxed_metadata,
    }
  }

  /// Construct a PointerWithMetadata from a trait pointer
  pub fn from_trait_pointer<T, Trait>(pointer: *mut T) -> Self
  where
    // Ensure that Trait is a valid "dyn Trait" object
    Trait: ?Sized + Pointee<Metadata = DynMetadata<Trait>> + 'static,
    // Allows us to cast from *mut T to *mut dyn Trait using "as"
    T: Unsize<Trait>,
  {
    let (pointer, metadata) = (pointer as *mut Trait).to_raw_parts();
    let boxed_metadata = unsafe { transmute(Box::new(metadata)) };

    Self::new(pointer, boxed_metadata)
  }

  /// Cast this pointer into `Box<dyn Trait>`.
  ///
  /// This will result in undefined behavior if the Trait does not match
  ///  the one used to construct this pointer.
  pub unsafe fn into_boxed<Trait>(&self) -> Box<Trait>
  where
    // Ensure that Trait is a valid "dyn Trait" object
    Trait: ?Sized + Pointee<Metadata = DynMetadata<Trait>> + 'static,
  {
    Box::from_raw(self.reconstruct_ptr())
  }

  /// Cast this pointer into `&dyn Trait`.
  ///
  /// This will result in undefined behavior if the Trait does not match
  ///  the one used to construct this pointer.
  pub unsafe fn reconstruct_ref<'a, Trait>(&self) -> &'a Trait
  where
    // Ensure that Trait is a valid "dyn Trait" object
    Trait: ?Sized + Pointee<Metadata = DynMetadata<Trait>> + 'static,
  {
    &*self.reconstruct_ptr()
  }

  /// Cast this pointer into `&mut dyn Trait`.
  ///
  /// This will result in undefined behavior if the Trait does not match
  ///  the one used to construct this pointer.
  pub unsafe fn reconstruct_mut<'a, Trait>(&self) -> &'a mut Trait
  where
    // Ensure that Trait is a valid "dyn Trait" object
    Trait: ?Sized + Pointee<Metadata = DynMetadata<Trait>> + 'static,
  {
    &mut *self.reconstruct_ptr()
  }

  /// Cast this pointer into *mut dyn Trait.
  /// This function is where the real black magic happens!
  ///
  /// This will result in undefined behavior if the Trait does not match
  ///  the one used to construct this pointer.
  pub fn reconstruct_ptr<Trait>(&self) -> *mut Trait
  where
    // Ensure that Trait is a valid "dyn Trait" object
    Trait: ?Sized + Pointee<Metadata = DynMetadata<Trait>> + 'static,
  {
    let metadata: <Trait as Pointee>::Metadata =
      unsafe { *transmute::<_, *const <Trait as Pointee>::Metadata>(self.boxed_metadata.as_ref()) };
    ptr::from_raw_parts_mut::<Trait>(self.pointer, metadata)
  }
}

impl<'a> Context<'a> {
  /// Downcast into the concrete [TypedContext].
  ///
  /// # Panics
  ///
  /// This method panics if the type parameter `T` does not match the concrete type.
  ///
  /// # Examples
  ///
  /// ```
  /// impl TraitMapEntry for MyStruct {
  ///   fn on_create<'a>(&mut self, context: Context<'a>) {
  ///     context
  ///      .downcast::<Self>()
  ///      .add_trait::<dyn ExampleTrait>()
  ///      .add_trait::<dyn ExampleTraitTwo>();
  ///   }
  /// }
  /// ```
  pub fn downcast<T>(self) -> TypedContext<'a, T>
  where
    T: 'static,
  {
    self.try_downcast::<T>().expect("Invalid downcast")
  }

  /// Try to downcast into a concrete [TypedContext].
  ///
  /// # Errors
  ///
  /// Returns `None` if the type parameter `T` does not match the concrete type.
  ///
  /// # Examples
  ///
  /// ```
  /// impl TraitMapEntry for MyStruct {
  ///   fn on_create<'a>(&mut self, context: Context<'a>) {
  ///     if let Some(context) = context.try_downcast::<Self>() {
  ///       context
  ///        .add_trait::<dyn ExampleTrait>()
  ///        .add_trait::<dyn ExampleTraitTwo>();
  ///     }
  ///   }
  /// }
  /// ```
  pub fn try_downcast<T>(self) -> Result<TypedContext<'a, T>, Self>
  where
    T: 'static,
  {
    if self.type_id != TypeId::of::<T>() {
      Err(self)
    } else {
      Ok(TypedContext {
        entry_id: self.entry_id,
        pointer: self.pointer.cast(),
        traits: self.traits,
      })
    }
  }
}

impl<'a, Entry> TypedContext<'a, Entry>
where
  Entry: 'static,
{
  /// Convert back into an untyped [Context].
  pub fn upcast(self) -> Context<'a> {
    Context {
      entry_id: self.entry_id,
      pointer: self.pointer.cast(),
      type_id: TypeId::of::<Entry>(),
      traits: self.traits,
    }
  }

  /// Add a trait to the type map.
  /// This method is idempotent, so adding a trait multiple times will only register it once.
  ///
  /// By default, every type is associated with the [TraitMapEntry] trait.
  ///
  /// # Examples
  ///
  /// ```
  /// impl TraitMapEntry for MyStruct {
  ///   fn on_create<'a>(&mut self, context: Context<'a>) {
  ///     context
  ///      .downcast::<Self>()
  ///      .add_trait::<dyn ExampleTrait>()
  ///      .add_trait::<dyn ExampleTraitTwo>();
  ///   }
  /// }
  /// ```
  pub fn add_trait<Trait>(&mut self) -> &mut Self
  where
    // Ensure that Trait is a valid "dyn Trait" object
    Trait: ?Sized + Pointee<Metadata = DynMetadata<Trait>> + 'static,
    // Allows us to cast from &T to &dyn Trait using "as"
    Entry: Unsize<Trait>,
  {
    let type_id = TypeId::of::<Trait>();
    let pointer = PointerWithMetadata::from_trait_pointer::<Entry, Trait>(self.pointer.as_ptr());

    let traits = self.traits.entry(type_id).or_default();
    traits.insert(self.entry_id, pointer);

    self
  }

  /// Remove a trait from the type map.
  /// This method is idempotent, so removing a trait multiple times is a no-op.
  ///
  /// By default, every type is associated with the [TraitMapEntry] trait.
  /// As such, this trait **cannot** be removed from an entry.
  /// Trying to call `.remove_trait::<dyn TraitMapEntry>()` is a no-op.
  ///
  /// # Examples
  ///
  /// ```
  /// impl TraitMapEntry for MyStruct {
  ///   // ...
  ///
  ///   fn on_update<'a>(&mut self, context: Context<'a>) {
  ///     context
  ///      .downcast::<Self>()
  ///      .remove_trait::<dyn ExampleTrait>()
  ///      .remove_trait::<dyn ExampleTraitTwo>();
  ///   }
  /// }
  /// ```
  pub fn remove_trait<Trait>(&mut self) -> &mut Self
  where
    // Ensure that Trait is a valid "dyn Trait" object
    Trait: ?Sized + Pointee<Metadata = DynMetadata<Trait>> + 'static,
    // Allows us to cast from &T to &dyn Trait using "as"
    Entry: Unsize<Trait>,
  {
    let type_id = TypeId::of::<Trait>();

    // Special case: we are not allowed to remove "dyn TraitMapEntry" as a trait
    //  This may cause a memory leak in our system and will mess up the .all_entries() method
    if type_id == TypeId::of::<dyn TraitMapEntry>() {
      return self;
    }

    if let Some(traits) = self.traits.get_mut(&type_id) {
      traits.remove(&self.entry_id);
    }

    self
  }

  /// Test if the trait is registered with the type map.
  ///
  /// # Examples
  ///
  /// ```
  /// impl TraitMapEntry for MyStruct {
  ///   // ...
  ///
  ///   fn on_update<'a>(&mut self, context: Context<'a>) {
  ///     let mut context = context.downcast::<Self>();
  ///     if !context.has_trait::<dyn ExampleTrait>() {
  ///       context.add_trait<dyn ExampleTrait>();
  ///     }
  ///   }
  /// }
  /// ```
  pub fn has_trait<Trait>(&self) -> bool
  where
    // Ensure that Trait is a valid "dyn Trait" object
    Trait: ?Sized + Pointee<Metadata = DynMetadata<Trait>> + 'static,
    // Allows us to cast from &T to &dyn Trait using "as"
    Entry: Unsize<Trait>,
  {
    let type_id = TypeId::of::<Trait>();
    self
      .traits
      .get(&type_id)
      .map(|traits| traits.contains_key(&self.entry_id))
      .unwrap_or(false)
  }
}

#[cfg(test)]
mod test {
  use super::*;

  // Required because we are deriving in the same crate that TraitMapEntry is defined
  extern crate self as trait_map;

  trait TraitOne {
    fn add_with_offset(&self, a: u32, b: u32) -> u32;
    fn mul_with_mut(&mut self, a: u32, b: u32) -> u32;
  }

  trait TraitTwo {
    fn compute(&self) -> f64;
  }

  trait TraitThree {
    fn unused(&self) -> (i8, i8);
  }

  struct OneAndTwo {
    offset: u32,
    compute: f64,
    on_create_fn: Option<Box<dyn FnMut(&mut Self, Context) -> ()>>,
    on_update_fn: Option<Box<dyn FnMut(&mut Self, Context) -> ()>>,
  }

  impl OneAndTwo {
    pub fn new(offset: u32, compute: f64) -> Self {
      Self {
        offset,
        compute,
        on_create_fn: Some(Box::new(|_, context| {
          context
            .downcast::<Self>()
            .add_trait::<dyn TraitOne>()
            .add_trait::<dyn TraitTwo>();
        })),
        on_update_fn: None,
      }
    }
  }

  struct TwoOnly {
    compute: f64,
    on_create_fn: Option<Box<dyn FnMut(&mut Self, Context) -> ()>>,
    on_update_fn: Option<Box<dyn FnMut(&mut Self, Context) -> ()>>,
  }

  impl TwoOnly {
    pub fn new(compute: f64) -> Self {
      Self {
        compute,
        on_create_fn: Some(Box::new(|_, context| {
          context.downcast::<Self>().add_trait::<dyn TraitTwo>();
        })),
        on_update_fn: None,
      }
    }
  }

  #[derive(TraitMapEntry)]
  #[trait_map(TraitTwo)]
  struct DeriveTest {
    output: Box<f64>,
  }

  impl TraitOne for OneAndTwo {
    fn add_with_offset(&self, a: u32, b: u32) -> u32 {
      a + b + self.offset
    }

    fn mul_with_mut(&mut self, a: u32, b: u32) -> u32 {
      self.offset = a * b;
      a + b + self.offset
    }
  }

  impl TraitTwo for OneAndTwo {
    fn compute(&self) -> f64 {
      self.compute
    }
  }

  impl TraitTwo for TwoOnly {
    fn compute(&self) -> f64 {
      self.compute * self.compute
    }
  }

  impl TraitTwo for DeriveTest {
    fn compute(&self) -> f64 {
      *self.output
    }
  }

  impl TraitMapEntry for OneAndTwo {
    fn on_create<'a>(&mut self, context: Context<'a>) {
      if let Some(mut on_create_fn) = self.on_create_fn.take() {
        on_create_fn(self, context);
        self.on_create_fn = Some(on_create_fn);
      }
    }

    fn on_update<'a>(&mut self, context: Context<'a>) {
      if let Some(mut on_update_fn) = self.on_update_fn.take() {
        on_update_fn(self, context);
        self.on_update_fn = Some(on_update_fn);
      }
    }
  }

  impl TraitMapEntry for TwoOnly {
    fn on_create<'a>(&mut self, context: Context<'a>) {
      if let Some(mut on_create_fn) = self.on_create_fn.take() {
        on_create_fn(self, context);
        self.on_create_fn = Some(on_create_fn);
      }
    }

    fn on_update<'a>(&mut self, context: Context<'a>) {
      if let Some(mut on_update_fn) = self.on_update_fn.take() {
        on_update_fn(self, context);
        self.on_update_fn = Some(on_update_fn);
      }
    }
  }

  #[test]
  fn test_adding_and_queries_traits() {
    let mut map = TraitMap::new();
    let entry_one_id = map.add_entry(OneAndTwo::new(3, 10.0));
    let entry_two_id = map.add_entry(TwoOnly::new(10.0));

    assert_eq!(map.all_entries().len(), 2);

    // Test the first trait
    let entries = map.get_entities_mut::<dyn TraitOne>();
    assert_eq!(entries.len(), 1);
    for (entry_id, entry) in entries.into_iter() {
      assert_eq!(entry_id, entry_one_id);
      assert_eq!(entry.add_with_offset(1, 2), 6);
      assert_eq!(entry.mul_with_mut(1, 2), 5);
      assert_eq!(entry.add_with_offset(1, 2), 5);
    }

    // Test the second trait
    let entries = map.get_entities::<dyn TraitTwo>();
    let entry_one = entries.get(&entry_one_id);
    let entry_two = entries.get(&entry_two_id);
    assert_eq!(entries.len(), 2);
    assert!(entry_one.is_some());
    assert_eq!(entry_one.unwrap().compute(), 10.0);
    assert!(entry_two.is_some());
    assert_eq!(entry_two.unwrap().compute(), 100.0);
  }

  #[test]
  fn test_removing_traits() {
    let mut map = TraitMap::new();
    let mut entry = OneAndTwo::new(3, 10.0);
    entry.on_update_fn = Some(Box::new(|_, context| {
      context.downcast::<OneAndTwo>().remove_trait::<dyn TraitOne>();
    }));
    let entry_id = map.add_entry(entry);

    assert_eq!(map.get_entities::<dyn TraitOne>().len(), 1);
    assert_eq!(map.get_entities::<dyn TraitTwo>().len(), 1);

    map.update_entry(entry_id);

    assert_eq!(map.get_entities::<dyn TraitOne>().len(), 0);
    assert_eq!(map.get_entities::<dyn TraitTwo>().len(), 1);
  }

  #[test]
  fn test_adding_and_removing_entry() {
    let mut map = TraitMap::new();
    let entry_one_id = map.add_entry(TwoOnly::new(10.0));
    let entry_two_id = map.add_entry(TwoOnly::new(20.0));
    let entry_three_id = map.add_entry(TwoOnly::new(30.0));

    assert_eq!(map.get_entities::<dyn TraitTwo>().len(), 3);
    assert!(map.get_entry::<dyn TraitTwo>(entry_one_id).is_some());
    assert!(map.get_entry::<dyn TraitTwo>(entry_two_id).is_some());
    assert!(map.get_entry::<dyn TraitTwo>(entry_three_id).is_some());

    map.remove_entry(entry_two_id);

    assert_eq!(map.get_entities::<dyn TraitTwo>().len(), 2);
    assert!(map.get_entry::<dyn TraitTwo>(entry_one_id).is_some());
    assert!(map.get_entry::<dyn TraitTwo>(entry_two_id).is_none());
    assert!(map.get_entry::<dyn TraitTwo>(entry_three_id).is_some());

    let entry_four_id = map.add_entry(TwoOnly::new(40.0));

    assert_eq!(map.get_entities::<dyn TraitTwo>().len(), 3);
    assert!(map.get_entry::<dyn TraitTwo>(entry_one_id).is_some());
    assert!(map.get_entry::<dyn TraitTwo>(entry_two_id).is_none());
    assert!(map.get_entry::<dyn TraitTwo>(entry_three_id).is_some());
    assert!(map.get_entry::<dyn TraitTwo>(entry_four_id).is_some());
  }

  #[test]
  #[should_panic]
  fn test_context_invalid_downcast_panics() {
    let mut map = TraitMap::new();
    let mut entry = OneAndTwo::new(3, 10.0);
    entry.on_create_fn = Some(Box::new(|_, context| {
      context.downcast::<TwoOnly>().add_trait::<dyn TraitTwo>();
    }));
    map.add_entry::<OneAndTwo>(entry);
  }

  #[test]
  fn test_get_entry() {
    let mut map = TraitMap::new();
    let entry_one_id = map.add_entry(TwoOnly::new(10.0));
    let entry_two_id = map.add_entry(OneAndTwo::new(1, 20.0));

    assert!(map.get_entry::<dyn TraitOne>(entry_one_id).is_none()); // Doesn't implement trait
    assert!(map.get_entry::<dyn TraitTwo>(entry_one_id).is_some());
    assert!(map.get_entry::<dyn TraitThree>(entry_one_id).is_none()); // Doesn't implement trait
    assert!(map.get_entry_mut::<dyn TraitOne>(entry_two_id).is_some());
    assert!(map.get_entry_mut::<dyn TraitTwo>(entry_two_id).is_some());
    assert!(map.get_entry_mut::<dyn TraitThree>(entry_two_id).is_none()); // Doesn't implement trait
  }

  #[test]
  #[should_panic]
  fn test_get_entry_invalid_downcast_panics() {
    let mut map = TraitMap::new();
    let entry_id = map.add_entry(OneAndTwo::new(1, 4.5));

    map.get_entry_downcast::<TwoOnly>(entry_id);
  }

  #[test]
  fn test_take_entry_downcast() {
    let mut map = TraitMap::new();
    let entry_id = map.add_entry(OneAndTwo::new(1, 4.5));

    let take = map.take_entry_downcast::<OneAndTwo>(entry_id);
    assert!(take.is_some());
    assert_eq!(take.unwrap().offset, 1);
  }

  #[test]
  #[should_panic]
  fn test_take_entry_invalid_downcast_panics() {
    let mut map = TraitMap::new();
    let entry_id = map.add_entry(OneAndTwo::new(1, 4.5));

    map.take_entry_downcast::<TwoOnly>(entry_id);
  }

  #[test]
  fn test_cannot_remove_trait_map_entry() {
    let mut map = TraitMap::new();
    let mut entry = OneAndTwo::new(3, 10.0);
    entry.on_update_fn = Some(Box::new(|_, context| {
      context
        .downcast::<OneAndTwo>()
        .remove_trait::<dyn TraitOne>()
        .remove_trait::<dyn TraitMapEntry>(); // Try to remove "dyn TraitMapEntry"
    }));
    let entry_id = map.add_entry(entry);
    map.add_entry(TwoOnly::new(1.5));

    assert_eq!(map.all_entries().len(), 2);
    assert_eq!(map.get_entities::<dyn TraitOne>().len(), 1);
    assert_eq!(map.get_entities::<dyn TraitTwo>().len(), 2);

    map.update_entry(entry_id);

    assert_eq!(map.all_entries().len(), 2);
    assert_eq!(map.get_entities::<dyn TraitOne>().len(), 0);
    assert_eq!(map.get_entities::<dyn TraitTwo>().len(), 2);
  }
}
