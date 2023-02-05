#![feature(ptr_metadata)]
#![feature(unsize)]
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

use std::any::TypeId;
use std::cell::{RefCell, RefMut};
use std::collections::HashMap;
use std::marker::Unsize;
use std::mem::transmute;
use std::ptr::{self, DynMetadata, NonNull, Pointee};
use std::rc::Rc;

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
pub struct EntryID(NonNull<()>);

// Maps from the raw *mut T to the boxed metadata associated with the trait
type BoxedMetadata = Rc<*const ()>;
type PointerMetadataMap = HashMap<NonNull<()>, BoxedMetadata>;

/// Map structure that allows types to be dynamically queries by trait.
#[derive(Debug, Default)]
pub struct TraitMap {
  traits: RefCell<HashMap<TypeId, PointerMetadataMap>>,
  concrete_types: HashMap<NonNull<()>, TypeId>,
}

/// Stores type information about an entry inside of a [TraitMap]
///
/// Must be cast to a [TypedContext] using the [`.downcast()`](Context::downcast) or [`.try_downcast()`](Context::try_downcast)
/// methods for adding or removing traits from the map.
#[derive(Debug)]
pub struct Context<'a> {
  pointer: NonNull<()>,
  type_id: TypeId,
  traits: RefMut<'a, HashMap<TypeId, PointerMetadataMap>>,
}

/// Stores concrete type for an entry inside a [TraitMap]
///
/// It can be upcast to an untyped [Context] using the [`.upcast()`](TypedContext::upcast) method.
#[derive(Debug)]
pub struct TypedContext<'a, E: ?Sized> {
  pointer: NonNull<E>,
  traits: RefMut<'a, HashMap<TypeId, PointerMetadataMap>>,
}

impl TraitMap {
  pub fn new() -> Self {
    Self::default()
  }

  /// Add an entry to the map.
  pub fn add_entry<Entry: TraitMapEntry + 'static>(&mut self, entry: Entry) -> EntryID {
    let entry_ref = Box::leak(Box::new(entry));

    // Generate the EntryID
    let pointer: NonNull<Entry> = entry_ref.into();
    let entry_id = EntryID(pointer.cast());

    // Save the concrete type for downcasting later
    self.concrete_types.insert(entry_id.0, TypeId::of::<Entry>());

    // All entities get the dyn entity trait by default
    //  This stores the unique ownership of the object
    let mut context = TypedContext {
      pointer,
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
    if let Some((pointer, metadata)) = self
      .traits
      .borrow()
      .get(&TypeId::of::<dyn TraitMapEntry>())
      .and_then(|traits| traits.get_key_value(&entry_id.0))
    {
      drop(unsafe { PointerWithMetadata::new(pointer.as_ptr(), metadata.clone()).into_boxed::<dyn TraitMapEntry>() });
      removed = true;
    }

    // Drop the entry from the concrete types list
    self.concrete_types.remove(&entry_id.0);

    // Also remove any trait references to the entry
    let (pointer, _) = entry_id.0.to_raw_parts();
    for traits in self.traits.borrow_mut().values_mut() {
      traits.remove(&pointer);
    }

    removed
  }

  /// Call the [`on_update()`](TraitMapEntry::on_update) handler for an entry.
  ///
  /// Returns `true` if the entry exists and was updated, or `false` otherwise.
  pub fn update_entry(&mut self, entry_id: EntryID) -> bool {
    let type_id = self.concrete_types.get(&entry_id.0).cloned();
    let entry = self
      .traits
      .borrow()
      .get(&TypeId::of::<dyn TraitMapEntry>())
      .and_then(|traits| traits.get_key_value(&entry_id.0))
      .map(|(p_ptr, p_metadata)| unsafe {
        PointerWithMetadata::new(p_ptr.as_ptr(), p_metadata.clone()).reconstruct_mut::<dyn TraitMapEntry>()
      });

    if let (Some(entry), Some(type_id)) = (entry, type_id) {
      entry.on_update(Context {
        pointer: entry_id.0,
        type_id,
        traits: self.traits.borrow_mut(),
      });
      true
    } else {
      false
    }
  }

  /// Get the concrete type for an entry in the map
  pub fn get_entry_type(&self, entry_id: EntryID) -> Option<TypeId> {
    self.concrete_types.get(&entry_id.0).cloned()
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
          .map(|(p_ptr, p_metadata)| {
            (EntryID(*p_ptr), unsafe {
              PointerWithMetadata::new(p_ptr.as_ptr(), p_metadata.clone()).reconstruct_ref()
            })
          })
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
          .map(|(p_ptr, p_metadata)| {
            (EntryID(*p_ptr), unsafe {
              PointerWithMetadata::new(p_ptr.as_ptr(), p_metadata.clone()).reconstruct_mut()
            })
          })
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
      .and_then(|traits| traits.get_key_value(&entry_id.0))
      .map(|(p_ptr, p_metadata)| unsafe {
        PointerWithMetadata::new(p_ptr.as_ptr(), p_metadata.clone()).reconstruct_ref()
      })
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
      .and_then(|traits| traits.get_key_value(&entry_id.0))
      .map(|(p_ptr, p_metadata)| unsafe {
        PointerWithMetadata::new(p_ptr.as_ptr(), p_metadata.clone()).reconstruct_mut()
      })
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
      .and_then(|traits| traits.remove_entry(&entry_id.0))
      .map(|(ptr, _)| *unsafe { Box::from_raw(ptr.as_ptr() as *mut T) })?;

    // Safe: we already removed the entry from <dyn TraitMapEntry> so it won't be double freed
    self.remove_entry(entry_id);

    Some(Some(entry))
  }
}

impl Drop for TraitMap {
  fn drop(&mut self) {
    if let Some(traits) = self.traits.borrow().get(&TypeId::of::<dyn TraitMapEntry>()) {
      for (p_ptr, p_metadata) in traits {
        drop(unsafe { PointerWithMetadata::new(p_ptr.as_ptr(), p_metadata.clone()).into_boxed::<dyn TraitMapEntry>() })
      }
    }
  }
}

/// Stores a "*mut dyn Trait" inside a fixed-size struct
struct PointerWithMetadata {
  pointer: *mut (),
  boxed_metadata: BoxedMetadata,
}

impl PointerWithMetadata {
  /// Construct from a raw data pointer and BoxedMetadata
  pub fn new(pointer: *mut (), boxed_metadata: BoxedMetadata) -> Self {
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
    let boxed_metadata = unsafe { transmute(Rc::new(metadata)) };

    Self {
      pointer,
      boxed_metadata,
    }
  }

  /// Cast this pointer into `Box<dyn Trait>`.
  ///
  /// This will result in undefined behavior if the Trait does not match
  ///  the one used to construct this pointer.
  pub unsafe fn into_boxed<Trait>(self) -> Box<Trait>
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
  pub unsafe fn reconstruct_ref<'a, Trait>(self) -> &'a Trait
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
  pub unsafe fn reconstruct_mut<'a, Trait>(self) -> &'a mut Trait
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
  pub fn reconstruct_ptr<Trait>(self) -> *mut Trait
  where
    // Ensure that Trait is a valid "dyn Trait" object
    Trait: ?Sized + Pointee<Metadata = DynMetadata<Trait>> + 'static,
  {
    let metadata: <Trait as Pointee>::Metadata =
      *unsafe { transmute::<_, Rc<<Trait as Pointee>::Metadata>>(self.boxed_metadata) };
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
    self.try_downcast().expect("Invalid downcast")
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
      pointer: self.pointer.cast(),
      type_id: TypeId::of::<Entry>(),
      traits: self.traits,
    }
  }

  /// Register a trait to be saved into the type map
  pub fn add_trait<Trait>(&mut self) -> &mut Self
  where
    // Ensure that Trait is a valid "dyn Trait" object
    Trait: ?Sized + Pointee<Metadata = DynMetadata<Trait>> + 'static,
    // Allows us to cast from &T to &dyn Trait using "as"
    Entry: Unsize<Trait>,
  {
    let type_id = TypeId::of::<Trait>();
    let PointerWithMetadata {
      pointer,
      boxed_metadata,
    } = PointerWithMetadata::from_trait_pointer::<Entry, Trait>(self.pointer.as_ptr());

    let traits = self.traits.entry(type_id).or_default();
    traits.insert(unsafe { NonNull::new_unchecked(pointer) }, boxed_metadata);

    self
  }

  /// Unregister a trait from the type map
  pub fn remove_trait<Trait>(&mut self) -> &mut Self
  where
    // Ensure that Trait is a valid "dyn Trait" object
    Trait: ?Sized + Pointee<Metadata = DynMetadata<Trait>> + 'static,
    // Allows us to cast from &T to &dyn Trait using "as"
    Entry: Unsize<Trait>,
  {
    let type_id = TypeId::of::<Trait>();
    let (pointer, _) = (self.pointer.as_ptr() as *mut Trait).to_raw_parts();

    if let Some(traits) = self.traits.get_mut(&type_id) {
      traits.remove(&unsafe { NonNull::new_unchecked(pointer) });
    }

    self
  }
}
