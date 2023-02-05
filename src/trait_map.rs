use std::any::TypeId;
use std::cell::{RefCell, RefMut};
use std::collections::HashMap;
use std::marker::Unsize;
use std::mem::transmute;
use std::ptr::{self, DynMetadata, NonNull, Pointee};
use std::rc::Rc;

/// Any type to be stored in a TraitMap must implement this trait
pub trait TraitMapEntry: 'static {
  fn on_create<'a>(&mut self, context: Context<'a>);

  // Default implementation does nothing
  fn on_update<'a>(&mut self, _context: Context<'a>) {}
}

/// Unique ID for each entry in the trait map
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct EntryID(NonNull<()>);

// Maps from the raw *mut T to the boxed metadata associated with the trait
type BoxedMetadata = Rc<*const ()>;
type PointerMetadataMap = HashMap<NonNull<()>, BoxedMetadata>;

/// Map structure that allows types to be dynamically searched by trait
#[derive(Debug, Default)]
pub struct TraitMap {
  traits: RefCell<HashMap<TypeId, PointerMetadataMap>>,
  concrete_types: HashMap<NonNull<()>, TypeId>,
}

/// Must be cast to a `TypeContext` using the `.downcast()` method.
#[derive(Debug)]
pub struct Context<'a> {
  pointer: NonNull<()>,
  type_id: TypeId,
  traits: RefMut<'a, HashMap<TypeId, PointerMetadataMap>>,
}

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

  /// Call the `on_update()` handler for an entry.
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
    self.search_entities()
  }

  /// Get the list of all entries as a mutable reference
  pub fn all_entries_mut(&mut self) -> HashMap<EntryID, &mut dyn TraitMapEntry> {
    self.search_entities_mut()
  }

  /// Search for all entries that are registered with a specific trait.
  /// Returns an immutable reference.
  pub fn search_entities<Trait>(&self) -> HashMap<EntryID, &Trait>
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

  /// Search for all entries that are registered with a specific trait
  /// Returns a mutable reference.
  pub fn search_entities_mut<Trait>(&mut self) -> HashMap<EntryID, &mut Trait>
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

  /// Get a specific entry that implements a trait as an immutable reference
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

  /// Get a specific entry that implements a trait as a mutable reference
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

  /// Returns `None` if the entry does not exist.
  /// Panics if the downcast is invalid.
  pub fn get_entry_downcast<T: TraitMapEntry + 'static>(&self, entry_id: EntryID) -> Option<&T> {
    self
      .try_get_entry_downcast(entry_id)
      .map(|entry| entry.expect("Invalid downcast"))
  }

  /// Returns `None` if the entry does not exist.
  /// Panics if the downcast is invalid.
  pub fn get_entry_downcast_mut<T: TraitMapEntry + 'static>(&mut self, entry_id: EntryID) -> Option<&mut T> {
    self
      .try_get_entry_downcast_mut(entry_id)
      .map(|entry| entry.expect("Invalid downcast"))
  }

  /// Returns `None` if the entry does not exist.
  /// Panics if the downcast is invalid.
  pub fn take_entry_downcast<T: TraitMapEntry + 'static>(&mut self, entry_id: EntryID) -> Option<T> {
    self
      .try_take_entry_downcast(entry_id)
      .map(|entry| entry.expect("Invalid downcast"))
  }

  /// Returns `None` if the entry does not exist.
  /// Returns `Some(None)` if the downcast is invalid.
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

  /// Returns `None` if the entry does not exist.
  /// Returns `Some(None)` if the downcast is invalid.
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

  /// Returns `None` if the entry does not exist.
  /// Returns `Some(None)` if the downcast is invalid.
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
  /// Downcast into the concreate TypedContext.
  /// Panics if T is different from the concrete type.
  pub fn downcast<T>(self) -> TypedContext<'a, T>
  where
    T: 'static,
  {
    self.try_downcast().expect("Invalid downcast")
  }

  /// Try to downcast into a concrete TypedContext
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
  /// Convert back into an untyped context
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
