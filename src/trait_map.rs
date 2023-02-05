use std::any::TypeId;
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
  traits: HashMap<TypeId, PointerMetadataMap>,
}

/// Must be cast to a `TypeContext` using the `.downcast()` method.
#[derive(Debug)]
pub struct Context<'a> {
  pointer: NonNull<()>,
  type_id: TypeId,
  traits: &'a mut HashMap<TypeId, PointerMetadataMap>,
}

#[derive(Debug)]
pub struct TypedContext<'a, E: ?Sized> {
  pointer: NonNull<E>,
  traits: &'a mut HashMap<TypeId, PointerMetadataMap>,
}

impl TraitMap {
  pub fn new() -> Self {
    Self::default()
  }

  /// Add an entry to the map.
  pub fn add_entry<Entry: TraitMapEntry + 'static>(&mut self, entry: Entry) -> EntryID {
    let entry_pointer = Box::leak(Box::new(entry));

    // All entities get the dyn entity trait by default
    //  This stores the unique ownership of the object
    let mut context = TypedContext {
      pointer: entry_pointer.into(),
      traits: &mut self.traits,
    };
    context.add_trait::<dyn TraitMapEntry>();

    // Add any other traits as required
    entry_pointer.on_create(context.upcast());

    EntryID(unsafe { NonNull::new_unchecked((entry_pointer as *mut Entry).to_raw_parts().0) })
  }

  /// Remove an entry from the map using its unique ID.
  /// Returns `true` if the entry was removed, or `false` otherwise.
  pub fn remove_entry(&mut self, entry_id: EntryID) -> bool {
    let mut removed = false;

    // Drop the entry from the map
    if let Some((pointer, metadata)) = self
      .traits
      .get(&TypeId::of::<dyn TraitMapEntry>())
      .and_then(|traits| traits.get_key_value(&entry_id.0))
    {
      drop(unsafe { PointerWithMetadata::new(pointer.as_ptr(), metadata.clone()).into_boxed::<dyn TraitMapEntry>() });
      removed = true;
    }

    // Also remove any trait references to the entry
    let (pointer, _) = entry_id.0.to_raw_parts();
    for traits in self.traits.values_mut() {
      traits.remove(&pointer);
    }

    removed
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
      .get(&TypeId::of::<Trait>())
      .and_then(|traits| traits.get_key_value(&entry_id.0))
      .map(|(p_ptr, p_metadata)| unsafe {
        PointerWithMetadata::new(p_ptr.as_ptr(), p_metadata.clone()).reconstruct_mut()
      })
  }
}

impl Drop for TraitMap {
  fn drop(&mut self) {
    if let Some(traits) = self.traits.get(&TypeId::of::<dyn TraitMapEntry>()) {
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
