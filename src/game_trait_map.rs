use std::any::TypeId;
use std::collections::{HashMap, HashSet};
use std::marker::Unsize;
use std::mem::transmute;
use std::ptr::{self, DynMetadata, Pointee};

pub trait Entity {}

pub trait ConstructableEntity: Entity {
  fn on_create<'a>(&mut self, context: Context<'a, Self>);
}

#[derive(Debug, Clone, Default)]
pub struct GameTraitMap {
  entities: HashSet<*mut dyn Entity>,
  traits: HashMap<TypeId, HashMap<*const (), Box<*const ()>>>,
}

pub struct Context<'a, E: ?Sized> {
  pointer: *const E,
  traits: &'a mut HashMap<TypeId, HashMap<*const (), Box<*const ()>>>,
}

impl GameTraitMap {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn add_entity<E: ConstructableEntity + 'static>(&mut self, entity: E) {
    let entity_pointer = Box::leak(Box::new(entity));
    let entity_trait = entity_pointer as *mut dyn Entity;
    self.entities.insert(entity_trait);

    entity_pointer.on_create(Context {
      pointer: entity_pointer,
      traits: &mut self.traits,
    });
  }

  pub fn get_entities<Trait>(&self) -> Vec<&Trait>
  where
    // Ensure that Trait is a valid "dyn Trait" object
    Trait: ?Sized + Pointee<Metadata = DynMetadata<Trait>> + 'static,
  {
    let type_id = TypeId::of::<Trait>();
    self
      .traits
      .get(&type_id)
      .map(|traits| {
        traits
          .iter()
          .map(|(p_ptr, p_metadata)| unsafe {
            let metadata: <Trait as Pointee>::Metadata = *transmute::<_, &<Trait as Pointee>::Metadata>(&**p_metadata);
            &*ptr::from_raw_parts(*p_ptr, metadata)
          })
          .collect()
      })
      .unwrap_or_default()
  }
}

impl Drop for GameTraitMap {
  fn drop(&mut self) {
    for entity in self.entities.iter() {
      unsafe {
        drop(Box::from_raw(*entity));
      }
    }
  }
}

impl<'a, E> Context<'a, E> {
  pub fn add_trait<Trait>(&mut self) -> &mut Self
  where
    // Ensure that Trait is a valid "dyn Trait" object
    Trait: ?Sized + Pointee<Metadata = DynMetadata<Trait>> + 'static,
    // Allows us to cast from &T to &dyn Trait using "as"
    E: Unsize<Trait>,
  {
    let type_id = TypeId::of::<Trait>();
    let pointer: *const Trait = self.pointer as *const Trait;

    let (pointer, metadata) = pointer.to_raw_parts();
    let metadata = unsafe { transmute(Box::new(metadata)) };

    let traits = self.traits.entry(type_id).or_default();
    traits.insert(pointer, metadata);

    self
  }
}
