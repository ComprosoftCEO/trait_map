use std::any::TypeId;
use std::collections::{HashMap, HashSet};
use std::mem::transmute;
use std::ptr::{self, Pointee};

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

  pub fn get_entities<T>(&self) -> Vec<&T>
  where
    T: ?Sized + 'static,
  {
    let type_id = TypeId::of::<T>();
    self
      .traits
      .get(&type_id)
      .map(|traits| {
        traits
          .iter()
          .map(|(p_ptr, p_metadata)| unsafe {
            let metadata: <T as Pointee>::Metadata = *transmute::<_, &<T as Pointee>::Metadata>(&**p_metadata);
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
  pub fn add_trait<'b, T>(&mut self) -> &mut Self
  where
    T: ?Sized + 'static,
    E: 'b,
    &'b E: Into<&'b T>,
  {
    let type_id = TypeId::of::<T>();
    let pointer: *const T = unsafe { &*self.pointer }.into();

    let (pointer, metadata) = pointer.to_raw_parts();
    let metadata = unsafe { transmute(Box::new(metadata)) };

    let traits = self.traits.entry(type_id).or_default();
    traits.insert(pointer, metadata);

    self
  }
}

macro_rules! export_trait {
  ($trait:ident) => {
    impl<'a, 'b, T> From<&'a T> for &'b dyn $trait
    where
      T: $trait,
      'a: 'b,
    {
      fn from(value: &'a T) -> Self {
        value
      }
    }
  };
}

pub(crate) use export_trait;
