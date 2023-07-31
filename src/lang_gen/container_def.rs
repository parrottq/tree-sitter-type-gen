use std::fmt;

use super::{Enum, Struct, TyConstuctor, TyConstuctorIncomplete, TyName};

#[derive(Debug, Clone)]
pub enum TypeDef<T> {
    Struct(Struct<T>),
    Enum(Enum<T>),
}

impl<T> TypeDef<T> {
    pub fn name(&self) -> TyName {
        match self {
            TypeDef::Struct(e) => e.name(),
            TypeDef::Enum(e) => e.name(),
        }
    }
}

impl TypeDef<TyConstuctor> {
    pub fn ty_constructor(&self) -> TyConstuctor {
        match self {
            TypeDef::Struct(e) => e.ty_constructor(),
            TypeDef::Enum(e) => e.ty_constructor(),
        }
    }
}

impl TypeDef<TyConstuctorIncomplete> {
    pub fn next_incomplete(&self) -> Option<&TyConstuctorIncomplete> {
        match self {
            TypeDef::Struct(e) => e.next_incomplete(),
            TypeDef::Enum(e) => e.next_incomplete(),
        }
    }

    pub fn next_incomplete_mut(&mut self) -> Option<&mut TyConstuctorIncomplete> {
        match self {
            TypeDef::Struct(e) => e.next_incomplete_mut(),
            TypeDef::Enum(e) => e.next_incomplete_mut(),
        }
    }

    pub fn into_completed(&mut self) -> Result<TypeDef<TyConstuctor>, &mut TyConstuctorIncomplete> {
        match self {
            TypeDef::Struct(e) => e.into_completed().map(Into::into),
            TypeDef::Enum(e) => e.into_completed().map(Into::into),
        }
    }
}

impl<T> From<Struct<T>> for TypeDef<T> {
    fn from(value: Struct<T>) -> Self {
        TypeDef::Struct(value)
    }
}

impl<T> From<Enum<T>> for TypeDef<T> {
    fn from(value: Enum<T>) -> Self {
        TypeDef::Enum(value)
    }
}

impl fmt::Display for TypeDef<TyConstuctor> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeDef::Struct(e) => write!(f, "{}", e),
            TypeDef::Enum(e) => write!(f, "{}", e),
        }
    }
}
