use std::{collections::BTreeSet, fmt};

use super::{Container, TyConstuctor, TyConstuctorIncomplete, TyName};

#[derive(Debug, Clone)]
pub struct Struct<T> {
    pub name: TyName,
    pub contents: Container<T>,
}

impl<T> Struct<T> {
    pub fn name(&self) -> TyName {
        self.name.clone()
    }
}

impl Struct<TyConstuctor> {
    pub fn ty_constructor(&self) -> TyConstuctor {
        let mut params = BTreeSet::new();
        match &self.contents {
            Container::Tuple(fields) => {
                for ty_const in fields {
                    params.extend(ty_const.lifetime_param.clone())
                }
            }
            Container::Named(fields) => {
                for ty_const in fields.values() {
                    params.extend(ty_const.lifetime_param.clone())
                }
            }
        }

        let mut params = params.into_iter().collect::<Vec<_>>();
        params.sort();

        TyConstuctor::new_simple(self.name(), params)
    }
}

impl Struct<TyConstuctorIncomplete> {
    pub fn next_incomplete(&self) -> Option<&TyConstuctorIncomplete> {
        self.contents.next_incomplete()
    }

    pub fn next_incomplete_mut(&mut self) -> Option<&mut TyConstuctorIncomplete> {
        self.contents.next_incomplete_mut()
    }

    pub fn into_completed(&mut self) -> Result<Struct<TyConstuctor>, &mut TyConstuctorIncomplete> {
        self.contents.into_completed().map(|x| Struct {
            name: self.name.clone(),
            contents: x,
        })
    }
}

impl fmt::Display for Struct<TyConstuctor> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "pub struct {}", self.ty_constructor())?;
        let tail = match self.contents {
            Container::Tuple(_) => ";",
            Container::Named(_) => "",
        };
        write!(f, "{}{}", self.contents, tail)?;
        Ok(())
    }
}
