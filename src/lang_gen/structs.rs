use std::{collections::BTreeSet, fmt};

use super::{Container, IntoCompleted, TyConstuctor, TyConstuctorIncomplete, TyName};

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
        let mut params = BTreeSet::new(); // Dedup and sort
        match &self.contents {
            Container::Tuple(fields) => {
                for ty_const in fields {
                    params.extend(ty_const.lifetime_param.as_ref())
                }
            }
            Container::Named(fields) => {
                for ty_const in fields.values() {
                    params.extend(ty_const.lifetime_param.as_ref())
                }
            }
        }

        let params = params.into_iter().collect::<Vec<char>>();

        TyConstuctor::new_simple(self.name(), params.into())
    }
}

impl Struct<TyConstuctorIncomplete> {
    pub fn next_incomplete(&self) -> Option<&TyConstuctorIncomplete> {
        self.contents.next_incomplete()
    }

    pub fn next_incomplete_mut(&mut self) -> Option<&mut TyConstuctorIncomplete> {
        self.contents.next_incomplete_mut()
    }
}

impl IntoCompleted for Struct<TyConstuctorIncomplete> {
    type Result = Struct<TyConstuctor>;

    fn into_completed(&mut self) -> Result<Self::Result, &mut TyConstuctorIncomplete> {
        match self.contents.into_completed() {
            Ok(x) => Ok(Struct {
                name: self.name.clone(),
                contents: x,
            }),
            Err(e) => Err(e),
        }
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
