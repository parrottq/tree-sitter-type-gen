use std::fmt;

use super::{ContainerDef, Impl, TyConstuctor, TyConstuctorIncomplete, TyName};

#[derive(Debug, Clone)]
pub struct TypeDef<T> {
    container: ContainerDef<T>,
    impls: Vec<Impl>,
}

impl<T> TypeDef<T> {
    pub fn new(container: ContainerDef<T>) -> Self {
        Self {
            container,
            impls: vec![],
        }
    }

    pub fn name(&self) -> TyName {
        self.container.name()
    }

    pub fn push_impl(&mut self, imp: Impl) {
        self.impls.push(imp);
    }
}

impl TypeDef<TyConstuctorIncomplete> {
    pub fn into_completed(&mut self) -> Result<TypeDef<TyConstuctor>, &mut TyConstuctorIncomplete> {
        let converted_container = self.container.into_completed()?;

        let mut converted_impls = Vec::with_capacity(self.impls.len());
        for imp in self.impls.iter_mut() {
            converted_impls.push(imp.clone());
        }

        Ok(TypeDef {
            container: converted_container,
            impls: converted_impls,
        })
    }
}

impl TypeDef<TyConstuctor> {
    pub fn ty_constructor(&self) -> TyConstuctor {
        self.container.ty_constructor()
    }
}

impl fmt::Display for TypeDef<TyConstuctor> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.container)?;
        for imp in self.impls.iter() {
            write!(f, "\n{}", imp.displayer(&self.ty_constructor()))?;
        }
        Ok(())
    }
}
