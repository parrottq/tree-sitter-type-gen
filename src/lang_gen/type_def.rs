use std::fmt;

use super::{ContainerDef, Impl, IntoCompleted, TyConstuctor, TyConstuctorIncomplete, TyName};

#[derive(Debug, Clone)]
pub struct TypeDef<T> {
    attr: Vec<&'static str>,
    container: ContainerDef<T>,
    impls: Vec<Impl<T>>,
}

impl<T> TypeDef<T> {
    pub fn new(container: ContainerDef<T>) -> Self {
        Self {
            attr: vec![],
            container,
            impls: vec![],
        }
    }

    pub fn name(&self) -> TyName {
        self.container.name()
    }

    pub fn push_impl(&mut self, imp: Impl<T>) {
        self.impls.push(imp);
    }

    pub fn push_attr(&mut self, attr: &'static str) {
        self.attr.push(attr);
    }
}

impl IntoCompleted for TypeDef<TyConstuctorIncomplete> {
    type Result = TypeDef<TyConstuctor>;

    fn into_completed(&mut self) -> Result<Self::Result, &mut TyConstuctorIncomplete> {
        let converted_container = self.container.into_completed()?;

        let mut converted_impls = Vec::with_capacity(self.impls.len());
        for imp in self.impls.iter_mut() {
            converted_impls.push(imp.into_completed()?);
        }

        Ok(TypeDef {
            attr: self.attr.clone(),
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
        for attr in self.attr.iter() {
            writeln!(f, "{}", attr)?;
        }
        write!(f, "{}", self.container)?;
        for imp in self.impls.iter() {
            write!(f, "\n{}", imp.displayer(&self.ty_constructor()))?;
        }
        Ok(())
    }
}
