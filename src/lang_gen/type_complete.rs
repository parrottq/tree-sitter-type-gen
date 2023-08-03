use super::TyConstuctorIncomplete;

pub trait IntoCompleted {
    type Result;
    fn into_completed(&mut self) -> Result<Self::Result, &mut TyConstuctorIncomplete>;
}
