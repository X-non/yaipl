pub struct Interned<Brand>(u32, PhantomData<Brand>);

impl<Brand> std::fmt::Debug for Interned<Brand>
where
    Brand: Debug + Default,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple(&format!("Interned<{:?}>", Brand::default()))
            .field(&self.0)
            .finish()
    }
}

impl<Brand> Eq for Interned<Brand> {}

impl<Brand> PartialEq for Interned<Brand> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<Brand> std::hash::Hash for Interned<Brand> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}
use std::{fmt::Debug, marker::PhantomData};

impl<Brand> Copy for Interned<Brand> {}

impl<Brand> Clone for Interned<Brand> {
    fn clone(&self) -> Self {
        Self(self.0.clone(), PhantomData)
    }
}

impl<Brand> Interned<Brand> {
    #[inline]
    pub const fn new(value: u32) -> Self {
        Interned(value, PhantomData)
    }

    #[inline]
    pub const fn value(&self) -> u32 {
        self.0
    }
}
