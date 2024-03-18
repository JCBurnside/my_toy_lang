use std::{rc::Rc, sync::Arc};

pub trait ExtraUtilFunctions: Sized {
    fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
    fn into_rc(self) -> Rc<Self> {
        Rc::new(self)
    }

    fn into_arc(self) -> Arc<Self> {
        Arc::new(self)
    }
}
impl<T> ExtraUtilFunctions for T {}

pub trait ExtraIterUtils: Iterator {
    fn advance_by(&mut self, n: usize) {
        for _ in 0..n {
            let _ = self.next();
        }
    }
}
impl<T: Iterator> ExtraIterUtils for T {}
