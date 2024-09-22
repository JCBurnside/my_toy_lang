pub trait ExtraIterUtils: Iterator {
    fn advance_by(&mut self, n: usize) {
        for _ in 0..n {
            let _ = self.next();
        }
    }
}
impl<T: Iterator> ExtraIterUtils for T {}
