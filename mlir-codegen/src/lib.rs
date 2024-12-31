mod bridge;

/// this is just used to ensure linking occurs;
fn _unused() {
    bridge::make_builder();
}

#[cfg(test)]
mod tests {
    #[test]
    fn t() {
        let mut builder = crate::bridge::make_builder();
        let x = builder.pin_mut().write_string();
        println!("{}",x);
        panic!(); 
    }
}