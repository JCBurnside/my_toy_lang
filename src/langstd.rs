use std::io::Write;

pub mod ops;

#[repr(C)]
pub struct MyStr {
    start: *const i8,
    end: *const i8,
}

#[no_mangle]
pub extern "C" fn print(arg0: MyStr) -> () {
    println!("IN PRINT");
    unsafe {
        let mut lock = std::io::stdout().lock();
        let mut curr = arg0.start;
        while curr != arg0.end {
            lock.write(&[*curr as u8]).unwrap();
            curr = curr.add(1);
        }
    }
    ()
}

#[no_mangle]
pub extern "C" fn put_char(c: i8) -> () {
    print!("{}", c as u8 as char);
}

#[no_mangle]
pub extern "C" fn put_int32(v: i32) -> () {
    print!("{}", v)
}
