use std::process::Command;



fn main() {
    println!("cargo:rerun-if-changed=src/bridge.rs");
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=mlir_cpp_lib");
    let include_dirs = Command::new("llvm-config-20")
        .arg("--includedir")
        .output().unwrap()
        .stdout
        ;
    let binding = String::from_utf8(include_dirs).unwrap();
    println!("cargo:warning={}",binding);
    let include_dirs = binding.lines();
    let mut bridge = cxx_build::bridge("src/bridge.rs");
    bridge
        .includes(include_dirs)
        .include("mlir_cpp_lib/include")
        .include(format!("{}/build/include",std::env::var("OUT_DIR").unwrap()))
        .compile("bridge");
    let lib_path = cmake::Config::new("mlir_cpp_lib")
        .always_configure(true)
        .init_cxx_cfg(bridge)
        .build();
    println!("cargo:rustc-link-search=native={}/lib",lib_path.display());
    println!("cargo:rustc-link-lib=static=FflatMlir");
    println!("cargo:rustc-link-lib=static=bridge");

    let llvm_lib_dir = Command::new("llvm-config-20")
        .arg("--libdir")
        .output().unwrap()
        .stdout;
    let llvm_lib_dir = String::from_utf8(llvm_lib_dir).unwrap();
    println!("cargo:warning={llvm_lib_dir}");
    println!("cargo:rustc-link-search=native={llvm_lib_dir}");
    for file in  std::fs::read_dir(llvm_lib_dir.trim())
        .unwrap() 
        .filter(|file| if let Ok(file) = file {
            file.file_type().is_ok_and(|ty| ty.is_file())
        } else {
            false
        })
    {
        let Ok(file) = file else { unreachable!("handled by filter?") };
        if file.file_name().to_str().unwrap().ends_with(".a") {
            let file_name = file.file_name();
            // println!("cargo:warning={:?}", file_name.to_str());
            let processed_name = file_name.to_str().unwrap().strip_prefix("lib").unwrap().strip_suffix(".a");
            // println!("cargo:warning={processed_name:?}");
            println!("cargo:rustc-link-lib=static={}", processed_name.unwrap());
        }
    }
}