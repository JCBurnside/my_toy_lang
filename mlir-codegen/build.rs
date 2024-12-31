use std::process::Command;



fn main() {
    println!("cargo:rerun-if-changed=src/bridge.rs");
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=mlir_cpp_lib");
    let include_dirs = Command::new("llvm-config-18")
        .arg("--includedir")
        .output().unwrap()
        .stdout
        ;
    let binding = String::from_utf8(include_dirs).unwrap();
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
}