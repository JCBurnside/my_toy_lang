{
    lib,
    rustPlatform,
    llvm
}:
rustPlatform.buildRustPackage(finalAttr: {
    pname="ffc";
    version="0.0.1";

    src = ./.;

    cargoHash = "sha256-gnr80+XvIFmZDjLgNxLeYOJkcTot5csEQhSX1jQdugc=";
})