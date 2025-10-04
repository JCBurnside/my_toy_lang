let 
    pkgs = import <nixpkgs> {};
    ffc = pkgs.callPackage ./package_it.nix { };
in
pkgs.mkShell {
    packages =  [
        ffc
    ];
}