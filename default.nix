let
    pkgs = import <nixpkgs> {};
in
{
    hellothere = pkgs.callPackage ./package_it.nix { };
}