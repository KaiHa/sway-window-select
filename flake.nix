{
  description = "A window selection tool for sway";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = (final: prev: {
        sway-window-select = final.haskellPackages.callCabal2nix "sway-window-select" ./. {};
      });
      packages = forAllSystems (system: {
         sway-window-select = nixpkgsFor.${system}.sway-window-select;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.sway-window-select);
      checks = self.packages;
  };
}
