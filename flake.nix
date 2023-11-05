{
  description = "A command-line replacement for the Google Authenticator App";
  inputs.nixpkgs.url = "nixpkgs";
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = (final: prev: {
        gamgee = final.haskellPackages.callCabal2nix "gamgee" ./. { };
      });
      packages = forAllSystems (system: {
        gamgee = nixpkgsFor.${system}.gamgee;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.gamgee);
      checks = self.packages;
      devShell = forAllSystems (system:
        let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.gamgee ];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
          ];
          # Change the prompt to show that you are in a devShell
          shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
        });
    };
}
