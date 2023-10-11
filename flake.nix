rec {
  description = "xterm";
  inputs = {
    env.url = "git+ssh://git@github.com/grybiena/purescript-environment?ref=grybiena";  
  };
  outputs = inputs@{ env, ... }:
    env.flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ ];

        pkgs = import env.nixpkgs {
          inherit system overlays;
          config.allowBroken = true;
        };

        ps-tools = env.ps-tools.legacyPackages.${system};
        purs-nix = env.purs-nix { inherit system; 
        };

        npmlock2nix = import env.npmlock2nix { inherit pkgs; };
        package = import ./package.nix { inherit pkgs npmlock2nix; } purs-nix;

        ps =
          purs-nix.purs { inherit (package) dependencies foreign test-dependencies;
                          dir = ./.;
                        };

      in 
         { packages.default =
             purs-nix.build
               { name = "xterm";
                 src.path = ./.;
                 info = package;
               };
           packages.output = ps.output {};
           devShells.default = 
             pkgs.mkShell
               { packages = with pkgs; [
                   nodejs
                   (ps.command {
                     bundle = {
                       esbuild = {
                         outfile = "test/main.js";
#                         minify = "true";
                         };
                       module = "Example.Main";
                     };
                   }) 
                   ps-tools.for-0_15.purescript-language-server
                   purs-nix.esbuild
                   purs-nix.purescript
                 ];             
               };
         }

   );
}


