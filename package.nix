{ npmlock2nix, pkgs, name, ... }:
{ ps-pkgs, ... }:
  with ps-pkgs;
  { version = "1.0.0";
    dependencies =
      [ aff-promise
        css
        options
        web-uievents
      ];
    src = "src";
    foreign."XTerm.Terminal".node_modules =
      npmlock2nix.v2.node_modules { src = ./.; } + /node_modules;
    foreign."XTerm.Addons.WebGL".node_modules =
      npmlock2nix.v2.node_modules { src = ./.; } + /node_modules;
    foreign."XTerm.Addons.Fit".node_modules =
      npmlock2nix.v2.node_modules { src = ./.; } + /node_modules;
    foreign."XTerm.Addons.WebLinks".node_modules =
      npmlock2nix.v2.node_modules { src = ./.; } + /node_modules;

    pursuit = {
      inherit name; 
      repo = "https://github.com/grybiena/xterm.git";
      license = pkgs.lib.licenses.mit;
    };

  }
