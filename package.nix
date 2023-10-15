{ npmlock2nix, ... }:
{ ps-pkgs, ... }:
  with ps-pkgs;
  { version = "1.0.0";
    dependencies =
      [ aff-promise
        argonaut
        argonaut-generic 
        js-bigints
        effect
        encoding
        colors
        console
        css
        halogen
        halogen-css
        options
        web-dom
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



    test-dependencies =
      [ test-unit
        node-buffer
        node-http 
        node-fs
      ];
    test-src = "test";
  }
