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
        options
        web-dom
        web-uievents

      ];
    src = "src";
    foreign."XTerm.Api.Terminal".node_modules =
      npmlock2nix.v2.node_modules { src = ./.; } + /node_modules;
    foreign."XTerm.Api.Terminal.Addon.WebGL".node_modules =
      npmlock2nix.v2.node_modules { src = ./.; } + /node_modules;

    test-dependencies =
      [ test-unit
        node-buffer
        node-http 
        node-fs
      ];
    test-src = "test";
  }
