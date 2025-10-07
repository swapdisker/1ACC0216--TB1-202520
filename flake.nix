{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { nixpkgs, ... }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        config.allowUnfreePredicate = pkg: builtins.elem (pkgs.lib.getName pkg) [
          "positron-bin"
        ];
      };
    in {
      devShells.x86_64-linux.default = pkgs.mkShell {
        nativeBuildInputs = with pkgs; [
          positron-bin
	  R
	  rPackages.dplyr
	  rPackages.readr
	  rPackages.janitor
	  rPackages.ggplot2
	  rPackages.jsonlite
	  rPackages.scales
	  rPackages.GGally
	  rPackages.rvest
          rPackages.xml2
	  rPackages.XML
	  rPackages.tidyr
	  rPackages.DescTools
          (python3.withPackages (ps: with ps; [
            numpy
            matplotlib
            jupyterlab
            scikit-learn
	    
            notebook
            networkx
            ipykernel
            nbconvert
            nbdime
            pandoc
            sympy
            graphviz
            pandas
          ]))
        ];
      };
    };
}
