let
  pkgs = import <nixpkgs> {};
in
  pkgs.mkShell {
    buildInputs = with pkgs; [
      lolcat
      python311Packages.epc
      python311Packages.orjson
      python311Packages.sexpdata
      python311Packages.six
      python311Packages.setuptools
      python311Packages.paramiko
      python311Packages.rapidfuzz
    ];
    shellHook = ''
    echo "initiating emacs env" | lolcat
    '';
  }
