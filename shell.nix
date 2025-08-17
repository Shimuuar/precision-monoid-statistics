let
  pkgs   = import <nixpkgs> {};
  pkgs_py = pkgs.python3.withPackages (ps: with ps;
    [ notebook
      jupyterlab
      ipympl
      ipywidgets
      mypy
      #
      numpy
      scipy
      mpmath
      matplotlib
      h5py
    ]);
  # Export environment variables that we need
  #
  # inline-python & GHCi combo is particularly capricious
  #
  #  - It needs PYTHONHOME set in order to be able load python libraries
  #
  #  - Python C extension won't be able to locate symbols from
  #    libpython3 in ghci without LD_PRELOAD set. Compiled programs
  #    work fine.
  shell_hook = ''
    export PYTHONHOME=${pkgs_py}  
    alias PY=LD_PRELOAD=${pkgs.python3}/lib/libpython3.12.so

    export LD_LIBRARY_PATH=''${LD_LIBRARY_PATH:+''${LD_LIBRARY_PATH}:}${pkgs.hdf5}/lib
    '';
in
pkgs.mkShell {
  shellHook        = shell_hook;
  buildInputs = with pkgs; [
    hdf5
    pkg-config
    pkgs_py
  ];
}
