{ lib, buildPythonPackage, pydantic, jsbeautifier, pytest, build, hatchling
}:
buildPythonPackage rec {
  pname    = "okaflow";
  version  = "0.1.0";
  prePatch = ''
    cd python/okaflow
  '';
  src = builtins.fetchGit
    { url = "https://github.com/Shimuuar/oka-flow";
      rev = "8ad7b2756426b2db569ec520775a04b6a52a6cd7";
      ref = "master";
    };
  #----
  format                = "pyproject";
  checkInputs           = [];
  buildInputs           = [build hatchling];
  propagatedBuildInputs = [pydantic jsbeautifier];
  patches               = [ ];
  doCheck               = false;
}

