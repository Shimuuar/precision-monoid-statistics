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
      rev = "a39d29c5cead7f01d358c9eba2b1c05426601287";
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

