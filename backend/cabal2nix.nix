{ mkDerivation, aeson, base, lib, optparse-applicative, servant
, servant-server, sqlite-simple, text, time, wai, warp
}:
mkDerivation {
  pname = "raytracker-backend";
  version = "1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base optparse-applicative servant servant-server
    sqlite-simple text time wai warp
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
