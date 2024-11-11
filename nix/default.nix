{
  lib,
  llvmPackages,
  cmake,
  python3,
  curl,
  libxml2,
  libffi,
  xar,
  versionCheckHook,
}: let 
  inherit (builtins) baseNameOf toString readFile split elemAt;
  inherit (lib.sources) cleanSourceWith cleanSource; 
  inherit (lib.lists) findSingle findFirst;
  inherit (lib.asserts) assertMsg;
  inherit (lib.strings) hasInfix hasSuffix splitString removeSuffix removePrefix concatMapStringsSep;
in 
llvmPackages.stdenv.mkDerivation (finalAttrs: {
  pname = "c3c";
  version = let
    versionFile = readFile ../src/version.h;
    splitted = splitString "\n" versionFile;
    findLine = findFirst (x: hasInfix "COMPILER_VERSION" x) "none";
    foundLine = findLine splitted;
    foundLineSplitted = splitString " " foundLine;
    unformattedVersion = elemAt foundLineSplitted 2;
    version = removeSuffix "\"" ( removePrefix "\"" unformattedVersion );
    dumpStrList = concatMapStringsSep " " (x: "\""+x+"\"");
  in 
    assert assertMsg (foundLine != "none") "No COMPILER_VERSION substring was found in version.h. Lines: [${dumpStrList splitted}]";
    version;

  src = cleanSourceWith {
    filter = _path: _type: !(hasSuffix ".nix" (baseNameOf(toString _path)));
    src = cleanSource ../.;
  };

  postPatch = ''
    substituteInPlace CMakeLists.txt \
      --replace-fail "\''${LLVM_LIBRARY_DIRS}" "${llvmPackages.lld.lib}/lib ${llvmPackages.llvm.lib}/lib"
  '';

  nativeBuildInputs = [ cmake ];

  buildInputs = [
    llvmPackages.llvm
    llvmPackages.lld
    curl
    libxml2
    libffi
  ] ++ lib.optionals llvmPackages.stdenv.hostPlatform.isDarwin [ xar ];

  nativeCheckInputs = [ python3 ];

  doCheck = llvmPackages.stdenv.system == "x86_64-linux";

  checkPhase = ''
    runHook preCheck
    ( cd ../resources/testproject; ../../build/c3c build )
    ( cd ../test; python src/tester.py ../build/c3c test_suite )
    runHook postCheck
  '';

  nativeInstallCheckInputs = [ versionCheckHook ];
  doInstallCheck = true;

  meta = with lib; {
    description = "Compiler for the C3 language";
    homepage = "https://github.com/c3lang/c3c";
    license = licenses.lgpl3Only;
    maintainers = with maintainers; [
      luc65r
      anas
    ];
    platforms = platforms.all;
    mainProgram = "c3c";
  };
})

