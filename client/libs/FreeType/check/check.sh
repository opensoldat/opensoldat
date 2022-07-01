#!/bin/bash

function setup_msvc() {
  # These lines will be installation-dependent.
  export VSINSTALLDIR='C:\Program Files (x86)\Microsoft Visual Studio 10.0\'
  export WindowsSdkDir='C:\Program Files (x86)\Microsoft SDKs\Windows\v7.0A\'
  export FrameworkDir='C:\WINDOWS\Microsoft.NET\Framework\'
  export FrameworkVersion=v4.0.30319
  export Framework35Version=v3.5

  # The following should be largely installation-independent.
  export VCINSTALLDIR="$VSINSTALLDIR"'VC\'
  export DevEnvDir="$VSINSTALLDIR"'Common7\IDE\'

  export FrameworkDIR32="$FrameworkDir"
  export FrameworkVersion32="$FrameworkVersion"

  export INCLUDE="${VCINSTALLDIR}INCLUDE;${WindowsSdkDir}include;"
  export LIB="${VCINSTALLDIR}LIB;${WindowsSdkDir}lib;"
  export LIBPATH="${FrameworkDir}${FrameworkVersion};"
  export LIBPATH="${LIBPATH}${FrameworkDir}${Framework35Version};"
  export LIBPATH="${LIBPATH}${VCINSTALLDIR}LIB;"

  c_VSINSTALLDIR=`cygpath -ua "$VSINSTALLDIR\\\\"`
  c_WindowsSdkDir=`cygpath -ua "$WindowsSdkDir\\\\"`
  c_FrameworkDir=`cygpath -ua "$FrameworkDir\\\\"`

  export PATH="${c_WindowsSdkDir}bin:$PATH"
  export PATH="${c_WindowsSdkDir}bin/NETFX 4.0 Tools:$PATH"
  export PATH="${c_VSINSTALLDIR}VC/VCPackages:$PATH"
  export PATH="${c_FrameworkDir}${Framework35Version}:$PATH"
  export PATH="${c_FrameworkDir}${FrameworkVersion}:$PATH"
  export PATH="${c_VSINSTALLDIR}Common7/Tools:$PATH"
  export PATH="${c_VSINSTALLDIR}VC/BIN:$PATH"
  export PATH="${c_VSINSTALLDIR}Common7/IDE/:$PATH"
}

#setup_msvc

VC='C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\vcvarsall.bat'
FTDIR='C:\Users\urraka\Desktop\freetype-2.6.1\include'

mkdir -p out

# # c
gcc -E -x c c_check.h.in | sed -E '/^(#|$)/d' > out/c_check.h
gcc -x c c_check.c -Iout -oout/c_check $(pkg-config --cflags --libs freetype2)
# echo "call \"$VC\"" > out/c_build.bat
# echo "cl \"/I$FTDIR\" /Fec_check.exe ..\\c_check.c" >> out/c_build.bat
# cd out
# start cmd //C c_build.bat
# cd -
#cl //nologo "/I$FTDIR" //Foout\\c_check.obj //Feout\\c_check.exe c_check.c
#./out/c_check.exe > out/c_result.txt
./out/c_check > out/c_result.txt

# # pas
gcc -E -x c pas_check.pas.in | sed -E '/^(#|$)/d' | sed -E 's/"/'"'/g" > out/pas_check.pas
fpc -Mdelphi -FEout out/pas_check.pas
#./out/pas_check.exe > out/pas_result.txt
./out/pas_check > out/pas_result.txt

echo ''
echo 'diff:'
diff -u out/c_result.txt out/pas_result.txt

#rm -fr out/
