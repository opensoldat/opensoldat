#!/bin/sh

getversion()
{
  version=$(otool -L ${2} | grep "lib${1}" | sed s/".*${1}\."// | sed s/"\.dylib.*"//)
}
getpngversion()
{
  version=$(otool -L ${1} | grep "libpng" | sed s/".*png[0-9]*\."// | sed s/"\.dylib.*"//)
}

opensoldatdir="OpenSoldat-macos-x64"

cd build

echo "Starting from $(pwd)"

echo "Fixing libs"

getversion "freetype" "${opensoldatdir}/opensoldat"
freetypeversion=${version}

getversion "physfs" "${opensoldatdir}/opensoldat"
physfsversion=${version}

getversion "protobuf" "Frameworks/libGameNetworkingSockets.dylib"
protobufversion=${version}

getpngversion "/usr/local/opt/freetype/lib/libfreetype.${freetypeversion}.dylib"
pngversion=${version}

cp Frameworks/libGameNetworkingSockets.dylib ${opensoldatdir}/
cp Frameworks/libstb.dylib ${opensoldatdir}/
cp /usr/local/opt/sdl2/lib/libSDL2-2.0.0.dylib ${opensoldatdir}/
cp /usr/local/opt/freetype/lib/libfreetype.${freetypeversion}.dylib ${opensoldatdir}/
cp /usr/local/opt/physfs/lib/libphysfs.${physfsversion}.dylib ${opensoldatdir}/
cp /usr/local/opt/libpng/lib/libpng${pngversion}.${pngversion}.dylib ${opensoldatdir}/
cp /usr/local/opt/protobuf/lib/libprotobuf.${protobufversion}.dylib ${opensoldatdir}/
cp /usr/local/opt/openssl@1.1/lib/libcrypto.1.1.dylib ${opensoldatdir}/
chmod -R u+w ${opensoldatdir}/lib*
install_name_tool -change /usr/local/opt/sdl2/lib/libSDL2-2.0.0.dylib @executable_path/libSDL2-2.0.0.dylib ./${opensoldatdir}/opensoldat
install_name_tool -change /usr/local/opt/freetype/lib/libfreetype.${freetypeversion}.dylib @executable_path/libfreetype.${freetypeversion}.dylib ./${opensoldatdir}/opensoldat
install_name_tool -change /usr/local/opt/physfs/lib/libphysfs.${physfsversion}.dylib @executable_path/libphysfs.${physfsversion}.dylib ./${opensoldatdir}/opensoldat
install_name_tool -change /usr/local/opt/physfs/lib/libphysfs.${physfsversion}.dylib @executable_path/libphysfs.${physfsversion}.dylib ./${opensoldatdir}/opensoldatserver
install_name_tool -change @executable_path/../Frameworks/libstb.dylib @executable_path/libstb.dylib ./${opensoldatdir}/opensoldat
install_name_tool -change @executable_path/../Frameworks/libGameNetworkingSockets.dylib @executable_path/libGameNetworkingSockets.dylib ./${opensoldatdir}/opensoldat
install_name_tool -change @executable_path/../Frameworks/libGameNetworkingSockets.dylib @executable_path/libGameNetworkingSockets.dylib ./${opensoldatdir}/opensoldatserver
install_name_tool -change /usr/local/opt/libpng/lib/libpng${pngversion}.${pngversion}.dylib @executable_path/libpng${pngversion}.${pngversion}.dylib ./${opensoldatdir}/libfreetype.${freetypeversion}.dylib
install_name_tool -change /usr/local/opt/protobuf/lib/libprotobuf.${protobufversion}.dylib @executable_path/libprotobuf.${protobufversion}.dylib ./${opensoldatdir}/libGameNetworkingSockets.dylib
install_name_tool -change /usr/local/opt/openssl@1.1/lib/libcrypto.1.1.dylib @executable_path/libcrypto.1.1.dylib ./${opensoldatdir}/libGameNetworkingSockets.dylib
