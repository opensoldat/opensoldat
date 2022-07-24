#!/bin/sh

getversion()
{
  version=$(otool -L ${2} | grep "lib${1}" | sed s/".*${1}\."// | sed s/"\.dylib.*"//)
}
getpngversion()
{
  version=$(otool -L ${1} | grep "libpng" | sed s/".*png[0-9]*\."// | sed s/"\.dylib.*"//)
}

cd build

getversion "freetype" "opensoldat/opensoldat"
freetypeversion=${version}

getversion "physfs" "opensoldat/opensoldat"
physfsversion=${version}

getversion "protobuf" "Frameworks/libGameNetworkingSockets.dylib"
protobufversion=${version}

getpngversion "/usr/local/opt/freetype/lib/libfreetype.${freetypeversion}.dylib"
pngversion=${version}

cp Frameworks/libGameNetworkingSockets.dylib opensoldat/
cp Frameworks/libstb.dylib opensoldat/
cp /usr/local/opt/sdl2/lib/libSDL2-2.0.0.dylib opensoldat/
cp /usr/local/opt/freetype/lib/libfreetype.${freetypeversion}.dylib opensoldat/
cp /usr/local/opt/physfs/lib/libphysfs.${physfsversion}.dylib opensoldat/
cp /usr/local/opt/libpng/lib/libpng${pngversion}.${pngversion}.dylib opensoldat/
cp /usr/local/opt/protobuf/lib/libprotobuf.${protobufversion}.dylib opensoldat/
cp /usr/local/opt/openssl@1.1/lib/libcrypto.1.1.dylib opensoldat/
chmod -R u+w opensoldat/lib*
install_name_tool -change /usr/local/opt/sdl2/lib/libSDL2-2.0.0.dylib @executable_path/libSDL2-2.0.0.dylib ./opensoldat/opensoldat
install_name_tool -change /usr/local/opt/freetype/lib/libfreetype.${freetypeversion}.dylib @executable_path/libfreetype.${freetypeversion}.dylib ./opensoldat/opensoldat
install_name_tool -change /usr/local/opt/physfs/lib/libphysfs.${physfsversion}.dylib @executable_path/libphysfs.${physfsversion}.dylib ./opensoldat/opensoldat
install_name_tool -change /usr/local/opt/physfs/lib/libphysfs.${physfsversion}.dylib @executable_path/libphysfs.${physfsversion}.dylib ./opensoldat/opensoldatserver
install_name_tool -change @executable_path/../Frameworks/libstb.dylib @executable_path/libstb.dylib ./opensoldat/opensoldat
install_name_tool -change @executable_path/../Frameworks/libGameNetworkingSockets.dylib @executable_path/libGameNetworkingSockets.dylib ./opensoldat/opensoldat
install_name_tool -change @executable_path/../Frameworks/libGameNetworkingSockets.dylib @executable_path/libGameNetworkingSockets.dylib ./opensoldat/opensoldatserver
install_name_tool -change /usr/local/opt/libpng/lib/libpng${pngversion}.${pngversion}.dylib @executable_path/libpng${pngversion}.${pngversion}.dylib ./opensoldat/libfreetype.${freetypeversion}.dylib
install_name_tool -change /usr/local/opt/protobuf/lib/libprotobuf.${protobufversion}.dylib @executable_path/libprotobuf.${protobufversion}.dylib ./opensoldat/libGameNetworkingSockets.dylib
install_name_tool -change /usr/local/opt/openssl@1.1/lib/libcrypto.1.1.dylib @executable_path/libcrypto.1.1.dylib ./opensoldat/libGameNetworkingSockets.dylib

