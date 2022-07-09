# Libraries for macOS

The following files must be placed into the `Frameworks` directory:

* SDL2.framework
* libfreetype.dylib
* libstb.dylib

Precompiled libraries for macOS can be found in Oliver's Dropbox:

https://www.dropbox.com/sh/u9ajqtbk37pfygn/AABDJi3QtSEzRIwdPB909nGta/libs/macOS?dl=0

(These should be made available via Git LFS or similar somewhen.)

After installing all library dependencies, run `./setup_lib_links.sh`.

## Build instructions

These instructions assume that you're in the `Frameworks` directory.

### libfreetype.dylib

```
cd <SDL2 source directory>
CFLAGS="-arch i386" LDFLAGS="-arch i386" ./configure --host=i386-apple-darwin --prefix=$PWD/../out32 --with-png=no
make -j4 && make install
./configure --host=x86_64-apple-darwin --prefix=$PWD/../out64 --with-png=no
make -j4 && make install
cd ..
lipo -create -output libfreetype.dylib out32/lib/libfreetype.dylib out64/lib/libfreetype.dylib
install_name_tool -id @loader_path/libfreetype.dylib libfreetype.dylib
```

### libSDL2.dylib

Use the official `SDL2.framework` file from SDL's website.

*Alterantively,* compile libSDL2.dylib:

```
cd <SDL2 source directory>
CFLAGS="-arch i386" LDFLAGS="-arch i386" ./configure --host=i386-apple-darwin --prefix=/tmp/opensoldatdeps/out32
make -j4 && make install
./configure --host=x86_64-apple-darwin --prefix=/tmp/opensoldatdeps/out64
make -j4 && make install
cd /tmp/opensoldatdeps
lipo -create -output libSDL2.dylib out32/lib/libSDL2-2.0.0.dylib out64/lib/libSDL2-2.0.0.dylib
install_name_tool -id @loader_path/libSDL2.dylib libSDL2.dylib
```

### libstb.dylib

```
CFLAGS="-O3 -Wno-unused-value -Wno-parentheses"
clang $CFLAGS -arch i386 -arch x86_64 -shared -undefined dynamic_lookup -o libstb.dylib ../../libs/stb/src/stb.c
install_name_tool -id @rpath/libstb.dylib libstb.dylib
```
