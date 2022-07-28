<div align="center">
  <img src="https://i.imgur.com/HrYPYjh.png" />
  <h2>OpenSoldat</h2>
  <a href="https://discord.gg/6TqqtVpRdV"><img src="https://img.shields.io/discord/109442485285052416.svg" /></a>
</div>

OpenSoldat is a unique 2D (side-view) multiplayer action game. It has been influenced by the best of games such as Liero, Worms, Quake, Counter-Strike, and provides a fast-paced gaming experience with tons of blood and flesh.

This repository contains the source code of the so-called 1.8 version. Compared to the original version, the code has undergone many changes but is not in a finished state. We hope that by open-sourcing Soldat we can empower our community to improve the game at a faster pace.

## Dependencies

- FreePascal 3.0.4
- SDL 2.0.12
- OpenAL
- FreeType 2.6.1
- PhysFS 3.0.2
- [GameNetworkingSockets v1.4.1](https://github.com/ValveSoftware/GameNetworkingSockets/releases/tag/v1.4.1)

## Building OpenSoldat

OpenSoldat compiles on Windows, Linux and macOS.

### Compilation using CMake

This approach automates some build steps. OpenSoldat's assets will be downloaded for you, and you will not have to worry about downloading pre-built libraries. This is the simplest way to build OpenSoldat for Linux.

CMake 3.14+ is required.

#### Build steps for Linux (Ubuntu)

1. `sudo apt-get install build-essential g++ cmake git fpc libprotobuf-dev protobuf-compiler libssl-dev libsdl2-dev libopenal-dev libphysfs-dev libfreetype6`
2. `mkdir build && cd build`
3. `cmake ..`
4. `make`

#### Build steps for Windows

1. Install [freepascal 3.0.4](https://sourceforge.net/projects/freepascal/files/Win32/3.0.4/) (install `fpc-3.0.4.i386-win32.exe` first, and then `fpc-3.0.4.i386-win32.cross.x86_64-win64.exe`)
2. Install [Visual Studio with C++ compiler/build tools](https://visualstudio.microsoft.com/en) and [vcpkg](https://github.com/Microsoft/vcpkg)
3. Open Developer command prompt for Visual Studio
4. `vcpkg.exe --triplet x64-windows install sdl2 physfs openssl protobuf freetype openal-soft`
5. `set PATH=%PATH%;C:\fpc\3.0.4\bin\i386-win32`
6. `set OPENSSL_ROOT_DIR=C:\vcpkg\installed\x64-windows`
7. `set PHYSFSDIR=C:\vcpkg\installed\x64-windows`
8. `mkdir build`
9. `cd build`
10. `cmake -G "NMake Makefiles" -DCROSS_WINDOWS_64=1 -DCMAKE_TOOLCHAIN_FILE="C:\vcpkg\scripts\buildsystems\vcpkg.cmake" -DSDL2_BUILDING_LIBRARY=1 ..`
11. `nmake`

#### Build steps for macOS

1. `brew install openssl@1.1 protobuf fpc cmake sdl2 physfs freetype2`
2. `mkdir build && cd build`
3. `export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/usr/local/opt/openssl@1.1/lib/pkgconfig`
4. `cmake -DOPENSSL_ROOT_DIR=$(brew --prefix openssl@1.1) ..`
5. `make`

#### Available flags

The build can be customized by passing flags to `cmake` command. For example, you can choose whether you want to build the client, the server, or both. You can decide if you want to include OpenSoldat's assets in the build. There are also options for cross-compilation.

Check the `CMakeLists.txt` files in this repository to see the available options and their default values.

Example: `cmake .. -DCMAKE_BUILD_TYPE=Release -DADD_ASSETS=1 -DBUILD_CLIENT=0` to get a release build of the server with OpenSoldat's assets

### Compilation using other methods

If you decide to follow the approaches below, you will have to download OpenSoldat's assets and pre-built libraries for the game to work.
1. Download pre-built libraries. The best way would probably be to download libraries from the latest build of OpenSoldat (from Github Actions, or Releases). You can download latest from [here](https://nightly.link/opensoldat/opensoldat/workflows/opensoldat/develop) (includes libraries for 3 platforms - pick the ones you need) 2. Copy libraries to `client/build` and `server/build`
3. Get soldat.smod file from [base repository](https://github.com/opensoldat/base.git). You can either download the file from the [latest release](https://github.com/opensoldat/base/releases/latest) (recommended), or generate the .smod file yourself following the provided instructions
4. Copy `soldat.smod` file to `client/build` and `server/build`
5. Download `play-regular.ttf` file from [base repository](https://github.com/opensoldat/base) from the [latest release](https://github.com/opensoldat/base/releases/latest)
6. Copy `play-regular.ttf` file to `client/build`

#### Compilation using Lazarus IDE

1. Install [Lazarus IDE](https://www.lazarus-ide.org/)
2. Open `server/opensoldatserver.lpi` with Lazarus, press CTRL + F9 to compile the server
3. Open `client/opensoldat.lpi` with Lazarus, press CTRL + F9 to compile the game client

## Running OpenSoldat

You need to start the server first, and then join the game with client.
1. Run `opensoldatserver`
2. Run `opensoldat -join 127.0.0.1 23073` (more generically `-join ip port`)
