<div align="center">
  <img src="https://i.imgur.com/HrYPYjh.png" />
  <h2>Soldat</h2>
  <a href="https://discord.soldat.pl"><img src="https://img.shields.io/discord/234733999879094272.svg" /></a>
</div>

Soldat is a unique 2D (side-view) multiplayer action game. It has been influenced by the best of games such as Liero, Worms, Quake, Counter-Strike, and provides a fast-paced gaming experience with tons of blood and flesh.

This repository contains the source code of the so-called 1.8 version. Compared to the original version, the code has undergone many changes but is not in a finished state. We hope that by open-sourcing Soldat we can empower our community to improve the game at a faster pace.

## Dependencies

- FreePascal 3.0.4
- SDL 2.0.12
- OpenAL
- FreeType 2.6.1
- PhysFS 3.0.2
- [GameNetworkingSockets v1.2.0](https://github.com/ValveSoftware/GameNetworkingSockets/releases/tag/v1.2.0)

## Building Soldat

Soldat compiles on Windows, Linux and macOS.

1. Install [Lazarus IDE](https://www.lazarus-ide.org/) (or a standalone [FreePascal Compiler](https://freepascal.org) if you want to use Makefiles)
2. Download [pre-built libraries](https://github.com/Soldat/prebuilt-libs/archive/master.zip) and copy libraries from `win64_dlls` to `client/build` and `server/build`
3. Clone [base repository](https://github.com/soldat/base) and run `create_smod.sh` from it to create a game base archive
4. Copy `soldat.smod` to `client/build` and `server/build`
5. Copy `base/client/play-regular.ttf` to `client/build`

### Compilation using Lazarus IDE

1. Open `server/soldatserver.lpi` with Lazarus, press F9 to compile and start the server
2. Start another instance of Lazarus and open `client/soldat.lpi`, press F9 to build the game client

### Compilation using Makefiles

#### Compile server

1. Open commandline in server folder
2. run `make`
3. the executable can be found in the build folder (run with `soldatserver`)

#### Compile client

1. Open commandline in client folder
2. run `make`
3. the executable can be found in the build folder (run with `soldat -join 127.0.0.1 23073 test`)
