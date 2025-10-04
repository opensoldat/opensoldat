# SDL2-for-Pascal

Unit files for building
[Free Pascal](https://freepascal.org/) / [Delphi](https://www.embarcadero.com/products/delphi) applications
using the [SDL2 library](https://libsdl.org).

This repository is a community-maintained fork of the [Pascal-SDL-2-Headers](https://github.com/ev1313/Pascal-SDL-2-Headers) repo.

## Installation

Simply add the units to your include path. You can achieve this by:
 - (FPC) using the `{$UNITPATH XXX}` directive in your source code;
 - (FPC) using the `-FuXXX` command-line argument to the compiler;
 - just copying & pasting the units into the same directory as your main source code.

Use the `sdl2` unit for the main SDL2 library (should be always needed). Units for the other SDL2 libraries are also provided:
 - [`sdl2_gfx`](https://www.ferzkopp.net/wordpress/2016/01/02/sdl_gfx-sdl2_gfx/)
 - [`sdl2_image`](https://www.libsdl.org/projects/SDL_image/)
 - [`sdl2_mixer`](https://www.libsdl.org/projects/SDL_mixer/)
 - [`sdl2_net`](https://www.libsdl.org/projects/SDL_net/)
 - [`sdl2_ttf`](https://www.libsdl.org/projects/SDL_ttf/)

## Bugs / Contributions / ToDos

If you have any contributions or bugfixes, feel free to drop a pull request or send in a patch.

Please use the GitHub issue tracker for bug reports.

### ToDos

- (Continously) Update files by new SDL2 functions and types which are present in more recent SDL2 versions.
- (Continously atm.) Translate integer aliases into typed enums.
See part Enums on the [Cheat sheet](CHEATSHEET.md) for reference.
- (Continously) Check FPC/Delphi compatibility.
- (Continously) Adapt comments to [FPDoc format](https://www.freepascal.org/docs-html/fpdoc/fpdoc.html). (See issue #22)

## Code style guidelines

The main principle is to stay as tight as possible at the names in the C headers.
These guidelines aim to have better consistency in this community project and make
it easier to find certain code parts in the C headers/Pascal includes. Feel free
to discuss or extend these guidelines, use the issue tracker.

1. Names of C defines (constants) and function parameters shall not be modified or "pascalified"
Ex: `SDL_INIT_VIDEO` does not change into `SDLInitVideo`.

2. Names corresponding to reserved key words are kept and an underscore is added.
Ex.: `type` in C function `SDL_HasEvent(Uint32 type)` changes into `type_`
in Pascal function `SDL_HasEvent(type_: TSDL_EventType)`.

3. Use C data types like `UInt8`, `UInt16`, `UInt32`, `SInt8`, `SInt16`,
`SInt32`, `Float` and so on as often as possible if it is used  in the
original code. Do not replace them by Pascal equivalents.
Ex.: Use `UInt32` (if used in
the original code) instead of `Cardinal`, `LongWord` or `DWord` .

4. Have a look at our [Translation Cheat Sheet](CHEATSHEET.md) for reference.

## Versions

The version tag (see [tags](https://github.com/PascalGameDevelopment/SDL2-for-Pascal/tags)) refers to the version of this translation package [SDL2 for Pascal](https://github.com/PascalGameDevelopment/SDL2-for-Pascal), not the `SDL2 library`.

### v2.x (work in progress)

- be up-to-date with version 2.0.14 of the `SDL2 library`
- replaced all aliases by typed enums
- improve Delphi-compatibility (and even more important, DO NOT break it)

### v2.0

- first official release of the PGD community fork of the [Pascal-SDL-2-Headers](https://github.com/ev1313/Pascal-SDL-2-Headers)
  - its latest version git tag is 1.72, in the sdl2.pas it goes even up to version 1.80; hence starting with v2.0 for this fork is a senseful distinction
- this ia a highly Delphi-compatible and stable fallback package
- loosely is up-to-date with version 2.0.4 of the `SDL2 library`

## License

You may license the Pascal SDL2 units either
with the [MPL license](blob/master/MPL-LICENSE) or
with the [zlib license](blob/master/zlib-LICENSE).
