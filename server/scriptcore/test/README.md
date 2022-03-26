## Testing

### BFT

BFT is a ScriptCore3 integration test suite.

The tests are intrusive to normal development so they aren't run by default.
If you're using CMake, you can set the `ADD_BFT` option to true and the tests
will be copied to the scripts directory of soldatserver automatically. If you
are using another build system, just manually copy the bft directory to the
scripts directory (e.g. `server/build/x86_64-linux/scripts`).

The functionality which can be run without needing anything to happen in the
game is tested on startup. To run tests for functionality relying on in-game
events, join the game and say 'test' a few times (see `MyOnSpeak` for details).

### FFI fuzz testing

In order to test the custom assembly used to implement the PascalScript FFI,
some fuzz tests can be generated. To run the tests with CMake, make sure to
have `BUILD_SCRIPTCORE` and `ADD_FFI_FUZZ` options set to true. Then build
CMake target `gen-ffi-fuzz` (or run `gen_ffi_tests.py` manually), build and
run soldatserver.
