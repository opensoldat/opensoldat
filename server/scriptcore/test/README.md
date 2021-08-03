## Testing

BFT is a ScriptCore3 integration test suite.

The tests are intrusive to normal development so they aren't run by default.
If you're using CMake, you can set the `ADD_BFT` option to true and the tests
will be copied to the scripts directory of soldatserver automatically. If you
are using another build system, just manually copy the bft directory to the
scripts directory (e.g. `server/build/x86_64-linux/scripts`).

The functionality which can be run without needing anything to happen in the
game is tested on startup. To run tests for functionality relying on in-game
events, join the game and say 'test' a few times (see `MyOnSpeak` for details).
