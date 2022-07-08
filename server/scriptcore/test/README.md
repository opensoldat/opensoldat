## Testing

### BFT

BFT is a ScriptCore3 integration test suite.

The tests are intrusive to normal development so they aren't run by default.
If you're using CMake, you can set the `ADD_BFT` option to true and the tests
will be copied to the scripts directory of OpenSoldatServer automatically. If
you are using another build system, just manually copy the bft directory to the
scripts directory (e.g. `server/build/x86_64-linux/scripts`).

What do we test?
- Calls to ScriptCore functions with checks if they returned expected result.
These are run on server's startup and don't require user interaction. To check
result of tests, look at server's console (or logs)
- [ScriptCore events](https://wiki.soldat.pl/index.php/Category:Events).
We print messages to server's console whenever an event occurs. This requires
some interaction, as you have to join the server and make those events happen.
Verify that server prints info about each event that occurs in game
- Calls to ScriptCore functions that require user interaction. This is for methods
like [Player.Say](https://wiki.soldat.pl/index.php/TActivePlayer.Say), or
[Players.WorldText](https://wiki.soldat.pl/index.php/TPlayers.WorldText), etc...
You have to send chat messages with commands to run these tests. Say `!help` in
game for more information, or see implementation of `MyOnSpeak` in bft.pas

### FFI fuzz testing

In order to test the custom assembly used to implement the PascalScript FFI,
some fuzz tests can be generated. The script `gen_ffi_tests.py` generates 2
files. `ScriptFFITests.pas` is an internal SC3 file used to expose the
generated test functions to scripts. `ffi.pas` is an SC3 script designed to
call the generated functions with certain parameters, and check their return
values.

To run the tests with CMake, make sure to have `BUILD_SCRIPTCORE` and
`ADD_FFI_FUZZ` options set to true. Then build CMake target `gen-ffi-fuzz`
(or run `gen_ffi_tests.py` manually), build and run OpenSoldatServer.

To run the tests with another build system:
1. Run `./gen_ffi_tests.py`
2. Now you need to tell your compiler how to find `ScriptFFITests.pas`.
You have 2 options:
   - Temporarily copy `ScriptFFITests.pas` to the `server/scriptcore` directory
   - Add `server/scriptcore/test` directory to search paths of units (`-Fu` parameter)
3. Build OpenSoldatServer with `-dSCRIPT_FFI_FUZZ` custom option
4. Copy the `ffi` directory to the `scripts` directory of your OpenSoldatServer build
