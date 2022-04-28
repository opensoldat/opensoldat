## SteamWorks SDK + GameNetworkingSockets FPC bindings

These scripts automatically generate Pascal bindings for the SteamWorks SDK and
GameNetworkingSockets. They are only tested with FPC. There is no inline
documentation, consulting the official SteamWorks SDK documentation and headers
is recommended. By default, these headers also support linking with
GameNetworkingSockets, which only provides a subset of the SteamWorks API. To
use the full SteamWorks API, compile with `-DSTEAM`. Check git tags for
bindings to specific versions.

There is a little bit of Soldat specific code in here, such as `TSteam` and
`TSteamGS`. But it compiles on its own and should be useful to anyone wanting
to use the SteamWorks SDK from an FPC project.

The bindings make use of record helpers to provide a somewhat similar API as
the official C++ version with method syntax.

### Example

```pascal
var
  {$IFNDEF STEAM}
  ErrorMsg: SteamNetworkingErrMsg;
  {$ENDIF}
  NetworkingSockets: PISteamNetworkingSockets;
  NetworkingUtils: PISteamNetworkingUtils;
begin
  {$IFDEF STEAM}
  if not SteamAPI_Init() then
    raise Exception.Create('SteamAPI_Init has failed');
  {$ELSE}
  if not GameNetworkingSockets_Init(Nil, @ErrorMsg) then
    raise Exception.Create('GameNetworkingSockets_Init has failed: ' + PChar(ErrorMsg));
  {$ENDIF}

  {$IFDEF STEAM}
  {$IFDEF SERVER}
  NetworkingSockets := SteamAPI_SteamGameServerNetworkingSockets_SteamAPI_v012();
  {$ELSE}
  NetworkingSockets := SteamAPI_SteamNetworkingSockets_SteamAPI_v012();
  {$ENDIF}
  {$ELSE}
  NetworkingSockets := SteamAPI_SteamNetworkingSockets_v009();
  {$ENDIF}
  if NetworkingSockets = Nil then
    raise Exception.Create('NetworkingSockets is null');

  {$IFDEF STEAM}
  NetworkingUtils := SteamAPI_SteamNetworkingUtils_SteamAPI_v004();
  {$ELSE}
  NetworkingUtils := SteamAPI_SteamNetworkingUtils_v003();
  {$ENDIF}
  if NetworkingUtils = Nil then
    raise Exception.Create('NetworkingUtils is null');

  // Method syntax. Works with pointer variables too.
  NetworkingUtils.SetGlobalCallback_SteamNetConnectionStatusChanged(@ProcessEventsCallback);
  NetworkingUtils.SetDebugOutputFunction(k_ESteamNetworkingSocketsDebugOutputType_Msg, DebugNet);
  // ...

  {$IFDEF STEAM}
  SteamAPI_Shutdown();
  {$ELSE}
  GameNetworkingSockets_Kill();
  {$ENDIF}
end.
```

### Instructions

To generate `Steam.pas`:
```sh
./gen_steam_bindings.py /path/to/steamworks/sdk
```

To test `Steam.pas` (prints diff of enum size/value, struct size, field offsets):
```sh
cp /path/to/appropriate/libsteam_api.{so,dll,dylib} check
cd check
sh test.sh
```

To generate `gns_functions.txt`:
```
./make_gns_functions_list.sh /path/to/libGameNetworkingSockets.so
```

### Todo:

* Do a more thorough job of checking structs which are not properly handled by `steam_api.json`.
* Translate custom operators.
* Cleaner `{$IFDEF STEAM}` checks.
* `k_ERemoteStoragePlatformAll` test fails, maybe something about signed vs. unsigned enum types? Or type enum value is cast to?
