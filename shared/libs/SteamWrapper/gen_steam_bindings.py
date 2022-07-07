#!/usr/bin/env python3

# This script takes as input the path to a version of the SteamWorks SDK, writes
# out generated Pascal bindings to `Steam.pas`, and some test files to the
# `check` subdirectory.

import functools
import json
import os
import shutil
import sys

os.chdir(os.path.dirname(os.path.abspath(sys.argv[0])))
if not os.path.exists('check'):
    os.mkdir('check')
api_path = sys.argv[1] + os.sep

# Utils.
################################################################################

def interpose(coll, s):
    if len(coll) < 2:
        return coll
    i = 1
    while i < len(coll):
        coll = coll[:i] + s + coll[i:]
        i += 2
    return coll


def interpose_s(coll, s):
    return functools.reduce(lambda a, b: a + b, interpose(coll, [s]), '')


def find_nth(haystack, needle, n):
    start = haystack.find(needle)
    while start >= 0 and n > 1:
        start = haystack.find(needle, start+len(needle))
        n -= 1
    return start


def get_all_under_tag(out, tag, var):
    if isinstance(var, dict):
        for key in var.keys():
            if key == tag:
                for item in var[key]:
                    out.append(item)
                    # Special case for struct constants...
                    if tag == 'consts' and 'struct' in var:
                        out[-1]['constname'] = var['struct'] + '__' + out[-1]['constname']
            else:
                get_all_under_tag(out, tag, var[key])
    elif isinstance(var, list):
        for v in var:
            get_all_under_tag(out, tag, v)


def fix_path(path):
    return path.replace('/', os.sep)


def get_or(d, keys, default):
    for key in keys:
        if key in d:
            return d[key]
    return default

# Fix types from `steam_api.json`.
################################################################################

type_replacements = {
        'bool': 'Boolean',
        'char': 'AnsiChar',
        'short': 'Int16',
        'unsigned short': 'UInt16',
        'int': 'Integer',
        'unsigned int': 'UInt32',
        'long long': 'Int64',
        'unsigned long long': 'UInt64',
        'int8': 'Int8',
        'int16': 'Int16',
        'int32': 'Int32',
        'int32_t': 'Int32',
        'int64': 'Int64',
        'lint64': 'Int64',
        'int64_t': 'Int64',
        'uint8': 'UInt8',
        'uint16': 'UInt16',
        'uint32': 'UInt32',
        'uint64': 'UInt64',
        'ulint64': 'UInt64',
        'intptr_t': 'IntPtr',
        'float': 'Single',
        'double': 'Double',
        'size_t': 'csize_t',
}

# Fix e.g. 'void (*)(int, const char *)'
def fix_function_pointer_type(type_text):
    ret = ''

    return_type = type_text[:type_text.index('(')].strip()
    if return_type == 'void':
        ret = 'procedure('
    else:
        ret = 'function('

    # void (*)(int, const char *)
    #          \_______________/
    #                |
    # Split contents inside parentheses by comma, parse as individual types.
    parameters = ['a' + str(i) + ': ' + fix_type(sub) for (i, sub) in enumerate(type_text[find_nth(type_text, '(', 2) + 1:-1].split(','))]
    ret += interpose_s(parameters, '; ') + ')'

    if return_type != 'void':
        ret += ': ' + fix_type(return_type);
    ret += '; cdecl'
    return ret


# Fix e.g. 'char [128]'
def fix_array_type(type_text):
    open_idx = type_text.index('[')
    close_idx = type_text.index(']')
    arr_size = type_text[open_idx + 1:close_idx]
    rest_of_type_text = type_text[0:open_idx] + type_text[close_idx + 1:]
    return 'Array[0..' + arr_size + ' - 1] of ' + fix_type(rest_of_type_text)


# Fix e.g. 'const char **'
def fix_pointer_type(type_text):
    ret = 'P' * type_text.count('*')
    left = type_text.replace('*', '').strip()
    if left == 'void':
        ret += 'ointer'
    else:
        ret += fix_type(left)

    return ret


def fix_type(type_text):
    type_text = type_text.replace('const ', '').replace('&', '*').strip()

    if '(*' in type_text:
        return fix_function_pointer_type(type_text)
    elif '[' in type_text:
        return fix_array_type(type_text)
    elif '*' in type_text:
        return fix_pointer_type(type_text)
    else:
        return type_replacements.get(type_text, type_text).replace('::', '__')

    return ret


# Allow for const, constref, var.
def build_param(param_name, type_text, is_interface=False):
    modifier = ''
    if is_interface:
        if type_text.startswith('const '):
            if '&' in type_text:
                modifier = 'constref '
            else:
                modifier = 'const '
            type_text = type_text[len('const '):]
        elif '&' in type_text:
            modifier = 'var '
        type_text = type_text.replace('&', '').strip()

    return modifier + param_name + ': ' + fix_type(type_text)


def fix_constant_val(constant_val):
    return constant_val.replace('STEAMGAMESERVER_QUERY_PORT_SHARED', '$ffff').replace('k_nSteamNetworkingSend_Reliable | k_nSteamNetworkingSend_NoNagle', '8 or 1').replace('k_nSteamNetworkingSend_Unreliable | k_nSteamNetworkingSend_NoDelay | k_nSteamNetworkingSend_NoNagle', '0 or 4 or 1').replace('k_nSteamNetworkingSend_Unreliable | k_nSteamNetworkingSend_NoNagle', '0 or 1').replace('( SteamItemInstanceID_t ) ~ 0', 'not 0').replace('( ( uint32 ) \'d\' << 16U ) | ( ( uint32 ) \'e\' << 8U ) | ( uint32 ) \'v\'', '6579574').replace('0x', '$').replace('~', 'not').replace('|', 'or').replace('ull', '').replace('<<', 'shl').replace('>>', 'shr')


def enum_cpp_name(enum):
    if 'fqname' in enum:
        return enum['fqname']
    else:
        return enum['enumname']


def enum_pas_name(enum):
    return enum_cpp_name(enum).replace(':', '_')


def method_prototype(method_name, return_type, params, is_interface=False):
    proto = ''
    if return_type != 'void':
        proto = 'function '
    else:
        proto = 'procedure '

    proto += method_name + '('
    proto += interpose_s([build_param(param_name, param_type, is_interface) for (param_name, param_type) in params], '; ')
    proto += ')'

    if return_type != 'void':
        proto += ': ' + fix_type(return_type)
    return proto


def steam_api_object_method_prototype(method, interface_type='', method_name_qualifier='', exposed=False):
    method_name = method_name_qualifier
    if interface_type != '':
        method_name += method['methodname_flat']
    else:
        method_name += method['methodname']

    params = []
    if interface_type != '':
        params.append(('A' + interface_type, 'P' + interface_type))
    params += [(param['paramname'], param['paramtype']) for param in method['params']]

    proto = method_prototype(method_name, method['returntype'], params, exposed == False) + ';'

    if method_name_qualifier == '' and 'overload' in method and exposed == False:
        proto += ' overload;'
    if exposed:
        proto += ' cdecl; external STEAMLIB;'
    return proto

# Read GNS functions.
################################################################################

gns_funcs = set()
with open('gns_functions.txt', 'r') as gns_funcs_file:
    for func in gns_funcs_file.readlines():
        gns_funcs.add(func.strip('\r\n'))

# Read and clean up API data from `steam_api.json`.
################################################################################

with open(fix_path(api_path + 'public/steam/steam_api.json'), 'r') as api_file:
    api = json.load(api_file)

# Cleanup invalid parameter names.
param_name_replacements = {
        'type': 'aType',
        'val': 'value',
        'result': 'aResult',
        }

for interface in api['callback_structs'] + api['structs'] + api['interfaces']:
    if 'methods' not in interface:
        continue

    for method in interface['methods']:
        for param in method['params']:
            if param['paramname'] in param_name_replacements:
                param['paramname'] = param_name_replacements[param['paramname']]

# Check for overloads.
for interface in api['callback_structs'] + api['structs'] + api['interfaces']:
    if 'methods' not in interface:
        continue

    for method in interface['methods']:
        for other_method in interface['methods']:
            if other_method['methodname'] == method['methodname'] and other_method['methodname_flat'] != method['methodname_flat']:
                method['overload'] = 'true'
                break;

# Write out bindings to `Steam.pas`.
################################################################################

f = open('Steam.pas', 'w')

f.write('''{ Warning: This file is generated. Edit gen_steam_bindings.py instead. }

unit Steam;

{$IFDEF FPC}
{$MODE DELPHI}
{$WARN 3031 OFF : Values in enumeration types have to be ascending}
{$WARN 4110 OFF : Range check error while evaluating constants}
{$ENDIF}

interface

uses
  ctypes, sysutils;

{$PACKENUM 4}

{$IFDEF UNIX}
{$PACKRECORDS 4}
{$ELSE}
{$PACKRECORDS 8}
{$ENDIF}

const
  {$IFDEF STEAM}
  {$IFDEF WINDOWS}
  {$IFDEF CPUX86_64}
  STEAMLIB = 'steam_api64.dll';
  {$ELSE}
  STEAMLIB = 'steam_api.dll';
  {$ENDIF}
  {$ENDIF}
  {$IFDEF DARWIN}
  STEAMLIB = 'libsteam_api.dylib';
  {$ENDIF}
  {$IFDEF LINUX}
  STEAMLIB = 'libsteam_api.so';
  {$ENDIF}
  {$ELSE}
  {$IFDEF WINDOWS}
  STEAMLIB = 'GameNetworkingSockets.dll';
  {$ENDIF}
  {$IFDEF DARWIN}
  STEAMLIB = 'libGameNetworkingSockets.dylib';
  {$ENDIF}
  {$IFDEF LINUX}
  STEAMLIB = 'libGameNetworkingSockets.so';
  {$ENDIF}
  {$ENDIF}

  STEAM_APPID = 638490;

type
''')

# Pointer to struct types.
for struct in api['callback_structs'] + api['structs']:
    f.write('  P' + struct['struct'] + ' = ^' + struct['struct'] + ';\n')

# Add double pointer types as necessary, don't fill the file with them...
f.write('  PPMatchMakingKeyValuePair_t = ^PMatchMakingKeyValuePair_t;\n')
f.write('  PPSteamNetworkingMessage_t = ^PSteamNetworkingMessage_t;\n')

f.write('\n');

# Enums.
enums = []
get_all_under_tag(enums, 'enums', api)

for enum in enums:
    enum_name = enum_pas_name(enum)
    f.write('  P' + enum_name + ' = ^' + enum_name + ';\n')
    f.write('  ' + enum_name + ' = (\n')
    enum_vals = ['    ' + val['name'] + ' = ' + val['value'] for val in enum['values']]
    f.write(interpose_s(enum_vals, ',\n') + '\n  );\n\n')

# Typedefs.
ignore_typedefs = {'uint8', 'int8', 'int16', 'uint16', 'int32', 'uint32',
        'int64', 'uint64', 'lint64', 'ulint64', 'intp', 'uintp'}

for typedef in api['typedefs']:
    if typedef['typedef'] in ignore_typedefs:
        continue

    f.write('  P' + typedef['typedef'] + ' = ^' + typedef['typedef'] + ';\n')
    f.write('  ' + typedef['typedef'] + ' = ' + fix_type(typedef['type']) + ';\n')

# Missing typedef, SteamAPIWarningMessageHook_t
f.write('  SteamAPIWarningMessageHook_t = ' + fix_type('void (*)(int, const char *)') + ';\n')

f.write('\n');

# CSteamID, CGameID, SteamNetworkingConfigValue_t. TODO: Big Endian?
f.write('''  {$PACKRECORDS 1}
  PCSteamID = ^CSteamID;
  CSteamID = bitpacked record
    // n = num_bits, 0..(2^n - 1)
    m_unAccountID: 0..4294967295; // 32 bits
    m_unAccountInstance: 0..1048575; // 20 bits
    m_EAccountType: 0..15; // 4 bits
    m_EUniverse: 0..255; // 8 bits
  end;

  PCGameID = ^CGameID;
  CGameID = bitpacked record
    // n = num_bits, 0..(2^n - 1)
    m_nAppID: 0..16777215; // 24 bits
    m_nType: 0..255; // 8 bits
    m_nModID: 0..4294967295; // 32 bits
  end;

  {$PACKRECORDS C}
  SteamNetworkingConfigValue_t = record
    m_eValue: ESteamNetworkingConfigValue;
    m_eDataType: ESteamNetworkingConfigDataType;
    case Integer of
      0: (m_int32: int32);
      1: (m_int64: int64);
      2: (m_float: Single);
      3: (m_string: PAnsiChar);
      4: (m_functionPtr: procedure);
  end;
  {$IFDEF UNIX}
  {$PACKRECORDS 4}
  {$ELSE}
  {$PACKRECORDS 8}
  {$ENDIF}\n\n''')

# Structs.
special_pack = {
        'InputAnalogActionData_t': '1',
        'InputDigitalActionData_t': '1',
        'SteamNetworkingIPAddr': '1',
        'SteamNetworkingIdentity': '1',
        'SteamNetworkingMessage_t': 'C',
        'SteamNetworkPingLocation_t': 'C',
        'SteamNetworkingConfigValue_t': 'C',
        'SteamNetworkingMessagesSessionRequest_t': '1',
        'SteamNetworkingMessagesSessionFailed_t': '1',
        'SteamDatagramRelayAuthTicket': 'C',
        'SteamRelayNetworkStatus_t': 'C',
        'SteamParentalSettingsChanged_t': 'C',
        'MatchMakingKeyValuePair_t': 'C',
        'SteamIPAddress_t': '1',
        'GameConnectedChatLeave_t': '1',
        'PSNGameBootInviteResult_t': '1',
        'P2PSessionConnectFail_t': '1',
        'GSClientGroupStatus_t': '1',
        }

for struct in api['structs'] + api['callback_structs']:
    # We add our own Pascal version of these manually.
    if struct['struct'] in {'SteamInputActionEvent_t', 'SteamNetworkingConfigValue_t'}:
        continue

    if struct['struct'] in special_pack:
        f.write('  {$PACKRECORDS ' + special_pack[struct['struct']] + '}\n')
    f.write('  ' + struct['struct'] + ' = record\n')

    for field in struct['fields']:
        f.write('    ' + field['fieldname'] + ': ' + fix_type(field['fieldtype']) + ';\n')

    # Size of empty structs should be 1 byte.
    if len(struct['fields']) == 0:
        f.write('    Dummy: Byte;\n');

    f.write('  end;\n');
    if struct['struct'] in special_pack:
        f.write('''  {$IFDEF UNIX}
  {$PACKRECORDS 4}
  {$ELSE}
  {$PACKRECORDS 8}
  {$ENDIF}\n''')
    f.write('\n')

# SteamInputActionEvent_t
f.write('''  {$PACKRECORDS 1}
  SteamInputActionEvent_t__DigitalAction_t = record
    actionHandle: InputDigitalActionHandle_t;
    digitalActionData: InputDigitalActionData_t;
  end;

  SteamInputActionEvent_t__AnalogAction_t = record
    actionHandle: InputAnalogActionHandle_t;
    analogActionData: InputAnalogActionData_t;
  end;

  SteamInputActionEvent_t = record
    controllerHandle: InputHandle_t;
    eEventType: ESteamInputActionEventType;
    case Integer of
      0: (analogAction: SteamInputActionEvent_t__AnalogAction_t);
      1: (digitalAction: SteamInputActionEvent_t__DigitalAction_t);
  end;
  {$IFDEF UNIX}
  {$PACKRECORDS 4}
  {$ELSE}
  {$PACKRECORDS 8}
  {$ENDIF}\n\n''');

# SteamDatagramRelayAuthTicket
f.write('  PSteamDatagramRelayAuthTicket = ^SteamDatagramRelayAuthTicket;\n')
f.write('  SteamDatagramRelayAuthTicket = record end;\n\n')

# Constants.
f.write('const\n')

for callback in api['callback_structs']:
    f.write('  CBID_' + callback['struct'] + ' = '
                      + str(callback['callback_id']) + ';\n')
f.write('\n')

constants = []
get_all_under_tag(constants, 'consts', api)

for constant in constants:
    f.write('  ' + constant['constname'] + ': '
                 + fix_type(constant['consttype']) + ' = '
                 + fix_type(constant['consttype']) + '('
                 + fix_constant_val(constant['constval']) + ');\n')
f.write('\n');

# Interface types.
f.write('type\n')

# Some interfaces with no exposed methods are not listed.
other_interfaces = [
        'ISteamNetworkingConnectionSignaling',
        'ISteamNetworkingSignalingRecvContext',
        ]
interface_names = [interface['classname'] for interface in api['interfaces']]
interface_names += other_interfaces
for interface_name in interface_names:
    f.write('  P' + interface_name + ' = ^' + interface_name + ';\n')
    f.write('  ' + interface_name + ' = record end;\n')

f.write('\n')

# CallbackMsg_t.
f.write('''  PCallbackMsg_t = ^CallbackMsg_t;
  CallbackMsg_t = record
    m_hSteamUser: HSteamUser;
    m_iCallback: Integer;
    m_pubParam: PUInt8;
    m_cubParam: Integer;
  end;\n\n''')

# GNS functions.
f.write('{$IFNDEF STEAM}\n')
f.write('function GameNetworkingSockets_Init(const pIdentity: PSteamNetworkingIdentity; errMsg: PSteamNetworkingErrMsg): Boolean; cdecl; external STEAMLIB;\n')
f.write('procedure GameNetworkingSockets_Kill(); cdecl; external STEAMLIB;\n')
f.write('function SteamAPI_SteamNetworkingSockets_v009(): PISteamNetworkingSockets; cdecl; external STEAMLIB;\n')
f.write('function SteamAPI_SteamNetworkingUtils_v003(): PISteamNetworkingUtils; cdecl; external STEAMLIB;\n')
f.write('{$ENDIF}\n\n')

# Accessors.
for interface in api['interfaces']:
    if 'accessors' not in interface:
        continue

    for accessor in interface['accessors']:
        f.write('function ' + accessor['name_flat'] + '(): P' + interface['classname'] + '; cdecl; external STEAMLIB;\n')
f.write('\n')

# Flat methods.
for interface in api['callback_structs'] + api['structs'] + api['interfaces']:
    if 'methods' not in interface:
        continue;

    type_name = get_or(interface, ['classname', 'struct'], '')
    for method in interface['methods']:
        f.write(steam_api_object_method_prototype(method, type_name, '', True) + '\n')
    f.write('\n')

# Method helpers.
f.write('type\n')
for interface in api['callback_structs'] + api['structs'] + api['interfaces']:
    if 'methods' not in interface:
        continue

    type_name = get_or(interface, ['classname', 'struct'], '')
    f.write('  ' + type_name + 'Helper = record helper for ' + type_name + '\n')
    for method in interface['methods']:
        if 'operator' in method['methodname']:
            continue

        if method['methodname_flat'] not in gns_funcs:
            f.write('    {$IFDEF STEAM}\n')
        f.write('    ' + steam_api_object_method_prototype(method, '', '') + '\n')
        if method['methodname_flat'] not in gns_funcs:
            f.write('    {$ENDIF}\n')
    f.write('  end;\n\n')

# CSteamID to string.
f.write('// SteamID3 string format [U:1:ID]\n')
f.write('function SteamID3(SteamID: CSteamID): String;\n\n')

# steam_api.h
f.write('function SteamAPI_Init(): Boolean; cdecl; external STEAMLIB;\n')
f.write('procedure SteamAPI_Shutdown(); cdecl; external STEAMLIB;\n')
f.write('function SteamAPI_RestartAppIfNecessary(unOwnAppID: UInt32): Boolean; cdecl; external STEAMLIB;\n')
f.write('procedure SteamAPI_ReleaseCurrentThreadMemory(); cdecl; external STEAMLIB;\n')
f.write('procedure SteamAPI_WriteMiniDump(uStructuredExceptionCode: UInt32; pvExceptionInfo: Pointer; uBuildID: UInt32); cdecl; external STEAMLIB;\n')
f.write('procedure SteamAPI_SetMiniDumpContent(const pchMsg: PAnsiChar); cdecl; external STEAMLIB;\n')
f.write('procedure SteamAPI_ManualDispatch_Init(); cdecl; external STEAMLIB;\n')
f.write('procedure SteamAPI_ManualDispatch_RunFrame(hSteamPipe: HSteamPipe); cdecl; external STEAMLIB;\n')
f.write('function SteamAPI_ManualDispatch_GetNextCallback(hSteamPipe: HSteamPipe; pCallbackMsg: PCallbackMsg_t): Boolean; cdecl; external STEAMLIB;\n')
f.write('procedure SteamAPI_ManualDispatch_FreeLastCallback(hSteamPipe: HSteamPipe); cdecl; external STEAMLIB;\n')
f.write('function SteamAPI_ManualDispatch_GetAPICallResult(hSteamPipe: HSteamPipe; hSteamAPICall: SteamAPICall_t; pCallback: Pointer; cubCallback: Integer; iCallbackExpected: Integer; pbFailed: PBoolean): Boolean; cdecl; external STEAMLIB;\n')

# steam_api_common.h
f.write('procedure SteamAPI_RunCallbacks(); cdecl; external STEAMLIB;\n')
f.write('procedure SteamGameServer_RunCallbacks(); cdecl; external STEAMLIB;\n')

# steam_api_internal.h
f.write('function SteamAPI_GetHSteamPipe(): HSteamPipe; cdecl; external STEAMLIB;\n')
f.write('function SteamAPI_GetHSteamUser(): HSteamUser; cdecl; external STEAMLIB;\n')
f.write('function SteamGameServer_GetHSteamPipe(): HSteamPipe; cdecl; external STEAMLIB;\n')
f.write('function SteamGameServer_GetHSteamUser(): HSteamUser; cdecl; external STEAMLIB;\n')

# steam_gameserver.h
f.write('function SteamInternal_GameServer_Init(unIP: UInt32; usLegacySteamPort: UInt16; usGamePort: UInt16; usQueryPort: UInt16; eServerMode: EServerMode; const pchVersionString: PAnsiChar): Boolean; cdecl; external STEAMLIB;\n')
f.write('procedure SteamGameServer_Shutdown(); cdecl; external STEAMLIB;\n\n')

# TSteam, TSteamGS.
f.write('''{$IFDEF STEAM}
type
  TSteam = class
  public
    SteamPipeHandle: HSteamPipe;
    Utils: PISteamUtils;
    User: PISteamUser;
    Screenshots: PISTeamScreenshots;
    Friends: PISteamFriends;
    UGC: PISteamUGC;
    constructor Init();
  end;

  TSteamGS = class
  public
    SteamPipeHandle: HSteamPipe;
    Utils: PISteamUtils;
    GameServer: PISteamGameServer;
    GameServerStats: PISteamGameServerStats;
    UGC: PISteamUGC;
    constructor Init(unIP: uint32; usGamePort: uint16; usQueryPort: uint16; ServerMode: EServerMode; pchVersionString: PChar);
  end;
{$ENDIF}\n\n''')

f.write('implementation\n\n')

# CSteamID to string.
f.write('function SteamID3(SteamID: CSteamID): String;\n')
f.write('begin\n')
f.write('  Result := \'[U:\' + IntToStr(Integer(SteamID.m_EUniverse)) + \':\' + IntToStr(SteamID.m_unAccountID) + \']\';\n')
f.write('end;\n\n')

# TSteam, TSteamGS.
f.write('''{$IFDEF STEAM}
constructor TSteam.Init;
begin
  if not SteamAPI_Init() then
    raise Exception.Create('SteamAPI_Init has failed');

  SteamPipeHandle := SteamAPI_GetHSteamPipe();

  if SteamPipeHandle = 0 then
    raise Exception.Create('SteamPipeHandle is null');

  UGC := SteamAPI_SteamUGC_v016();
  Utils := SteamAPI_SteamUtils_v010();
  User := SteamAPI_SteamUser_v021();
  Screenshots := SteamAPI_SteamScreenshots_v003();
  Friends := SteamAPI_SteamFriends_v017();
end;

constructor TSteamGS.Init(unIP: uint32; usGamePort: uint16; usQueryPort: uint16; ServerMode: EServerMode; pchVersionString: PChar);
begin
  if not SteamInternal_GameServer_Init(unIP, 0, usGamePort, usQueryPort, ServerMode, pchVersionString) then
    raise Exception.Create('SteamAPI_GameServer_Init has failed');

  SteamPipeHandle := SteamGameServer_GetHSteamPipe();

  UGC := SteamAPI_SteamGameServerUGC_v016();
  Utils := SteamAPI_SteamGameServerUtils_v010();
  GameServer := SteamAPI_SteamGameServer_v014();
  GameServerStats := SteamAPI_SteamGameServerStats_v001();
end;
{$ENDIF}\n\n''')

# Implement method helpers.
for interface in api['callback_structs'] + api['structs'] + api['interfaces']:
    if 'methods' not in interface:
        continue

    type_name = get_or(interface, ['classname', 'struct'], '')
    for method in interface['methods']:
        # TODO
        if 'operator' in method['methodname']:
            continue

        if method['methodname_flat'] not in gns_funcs:
            f.write('{$IFDEF STEAM}\n')
        f.write(steam_api_object_method_prototype(method, '', type_name + 'Helper.') + '\n')
        f.write('begin\n')

        f.write('  ')
        if method['returntype'] != 'void':
            f.write('Result := ')
        f.write(method['methodname_flat'] + '(@Self')
        for param in method['params']:
            if '&' in param['paramtype']:
                f.write(', @' + param['paramname'])
            else:
                f.write(', ' + param['paramname'])
        f.write(');\n')

        f.write('end;\n')
        if method['methodname_flat'] not in gns_funcs:
            f.write('{$ENDIF}\n')
        f.write('\n')

f.write('end.\n')
f.close()

# Check files.
################################################################################

# TODO: At least check size?
skip_structs = {
        'SteamDatagramHostedAddress',
        'SteamNetworkingMessage_t',
        'servernetadr_t',
        'gameserveritem_t',
        'SteamDatagramGameCoordinatorServerLogin',
        'SteamNetworkingConfigValue_t',
        }

skip_enums = {
        'k_ESteamNetworkingIdentityType_XboxPairwiseID',
        'k_ESteamNetworkingIdentityType_SonyPSN',
        'k_ESteamNetworkingIdentityType_GoogleStadia',
        }

# Write out Pascal check file.
f = open('check/PasCheck.pas', 'w')

f.write('''{ Warning: This file is generated. Edit gen_steam_bindings.py instead. }

program PasCheck;

{$IFDEF FPC}
{$MODE DELPHI}
{$WARN 4056 OFF : Conversion between ordinals and pointers is not portable}
{$ENDIF}

uses
  Steam in '../Steam.pas';

''')

# Struct variables, used to check offsets.
f.write('var\n')
for struct in api['callback_structs'] + api['structs']:
    if struct['struct'] in skip_structs:
        continue
    if len(struct['fields']) > 0:
        f.write('  var_' + struct['struct'] + ': ' + struct['struct'] + ';\n')

# Main function.
f.write('begin\n')

for enum in enums:
    enum_name = enum_pas_name(enum)
    enum_name_cpp = enum_cpp_name(enum)

    # Enum sizes.
    f.write('  WriteLn(\'' + enum_name_cpp + ' \', SizeOf(' + enum_name + '));\n')

    # Enum values.
    for value in enum['values']:
        if value['name'] in skip_enums:
            continue
        f.write('  WriteLn(\'' + value['name'] + ' \', Int64(' + value['name'] + '));\n')

for struct in api['callback_structs'] + api['structs']:
    if struct['struct'] in skip_structs:
        continue

    # Struct sizes.
    f.write('  WriteLn(\'' + struct['struct'] + ' \', SizeOf(' + struct['struct'] + '));\n')

    # Field offsets.
    for field in struct['fields']:
        memb = struct['struct'] + '.' + field['fieldname']
        if '(*)' in field['fieldtype']:
            f.write('  WriteLn(\'' + memb + ' \', ' +
                    'LongWord(@@var_' + memb + ') - ' +
                    'LongWord(@var_' + struct['struct'] + '));\n')
        else:
            f.write('  WriteLn(\'' + memb + ' \', ' +
                    'LongWord(@var_' + memb + ') - ' +
                    'LongWord(@var_' + struct['struct'] + '));\n')

f.write('  WriteLn(\'CSteamID \', SizeOf(CSteamID));\n')
f.write('  WriteLn(\'CGameID \', SizeOf(CGameID));\n')

f.write('end.\n')
f.close()

# Write out C++ check file.
f = open('check/CPPCheck.cpp', 'w')
f.write('// Warning: This file is generated. Edit gen_steam_bindings.py instead.\n\n')

# Include steam header files.
f.write('#include "' + fix_path(api_path + 'public/steam/steam_api.h') + '"\n')
f.write('#include "' + fix_path(api_path + 'public/steam/steam_gameserver.h') + '"\n')
f.write('#include "' + fix_path(api_path + 'public/steam/steamnetworkingfakeip.h') + '"\n')
f.write('#include <iostream>\n\n')

# Main function.
f.write('int main() {\n')

for enum in enums:
    enum_name = enum_cpp_name(enum)

    # Enum sizes.
    f.write('  std::cout << "' + enum_name + ' " << sizeof(enum ' + enum_name + ') << std::endl;\n')

    # Enum values.
    for value in enum['values']:
        if value['name'] in skip_enums:
            continue
        f.write('  std::cout << "' + value['name'] + ' " << ' + enum_name + '::' + value['name'] + ' << std::endl;\n')

for struct in api['callback_structs'] + api['structs']:
    if struct['struct'] in skip_structs:
        continue

    # Struct sizes.
    f.write('  std::cout << "' + struct['struct'] + ' " << sizeof(' + struct['struct'] + ') << std::endl;\n')

    # Field offsets.
    if len(struct['fields']) > 0:
        f.write('  ' + struct['struct'] + ' var_' + struct['struct'] + ';\n')
    for field in struct['fields']:
        memb = struct['struct'] + '.' + field['fieldname']
        f.write('  std::cout << "' + memb + ' " ' +
                '<< (int)(((unsigned char*)&var_' + memb + ') - ((unsigned char*)&var_' + struct['struct'] + '))  << std::endl;\n')

f.write('  std::cout << "CSteamID " << sizeof(CSteamID) << std::endl;\n')
f.write('  std::cout << "CGameID " << sizeof(CGameID) << std::endl;\n')

f.write('}\n')
f.close()

# Check test script.
################################################################################

f = open(fix_path('check/test.sh'), 'w')

f.write('''#!/bin/sh

# Warning: This file is generated. Edit gen_steam_bindings.py instead.

set -eu

c++ CPPCheck.cpp -o CPPCheck
fpc PasCheck.pas -dSTEAM -k-rpath=\'''' + fix_path(api_path + 'redistributable_bin/linux64') + '''\' \\
                 -Fl\'''' + fix_path(api_path + 'redistributable_bin/linux64') + '''\'


./CPPCheck >CPPOut
./PasCheck >PasOut

diff --unified=0 CPPOut PasOut
''')

f.close()

f = open(fix_path('check/test.bat'), 'w')

f.write('''cl /std:c++14 /EHsc CPPCheck.cpp
fpc -Twin64 -Px86_64 PasCheck.pas

.\CPPCheck.exe >CPPOut
.\PasCheck.exe -Fl\'''' + fix_path(api_path + 'redistributable_bin/win64/') + '''\' >PasOut

fc /L /N CPPOut PasOut
''')

f.close()
