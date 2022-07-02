#!/usr/bin/env python3

# This script writes out a file `ScriptFFITests.pas` and a script `ffi.pas`
# designed to test the PascalScript FFI functionality.

# It could definitely stand to be refactored but it works.

max_members_per_struct = 10
num_structs = 10
max_args_per_function = 10
num_functions = 100


import os
import random
import sys

random.seed()
os.chdir(os.path.dirname(sys.argv[0]))

# Generate types.
type_names = ['SmallInt', 'ShortInt', 'Integer', 'Int64', 'Single', 'Double',
        'Extended', 'String']

def get_value(type_name):
    if type_name == 'SmallInt':
        return str(random.randrange(-128, 128))
    elif type_name == 'ShortInt':
        return str(random.randrange(-128, 128))
    elif type_name == 'Integer':
        return str(random.randrange(-128, 128))
    elif type_name == 'Int64':
        return str(random.randrange(-128, 128))
    elif type_name == 'Single':
        return str(random.randrange(0, 10)) + '.0'
    elif type_name == 'Double':
        return str(random.randrange(0, 10)) + '.0'
    elif type_name == 'Extended':
        return str(random.randrange(0, 10)) + '.0'
    elif type_name == 'String':
        return '\'' + str(random.random()) + '\''

    ret = []
    for sub in record_types[type_name]:
        ret.append(get_value(sub))

    return ret

record_types = {}

def gen_record_type(i):
    name = 'r' + str(i)
    record_types[name] = [random.choice(type_names) for i in range(random.randrange(1, max_members_per_struct))]
    type_names.append(name)

for i in range(num_structs):
    gen_record_type(i)

# Generate functions.
fns = []

def gen_fn(i):
    ret = {}
    ret['name'] = 'fn' + str(i)
    ret['args'] = [random.choice(type_names) for i in range(random.randrange(1, max_args_per_function))]
    ret['expargs'] = [get_value(type_name) for type_name in ret['args']]
    ret['ret'] = random.choice(type_names)
    ret['expret'] = get_value(ret['ret'])

    ret['proto'] = 'function ' + ret['name'] + '('
    for i in range(len(ret['args'])):
        ret['proto'] += 'a' + str(i) + ': ' + ret['args'][i]
        if i != len(ret['args']) - 1:
            ret['proto'] += '; '
        else:
            ret['proto'] += ')'
    ret['proto'] += ': ' + ret['ret'] + ';'

    fns.append(ret)

for i in range(num_functions):
    gen_fn(i)

# Write SC3 internal file.
f = open('ScriptFFITests.pas', 'w')

indent = 0
def addl(s):
    global indent
    f.write((' ' * indent * 2) + s + '\n')

addl('''{ Warning: This file is generated. Edit gen_ffi_tests.py instead. }

unit ScriptFFITests;

interface

uses
  PascalCompiler,
  PascalExec,
  ScriptCore3Api;

type
  TScriptFFITestsAPI = class(TScriptCore3API)
    procedure CompilerRegister(Compiler: TPascalCompiler); override;
    procedure RuntimeRegisterApi(Exec: TPascalExec); override;
  end;

implementation

type''')

# Record definitions.
def print_record_def(rt):
    global indent
    f.write('record\n')
    indent += 1
    for i in range(len(record_types[rt])):
        addl('m' + str(i) + ': ' + record_types[rt][i] + ';')
    indent -= 1
    addl('end;\n')

def print_record_def_one_line(rt):
    global indent
    f.write('record ')
    for i in range(len(record_types[rt])):
        f.write('m' + str(i) + ': ' + record_types[rt][i] + '; ')
    f.write('end;')

indent += 1
for rt in record_types.keys():
    f.write('  ' + rt + ' = packed ')
    print_record_def(rt)
indent -= 1

addl('{$PUSH}')
addl('{$ASSERTIONS ON}\n')

# Function definitions.
def print_exp_val_pascalscript(arg_type, arg_name, expval):
    if arg_type in record_types:
        for i in range(len(record_types[arg_type])):
            print_exp_val_pascalscript(record_types[arg_type][i], arg_name + '.m' + str(i), expval[i])
    else:
        addl('Assert(' + arg_name + ' = ' + expval + ');')

def assign_exp_val_pascalscript(var_type, var_name, expval):
    if var_type in record_types:
        for i in range(len(record_types[var_type])):
            assign_exp_val_pascalscript(record_types[var_type][i], var_name + '.m' + str(i), expval[i])
    else:
        addl(var_name + ' := ' + expval + ';')

for fn in fns:
    addl(fn['proto'])
    i = 0
#    for arg in fn['args']:
#        if arg in record_types:
#            if i == 0:
#                addl('var')
#            indent += 1
#            addl('a' + str(i) + ': ' + arg + ';')
#            indent -= 1
#            i += 1
    addl('var')
    indent += 1
    addl('ret: ' + fn['ret'] + ';')
    indent -= 1
    addl('begin')
    i = 0
    indent += 1
    for i in range(len(fn['args'])):
        print_exp_val_pascalscript(fn['args'][i], 'a' + str(i), fn['expargs'][i])
    assign_exp_val_pascalscript(fn['ret'], 'ret', fn['expret'])
    addl('Result := ret;')
    indent -= 1
    addl('end;\n')

# Register types and functions at compile time.
addl('procedure TScriptFFITestsAPI.CompilerRegister(Compiler: TPascalCompiler);')
addl('begin')
indent += 1
for rt in record_types.keys():
    f.write('  Compiler.AddType(\'' + rt + '\', \'')
    print_record_def_one_line(rt)
    f.write('\');\n')
for fn in fns:
    addl('Compiler.AddFunction(\'' + fn['proto'] + '\');')
indent -= 1
addl('end;\n')

# Register functions at runtime.
addl('procedure TScriptFFITestsAPI.RuntimeRegisterApi(Exec: TPascalExec);')
addl('begin')
indent += 1
for fn in fns:
    addl('Exec.AddFunction(@' + fn['name'] + ', \'' + fn['name'] + '\');')
indent -= 1
addl('end;\n')
addl('{$POP}\n')
addl('end.')

f.close()

# Write SC3 script.
f = open('ffi/ffi.pas', 'w')

addl('{ Warning: This file is generated, edit gen_ffi_tests.py instead. }\n')

addl('''procedure Assert(b: Boolean);
begin
  if not b then
    RaiseException(ERCustomError, 'Unexpected value.');
end;\n''')
i = 0
j = 0
addl('var')
indent += 1
for fn in fns:
    for arg in fn['args']:
        addl('a' + str(i) + ': ' + arg + ';')
        i += 1
    addl('ret' + str(j) + ': ' + fn['ret'] + ';')
    j += 1
indent -= 1
addl('begin')
indent += 1
addl('WriteLn(\'Starting FFI tests...\');');
i = 0
j = 0
for fn in fns:
    for idx in range(len(fn['args'])):
        assign_exp_val_pascalscript(fn['args'][idx], 'a' + str(i), fn['expargs'][idx])
        i += 1
    i -= len(fn['args'])
    fncall = 'ret' + str(j) + ' := ' + fn['name'] + '('
    for idx in range(len(fn['args'])):
        fncall += 'a' + str(i)
        i += 1
        if idx == len(fn['args']) - 1:
            fncall += ');'
        else:
            fncall += ', '
    addl('WriteLn(IntToStr(' + str(j) + '));')
    addl(fncall)
    print_exp_val_pascalscript(fn['ret'], 'ret' + str(j), fn['expret'])
    j += 1

addl('WriteLn(\'All tests passed!\');');
indent -= 1

addl('end.')

f.close()
