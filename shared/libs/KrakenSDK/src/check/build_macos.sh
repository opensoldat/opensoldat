#!/bin/bash
set -e
OUTFILE=build/libfaecheck.dylib
mkdir -p $(dirname "$OUTFILE")
clang -fPIC -fvisibility=hidden -fdata-sections -ffunction-sections -flto \
    -Wall -Wextra -O2 -I../shared -DFAECHECK_EXPORT=1 -DBLAKE2_NO_UNROLLING=1 \
    -shared -Wl,-dead_strip -o "$OUTFILE" faecheck.c rand_posix.c ../shared/monocypher.c
strip -x "$OUTFILE"
install_name_tool -id @loader_path/libfaecheck.dylib "$OUTFILE"
file "$OUTFILE"
