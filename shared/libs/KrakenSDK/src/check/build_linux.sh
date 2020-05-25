#!/bin/bash
set -e
OUTFILE=build/libfaecheck.so
mkdir -p $(dirname "$OUTFILE")
gcc $CFLAGS -fPIC -fvisibility=hidden -fdata-sections -ffunction-sections -flto \
    -Wall -Wextra -O2 -I../shared -DFAECHECK_EXPORT=1 -DBLAKE2_NO_UNROLLING=1 \
    -shared -Wl,--gc-sections -o "$OUTFILE" faecheck.c rand_posix.c ../shared/monocypher.c
strip -x "$OUTFILE"
file "$OUTFILE"
