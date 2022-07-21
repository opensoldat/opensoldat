#!/bin/sh

# Script to copy all necessary libraries for OpenSoldat client and server.
# Possibly set `LD_LIBRARY_PATH`, pass the directory where the binaries are
# located, and the destination for the libraries.
#
# Relies on curl, ldd and patchelf.

set -eux

die() {
    echo "$@"
    exit 1
}

if [ $# -ne 2 ]; then
  die "$0 <binary_path> <library_output_path>"
fi

binary_path="$(readlink -f "$1")"
library_output_path="$(readlink -f "$2")"

# Download list of libraries that shouldn't be embedded in the package
lib_blacklist="$(curl https://raw.githubusercontent.com/AppImage/pkg2appimage/master/excludelist \
  | sed "s|#.*||" \
  | sed "s|\+|\\\+|g" \
  | grep -E -v '^$' \
  | tr '\n' '|')"
lib_blacklist="${lib_blacklist%?}"

find "$binary_path" -type f \( -executable -or -name "*.so*" \) -exec ldd {} + \
  | grep "=> /" \
  | awk '{print $3}' \
  | grep -v "$(git rev-parse --show-toplevel)" \
  | grep -E -v "($lib_blacklist)" \
  | sort -u \
  | xargs install -Dm 644 -t "$library_output_path"

# Unfortunately, the GNU dynamic linker ld.so ignores any DT_RPATH entries if
# there is a single DT_RUNPATH entry in an entire DAG. So, we have to remove any
# DT_RUNPATH entries to have our RPATH settings respected. Since we copy the
# whole DAG, removing RPATH settings shouldn't affect anything.
for lib in "$binary_path"/*.so*; do
  patchelf --remove-rpath "$lib"
done
