#!/bin/sh

# Script to copy all necessary libraries for OpenSoldat client and server.
# Possibly `LD_LIBRARY_PATH`, pass the directory where the binaries are located,
# and the destination for the libraries.

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
  | grep "=> /" | awk '{print $3}' \
  | grep -v "$(git rev-parse --show-toplevel)" \
  | grep -E -v "($lib_blacklist)" \
  | sort -u \
  | xargs install -Dm 644 -t "$library_output_path"
