#!/bin/sh

set -eu

if [ "$#" -ne "1" ]; then
  echo "Usage: $0 /path/to/libGameNetworkingSockets.so"
  exit 1
fi

nm --demangle "$1" \
  | grep --ignore-case steamapi \
  | grep --invert-match '::' \
  | awk '{print $3}' >gns_functions.txt
