#!/bin/bash
# Creates symlinks from build/ to dynamic libraries in libs/macOS/
set -ex
cd "$(dirname $0)/../build"
ln -sf "../macOS/Frameworks/"*".dylib" .
ln -sf "../macOS/Frameworks/"*".framework" .
