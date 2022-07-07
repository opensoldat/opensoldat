#!/bin/bash
# Creates 'OpenSoldat Client.app' from files in build/

# Abort on non-zero return code
set -e

ARCH=x86_64
SRC="$(dirname $0)/.."
BUILD="$SRC/build"
BUNDLE="$BUILD/OpenSoldat Client.app"
CODESIGN_FLAGS=""

if [ -z "$OPENSOLDAT_CS_IDENTITY" ]; then
    echo "Error: Set OPENSOLDAT_CS_IDENTITY to your code signing certificate's name."
    exit 1
fi

if [ ! -z "$OPENSOLDAT_CS_NO_TIMESTAMP" ]; then
    echo "Warning: Time stamping is disabled (use for development only)"
    CODESIGN_FLAGS="$CODESIGN_OPTS --timestamp=none"
fi

echo "Source directory: $SRC"

# Create clean bundle structure
echo "Creating bundle skeleton..."
rm -rf "$BUNDLE"
mkdir -p "$BUNDLE/Contents/Frameworks"
mkdir -p "$BUNDLE/Contents/MacOS"
mkdir -p "$BUNDLE/Contents/Resources/base"

# Create Info.plist
# TODO Write version information
echo "Writing Info.plist..."
cp "$SRC/macOS/Info.plist" "$BUNDLE/Contents/"

# Copy binaries
echo "Writing binaries..."
rsync -pt "$BUILD/opensoldat" "$BUNDLE/Contents/MacOS/"
rsync -rlpt "$SRC/macOS/Frameworks/"*".dylib" "$BUNDLE/Contents/Frameworks"
rsync -rlpt --exclude Headers "$SRC/macOS/Frameworks/"*".framework" "$BUNDLE/Contents/Frameworks"

#Copy game files
echo "Copying game files..."
rsync -pt "$BUILD/soldat.smod" "$BUNDLE/Contents/Resources/"
rsync -pt "$BUILD/"*".ttf" "$BUNDLE/Contents/Resources/"

# Thin bundle for $ARCH and remove clutter
echo "Thinning bundle..."
ditto -arch "$ARCH" "$BUNDLE" "$BUNDLE.thin"
rm -rf "$BUNDLE"
mv "$BUNDLE.thin" "$BUNDLE"

# Seal bundle
#   deep: Also sign embedded dynamic libraries and frameworks
#   force: Replace existing (framework) signatures with our own
#   entitlements: Configures application sandbox
#   options: kill (enforce code signature during runtime), library (enforce team identification)
echo "Sealing bundle with your identity \`$OPENSOLDAT_CS_IDENTITY'..."
codesign \
    --sign "$OPENSOLDAT_CS_IDENTITY" \
    --deep \
    --force \
    --entitlements "$SRC/macOS/opensoldat.entitlements" \
    --options "kill,library" \
    $CODESIGN_FLAGS \
    "$BUNDLE"

# \o/
echo "Bundle built at \`$BUNDLE'. Fair winds!"
