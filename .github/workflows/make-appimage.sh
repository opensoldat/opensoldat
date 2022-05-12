#!/bin/sh

# Script to generate `.AppImage` files for Soldat client and server.
# Pass the directory containing the AppDir, not the AppDir itself, and as the
# second argument the string "client" or "server".

set -eu

die() {
    echo "$@"
    exit 1
}

if [ $# -ne 2 ]; then
  die "Usage: $0 <appdir_path> <client|server>"
fi

appdir_path="$1"
appimage_type="$2"

if [ "$appimage_type" = "client" ]; then
  # Add icon
  convert client/soldat.ico build/soldat.png
  install -Dm 644 build/soldat-2.png "$appdir_path/AppDir/soldat.png"

  # Generate desktop file
  cat <<EOF > "$appdir_path/AppDir/soldat.desktop"
[Desktop Entry]
Type=Application
Categories=Game
Name=Soldat
Exec=soldat -joinurl %u
Icon=soldat
StartupNotify=false
Terminal=false
MimeType=x-scheme-handler/soldat;
EOF

  # Generate main run script
  cat <<EOF > "$appdir_path/AppDir/AppRun"
#!/bin/sh

set -e

cd "\$(dirname "\$0")"
data_dir="\${XDG_DATA_HOME:-\$HOME/.local/share}/Soldat/Soldat"
mkdir -p "\$data_dir"

export LD_LIBRARY_PATH=lib
if [ "\$1" = "server" ]; then
  shift
  ./bin/soldatserver -fs_userpath "\$data_dir" "\$@"
else
  ./bin/soldat -fs_portable 0 -fs_userpath "\$data_dir" "\$@"
fi
EOF
else
  # Add icon
  convert client/soldat.ico build/soldat.png
  install -Dm 644 build/soldat-2.png "$appdir_path/AppDir/soldat.png"

  # Generate desktop file
  cat <<EOF > "$appdir_path/AppDir/soldatserver.desktop"
[Desktop Entry]
Type=Application
Categories=Game
Name=SoldatServer
Exec=soldatserver
Icon=soldat
StartupNotify=false
Terminal=true
EOF

  # Generate main run script
  cat <<EOF > "$appdir_path/AppDir/AppRun"
#!/bin/sh

set -e

cd "\$(dirname "\$0")"
data_dir="\${XDG_DATA_HOME:-\$HOME/.local/share}/Soldat/SoldatServer"
mkdir -p "\$data_dir"

export LD_LIBRARY_PATH=lib
./bin/soldatserver -fs_userpath "\$data_dir" "\$@"
EOF
fi

chmod +x "$appdir_path/AppDir/AppRun"

# Download list of libraries that shouldn't be embedded in the package
lib_blacklist="$(curl https://raw.githubusercontent.com/AppImage/pkg2appimage/master/excludelist \
  | sed "s|#.*||" \
  | sed "s|\+|\\\+|g" \
  | grep -E -v '^$' \
  | tr '\n' '|')"
lib_blacklist="${lib_blacklist%?}"

# Copy dependencies inside the package
LD_LIBRARY_PATH="$appdir_path/AppDir/lib" find "$appdir_path/AppDir" -type f \( -executable -or -name "*.so*" \) -exec ldd {} + \
  | grep "=> /" | awk '{print $3}' | sort -u \
  | grep -v "$appdir_path/AppDir" \
  | grep -E -v "($lib_blacklist)" \
  | xargs install -Dm 644 -t "$appdir_path/AppDir/lib"

# Build AppImage
arch="$(uname -m)"
version="$(git describe --tags --always)"
if [ "$appimage_type" = "client" ]; then
  appimage_name="Soldat-$version-$arch.AppImage"
else
  appimage_name="SoldatServer-$version-$arch.AppImage"
fi

appimagetool_name="appimagetool-$arch.AppImage"
appimagetool="build/$appimagetool_name"
if [ ! -f "$appimagetool" ]; then
  curl -L "https://github.com/AppImage/AppImageKit/releases/download/continuous/$appimagetool_name" --output "$appimagetool"
  chmod u+x "$appimagetool"
fi

"$appimagetool" "$appdir_path/AppDir" "$appdir_path/$appimage_name"
chmod u+x "$appdir_path/$appimage_name"
