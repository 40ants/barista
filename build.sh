#!/usr/bin/env bash
# build.sh -- Build Barista as a standalone macOS application bundle.
#
# Prerequisites:
#   ros    (Roswell)  https://github.com/roswell/roswell
#   qlot              https://github.com/fukamachi/qlot
#   sips / iconutil   (bundled with macOS Xcode command-line tools)
#
# Usage:
#   ./build.sh            -- build Barista.app in dist/
#   INSTALL=1 ./build.sh  -- also install to /Applications/

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DIST_DIR="$SCRIPT_DIR/dist"
APP_NAME="Barista"
APP_BUNDLE="$DIST_DIR/$APP_NAME.app"
EXECUTABLE_NAME="barista"
LISP=sbcl-bin

cd "$SCRIPT_DIR"

# ---- 1. Install pinned dependencies via qlot ------------------------------
if [ ! -d ".qlot" ]; then
  echo "==> Installing dependencies via qlot..."
  qlot install
fi

# ---- 2. Build SBCL executable image via ASDF -------------------------------
# ASDF build creates a clean executable without linker-signed signature
# (avoiding the conflict with codesign that ros dump has).
echo "==> Building SBCL executable..."
rm -f barista
qlot exec ros run -L $LISP -e '(asdf:make :barista)'

# ---- 3. Create .app bundle ------------------------------------------------
echo "==> Creating $APP_NAME.app bundle..."
rm -rf "$APP_BUNDLE"
mkdir -p "$APP_BUNDLE/Contents/MacOS"
mkdir -p "$APP_BUNDLE/Contents/Resources"

mv "$SCRIPT_DIR/$EXECUTABLE_NAME" "$APP_BUNDLE/Contents/MacOS/$EXECUTABLE_NAME"
chmod +x "$APP_BUNDLE/Contents/MacOS/$EXECUTABLE_NAME"

# ---- 4. Generate .icns icon -----------------------------------------------
ICON_PLIST_ENTRY=""
if [ -f "$SCRIPT_DIR/images/icon.png" ]; then
  echo "==> Generating app icon..."
  ICONSET_DIR="$DIST_DIR/$APP_NAME.iconset"
  mkdir -p "$ICONSET_DIR"
  for SIZE in 16 32 64 128 256 512; do
    sips -z $SIZE $SIZE "$SCRIPT_DIR/images/icon.png" \
         --out "$ICONSET_DIR/icon_${SIZE}x${SIZE}.png" >/dev/null 2>&1 || true
    DOUBLE=$((SIZE * 2))
    sips -z $DOUBLE $DOUBLE "$SCRIPT_DIR/images/icon.png" \
         --out "$ICONSET_DIR/icon_${SIZE}x${SIZE}@2x.png" >/dev/null 2>&1 || true
  done
  iconutil -c icns "$ICONSET_DIR" -o "$APP_BUNDLE/Contents/Resources/$APP_NAME.icns"
  rm -rf "$ICONSET_DIR"
  ICON_PLIST_ENTRY="  <key>CFBundleIconFile</key>
  <string>$APP_NAME</string>"
else
  echo "==> (No images/icon.png found; skipping icon generation)"
fi

# ---- 5. Write Info.plist --------------------------------------------------
cat > "$APP_BUNDLE/Contents/Info.plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleName</key>
  <string>$APP_NAME</string>
  <key>CFBundleDisplayName</key>
  <string>$APP_NAME</string>
  <key>CFBundleIdentifier</key>
  <string>com.barista.app</string>
  <key>CFBundleVersion</key>
  <string>1.0</string>
  <key>CFBundleShortVersionString</key>
  <string>1.0</string>
  <key>CFBundleExecutable</key>
  <string>$EXECUTABLE_NAME</string>
  <key>CFBundlePackageType</key>
  <string>APPL</string>
  <key>LSMinimumSystemVersion</key>
  <string>11.0</string>
  <key>NSHighResolutionCapable</key>
  <true/>
  $ICON_PLIST_ENTRY
</dict>
</plist>
PLIST

echo "==> Built: $APP_BUNDLE"

# ---- 6. Optionally install to /Applications/ ------------------------------
if [ "${INSTALL:-0}" = "1" ]; then
  echo "==> Installing to /Applications/$APP_NAME.app..."
  rm -rf "/Applications/$APP_NAME.app"
  cp -R "$APP_BUNDLE" "/Applications/$APP_NAME.app"
  echo "==> Installed."
fi

echo "==> Done."
