# Binary Signing Issue

## The Problem: `linker-signed` vs ad-hoc signature conflict

When building Barista with `ros dump executable`, the resulting binary (`dist/Barista.app/Contents/MacOS/barista`) cannot be signed with `codesign --force --sign -`.

### What happens with SBCL binary

When `ros dump executable` creates the binary:

1. **Compiles the SBCL kernel** → produces a Mach-O binary with embedded code signature
2. **The linker automatically signs it** → this is called `linker-signed` (flag `0x20002 = adhoc + linker-signed`)
3. **Inserts the binary into the app bundle** → `dist/Barista.app/Contents/MacOS/barista`

### macOS signature types

| Type | Description | Replaceable? |
|---|---|---|
| **linker-signed** | Automatic signature from linker at build time (clang/LLVM) | No |
| **ad-hoc** | Empty signature (`-`), no certificate required. For local use. | Yes |
| **Developer ID** | Requires Apple Developer `$99/year`. For distribution without Gatekeeper warnings. | Yes |

### The conflict

When we attempt `codesign --force --sign - dist/Barista.app`:

```
appsig: replaced with ad-hoc signature
app(bundle): checks executable → encounters linker-signed binary → error: "main executable failed strict validation"
```

**Why this happens:**

- `--deep` says: "sign everything recursively, all binaries inside the bundle"
- The executable file (`barista`) already has a `linker-signed` signature from the linker
- macOS validates the chain: "new app signature" → "old binary signature" → **conflict**
- Strict validation forbids combining `linker-signed` + ad-hoc

### Why flags don't fix it

Attempts with `--preserve-metadata`, `--remove-signature`, `--force`:

```bash
codesign --remove-signature dist/Barista.app  # removes bundle signature
codesign -s - -f --deep dist/Barista.app     # but linker-signed binary still inside
```

The problem: `linker-signed` stamp is embedded **inside the Mach-O**, not in the external list. `codesign` cannot overwrite it without rebuilding.

### Alternatives and why they fail

| Approach | Problem |
|---|---|
| Don't use `--deep` | Only the bundle is signed, but binaries inside remain unsigned/incorrect → macOS still complains |
| Rebuild SBCL without signed binary | Requires rebuilding `ros dump` with linker flags — not trivial |
| macOS `notarization` | Requires Developer ID account, certificate, and upload to Apple. Overkill for open-source |
| **Skip signing** | Works, but first launch shows Gatekeeper window "unknown developer". Users only see this once. |

### Future solution (if needed for production)

For production signing:

1. Disable `linker-signed` when dumping:
   ```lisp
   ;; in build.sh or app.ros
   (sb-ext:save-lisp-and-die "app" :executable t ...)
   ```
   instead of `ros dump` — but `ros dump` is more convenient.

2. Get Apple Developer certificate and do notarization:
   ```bash
   codesign --sign "Developer ID Application: ..." dist/Barista.app
   xcrun notarytool submit Barista.dmg --apple-id ... --password ... --team-id ...
   ```

---

## Conclusion

**The conflict:** SBCL binary has built-in `linker-signed` signature from the build, which cannot be replaced with ad-hoc via `codesign`.  
**Solution:** Stop fighting macOS, skip the signing. Gatekeeper warning once on first install is acceptable for an open-source project without commercial requirements.

For more details, see: [Barista lessons-learned.md](/lessons-learned.md) entry for 2026-06-07.
