# Build Targets

Gorget can compile programs for different environments. The `--target` flag
selects the runtime environment and calling conventions.

## Available Targets

### `native` (default)

```bash
gg build app.gg                    # same as --target native
gg build --target native app.gg
```

Builds for the host operating system with the full Gorget runtime: libc,
file I/O, networking, threads, the full standard library. This is the
default when no `--target` is specified.

### `freestanding`

```bash
gg build --target freestanding demo.gg
```

Builds a bare-metal UEFI application with no operating system, no libc, and
no standard library. The program runs directly on hardware (or in a VM like
QEMU/UTM).

The freestanding runtime provides:
- A bump allocator (64 MB static arena)
- `String` and `Vector` with the same API as the hosted runtime
- `gg.fb` module for framebuffer pixel access
- Panic handler that halts the CPU

The freestanding runtime does NOT provide:
- File I/O (`std.fs`, `std.io`)
- Networking (`std.net`)
- Threads or async (`std.thread`, `std.async`)
- Process management (`std.process`)
- Any `xtd.*` modules

**Architecture selection:**

Bare `--target freestanding` auto-detects the host architecture. On Apple
Silicon, this produces an aarch64 UEFI application. On Intel/AMD, it produces
x86_64. You can override with an explicit architecture suffix:

```bash
gg build --target freestanding           demo.gg   # host arch (auto-detect)
gg build --target freestanding-x86_64    demo.gg   # Intel/AMD 64-bit
gg build --target freestanding-aarch64   demo.gg   # ARM 64-bit
```

The output is a `.efi` PE binary and an `esp/` directory ready for QEMU:

```
demo.efi                    # the UEFI application
esp/EFI/BOOT/BOOTX64.EFI   # (x86_64) or BOOTAA64.EFI (aarch64)
```

**Prerequisites:**

- [LLVM/Clang](https://llvm.org/) with `lld` linker
  - macOS: `brew install llvm` then add `$(brew --prefix llvm)/bin` to `PATH`
  - Linux: `apt install clang lld`

**Running in QEMU:**

The build output prints the exact QEMU command. For x86_64:

```bash
qemu-system-x86_64 \
    -bios OVMF.fd \
    -drive format=raw,file=fat:rw:demo/esp/ \
    -m 128M -vga std -net none
```

For aarch64 (native on Apple Silicon via UTM):

```bash
qemu-system-aarch64 \
    -M virt -cpu cortex-a72 \
    -bios AAVMF_CODE.fd \
    -drive format=raw,file=fat:rw:demo/esp/ \
    -m 128M -device ramfb -net none
```

OVMF/AAVMF firmware is included with QEMU (`brew install qemu`) or
available from the [EDK2 releases](https://github.com/tianocore/edk2/releases).

**Creating a bootable disk image:**

The build produces an `esp/` directory and a `.efi` binary next to the source
file. For a single-file bootable image (easier for UTM, portable to USB
drives), use `mtools` from the directory containing the `.efi`:

```bash
# Install mtools (one-time)
brew install mtools    # macOS
apt install mtools     # Linux

# From the directory containing your .efi (e.g., demo/)
cd demo/
dd if=/dev/zero of=mandelbrot.img bs=1M count=33
mformat -i mandelbrot.img -F ::
mmd -i mandelbrot.img ::/EFI ::/EFI/BOOT
mcopy -i mandelbrot.img mandelbrot.efi ::/EFI/BOOT/BOOTX64.EFI   # or BOOTAA64.EFI
```

Then boot from the image directly:

```bash
qemu-system-x86_64 -bios OVMF.fd -drive format=raw,file=demo/mandelbrot.img -m 128M
```

For UTM's preferred qcow2 format:

```bash
qemu-img convert -f raw -O qcow2 demo/mandelbrot.img demo/mandelbrot.qcow2
```

## Framebuffer API (`gg.fb`)

The `gg.fb` module provides pixel-level access to the UEFI framebuffer.
Available only with `--target freestanding`.

```gorget
from gg.fb import plot, fb_width, fb_height

void main():
    int w = fb_width()
    int h = fb_height()
    int y = 0
    while y < h:
        int x = 0
        while x < w:
            int color = x * 256 / w          # red gradient
            plot(x, y, color * 65536)
            x = x + 1
        y = y + 1
```

**Functions:**

| Function | Description |
|---|---|
| `void plot(int x, int y, int color)` | Draw a pixel. Color is `0xRRGGBB`. |
| `int fb_width()` | Framebuffer width in pixels. |
| `int fb_height()` | Framebuffer height in pixels. |

## Example: Mandelbrot on Bare Metal

```gorget
from gg.fb import plot, fb_width, fb_height

int mandelbrot_color(float cx, float cy, int max_iter):
    float zx = 0.0
    float zy = 0.0
    int i = 0
    while i < max_iter:
        float zx2 = zx * zx
        float zy2 = zy * zy
        if zx2 + zy2 > 4.0:
            int r = (i * 9) % 256
            int g = (i * 5 + 64) % 256
            int b = (i * 13 + 128) % 256
            return r * 65536 + g * 256 + b
        zy = 2.0 * zx * zy + cy
        zx = zx2 - zy2 + cx
        i = i + 1
    return 0

void main():
    int w = fb_width()
    int h = fb_height()
    int y = 0
    while y < h:
        int x = 0
        while x < w:
            float cx = (x as float / w as float) * 3.5 - 2.5
            float cy = (y as float / h as float) * 2.0 - 1.0
            plot(x, y, mandelbrot_color(cx, cy, 256))
            x = x + 1
        y = y + 1
```

Build and run:

```bash
gg build --target freestanding demo/mandelbrot.gg
qemu-system-x86_64 -bios OVMF.fd -drive format=raw,file=fat:rw:demo/esp/ -m 128M
```
