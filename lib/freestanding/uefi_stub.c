/*
 * Gorget UEFI entry point — boots into framebuffer mode and calls main().
 * Minimal: just GOP framebuffer + ConOut text. No file I/O, no interrupts.
 */

#include "uefi.h"

/* ── Framebuffer state ──────────────────────────────────────────── */

static uint32_t* __fb_base   = NULL;
static uint32_t  __fb_width  = 0;
static uint32_t  __fb_height = 0;
static uint32_t  __fb_stride = 0;  /* pixels per scanline */
static EFI_GRAPHICS_PIXEL_FORMAT __fb_format = PixelBlueGreenRedReserved8BitPerColor;

/* ── Framebuffer API (called by Gorget code) ────────────────────── */

void gorget_fb_plot(int64_t x, int64_t y, int64_t color) {
    if (!__fb_base) return;
    if (x < 0 || x >= (int64_t)__fb_width) return;
    if (y < 0 || y >= (int64_t)__fb_height) return;

    uint32_t pixel;
    uint8_t r = (color >> 16) & 0xFF;
    uint8_t g = (color >>  8) & 0xFF;
    uint8_t b = (color      ) & 0xFF;

    if (__fb_format == PixelRedGreenBlueReserved8BitPerColor) {
        pixel = ((uint32_t)r << 0) | ((uint32_t)g << 8) | ((uint32_t)b << 16);
    } else {
        /* PixelBlueGreenRedReserved8BitPerColor (common default) */
        pixel = ((uint32_t)b << 0) | ((uint32_t)g << 8) | ((uint32_t)r << 16);
    }

    __fb_base[y * __fb_stride + x] = pixel;
}

int64_t gorget_fb_width(void)  { return (int64_t)__fb_width; }
int64_t gorget_fb_height(void) { return (int64_t)__fb_height; }

/* ── ConOut text (for panic messages) ───────────────────────────── */

static EFI_SIMPLE_TEXT_OUTPUT_PROTOCOL* __con_out = NULL;

static void __uefi_putchar(int ch) {
    if (!__con_out) return;
    CHAR16 buf[2] = { (CHAR16)ch, 0 };
    __con_out->OutputString(__con_out, buf);
}

/* Import from runtime.c */
extern void __gorget_set_putchar(void (*fn)(int));

/* ── Gorget main (defined by the compiled program) ──────────────── */

extern int main(void);

/* ── UEFI entry point ───────────────────────────────────────────── */

EFI_STATUS efi_main(EFI_HANDLE image_handle, EFI_SYSTEM_TABLE* system_table) {
    (void)image_handle;

    /* Set up text console for panic output */
    __con_out = system_table->ConOut;
    __gorget_set_putchar(__uefi_putchar);

    if (__con_out) {
        __con_out->ClearScreen(__con_out);
    }

    /* Locate GOP for framebuffer access */
    EFI_GRAPHICS_OUTPUT_PROTOCOL* gop = NULL;
    EFI_STATUS status = system_table->BootServices->LocateProtocol(
        &gEfiGraphicsOutputProtocolGuid, NULL, (void**)&gop);

    if (status != EFI_SUCCESS || !gop) {
        CHAR16 msg[] = u"Error: GOP not available\r\n";
        if (__con_out) __con_out->OutputString(__con_out, msg);
        for (;;) {
#if defined(__x86_64__) || defined(_M_X64)
            __asm__ volatile("hlt");
#elif defined(__aarch64__)
            __asm__ volatile("wfi");
#endif
        }
    }

    /* Try to find a good resolution (prefer 1024x768, fall back to current) */
    uint32_t best_mode = gop->Mode->Mode;
    uint32_t best_w = gop->Mode->Info->HorizontalResolution;
    uint32_t best_h = gop->Mode->Info->VerticalResolution;

    for (uint32_t m = 0; m < gop->Mode->MaxMode; m++) {
        UINTN info_size;
        EFI_GRAPHICS_OUTPUT_MODE_INFORMATION* info;
        if (gop->QueryMode(gop, m, &info_size, &info) != EFI_SUCCESS) continue;
        if (info->PixelFormat == PixelBltOnly) continue;

        uint32_t w = info->HorizontalResolution;
        uint32_t h = info->VerticalResolution;

        /* Exact match for 1024x768 */
        if (w == 1024 && h == 768) {
            best_mode = m; best_w = w; best_h = h;
            break;
        }
        /* Otherwise prefer larger than current */
        if (w * h > best_w * best_h) {
            best_mode = m; best_w = w; best_h = h;
        }
    }

    /* Set the chosen mode */
    if (best_mode != gop->Mode->Mode) {
        gop->SetMode(gop, best_mode);
    }

    /* Record framebuffer info */
    __fb_base   = (uint32_t*)(uintptr_t)gop->Mode->FrameBufferBase;
    __fb_width  = gop->Mode->Info->HorizontalResolution;
    __fb_height = gop->Mode->Info->VerticalResolution;
    __fb_stride = gop->Mode->Info->PixelsPerScanLine;
    __fb_format = gop->Mode->Info->PixelFormat;

    /* Clear framebuffer to black */
    for (uint32_t i = 0; i < __fb_stride * __fb_height; i++) {
        __fb_base[i] = 0;
    }

    /* Run the Gorget program */
    main();

    /* Halt — program is done */
    for (;;) {
#if defined(__x86_64__) || defined(_M_X64)
        __asm__ volatile("hlt");
#elif defined(__aarch64__)
        __asm__ volatile("wfi");
#endif
    }
    return EFI_SUCCESS;
}
