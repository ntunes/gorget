#ifndef GORGET_UEFI_H
#define GORGET_UEFI_H

/*
 * Minimal UEFI type definitions — just enough for GOP framebuffer access.
 * No EDK2 dependency. Hand-written from the UEFI 2.10 spec.
 */

#include <stdint.h>
#include <stddef.h>

typedef uint64_t UINTN;
typedef int64_t  INTN;
typedef uint64_t EFI_STATUS;
typedef void*    EFI_HANDLE;
typedef uint16_t CHAR16;
typedef uint8_t  BOOLEAN;

#define EFI_SUCCESS 0

/* GUIDs */
typedef struct {
    uint32_t Data1;
    uint16_t Data2;
    uint16_t Data3;
    uint8_t  Data4[8];
} EFI_GUID;

/* Forward declarations */
typedef struct EFI_SIMPLE_TEXT_OUTPUT_PROTOCOL EFI_SIMPLE_TEXT_OUTPUT_PROTOCOL;
typedef struct EFI_BOOT_SERVICES EFI_BOOT_SERVICES;
typedef struct EFI_SYSTEM_TABLE EFI_SYSTEM_TABLE;

/* Simple Text Output */
typedef EFI_STATUS (*EFI_TEXT_STRING)(EFI_SIMPLE_TEXT_OUTPUT_PROTOCOL* This, CHAR16* String);
typedef EFI_STATUS (*EFI_TEXT_CLEAR_SCREEN)(EFI_SIMPLE_TEXT_OUTPUT_PROTOCOL* This);

struct EFI_SIMPLE_TEXT_OUTPUT_PROTOCOL {
    void*                  Reset;
    EFI_TEXT_STRING        OutputString;
    void*                  TestString;
    void*                  QueryMode;
    void*                  SetMode;
    void*                  SetAttribute;
    EFI_TEXT_CLEAR_SCREEN  ClearScreen;
    /* ... rest not needed */
};

/* Boot Services (only LocateProtocol needed) */
typedef EFI_STATUS (*EFI_LOCATE_PROTOCOL)(EFI_GUID* Protocol, void* Registration, void** Interface);

struct EFI_BOOT_SERVICES {
    char _pad[320];  /* TableHeader + preceding function pointers */
    EFI_LOCATE_PROTOCOL LocateProtocol;
    /* ... rest not needed */
};

/* System Table */
struct EFI_SYSTEM_TABLE {
    char                           _hdr[24];   /* EFI_TABLE_HEADER */
    CHAR16*                        FirmwareVendor;
    uint32_t                       FirmwareRevision;
    EFI_HANDLE                     ConsoleInHandle;
    void*                          ConIn;
    EFI_HANDLE                     ConsoleOutHandle;
    EFI_SIMPLE_TEXT_OUTPUT_PROTOCOL* ConOut;
    EFI_HANDLE                     StandardErrorHandle;
    void*                          StdErr;
    void*                          RuntimeServices;
    EFI_BOOT_SERVICES*             BootServices;
};

/* Graphics Output Protocol (GOP) */
typedef enum {
    PixelRedGreenBlueReserved8BitPerColor,
    PixelBlueGreenRedReserved8BitPerColor,
    PixelBitMask,
    PixelBltOnly,
} EFI_GRAPHICS_PIXEL_FORMAT;

typedef struct {
    uint32_t Version;
    uint32_t HorizontalResolution;
    uint32_t VerticalResolution;
    EFI_GRAPHICS_PIXEL_FORMAT PixelFormat;
    /* PixelBitmask */
    uint32_t RedMask;
    uint32_t GreenMask;
    uint32_t BlueMask;
    uint32_t ReservedMask;
    uint32_t PixelsPerScanLine;
} EFI_GRAPHICS_OUTPUT_MODE_INFORMATION;

typedef struct {
    uint32_t MaxMode;
    uint32_t Mode;
    EFI_GRAPHICS_OUTPUT_MODE_INFORMATION* Info;
    UINTN SizeOfInfo;
    UINTN FrameBufferBase;
    UINTN FrameBufferSize;
} EFI_GRAPHICS_OUTPUT_PROTOCOL_MODE;

typedef struct EFI_GRAPHICS_OUTPUT_PROTOCOL EFI_GRAPHICS_OUTPUT_PROTOCOL;

typedef EFI_STATUS (*EFI_GRAPHICS_OUTPUT_PROTOCOL_QUERY_MODE)(
    EFI_GRAPHICS_OUTPUT_PROTOCOL* This, uint32_t ModeNumber,
    UINTN* SizeOfInfo, EFI_GRAPHICS_OUTPUT_MODE_INFORMATION** Info);

typedef EFI_STATUS (*EFI_GRAPHICS_OUTPUT_PROTOCOL_SET_MODE)(
    EFI_GRAPHICS_OUTPUT_PROTOCOL* This, uint32_t ModeNumber);

struct EFI_GRAPHICS_OUTPUT_PROTOCOL {
    EFI_GRAPHICS_OUTPUT_PROTOCOL_QUERY_MODE QueryMode;
    EFI_GRAPHICS_OUTPUT_PROTOCOL_SET_MODE   SetMode;
    void*                                   Blt;
    EFI_GRAPHICS_OUTPUT_PROTOCOL_MODE*      Mode;
};

/* GOP GUID: {9042a9de-23dc-4a38-96fb-7aded080516a} */
static EFI_GUID gEfiGraphicsOutputProtocolGuid = {
    0x9042a9de, 0x23dc, 0x4a38,
    {0x96, 0xfb, 0x7a, 0xde, 0xd0, 0x80, 0x51, 0x6a}
};

#endif /* GORGET_UEFI_H */
