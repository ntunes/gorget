# std.gfx — High-Level 2D Graphics

A simple, Gorget-idiomatic API for 2D drawing. Built on top of [`std.sdl`](stdlib-sdl.md) and written entirely in Gorget (see `lib/std/gfx.gg`).

For event handling, import `std.sdl` directly alongside `std.gfx`.

## Prerequisites

Same as `std.sdl` — install SDL2 development libraries:

```bash
# macOS
brew install sdl2 sdl2_image sdl2_ttf

# Ubuntu / Debian
apt install libsdl2-dev libsdl2-image-dev libsdl2-ttf-dev
```

## Import

```gorget
from std.gfx import Canvas, Color, gfx_open, gfx_close
from std.gfx import gfx_clear, gfx_present, gfx_fill_rect, gfx_fill_circle
from std.gfx import gfx_delay
from std.sdl import sdl_poll_event, SDL_QUIT  # events come from std.sdl
```

## Types

### Color

A color with red, green, blue, and alpha channels (0-255 each).

```gorget
struct Color:
    int r
    int g
    int b
    int a
```

Construct directly:

```gorget
auto red = Color(255, 0, 0, 255)
auto semi_transparent_blue = Color(0, 0, 255, 128)
```

### Canvas

An opaque drawing surface wrapping an SDL window and renderer.

```gorget
struct Canvas:
    SDLWindow win
    SDLRenderer ren
```

Created via `gfx_open()`, destroyed via `gfx_close()`.

## Functions

### Window management

```gorget
Canvas gfx_open(str title, int w, int h)  # Open a window and return a Canvas
void gfx_close(Canvas c)                   # Close the window and shut down SDL
int gfx_width(Canvas c)                    # Get window width in pixels
int gfx_height(Canvas c)                   # Get window height in pixels
```

### Drawing

All drawing functions take a `Color` parameter — no need to set draw color separately.

```gorget
void gfx_clear(Canvas c, Color color)                                        # Fill entire canvas
void gfx_present(Canvas c)                                                    # Display the frame
void gfx_fill_rect(Canvas c, int x, int y, int w, int h, Color color)        # Filled rectangle
void gfx_draw_rect(Canvas c, int x, int y, int w, int h, Color color)        # Rectangle outline
void gfx_draw_line(Canvas c, int x1, int y1, int x2, int y2, Color color)    # Line segment
void gfx_draw_point(Canvas c, int x, int y, Color color)                      # Single pixel
void gfx_draw_circle(Canvas c, int cx, int cy, int radius, Color color)       # Circle outline
void gfx_fill_circle(Canvas c, int cx, int cy, int radius, Color color)       # Filled circle
```

### Timing

```gorget
void gfx_delay(int ms)   # Sleep for ms milliseconds
int gfx_ticks()           # Milliseconds since SDL was initialized
```

## Example

A bouncing ball with a trail:

```gorget
from std.gfx import Canvas, Color, gfx_open, gfx_close
from std.gfx import gfx_clear, gfx_present, gfx_fill_circle, gfx_delay
from std.gfx import gfx_width, gfx_height
from std.sdl import sdl_poll_event, SDL_QUIT

void main():
    auto canvas = gfx_open("Bouncing Ball", 800, 600)
    auto black = Color(0, 0, 0, 255)
    auto red = Color(220, 50, 50, 255)

    int bx = 400
    int by = 300
    int dx = 3
    int dy = 2

    auto running = true
    while running:
        auto event = sdl_poll_event()
        if event.event_type == SDL_QUIT:
            running = false

        bx = bx + dx
        by = by + dy
        int w = gfx_width(canvas)
        int h = gfx_height(canvas)
        if bx < 20:
            dx = 0 - dx
        if bx > w - 20:
            dx = 0 - dx
        if by < 20:
            dy = 0 - dy
        if by > h - 20:
            dy = 0 - dy

        gfx_clear(canvas, black)
        gfx_fill_circle(canvas, bx, by, 20, red)
        gfx_present(canvas)
        gfx_delay(16)

    gfx_close(canvas)
```

See `examples/gfx_demo.gg` for a more complete demo.

## Design Notes

`std.gfx` is implemented as a real Gorget source file (`lib/std/gfx.gg`), not as a compiler-level module. This means:

- It dogfoods the language — the circle algorithms use Gorget's while loops, arithmetic, and control flow
- It demonstrates cross-module imports (gfx.gg imports from std.sdl)
- The source is embedded in the compiler binary at build time via `include_str!`
- No special C runtime or codegen dispatch is needed — gfx functions compile like any other Gorget code
