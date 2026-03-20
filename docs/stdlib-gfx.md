# gg.gfx — High-Level 2D Graphics

A simple, Gorget-idiomatic API for 2D drawing. Built on top of [`gg.sdl`](stdlib-sdl.md) and written entirely in Gorget (see `lib/gg/gfx.gg`).

For event handling, import `gg.sdl` directly alongside `gg.gfx`.

## Prerequisites

Same as `gg.sdl` — install SDL2 development libraries:

```bash
# macOS
brew install sdl2 sdl2_image sdl2_ttf

# Ubuntu / Debian
apt install libsdl2-dev libsdl2-image-dev libsdl2-ttf-dev
```

## Import

```gorget
from gg.gfx import Canvas, Color, open, close
from gg.gfx import clear, present, fill_rect, fill_circle
from gg.gfx import delay
from gg.sdl import sdl_poll_event, SDL_QUIT  # events come from gg.sdl
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

Created via `open()`, destroyed via `close()`.

## Functions

### Window management

```gorget
Canvas open(String title, int w, int h)  # Open a window and return a Canvas
void close(Canvas c)                   # Close the window and shut down SDL
int width(Canvas c)                    # Get window width in pixels
int height(Canvas c)                   # Get window height in pixels
```

### Drawing

All drawing functions take a `Color` parameter — no need to set draw color separately.

```gorget
void clear(Canvas c, Color color)                                        # Fill entire canvas
void present(Canvas c)                                                    # Display the frame
void fill_rect(Canvas c, int x, int y, int w, int h, Color color)        # Filled rectangle
void draw_rect(Canvas c, int x, int y, int w, int h, Color color)        # Rectangle outline
void draw_line(Canvas c, int x1, int y1, int x2, int y2, Color color)    # Line segment
void draw_point(Canvas c, int x, int y, Color color)                      # Single pixel
void draw_circle(Canvas c, int cx, int cy, int radius, Color color)       # Circle outline
void fill_circle(Canvas c, int cx, int cy, int radius, Color color)       # Filled circle
```

### Timing

```gorget
void delay(int ms)   # Sleep for ms milliseconds
int ticks()           # Milliseconds since SDL was initialized
```

## Example

A bouncing ball with a trail:

```gorget
from gg.gfx import Canvas, Color, open, close
from gg.gfx import clear, present, fill_circle, delay
from gg.gfx import width, height
from gg.sdl import sdl_poll_event, SDL_QUIT

void main():
    auto canvas = open("Bouncing Ball", 800, 600)
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
        int w = width(canvas)
        int h = height(canvas)
        if bx < 20:
            dx = 0 - dx
        if bx > w - 20:
            dx = 0 - dx
        if by < 20:
            dy = 0 - dy
        if by > h - 20:
            dy = 0 - dy

        clear(canvas, black)
        fill_circle(canvas, bx, by, 20, red)
        present(canvas)
        delay(16)

    close(canvas)
```

See `examples/gfx_demo.gg` for a more complete demo.

## Design Notes

`gg.gfx` is implemented as a real Gorget source file (`lib/gg/gfx.gg`), not as a compiler-level module. This means:

- It dogfoods the language — the circle algorithms use Gorget's while loops, arithmetic, and control flow
- It demonstrates cross-module imports (gfx.gg imports from gg.sdl)
- The source is embedded in the compiler binary at build time via `include_str!`
- No special C runtime or codegen dispatch is needed — gfx functions compile like any other Gorget code
