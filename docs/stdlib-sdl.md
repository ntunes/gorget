# gg.sdl — Raw SDL2 Bindings

Low-level, 1:1 bindings to SDL2 for window management, 2D rendering, input events, textures, text, and timing.

For a simpler drawing API, see [`gg.gfx`](stdlib-gfx.md).

## Prerequisites

Install SDL2 development libraries:

```bash
# macOS
brew install sdl2 sdl2_image sdl2_ttf

# Ubuntu / Debian
apt install libsdl2-dev libsdl2-image-dev libsdl2-ttf-dev
```

## Import

```gorget
from gg.sdl import sdl_init, sdl_create_window, sdl_create_renderer
from gg.sdl import SDL_INIT_VIDEO, SDL_QUIT, SDL_KEYDOWN, SDLK_ESCAPE
```

## Types

| Type | Description |
|------|-------------|
| `SDLWindow` | Opaque handle to an SDL window |
| `SDLRenderer` | Opaque handle to an SDL renderer |
| `SDLTexture` | Opaque handle to an SDL texture |
| `SDLFont` | Opaque handle to a TTF font |
| `SDLEvent` | Event struct with fields below |

### SDLEvent fields

| Field | Type | Description |
|-------|------|-------------|
| `event_type` | `int` | Event type (SDL_QUIT, SDL_KEYDOWN, etc.) |
| `key_code` | `int` | SDLK_* value for key events |
| `mouse_x` | `int` | Mouse X position |
| `mouse_y` | `int` | Mouse Y position |
| `mouse_button` | `int` | Mouse button index |

## Constants

### Init flags

| Constant | Value | Description |
|----------|-------|-------------|
| `SDL_INIT_VIDEO` | 0x20 | Initialize video subsystem |
| `SDL_INIT_AUDIO` | 0x10 | Initialize audio subsystem |
| `SDL_INIT_EVERYTHING` | 0x7231 | Initialize all subsystems |

### Event types

| Constant | Value |
|----------|-------|
| `SDL_QUIT` | 256 |
| `SDL_KEYDOWN` | 768 |
| `SDL_KEYUP` | 769 |
| `SDL_MOUSEMOTION` | 1024 |
| `SDL_MOUSEBUTTONDOWN` | 1025 |
| `SDL_MOUSEBUTTONUP` | 1026 |

### Key codes

Arrow keys: `SDLK_LEFT`, `SDLK_RIGHT`, `SDLK_UP`, `SDLK_DOWN`

Special keys: `SDLK_ESCAPE` (27), `SDLK_SPACE` (32), `SDLK_RETURN` (13)

Letters: `SDLK_a` through `SDLK_z` (97-122)

### Window flags

`SDL_WINDOW_SHOWN` (4), `SDL_WINDOW_RESIZABLE` (32), `SDL_WINDOW_FULLSCREEN` (1)

### Renderer flags

`SDL_RENDERER_ACCELERATED` (2), `SDL_RENDERER_PRESENTVSYNC` (4)

## Functions

### Lifecycle

```gorget
int sdl_init(int flags)       # Initialize SDL. Returns 0 on success.
void sdl_quit()                # Shut down SDL, SDL_image, and SDL_ttf.
```

### Window

```gorget
SDLWindow sdl_create_window(str title, int w, int h, int flags)
void sdl_destroy_window(SDLWindow win)
int sdl_get_window_width(SDLWindow win)
int sdl_get_window_height(SDLWindow win)
```

### Renderer

```gorget
SDLRenderer sdl_create_renderer(SDLWindow win, int flags)
void sdl_destroy_renderer(SDLRenderer r)
void sdl_set_draw_color(SDLRenderer r, int red, int green, int blue, int alpha)
void sdl_clear(SDLRenderer r)
void sdl_present(SDLRenderer r)
void sdl_draw_rect(SDLRenderer r, int x, int y, int w, int h)
void sdl_fill_rect(SDLRenderer r, int x, int y, int w, int h)
```

### Drawing

```gorget
void sdl_draw_line(SDLRenderer r, int x1, int y1, int x2, int y2)
void sdl_draw_point(SDLRenderer r, int x, int y)
void sdl_set_blend_mode(SDLRenderer r, int mode)
```

### Textures (SDL2_image)

```gorget
SDLTexture sdl_load_texture(SDLRenderer r, str path)
void sdl_destroy_texture(SDLTexture t)
void sdl_render_texture(SDLRenderer r, SDLTexture t, int x, int y)
void sdl_render_texture_sized(SDLRenderer r, SDLTexture t, int x, int y, int w, int h)
int sdl_texture_width(SDLTexture t)
int sdl_texture_height(SDLTexture t)
void sdl_set_texture_alpha(SDLTexture t, int alpha)
```

### Text (SDL2_ttf)

```gorget
SDLFont sdl_load_font(str path, int size)
void sdl_close_font(SDLFont f)
SDLTexture sdl_render_text(SDLRenderer r, SDLFont f, str text, int red, int green, int blue)
void sdl_draw_text(SDLRenderer r, SDLFont f, str text, int x, int y, int red, int green, int blue)
int sdl_text_width(SDLFont f, str text)
int sdl_text_height(SDLFont f, str text)
```

### Events

```gorget
SDLEvent sdl_poll_event()    # Returns zero-initialized event if none pending
bool sdl_has_event()         # Peek without consuming
```

### Timing

```gorget
void sdl_delay(int ms)
int sdl_get_ticks()                # Milliseconds since sdl_init
int sdl_get_performance_counter()  # High-resolution timer
```

### Screen info

```gorget
int sdl_get_display_width()
int sdl_get_display_height()
```

## Example

```gorget
from gg.sdl import sdl_init, sdl_create_window, sdl_create_renderer
from gg.sdl import sdl_set_draw_color, sdl_clear, sdl_present, sdl_fill_rect
from gg.sdl import sdl_poll_event, sdl_delay, sdl_quit
from gg.sdl import SDL_INIT_VIDEO, SDL_QUIT, SDL_KEYDOWN, SDLK_ESCAPE

void main():
    sdl_init(SDL_INIT_VIDEO)
    auto win = sdl_create_window("Hello SDL", 800, 600, 0)
    auto ren = sdl_create_renderer(win, 0)

    auto running = true
    while running:
        auto event = sdl_poll_event()
        if event.event_type == SDL_QUIT:
            running = false
        if event.event_type == SDL_KEYDOWN:
            if event.key_code == SDLK_ESCAPE:
                running = false

        sdl_set_draw_color(ren, 0, 0, 0, 255)
        sdl_clear(ren)
        sdl_set_draw_color(ren, 255, 0, 0, 255)
        sdl_fill_rect(ren, 100, 200, 50, 50)
        sdl_present(ren)
        sdl_delay(16)

    sdl_quit()
```

See `examples/sdl_hello.gg` for a runnable version.
