
// ── xtd.sdl runtime ──────────────────────────────────────────
#include <SDL2/SDL.h>
#ifdef GORGET_USE_SDL_IMAGE
#include <SDL2/SDL_image.h>
#endif
#ifdef GORGET_USE_SDL_TTF
#include <SDL2/SDL_ttf.h>
#endif

// ── Opaque handle wrappers ───────────────────────────────────
typedef struct { SDL_Window* ptr; } GorgetSDLWindow;
typedef struct { SDL_Renderer* ptr; } GorgetSDLRenderer;
typedef struct { SDL_Texture* ptr; } GorgetSDLTexture;
#ifdef GORGET_USE_SDL_TTF
typedef struct { TTF_Font* ptr; } GorgetSDLFont;
#else
typedef struct { void* ptr; } GorgetSDLFont;
#endif

// ── SDL Event wrapper ────────────────────────────────────────
typedef struct {
    int64_t event_type;
    int64_t key_code;
    int64_t mouse_x;
    int64_t mouse_y;
    int64_t mouse_button;
} GorgetSDLEvent;

// ── Constants ────────────────────────────────────────────────
// Init flags
static const int64_t GORGET_SDL_INIT_VIDEO = 0x00000020;
static const int64_t GORGET_SDL_INIT_AUDIO = 0x00000010;
static const int64_t GORGET_SDL_INIT_EVERYTHING = 0x00007231;

// Event types
static const int64_t GORGET_SDL_QUIT = 0x100;           // 256
static const int64_t GORGET_SDL_KEYDOWN = 0x300;        // 768
static const int64_t GORGET_SDL_KEYUP = 0x301;          // 769
static const int64_t GORGET_SDL_MOUSEMOTION = 0x400;    // 1024
static const int64_t GORGET_SDL_MOUSEBUTTONDOWN = 0x401; // 1025
static const int64_t GORGET_SDL_MOUSEBUTTONUP = 0x402;  // 1026

// Key codes
static const int64_t GORGET_SDLK_ESCAPE = 27;
static const int64_t GORGET_SDLK_SPACE = 32;
static const int64_t GORGET_SDLK_RETURN = 13;
static const int64_t GORGET_SDLK_LEFT = 1073741904;
static const int64_t GORGET_SDLK_RIGHT = 1073741903;
static const int64_t GORGET_SDLK_UP = 1073741906;
static const int64_t GORGET_SDLK_DOWN = 1073741905;
static const int64_t GORGET_SDLK_a = 97;
static const int64_t GORGET_SDLK_b = 98;
static const int64_t GORGET_SDLK_c = 99;
static const int64_t GORGET_SDLK_d = 100;
static const int64_t GORGET_SDLK_e = 101;
static const int64_t GORGET_SDLK_f = 102;
static const int64_t GORGET_SDLK_g = 103;
static const int64_t GORGET_SDLK_h = 104;
static const int64_t GORGET_SDLK_i = 105;
static const int64_t GORGET_SDLK_j = 106;
static const int64_t GORGET_SDLK_k = 107;
static const int64_t GORGET_SDLK_l = 108;
static const int64_t GORGET_SDLK_m = 109;
static const int64_t GORGET_SDLK_n = 110;
static const int64_t GORGET_SDLK_o = 111;
static const int64_t GORGET_SDLK_p = 112;
static const int64_t GORGET_SDLK_q = 113;
static const int64_t GORGET_SDLK_r = 114;
static const int64_t GORGET_SDLK_s = 115;
static const int64_t GORGET_SDLK_t = 116;
static const int64_t GORGET_SDLK_u = 117;
static const int64_t GORGET_SDLK_v = 118;
static const int64_t GORGET_SDLK_w = 119;
static const int64_t GORGET_SDLK_x = 120;
static const int64_t GORGET_SDLK_y = 121;
static const int64_t GORGET_SDLK_z = 122;

// Window flags
static const int64_t GORGET_SDL_WINDOW_SHOWN = 0x00000004;
static const int64_t GORGET_SDL_WINDOW_RESIZABLE = 0x00000020;
static const int64_t GORGET_SDL_WINDOW_FULLSCREEN = 0x00000001;

// Renderer flags
static const int64_t GORGET_SDL_RENDERER_ACCELERATED = 0x00000002;
static const int64_t GORGET_SDL_RENDERER_PRESENTVSYNC = 0x00000004;

// ── Lifecycle ────────────────────────────────────────────────
static inline int64_t gorget_sdl_init(int64_t flags) {
    int result = SDL_Init((Uint32)flags);
    if (result == 0) {
#ifdef GORGET_USE_SDL_IMAGE
        IMG_Init(IMG_INIT_PNG | IMG_INIT_JPG);
#endif
#ifdef GORGET_USE_SDL_TTF
        TTF_Init();
#endif
    }
    return (int64_t)result;
}

static inline void gorget_sdl_quit(void) {
#ifdef GORGET_USE_SDL_TTF
    TTF_Quit();
#endif
#ifdef GORGET_USE_SDL_IMAGE
    IMG_Quit();
#endif
    SDL_Quit();
}

// ── Window ───────────────────────────────────────────────────
static inline GorgetSDLWindow gorget_sdl_create_window(const char* title, int64_t w, int64_t h, int64_t flags) {
    SDL_Window* win = SDL_CreateWindow(title, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, (int)w, (int)h, (Uint32)flags);
    if (!win) { fprintf(stderr, "gorget: SDL_CreateWindow failed: %s\n", SDL_GetError()); exit(1); }
    return (GorgetSDLWindow){ win };
}

static inline GorgetSDLWindow gorget_sdl_create_window_try(const char* title, int64_t w, int64_t h, int64_t flags) {
    SDL_Window* win = SDL_CreateWindow(title, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, (int)w, (int)h, (Uint32)flags);
    return (GorgetSDLWindow){ win };
}

static inline bool gorget_sdl_window_is_null(GorgetSDLWindow win) {
    return win.ptr == NULL;
}

static inline Str gorget_sdl_get_error(void) {
    return gorget_str_from_cstr(SDL_GetError());
}

static inline int64_t gorget_sdl_window_to_handle(GorgetSDLWindow win) {
    return (int64_t)(intptr_t)win.ptr;
}

static inline void gorget_sdl_destroy_window(GorgetSDLWindow win) {
    if (win.ptr) SDL_DestroyWindow(win.ptr);
}

static inline int64_t gorget_sdl_get_window_width(GorgetSDLWindow win) {
    int w, h;
    SDL_GetWindowSize(win.ptr, &w, &h);
    return (int64_t)w;
}

static inline int64_t gorget_sdl_get_window_height(GorgetSDLWindow win) {
    int w, h;
    SDL_GetWindowSize(win.ptr, &w, &h);
    return (int64_t)h;
}

// ── Renderer ─────────────────────────────────────────────────
static inline GorgetSDLRenderer gorget_sdl_create_renderer(GorgetSDLWindow win, int64_t flags) {
    SDL_Renderer* ren = SDL_CreateRenderer(win.ptr, -1, (Uint32)flags);
    if (!ren) { fprintf(stderr, "gorget: SDL_CreateRenderer failed: %s\n", SDL_GetError()); exit(1); }
    return (GorgetSDLRenderer){ ren };
}

static inline GorgetSDLRenderer gorget_sdl_create_renderer_try(GorgetSDLWindow win, int64_t flags) {
    SDL_Renderer* ren = SDL_CreateRenderer(win.ptr, -1, (Uint32)flags);
    return (GorgetSDLRenderer){ ren };
}

static inline bool gorget_sdl_renderer_is_null(GorgetSDLRenderer ren) {
    return ren.ptr == NULL;
}

static inline void gorget_sdl_destroy_renderer(GorgetSDLRenderer r) {
    if (r.ptr) SDL_DestroyRenderer(r.ptr);
}

static inline void gorget_sdl_set_draw_color(GorgetSDLRenderer r, int64_t red, int64_t green, int64_t blue, int64_t alpha) {
    SDL_SetRenderDrawColor(r.ptr, (Uint8)red, (Uint8)green, (Uint8)blue, (Uint8)alpha);
}

static inline void gorget_sdl_clear(GorgetSDLRenderer r) {
    SDL_RenderClear(r.ptr);
}

static inline void gorget_sdl_present(GorgetSDLRenderer r) {
    SDL_RenderPresent(r.ptr);
}

static inline void gorget_sdl_draw_rect(GorgetSDLRenderer r, int64_t x, int64_t y, int64_t w, int64_t h) {
    SDL_Rect rect = { (int)x, (int)y, (int)w, (int)h };
    SDL_RenderDrawRect(r.ptr, &rect);
}

static inline void gorget_sdl_fill_rect(GorgetSDLRenderer r, int64_t x, int64_t y, int64_t w, int64_t h) {
    SDL_Rect rect = { (int)x, (int)y, (int)w, (int)h };
    SDL_RenderFillRect(r.ptr, &rect);
}

// ── Drawing ──────────────────────────────────────────────────
static inline void gorget_sdl_draw_line(GorgetSDLRenderer r, int64_t x1, int64_t y1, int64_t x2, int64_t y2) {
    SDL_RenderDrawLine(r.ptr, (int)x1, (int)y1, (int)x2, (int)y2);
}

static inline void gorget_sdl_draw_point(GorgetSDLRenderer r, int64_t x, int64_t y) {
    SDL_RenderDrawPoint(r.ptr, (int)x, (int)y);
}

static inline void gorget_sdl_set_blend_mode(GorgetSDLRenderer r, int64_t mode) {
    SDL_SetRenderDrawBlendMode(r.ptr, (SDL_BlendMode)mode);
}

// ── Textures (SDL2_image) ────────────────────────────────────
#ifdef GORGET_USE_SDL_IMAGE
static inline GorgetSDLTexture gorget_sdl_load_texture(GorgetSDLRenderer r, const char* path) {
    SDL_Texture* tex = IMG_LoadTexture(r.ptr, path);
    if (!tex) { fprintf(stderr, "gorget: IMG_LoadTexture failed: %s\n", IMG_GetError()); exit(1); }
    return (GorgetSDLTexture){ tex };
}
#endif

static inline void gorget_sdl_destroy_texture(GorgetSDLTexture t) {
    if (t.ptr) SDL_DestroyTexture(t.ptr);
}

static inline void gorget_sdl_render_texture(GorgetSDLRenderer r, GorgetSDLTexture t, int64_t x, int64_t y) {
    int w, h;
    SDL_QueryTexture(t.ptr, NULL, NULL, &w, &h);
    SDL_Rect dst = { (int)x, (int)y, w, h };
    SDL_RenderCopy(r.ptr, t.ptr, NULL, &dst);
}

static inline void gorget_sdl_render_texture_sized(GorgetSDLRenderer r, GorgetSDLTexture t, int64_t x, int64_t y, int64_t w, int64_t h) {
    SDL_Rect dst = { (int)x, (int)y, (int)w, (int)h };
    SDL_RenderCopy(r.ptr, t.ptr, NULL, &dst);
}

static inline int64_t gorget_sdl_texture_width(GorgetSDLTexture t) {
    int w, h;
    SDL_QueryTexture(t.ptr, NULL, NULL, &w, &h);
    return (int64_t)w;
}

static inline int64_t gorget_sdl_texture_height(GorgetSDLTexture t) {
    int w, h;
    SDL_QueryTexture(t.ptr, NULL, NULL, &w, &h);
    return (int64_t)h;
}

static inline void gorget_sdl_set_texture_alpha(GorgetSDLTexture t, int64_t alpha) {
    SDL_SetTextureAlphaMod(t.ptr, (Uint8)alpha);
}

// ── Text (SDL2_ttf) ─────────────────────────────────────────
#ifdef GORGET_USE_SDL_TTF
static inline GorgetSDLFont gorget_sdl_load_font(const char* path, int64_t size) {
    TTF_Font* font = TTF_OpenFont(path, (int)size);
    if (!font) { fprintf(stderr, "gorget: TTF_OpenFont failed: %s\n", TTF_GetError()); exit(1); }
    return (GorgetSDLFont){ font };
}

static inline void gorget_sdl_close_font(GorgetSDLFont f) {
    if (f.ptr) TTF_CloseFont(f.ptr);
}

static inline GorgetSDLTexture gorget_sdl_render_text(GorgetSDLRenderer r, GorgetSDLFont f, const char* text, int64_t red, int64_t green, int64_t blue) {
    SDL_Color color = { (Uint8)red, (Uint8)green, (Uint8)blue, 255 };
    SDL_Surface* surf = TTF_RenderText_Blended(f.ptr, text, color);
    if (!surf) { fprintf(stderr, "gorget: TTF_RenderText_Blended failed: %s\n", TTF_GetError()); exit(1); }
    SDL_Texture* tex = SDL_CreateTextureFromSurface(r.ptr, surf);
    SDL_FreeSurface(surf);
    if (!tex) { fprintf(stderr, "gorget: SDL_CreateTextureFromSurface failed: %s\n", SDL_GetError()); exit(1); }
    return (GorgetSDLTexture){ tex };
}

static inline void gorget_sdl_draw_text(GorgetSDLRenderer r, GorgetSDLFont f, const char* text, int64_t x, int64_t y, int64_t red, int64_t green, int64_t blue) {
    SDL_Color color = { (Uint8)red, (Uint8)green, (Uint8)blue, 255 };
    SDL_Surface* surf = TTF_RenderText_Blended(f.ptr, text, color);
    if (!surf) return;
    SDL_Texture* tex = SDL_CreateTextureFromSurface(r.ptr, surf);
    int w = surf->w, h = surf->h;
    SDL_FreeSurface(surf);
    if (!tex) return;
    SDL_Rect dst = { (int)x, (int)y, w, h };
    SDL_RenderCopy(r.ptr, tex, NULL, &dst);
    SDL_DestroyTexture(tex);
}

static inline int64_t gorget_sdl_text_width(GorgetSDLFont f, const char* text) {
    int w, h;
    TTF_SizeText(f.ptr, text, &w, &h);
    return (int64_t)w;
}

static inline int64_t gorget_sdl_text_height(GorgetSDLFont f, const char* text) {
    int w, h;
    TTF_SizeText(f.ptr, text, &w, &h);
    return (int64_t)h;
}
#endif

// ── Events ───────────────────────────────────────────────────
static inline GorgetSDLEvent gorget_sdl_poll_event(void) {
    GorgetSDLEvent result = {0, 0, 0, 0, 0};
    SDL_Event e;
    if (SDL_PollEvent(&e)) {
        result.event_type = (int64_t)e.type;
        switch (e.type) {
            case SDL_KEYDOWN:
            case SDL_KEYUP:
                result.key_code = (int64_t)e.key.keysym.sym;
                break;
            case SDL_MOUSEMOTION:
                result.mouse_x = (int64_t)e.motion.xrel;
                result.mouse_y = (int64_t)e.motion.yrel;
                break;
            case SDL_MOUSEBUTTONDOWN:
            case SDL_MOUSEBUTTONUP:
                result.mouse_x = (int64_t)e.button.x;
                result.mouse_y = (int64_t)e.button.y;
                result.mouse_button = (int64_t)e.button.button;
                break;
            case SDL_TEXTINPUT:
                // Put first typed character into key_code for Gorget to read
                result.key_code = (int64_t)(unsigned char)e.text.text[0];
                break;
            default:
                break;
        }
    }
    return result;
}

static inline bool gorget_sdl_has_event(void) {
    return SDL_PollEvent(NULL) != 0;
}

// ── Timing ───────────────────────────────────────────────────
static inline void gorget_sdl_delay(int64_t ms) {
    SDL_Delay((Uint32)ms);
}

static inline int64_t gorget_sdl_get_ticks(void) {
    return (int64_t)SDL_GetTicks();
}

static inline int64_t gorget_sdl_get_performance_counter(void) {
    return (int64_t)SDL_GetPerformanceCounter();
}

// ── Screen info ──────────────────────────────────────────────
static inline int64_t gorget_sdl_get_display_width(void) {
    SDL_DisplayMode mode;
    if (SDL_GetCurrentDisplayMode(0, &mode) != 0) return 0;
    return (int64_t)mode.w;
}

static inline int64_t gorget_sdl_get_display_height(void) {
    SDL_DisplayMode mode;
    if (SDL_GetCurrentDisplayMode(0, &mode) != 0) return 0;
    return (int64_t)mode.h;
}

// ── Mouse capture / relative mode (for FPS controls) ────────

static inline void gorget_sdl_set_relative_mouse_mode(int64_t enabled) {
    SDL_SetRelativeMouseMode(enabled ? SDL_TRUE : SDL_FALSE);
}

static inline void gorget_sdl_show_cursor(int64_t toggle) {
    SDL_ShowCursor(toggle ? SDL_ENABLE : SDL_DISABLE);
}

static inline GorgetSDLEvent gorget_sdl_get_relative_mouse_state(void) {
    int x, y;
    Uint32 buttons = SDL_GetRelativeMouseState(&x, &y);
    GorgetSDLEvent ev;
    memset(&ev, 0, sizeof(ev));
    ev.mouse_x = (int64_t)x;
    ev.mouse_y = (int64_t)y;
    ev.mouse_button = (int64_t)buttons;
    return ev;
}

static inline void gorget_sdl_warp_mouse_in_window(GorgetSDLWindow w, int64_t x, int64_t y) {
    SDL_WarpMouseInWindow(w.ptr, (int)x, (int)y);
}

static inline GorgetSDLEvent gorget_sdl_get_mouse_state(void) {
    int x, y;
    Uint32 buttons = SDL_GetMouseState(&x, &y);
    GorgetSDLEvent ev;
    memset(&ev, 0, sizeof(ev));
    ev.mouse_x = (int64_t)x;
    ev.mouse_y = (int64_t)y;
    ev.mouse_button = (int64_t)buttons;
    return ev;
}

// ── Text input mode (for console / chat) ─────────────────────
static inline void gorget_sdl_start_text_input(void) {
    SDL_StartTextInput();
}

static inline void gorget_sdl_stop_text_input(void) {
    SDL_StopTextInput();
}

