
#include <SDL2/SDL_mixer.h>

typedef struct { Mix_Chunk* ptr; } GorgetAudioChunk;
typedef struct { Mix_Music* ptr; } GorgetAudioMusic;

static inline int64_t gorget_audio_init(int64_t frequency, int64_t channels, int64_t chunk_size) {
    if (Mix_OpenAudio((int)frequency, MIX_DEFAULT_FORMAT, (int)channels, (int)chunk_size) < 0) {
        return -1;
    }
    return 0;
}

static inline void gorget_audio_quit(void) { Mix_CloseAudio(); }

static inline void gorget_audio_allocate_channels(int64_t n) { Mix_AllocateChannels((int)n); }

static inline void gorget_audio_load_wav(Str path, int64_t* out_tag, GorgetAudioChunk* out_chunk, Str* out_err) {
    char cpath[4096];
    size_t n = path.len < 4095 ? path.len : 4095;
    memcpy(cpath, path.data, n);
    cpath[n] = '\0';
    Mix_Chunk* chunk = Mix_LoadWAV(cpath);
    if (!chunk) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr(Mix_GetError());
        return;
    }
    *out_tag = 0;
    out_chunk->ptr = chunk;
}

static inline void gorget_audio_free_chunk(GorgetAudioChunk c) { if (c.ptr) Mix_FreeChunk(c.ptr); }

static inline int64_t gorget_audio_play_channel(int64_t channel, GorgetAudioChunk chunk, int64_t loops) {
    return (int64_t)Mix_PlayChannel((int)channel, chunk.ptr, (int)loops);
}

static inline void gorget_audio_halt_channel(int64_t channel) { Mix_HaltChannel((int)channel); }
static inline void gorget_audio_set_channel_volume(int64_t channel, int64_t volume) { Mix_Volume((int)channel, (int)volume); }

static inline void gorget_audio_set_channel_position(int64_t channel, int64_t angle, int64_t distance) {
    Mix_SetPosition((int)channel, (Sint16)angle, (Uint8)distance);
}

static inline void gorget_audio_set_channel_panning(int64_t channel, int64_t left, int64_t right) {
    Mix_SetPanning((int)channel, (Uint8)left, (Uint8)right);
}

static inline void gorget_audio_load_music(Str path, int64_t* out_tag, GorgetAudioMusic* out_music, Str* out_err) {
    char cpath[4096];
    size_t n = path.len < 4095 ? path.len : 4095;
    memcpy(cpath, path.data, n);
    cpath[n] = '\0';
    Mix_Music* music = Mix_LoadMUS(cpath);
    if (!music) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr(Mix_GetError());
        return;
    }
    *out_tag = 0;
    out_music->ptr = music;
}

static inline void gorget_audio_free_music(GorgetAudioMusic m) { if (m.ptr) Mix_FreeMusic(m.ptr); }
static inline void gorget_audio_play_music(GorgetAudioMusic m, int64_t loops) { Mix_PlayMusic(m.ptr, (int)loops); }
static inline void gorget_audio_halt_music(void) { Mix_HaltMusic(); }
static inline void gorget_audio_set_music_volume(int64_t volume) { Mix_VolumeMusic((int)volume); }
static inline void gorget_audio_pause_music(void) { Mix_PauseMusic(); }
static inline void gorget_audio_resume_music(void) { Mix_ResumeMusic(); }

// ── Enhanced Audio Functions ────────────────────────────────

static inline int64_t gorget_audio_channel_playing(int64_t channel) { return Mix_Playing((int)channel) ? 1 : 0; }
static inline int64_t gorget_audio_channel_paused(int64_t channel) { return Mix_Paused((int)channel) ? 1 : 0; }
static inline void gorget_audio_pause_channel(int64_t channel) { Mix_Pause((int)channel); }
static inline void gorget_audio_resume_channel(int64_t channel) { Mix_Resume((int)channel); }
static inline int64_t gorget_audio_playing_count(void) { return (int64_t)Mix_Playing(-1); }
static inline int64_t gorget_audio_paused_count(void) { return (int64_t)Mix_Paused(-1); }

static inline int64_t gorget_audio_fade_in_channel(int64_t channel, GorgetAudioChunk chunk, int64_t loops, int64_t ms) {
    return (int64_t)Mix_FadeInChannel((int)channel, chunk.ptr, (int)loops, (int)ms);
}
static inline void gorget_audio_fade_out_channel(int64_t channel, int64_t ms) { Mix_FadeOutChannel((int)channel, (int)ms); }
static inline void gorget_audio_fade_in_music(GorgetAudioMusic m, int64_t loops, int64_t ms) { Mix_FadeInMusic(m.ptr, (int)loops, (int)ms); }
static inline void gorget_audio_fade_out_music(int64_t ms) { Mix_FadeOutMusic((int)ms); }

static inline int64_t gorget_audio_music_playing(void) { return Mix_PlayingMusic() ? 1 : 0; }
static inline int64_t gorget_audio_music_paused(void) { return Mix_PausedMusic() ? 1 : 0; }
static inline void gorget_audio_set_music_position(double position) { Mix_SetMusicPosition(position); }

static inline void gorget_audio_expire_channel(int64_t channel, int64_t ms) { Mix_ExpireChannel((int)channel, (int)ms); }

static inline int64_t gorget_audio_get_music_volume(void) { return (int64_t)Mix_VolumeMusic(-1); }
static inline int64_t gorget_audio_get_channel_volume(int64_t channel) { return (int64_t)Mix_Volume((int)channel, -1); }

static inline void gorget_audio_set_channel_distance(int64_t channel, int64_t distance) {
    Mix_SetDistance((int)channel, (Uint8)distance);
}

static inline void gorget_audio_load_wav_from_memory(const GorgetArray* data, int64_t* out_tag, GorgetAudioChunk* out_chunk, Str* out_err) {
    if (!data || !data->data || data->len == 0) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr("empty audio data");
        return;
    }
    SDL_RWops* rw = SDL_RWFromConstMem(data->data, (int)data->len);
    Mix_Chunk* chunk = Mix_LoadWAV_RW(rw, 1);
    if (!chunk) {
        *out_tag = 1;
        *out_err = gorget_str_from_cstr(Mix_GetError());
        return;
    }
    *out_tag = 0;
    out_chunk->ptr = chunk;
}
