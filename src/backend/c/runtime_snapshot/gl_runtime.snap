
#define GL_SILENCE_DEPRECATION
#ifdef __APPLE__
#include <OpenGL/gl.h>
#include <OpenGL/glext.h>
// macOS legacy GL: GL 3.x core functions available only via APPLE/ARB extensions
#define glGenVertexArrays glGenVertexArraysAPPLE
#define glDeleteVertexArrays glDeleteVertexArraysAPPLE
#define glBindVertexArray glBindVertexArrayAPPLE
#define glDrawArraysInstanced glDrawArraysInstancedARB
#define glDrawElementsInstanced glDrawElementsInstancedARB
#define glVertexAttribDivisor glVertexAttribDivisorARB
#define glBindBufferBase glBindBufferBaseEXT
#define glBindBufferRange glBindBufferRangeEXT
#define glProgramParameteri glProgramParameteriEXT
#define glFlushMappedBufferRange glFlushMappedBufferRangeAPPLE
// Functions not available even as extensions on macOS legacy GL — stub them
static inline GLuint gorget__stub_get_uniform_block_index(GLuint p, const GLchar* n) { (void)p; (void)n; return 0; }
static inline void gorget__stub_uniform_block_binding(GLuint p, GLuint bi, GLuint bp) { (void)p; (void)bi; (void)bp; }
static inline void* gorget__stub_map_buffer_range(GLenum t, GLintptr o, GLsizeiptr l, GLbitfield a) { (void)t; (void)o; (void)l; (void)a; return NULL; }
#define glGetUniformBlockIndex gorget__stub_get_uniform_block_index
#define glUniformBlockBinding gorget__stub_uniform_block_binding
#define glMapBufferRange gorget__stub_map_buffer_range
#else
#define GL_GLEXT_PROTOTYPES
#include <GL/gl.h>
#include <GL/glext.h>
#endif
#include <SDL2/SDL.h>

// ── Context (SDL-GL) ────────────────────────────────────────

typedef int64_t GorgetGLContext;

static inline GorgetGLContext gorget_gl_create_context(int64_t window_handle) {
    SDL_GLContext ctx = SDL_GL_CreateContext((SDL_Window*)(uintptr_t)window_handle);
    return (int64_t)(uintptr_t)ctx;
}

static inline void gorget_gl_destroy_context(GorgetGLContext gc) {
    SDL_GLContext ctx = (SDL_GLContext)(uintptr_t)gc;
    if (ctx) SDL_GL_DeleteContext(ctx);
}

static inline void gorget_gl_make_current(int64_t window_handle, GorgetGLContext gc) {
    SDL_GL_MakeCurrent((SDL_Window*)(uintptr_t)window_handle, (SDL_GLContext)(uintptr_t)gc);
}

static inline void gorget_gl_swap_window(int64_t window_handle) {
    SDL_GL_SwapWindow((SDL_Window*)(uintptr_t)window_handle);
}

static inline void gorget_gl_set_swap_interval(int64_t interval) {
    SDL_GL_SetSwapInterval((int)interval);
}

static inline void gorget_gl_set_attribute(int64_t attr, int64_t value) {
    SDL_GL_SetAttribute((SDL_GLattr)attr, (int)value);
}

// ── GL State ────────────────────────────────────────────────

static inline void gorget_gl_enable(int64_t cap) { glEnable((GLenum)cap); }
static inline void gorget_gl_disable(int64_t cap) { glDisable((GLenum)cap); }
static inline void gorget_gl_blend_func(int64_t s, int64_t d) { glBlendFunc((GLenum)s, (GLenum)d); }
static inline void gorget_gl_depth_func(int64_t f) { glDepthFunc((GLenum)f); }
static inline void gorget_gl_depth_mask(int64_t flag) { glDepthMask((GLboolean)flag); }
static inline void gorget_gl_cull_face(int64_t mode) { glCullFace((GLenum)mode); }
static inline void gorget_gl_front_face(int64_t mode) { glFrontFace((GLenum)mode); }
static inline void gorget_gl_viewport(int64_t x, int64_t y, int64_t w, int64_t h) { glViewport((GLint)x, (GLint)y, (GLsizei)w, (GLsizei)h); }
static inline void gorget_gl_scissor(int64_t x, int64_t y, int64_t w, int64_t h) { glScissor((GLint)x, (GLint)y, (GLsizei)w, (GLsizei)h); }
static inline void gorget_gl_clear(int64_t mask) { glClear((GLbitfield)mask); }
static inline void gorget_gl_clear_color(double r, double g, double b, double a) { glClearColor((GLfloat)r, (GLfloat)g, (GLfloat)b, (GLfloat)a); }
static inline void gorget_gl_polygon_offset(double factor, double units) { glPolygonOffset((GLfloat)factor, (GLfloat)units); }
static inline void gorget_gl_color_mask(int64_t r, int64_t g, int64_t b, int64_t a) { glColorMask((GLboolean)r, (GLboolean)g, (GLboolean)b, (GLboolean)a); }
static inline void gorget_gl_stencil_func(int64_t func, int64_t ref_val, int64_t mask) { glStencilFunc((GLenum)func, (GLint)ref_val, (GLuint)mask); }
static inline void gorget_gl_stencil_op(int64_t sfail, int64_t dpfail, int64_t dppass) { glStencilOp((GLenum)sfail, (GLenum)dpfail, (GLenum)dppass); }
static inline void gorget_gl_stencil_mask(int64_t mask) { glStencilMask((GLuint)mask); }

// ── Textures ────────────────────────────────────────────────

static inline int64_t gorget_gl_gen_texture(void) {
    GLuint tex;
    glGenTextures(1, &tex);
    return (int64_t)tex;
}
static inline void gorget_gl_delete_texture(int64_t tex) {
    GLuint t = (GLuint)tex;
    glDeleteTextures(1, &t);
}
static inline void gorget_gl_bind_texture(int64_t target, int64_t tex) { glBindTexture((GLenum)target, (GLuint)tex); }

static inline void gorget_gl_tex_image_2d(int64_t target, int64_t level, int64_t ifmt, int64_t w, int64_t h, int64_t fmt, int64_t type, const GorgetArray* data) {
    glTexImage2D((GLenum)target, (GLint)level, (GLint)ifmt, (GLsizei)w, (GLsizei)h, 0, (GLenum)fmt, (GLenum)type, data ? data->data : NULL);
}
static inline void gorget_gl_tex_parameter_i(int64_t target, int64_t pname, int64_t param) { glTexParameteri((GLenum)target, (GLenum)pname, (GLint)param); }
static inline void gorget_gl_generate_mipmap(int64_t target) {
#ifdef GL_ARB_framebuffer_object
    glGenerateMipmap((GLenum)target);
#endif
}
static inline void gorget_gl_active_texture(int64_t unit) { glActiveTexture((GLenum)unit); }

// ── VBO / Vertex Arrays ─────────────────────────────────────

static inline int64_t gorget_gl_gen_buffer(void) {
    GLuint buf;
    glGenBuffers(1, &buf);
    return (int64_t)buf;
}
static inline void gorget_gl_delete_buffer(int64_t buf) {
    GLuint b = (GLuint)buf;
    glDeleteBuffers(1, &b);
}
static inline void gorget_gl_bind_buffer(int64_t target, int64_t buf) { glBindBuffer((GLenum)target, (GLuint)buf); }
static inline void gorget_gl_buffer_data(int64_t target, const GorgetArray* data, int64_t usage) {
    glBufferData((GLenum)target, (GLsizeiptr)(data ? data->len : 0), data ? data->data : NULL, (GLenum)usage);
}
static inline void gorget_gl_buffer_sub_data(int64_t target, int64_t offset, const GorgetArray* data) {
    glBufferSubData((GLenum)target, (GLintptr)offset, (GLsizeiptr)(data ? data->len : 0), data ? data->data : NULL);
}
static inline void gorget_gl_draw_arrays(int64_t mode, int64_t first, int64_t count) { glDrawArrays((GLenum)mode, (GLint)first, (GLsizei)count); }
static inline void gorget_gl_draw_elements(int64_t mode, int64_t count, int64_t type, int64_t offset) {
    glDrawElements((GLenum)mode, (GLsizei)count, (GLenum)type, (const void*)(uintptr_t)offset);
}
static inline void gorget_gl_vertex_attrib_pointer(int64_t index, int64_t size, int64_t type, int64_t normalized, int64_t stride, int64_t offset) {
    glVertexAttribPointer((GLuint)index, (GLint)size, (GLenum)type, (GLboolean)normalized, (GLsizei)stride, (const void*)(uintptr_t)offset);
}
static inline void gorget_gl_enable_vertex_attrib_array(int64_t index) { glEnableVertexAttribArray((GLuint)index); }
static inline void gorget_gl_disable_vertex_attrib_array(int64_t index) { glDisableVertexAttribArray((GLuint)index); }

// ── Shaders ─────────────────────────────────────────────────

static inline int64_t gorget_gl_create_shader(int64_t type) { return (int64_t)glCreateShader((GLenum)type); }
static inline void gorget_gl_shader_source(int64_t shader, const char* source) {
    GLint len = (GLint)strlen(source);
    glShaderSource((GLuint)shader, 1, &source, &len);
}
static inline void gorget_gl_compile_shader(int64_t shader) { glCompileShader((GLuint)shader); }
static inline int64_t gorget_gl_create_program(void) { return (int64_t)glCreateProgram(); }
static inline void gorget_gl_attach_shader(int64_t program, int64_t shader) { glAttachShader((GLuint)program, (GLuint)shader); }
static inline void gorget_gl_link_program(int64_t program) { glLinkProgram((GLuint)program); }
static inline void gorget_gl_use_program(int64_t program) { glUseProgram((GLuint)program); }
static inline void gorget_gl_delete_shader(int64_t shader) { glDeleteShader((GLuint)shader); }
static inline void gorget_gl_delete_program(int64_t program) { glDeleteProgram((GLuint)program); }

static inline Str gorget_gl_get_shader_info_log(int64_t shader) {
    GLint len = 0;
    glGetShaderiv((GLuint)shader, GL_INFO_LOG_LENGTH, &len);
    if (len <= 0) return gorget_str_from_cstr("");
    char* buf = (char*)malloc(len + 1);
    glGetShaderInfoLog((GLuint)shader, len, NULL, buf);
    buf[len] = '\0';
    Str s = gorget_str_from_cstr(buf);
    free(buf);
    return s;
}

static inline Str gorget_gl_get_program_info_log(int64_t program) {
    GLint len = 0;
    glGetProgramiv((GLuint)program, GL_INFO_LOG_LENGTH, &len);
    if (len <= 0) return gorget_str_from_cstr("");
    char* buf = (char*)malloc(len + 1);
    glGetProgramInfoLog((GLuint)program, len, NULL, buf);
    buf[len] = '\0';
    Str s = gorget_str_from_cstr(buf);
    free(buf);
    return s;
}

// ── Uniforms ────────────────────────────────────────────────

static inline int64_t gorget_gl_get_uniform_location(int64_t program, const char* name) {
    return (int64_t)glGetUniformLocation((GLuint)program, name);
}
static inline void gorget_gl_uniform_1i(int64_t loc, int64_t v0) { glUniform1i((GLint)loc, (GLint)v0); }
static inline void gorget_gl_uniform_1f(int64_t loc, double v0) { glUniform1f((GLint)loc, (GLfloat)v0); }
static inline void gorget_gl_uniform_2f(int64_t loc, double v0, double v1) { glUniform2f((GLint)loc, (GLfloat)v0, (GLfloat)v1); }
static inline void gorget_gl_uniform_3f(int64_t loc, double v0, double v1, double v2) { glUniform3f((GLint)loc, (GLfloat)v0, (GLfloat)v1, (GLfloat)v2); }
static inline void gorget_gl_uniform_4f(int64_t loc, double v0, double v1, double v2, double v3) { glUniform4f((GLint)loc, (GLfloat)v0, (GLfloat)v1, (GLfloat)v2, (GLfloat)v3); }

// gl_uniform_matrix4fv: data is Vector[uint8] containing 16 doubles (from Mat4.to_gl())
// Converts f64→f32 before sending to GL.
static inline void gorget_gl_uniform_matrix4fv(int64_t loc, int64_t transpose, const GorgetArray* data) {
    GLfloat mat[16];
    if (data && data->len >= 16 * sizeof(double)) {
        const double* dp = (const double*)data->data;
        for (int i = 0; i < 16; i++) mat[i] = (GLfloat)dp[i];
    } else {
        memset(mat, 0, sizeof(mat));
    }
    glUniformMatrix4fv((GLint)loc, 1, (GLboolean)transpose, mat);
}

// ── Fixed-function (Q3 compat) ──────────────────────────────

static inline void gorget_gl_matrix_mode(int64_t mode) { glMatrixMode((GLenum)mode); }
static inline void gorget_gl_load_identity(void) { glLoadIdentity(); }
static inline void gorget_gl_load_matrix(const GorgetArray* m) {
    if (m && m->len >= 16 * sizeof(double)) {
        GLfloat mat[16];
        const double* dp = (const double*)m->data;
        for (int i = 0; i < 16; i++) mat[i] = (GLfloat)dp[i];
        glLoadMatrixf(mat);
    }
}
static inline void gorget_gl_push_matrix(void) { glPushMatrix(); }
static inline void gorget_gl_pop_matrix(void) { glPopMatrix(); }
static inline void gorget_gl_begin(int64_t mode) { glBegin((GLenum)mode); }
static inline void gorget_gl_end(void) { glEnd(); }
static inline void gorget_gl_vertex3f(double x, double y, double z) { glVertex3f((GLfloat)x, (GLfloat)y, (GLfloat)z); }
static inline void gorget_gl_tex_coord2f(double s, double t) { glTexCoord2f((GLfloat)s, (GLfloat)t); }
static inline void gorget_gl_normal3f(double x, double y, double z) { glNormal3f((GLfloat)x, (GLfloat)y, (GLfloat)z); }
static inline void gorget_gl_color4f(double r, double g, double b, double a) { glColor4f((GLfloat)r, (GLfloat)g, (GLfloat)b, (GLfloat)a); }
static inline void gorget_gl_color3f(double r, double g, double b) { glColor3f((GLfloat)r, (GLfloat)g, (GLfloat)b); }

// ── Client-side Vertex Arrays (Q3 renderergl1) ──────────

static inline void gorget_gl_vertex_pointer(int64_t size, int64_t type, int64_t stride, const GorgetArray* data) {
    glVertexPointer((GLint)size, (GLenum)type, (GLsizei)stride, data ? data->data : NULL);
}
static inline void gorget_gl_tex_coord_pointer(int64_t size, int64_t type, int64_t stride, const GorgetArray* data) {
    glTexCoordPointer((GLint)size, (GLenum)type, (GLsizei)stride, data ? data->data : NULL);
}
static inline void gorget_gl_color_pointer(int64_t size, int64_t type, int64_t stride, const GorgetArray* data) {
    glColorPointer((GLint)size, (GLenum)type, (GLsizei)stride, data ? data->data : NULL);
}
static inline void gorget_gl_normal_pointer(int64_t type, int64_t stride, const GorgetArray* data) {
    glNormalPointer((GLenum)type, (GLsizei)stride, data ? data->data : NULL);
}
static inline void gorget_gl_enable_client_state(int64_t cap) { glEnableClientState((GLenum)cap); }
static inline void gorget_gl_disable_client_state(int64_t cap) { glDisableClientState((GLenum)cap); }

// ── Fixed-function State (Q3) ───────────────────────────

static inline void gorget_gl_alpha_func(int64_t func, double ref_val) { glAlphaFunc((GLenum)func, (GLclampf)ref_val); }
static inline void gorget_gl_shade_model(int64_t mode) { glShadeModel((GLenum)mode); }
static inline void gorget_gl_tex_env_f(int64_t target, int64_t pname, int64_t param) { glTexEnvf((GLenum)target, (GLenum)pname, (GLfloat)param); }
static inline void gorget_gl_depth_range(double near_val, double far_val) { glDepthRange((GLclampd)near_val, (GLclampd)far_val); }
static inline void gorget_gl_polygon_mode(int64_t face, int64_t mode) { glPolygonMode((GLenum)face, (GLenum)mode); }
static inline void gorget_gl_translate_f(double x, double y, double z) { glTranslatef((GLfloat)x, (GLfloat)y, (GLfloat)z); }
static inline void gorget_gl_scale_f(double x, double y, double z) { glScalef((GLfloat)x, (GLfloat)y, (GLfloat)z); }
static inline void gorget_gl_rotate_f(double angle, double x, double y, double z) { glRotatef((GLfloat)angle, (GLfloat)x, (GLfloat)y, (GLfloat)z); }

// ── Projection / Clipping ───────────────────────────────

static inline void gorget_gl_ortho(double left, double right, double bottom, double top, double near_val, double far_val) {
    glOrtho(left, right, bottom, top, near_val, far_val);
}
static inline void gorget_gl_frustum(double left, double right, double bottom, double top, double near_val, double far_val) {
    glFrustum(left, right, bottom, top, near_val, far_val);
}
static inline void gorget_gl_clip_plane(int64_t plane, const GorgetArray* eq) {
    if (eq && eq->len >= 4 * sizeof(double)) {
        glClipPlane((GLenum)plane, (const GLdouble*)eq->data);
    }
}

// ── Draw Control ────────────────────────────────────────

static inline void gorget_gl_draw_buffer(int64_t mode) { glDrawBuffer((GLenum)mode); }
// gorget_gl_read_pixels defined below in "Read Pixels" section
static inline void gorget_gl_finish(void) { glFinish(); }
static inline void gorget_gl_flush(void) { glFlush(); }

// ── Clear Control ───────────────────────────────────────

static inline void gorget_gl_clear_depth(double depth) { glClearDepth(depth); }
static inline void gorget_gl_clear_stencil(int64_t s) { glClearStencil((GLint)s); }

// ── Additional Texture ──────────────────────────────────

static inline void gorget_gl_tex_sub_image_2d(int64_t target, int64_t level, int64_t xoff, int64_t yoff, int64_t w, int64_t h, int64_t format, int64_t type, const GorgetArray* data) {
    glTexSubImage2D((GLenum)target, (GLint)level, (GLint)xoff, (GLint)yoff, (GLsizei)w, (GLsizei)h, (GLenum)format, (GLenum)type, data ? data->data : NULL);
}
static inline void gorget_gl_tex_parameter_f(int64_t target, int64_t pname, double param) { glTexParameterf((GLenum)target, (GLenum)pname, (GLfloat)param); }
static inline void gorget_gl_copy_tex_sub_image_2d(int64_t target, int64_t level, int64_t xoff, int64_t yoff, int64_t x, int64_t y, int64_t w, int64_t h) {
    glCopyTexSubImage2D((GLenum)target, (GLint)level, (GLint)xoff, (GLint)yoff, (GLint)x, (GLint)y, (GLsizei)w, (GLsizei)h);
}

// ── Debug ───────────────────────────────────────────────

static inline void gorget_gl_line_width(double width) { glLineWidth((GLfloat)width); }

// ── Multitexture ────────────────────────────────────────

static inline void gorget_gl_client_active_texture(int64_t texture) { glClientActiveTexture((GLenum)texture); }
static inline void gorget_gl_multi_tex_coord2f(int64_t target, double s, double t) { glMultiTexCoord2f((GLenum)target, (GLfloat)s, (GLfloat)t); }

// ── Immediate Mode Variants ─────────────────────────────

static inline void gorget_gl_vertex2f(double x, double y) { glVertex2f((GLfloat)x, (GLfloat)y); }

// ── Compiled Vertex Arrays (EXT) ────────────────────────

static inline void gorget_gl_lock_arrays_ext(int64_t first, int64_t count) {
#ifdef GL_EXT_compiled_vertex_array
    glLockArraysEXT((GLint)first, (GLsizei)count);
#else
    (void)first; (void)count;
#endif
}
static inline void gorget_gl_unlock_arrays_ext(void) {
#ifdef GL_EXT_compiled_vertex_array
    glUnlockArraysEXT();
#endif
}

// ── Shader Introspection ────────────────────────────────

static inline int64_t gorget_gl_get_shader_iv(int64_t shader, int64_t pname) {
    GLint val = 0;
    glGetShaderiv((GLuint)shader, (GLenum)pname, &val);
    return (int64_t)val;
}
static inline int64_t gorget_gl_get_program_iv(int64_t program, int64_t pname) {
    GLint val = 0;
    glGetProgramiv((GLuint)program, (GLenum)pname, &val);
    return (int64_t)val;
}
static inline void gorget_gl_validate_program(int64_t program) { glValidateProgram((GLuint)program); }
static inline void gorget_gl_detach_shader(int64_t program, int64_t shader) { glDetachShader((GLuint)program, (GLuint)shader); }
static inline void gorget_gl_bind_attrib_location(int64_t program, int64_t index, const char* name) {
    glBindAttribLocation((GLuint)program, (GLuint)index, name);
}
static inline int64_t gorget_gl_get_attrib_location(int64_t program, const char* name) {
    return (int64_t)glGetAttribLocation((GLuint)program, name);
}

// ── Framebuffer Objects ─────────────────────────────────

static inline int64_t gorget_gl_gen_framebuffer(void) {
    GLuint fb;
    glGenFramebuffers(1, &fb);
    return (int64_t)fb;
}
static inline void gorget_gl_delete_framebuffer(int64_t fb) {
    GLuint f = (GLuint)fb;
    glDeleteFramebuffers(1, &f);
}
static inline void gorget_gl_bind_framebuffer(int64_t target, int64_t fb) { glBindFramebuffer((GLenum)target, (GLuint)fb); }
static inline int64_t gorget_gl_check_framebuffer_status(int64_t target) { return (int64_t)glCheckFramebufferStatus((GLenum)target); }
static inline void gorget_gl_framebuffer_texture_2d(int64_t target, int64_t attachment, int64_t tex_target, int64_t texture, int64_t level) {
    glFramebufferTexture2D((GLenum)target, (GLenum)attachment, (GLenum)tex_target, (GLuint)texture, (GLint)level);
}
static inline void gorget_gl_framebuffer_renderbuffer(int64_t target, int64_t attachment, int64_t rb_target, int64_t renderbuffer) {
    glFramebufferRenderbuffer((GLenum)target, (GLenum)attachment, (GLenum)rb_target, (GLuint)renderbuffer);
}
static inline int64_t gorget_gl_gen_renderbuffer(void) {
    GLuint rb;
    glGenRenderbuffers(1, &rb);
    return (int64_t)rb;
}
static inline void gorget_gl_delete_renderbuffer(int64_t rb) {
    GLuint r = (GLuint)rb;
    glDeleteRenderbuffers(1, &r);
}
static inline void gorget_gl_bind_renderbuffer(int64_t target, int64_t rb) { glBindRenderbuffer((GLenum)target, (GLuint)rb); }
static inline void gorget_gl_renderbuffer_storage(int64_t target, int64_t format, int64_t width, int64_t height) {
    glRenderbufferStorage((GLenum)target, (GLenum)format, (GLsizei)width, (GLsizei)height);
}
static inline void gorget_gl_blit_framebuffer(int64_t sx0, int64_t sy0, int64_t sx1, int64_t sy1, int64_t dx0, int64_t dy0, int64_t dx1, int64_t dy1, int64_t mask, int64_t filter) {
    glBlitFramebuffer((GLint)sx0, (GLint)sy0, (GLint)sx1, (GLint)sy1, (GLint)dx0, (GLint)dy0, (GLint)dx1, (GLint)dy1, (GLbitfield)mask, (GLenum)filter);
}
static inline void gorget_gl_renderbuffer_storage_multisample(int64_t target, int64_t samples, int64_t format, int64_t width, int64_t height) {
    glRenderbufferStorageMultisample((GLenum)target, (GLsizei)samples, (GLenum)format, (GLsizei)width, (GLsizei)height);
}

// ── Query ───────────────────────────────────────────────────

static inline int64_t gorget_gl_get_error(void) { return (int64_t)glGetError(); }
static inline Str gorget_gl_get_string(int64_t name) {
    const char* s = (const char*)glGetString((GLenum)name);
    return gorget_str_from_cstr(s ? s : "");
}
static inline int64_t gorget_gl_get_integer(int64_t pname) {
    GLint v = 0;
    glGetIntegerv((GLenum)pname, &v);
    return (int64_t)v;
}

// ── VAO (Vertex Array Objects) ──────────────────────────────

static inline int64_t gorget_gl_gen_vertex_array(void) {
    GLuint vao = 0;
    glGenVertexArrays(1, &vao);
    return (int64_t)vao;
}

static inline void gorget_gl_delete_vertex_array(int64_t vao) {
    GLuint v = (GLuint)vao;
    glDeleteVertexArrays(1, &v);
}

static inline void gorget_gl_bind_vertex_array(int64_t vao) {
    glBindVertexArray((GLuint)vao);
}

// ── Instanced Rendering ─────────────────────────────────────

static inline void gorget_gl_draw_arrays_instanced(int64_t mode, int64_t first, int64_t count, int64_t instance_count) {
    glDrawArraysInstanced((GLenum)mode, (GLint)first, (GLsizei)count, (GLsizei)instance_count);
}

static inline void gorget_gl_draw_elements_instanced(int64_t mode, int64_t count, int64_t type, int64_t offset, int64_t instance_count) {
    glDrawElementsInstanced((GLenum)mode, (GLsizei)count, (GLenum)type, (const void*)(intptr_t)offset, (GLsizei)instance_count);
}

static inline void gorget_gl_vertex_attrib_divisor(int64_t index, int64_t divisor) {
    glVertexAttribDivisor((GLuint)index, (GLuint)divisor);
}

// ── Occlusion Queries ───────────────────────────────────────

static inline int64_t gorget_gl_gen_query(void) {
    GLuint q = 0;
    glGenQueries(1, &q);
    return (int64_t)q;
}

static inline void gorget_gl_delete_query(int64_t query) {
    GLuint q = (GLuint)query;
    glDeleteQueries(1, &q);
}

static inline void gorget_gl_begin_query(int64_t target, int64_t query) {
    glBeginQuery((GLenum)target, (GLuint)query);
}

static inline void gorget_gl_end_query(int64_t target) {
    glEndQuery((GLenum)target);
}

static inline int64_t gorget_gl_get_query_result(int64_t query) {
    GLuint result = 0;
    glGetQueryObjectuiv((GLuint)query, GL_QUERY_RESULT, &result);
    return (int64_t)result;
}

static inline int64_t gorget_gl_get_query_result_available(int64_t query) {
    GLuint available = 0;
    glGetQueryObjectuiv((GLuint)query, GL_QUERY_RESULT_AVAILABLE, &available);
    return (int64_t)available;
}

// ── UBO (Uniform Buffer Objects) ────────────────────────────

static inline void gorget_gl_bind_buffer_base(int64_t target, int64_t index, int64_t buffer) {
    glBindBufferBase((GLenum)target, (GLuint)index, (GLuint)buffer);
}

static inline void gorget_gl_bind_buffer_range(int64_t target, int64_t index, int64_t buffer, int64_t offset, int64_t size) {
    glBindBufferRange((GLenum)target, (GLuint)index, (GLuint)buffer, (GLintptr)offset, (GLsizeiptr)size);
}

static inline int64_t gorget_gl_get_uniform_block_index(int64_t program, const char* name) {
    return (int64_t)glGetUniformBlockIndex((GLuint)program, name);
}

static inline void gorget_gl_uniform_block_binding(int64_t program, int64_t block_index, int64_t binding_point) {
    glUniformBlockBinding((GLuint)program, (GLuint)block_index, (GLuint)binding_point);
}

// ── 3D Textures / Texture Arrays ────────────────────────────

static inline void gorget_gl_tex_image_3d(int64_t target, int64_t level, int64_t internal_format, int64_t width, int64_t height, int64_t depth, int64_t format, int64_t type, const GorgetArray* data) {
    glTexImage3D((GLenum)target, (GLint)level, (GLint)internal_format, (GLsizei)width, (GLsizei)height, (GLsizei)depth, 0, (GLenum)format, (GLenum)type, data ? data->data : NULL);
}

static inline void gorget_gl_tex_sub_image_3d(int64_t target, int64_t level, int64_t xoff, int64_t yoff, int64_t zoff, int64_t w, int64_t h, int64_t d, int64_t format, int64_t type, const GorgetArray* data) {
    glTexSubImage3D((GLenum)target, (GLint)level, (GLint)xoff, (GLint)yoff, (GLint)zoff, (GLsizei)w, (GLsizei)h, (GLsizei)d, (GLenum)format, (GLenum)type, data ? data->data : NULL);
}

// ── Geometry Shaders ────────────────────────────────────────

static inline void gorget_gl_program_parameter_i(int64_t program, int64_t pname, int64_t value) {
    glProgramParameteri((GLuint)program, (GLenum)pname, (GLint)value);
}

// ── Map/Unmap Buffers ───────────────────────────────────────

static inline int64_t gorget_gl_map_buffer(int64_t target, int64_t access) {
    void* ptr = glMapBuffer((GLenum)target, (GLenum)access);
    return (int64_t)(intptr_t)ptr;
}

static inline int64_t gorget_gl_unmap_buffer(int64_t target) {
    return (int64_t)glUnmapBuffer((GLenum)target);
}

static inline int64_t gorget_gl_map_buffer_range(int64_t target, int64_t offset, int64_t length, int64_t access) {
    void* ptr = glMapBufferRange((GLenum)target, (GLintptr)offset, (GLsizeiptr)length, (GLbitfield)access);
    return (int64_t)(intptr_t)ptr;
}

static inline void gorget_gl_flush_mapped_buffer_range(int64_t target, int64_t offset, int64_t length) {
    glFlushMappedBufferRange((GLenum)target, (GLintptr)offset, (GLsizeiptr)length);
}

// ── Additional Uniform Types ────────────────────────────────

static inline void gorget_gl_uniform_3fv(int64_t location, int64_t count, const GorgetArray* data) {
    if (!data || !data->data) return;
    // Convert from double (Gorget float) to float for GL
    int num_floats = (int)count * 3;
    float tmp[256]; // max 85 vec3s
    const double* src = (const double*)data->data;
    for (int i = 0; i < num_floats && i < 256; i++) tmp[i] = (float)src[i];
    glUniform3fv((GLint)location, (GLsizei)count, tmp);
}

static inline void gorget_gl_uniform_4fv(int64_t location, int64_t count, const GorgetArray* data) {
    if (!data || !data->data) return;
    int num_floats = (int)count * 4;
    float tmp[256]; // max 64 vec4s
    const double* src = (const double*)data->data;
    for (int i = 0; i < num_floats && i < 256; i++) tmp[i] = (float)src[i];
    glUniform4fv((GLint)location, (GLsizei)count, tmp);
}

static inline void gorget_gl_uniform_matrix3fv(int64_t location, int64_t transpose, const GorgetArray* data) {
    if (!data || !data->data) return;
    float tmp[9];
    const double* src = (const double*)data->data;
    for (int i = 0; i < 9; i++) tmp[i] = (float)src[i];
    glUniformMatrix3fv((GLint)location, 1, (GLboolean)transpose, tmp);
}

// ── Blend Separate ──────────────────────────────────────────

static inline void gorget_gl_blend_func_separate(int64_t src_rgb, int64_t dst_rgb, int64_t src_alpha, int64_t dst_alpha) {
    glBlendFuncSeparate((GLenum)src_rgb, (GLenum)dst_rgb, (GLenum)src_alpha, (GLenum)dst_alpha);
}

static inline void gorget_gl_blend_equation(int64_t mode) {
    glBlendEquation((GLenum)mode);
}

static inline void gorget_gl_blend_equation_separate(int64_t mode_rgb, int64_t mode_alpha) {
    glBlendEquationSeparate((GLenum)mode_rgb, (GLenum)mode_alpha);
}

// ── Point Size ──────────────────────────────────────────────

static inline void gorget_gl_point_size(double size) {
    glPointSize((GLfloat)size);
}

// ── MRT Draw Buffers ────────────────────────────────────────

static inline void gorget_gl_draw_buffers(int64_t count, const GorgetArray* bufs) {
    if (!bufs || !bufs->data || count <= 0) return;
    GLenum draw_bufs[8];
    int n = (int)count > 8 ? 8 : (int)count;
    // bufs is packed as int64_t array (Gorget int)
    const int64_t* src = (const int64_t*)bufs->data;
    for (int i = 0; i < n; i++) draw_bufs[i] = (GLenum)src[i];
    glDrawBuffers(n, draw_bufs);
}

// ══════════════════════════════════════════════════════════════
// GL Tier 3 — GL 3.0 through GL 4.6
// (FBO/renderbuffer already defined in Tier 1-2 above)
// ══════════════════════════════════════════════════════════════

// ── GL 3.2+ functions (not available on macOS) ──────────────
#ifndef __APPLE__

// ── Sampler Objects (GL 3.3+) ───────────────────────────────

static inline int64_t gorget_gl_gen_sampler(void) {
    GLuint s = 0;
    glGenSamplers(1, &s);
    return (int64_t)s;
}
static inline void gorget_gl_delete_sampler(int64_t sampler) {
    GLuint s = (GLuint)sampler;
    glDeleteSamplers(1, &s);
}
static inline void gorget_gl_bind_sampler(int64_t unit, int64_t sampler) {
    glBindSampler((GLuint)unit, (GLuint)sampler);
}
static inline void gorget_gl_sampler_parameter_i(int64_t sampler, int64_t pname, int64_t param) {
    glSamplerParameteri((GLuint)sampler, (GLenum)pname, (GLint)param);
}
static inline void gorget_gl_sampler_parameter_f(int64_t sampler, int64_t pname, double param) {
    glSamplerParameterf((GLuint)sampler, (GLenum)pname, (GLfloat)param);
}

// ── Compute Shaders (GL 4.3+) ───────────────────────────────

static inline void gorget_gl_dispatch_compute(int64_t x, int64_t y, int64_t z) {
    glDispatchCompute((GLuint)x, (GLuint)y, (GLuint)z);
}
static inline void gorget_gl_dispatch_compute_indirect(int64_t offset) {
    glDispatchComputeIndirect((GLintptr)offset);
}
static inline void gorget_gl_memory_barrier(int64_t barriers) {
    glMemoryBarrier((GLbitfield)barriers);
}

// ── Shader Storage Buffer Objects (GL 4.3+) ─────────────────

static inline void gorget_gl_shader_storage_block_binding(int64_t program, int64_t block_index, int64_t binding) {
    glShaderStorageBlockBinding((GLuint)program, (GLuint)block_index, (GLuint)binding);
}
static inline int64_t gorget_gl_get_program_resource_index(int64_t program, int64_t interface, const char* name) {
    return (int64_t)glGetProgramResourceIndex((GLuint)program, (GLenum)interface, name);
}

// ── Immutable Texture Storage (GL 4.2+) ─────────────────────

static inline void gorget_gl_tex_storage_2d(int64_t target, int64_t levels, int64_t format, int64_t width, int64_t height) {
    glTexStorage2D((GLenum)target, (GLsizei)levels, (GLenum)format, (GLsizei)width, (GLsizei)height);
}
static inline void gorget_gl_tex_storage_3d(int64_t target, int64_t levels, int64_t format, int64_t width, int64_t height, int64_t depth) {
    glTexStorage3D((GLenum)target, (GLsizei)levels, (GLenum)format, (GLsizei)width, (GLsizei)height, (GLsizei)depth);
}
// gorget_gl_tex_sub_image_2d already defined above

// ── Buffer Storage (GL 4.4+) ────────────────────────────────

static inline void gorget_gl_buffer_storage(int64_t target, int64_t size, const GorgetArray* data, int64_t flags) {
    glBufferStorage((GLenum)target, (GLsizeiptr)size, data ? data->data : NULL, (GLbitfield)flags);
}

// ── Indirect Draw (GL 4.0+) ────────────────────────────────

static inline void gorget_gl_draw_arrays_indirect(int64_t mode, int64_t offset) {
    glDrawArraysIndirect((GLenum)mode, (const void*)(intptr_t)offset);
}
static inline void gorget_gl_draw_elements_indirect(int64_t mode, int64_t type, int64_t offset) {
    glDrawElementsIndirect((GLenum)mode, (GLenum)type, (const void*)(intptr_t)offset);
}
static inline void gorget_gl_multi_draw_arrays_indirect(int64_t mode, int64_t offset, int64_t count, int64_t stride) {
    glMultiDrawArraysIndirect((GLenum)mode, (const void*)(intptr_t)offset, (GLsizei)count, (GLsizei)stride);
}
static inline void gorget_gl_multi_draw_elements_indirect(int64_t mode, int64_t type, int64_t offset, int64_t count, int64_t stride) {
    glMultiDrawElementsIndirect((GLenum)mode, (GLenum)type, (const void*)(intptr_t)offset, (GLsizei)count, (GLsizei)stride);
}

// ── Tessellation (GL 4.0+) ─────────────────────────────────

static inline void gorget_gl_patch_parameter_i(int64_t pname, int64_t value) {
    glPatchParameteri((GLenum)pname, (GLint)value);
}
static inline void gorget_gl_patch_parameter_fv(int64_t pname, const GorgetArray* values) {
    if (!values || !values->data) return;
    glPatchParameterfv((GLenum)pname, (const GLfloat*)values->data);
}

// ── Transform Feedback (GL 3.0+) ───────────────────────────

static inline int64_t gorget_gl_gen_transform_feedback(void) {
    GLuint tf = 0;
    glGenTransformFeedbacks(1, &tf);
    return (int64_t)tf;
}
static inline void gorget_gl_delete_transform_feedback(int64_t id) {
    GLuint tf = (GLuint)id;
    glDeleteTransformFeedbacks(1, &tf);
}
static inline void gorget_gl_bind_transform_feedback(int64_t target, int64_t id) {
    glBindTransformFeedback((GLenum)target, (GLuint)id);
}
static inline void gorget_gl_begin_transform_feedback(int64_t mode) {
    glBeginTransformFeedback((GLenum)mode);
}
static inline void gorget_gl_end_transform_feedback(void) {
    glEndTransformFeedback();
}
static inline void gorget_gl_transform_feedback_varyings(int64_t program, int64_t count, const char* varyings_str, int64_t mode) {
    // varyings_str is a single space-separated string for simplicity
    // For proper multi-varying support, parse and split
    const char* v = varyings_str;
    glTransformFeedbackVaryings((GLuint)program, (GLsizei)count, &v, (GLenum)mode);
}
// bind_buffer_base/range already defined in UBO section above

// ── Clip Control (GL 4.5+) ─────────────────────────────────

static inline void gorget_gl_clip_control(int64_t origin, int64_t depth) {
    glClipControl((GLenum)origin, (GLenum)depth);
}

// ── Debug Output (GL 4.3+) ─────────────────────────────────

static inline void gorget_gl_debug_message_control(int64_t source, int64_t type, int64_t severity, int64_t enabled) {
    glDebugMessageControl((GLenum)source, (GLenum)type, (GLenum)severity, 0, NULL, (GLboolean)enabled);
}
static inline void gorget_gl_object_label(int64_t identifier, int64_t name, const char* label) {
    glObjectLabel((GLenum)identifier, (GLuint)name, -1, label);
}

// ── Copy Image (GL 4.3+) ───────────────────────────────────

static inline void gorget_gl_copy_image_sub_data(int64_t src, int64_t src_target, int64_t src_level, int64_t sx, int64_t sy, int64_t sz, int64_t dst, int64_t dst_target, int64_t dst_level, int64_t dx, int64_t dy, int64_t dz, int64_t w, int64_t h, int64_t d) {
    glCopyImageSubData((GLuint)src, (GLenum)src_target, (GLint)src_level, (GLint)sx, (GLint)sy, (GLint)sz, (GLuint)dst, (GLenum)dst_target, (GLint)dst_level, (GLint)dx, (GLint)dy, (GLint)dz, (GLsizei)w, (GLsizei)h, (GLsizei)d);
}

// ── Read Pixels ─────────────────────────────────────────────

static inline void gorget_gl_read_pixels(int64_t x, int64_t y, int64_t w, int64_t h, int64_t format, int64_t type, GorgetArray* data) {
    if (!data || !data->data) return;
    glReadPixels((GLint)x, (GLint)y, (GLsizei)w, (GLsizei)h, (GLenum)format, (GLenum)type, data->data);
}

// ── Texture Multisample (GL 3.2+) ──────────────────────────

static inline void gorget_gl_tex_image_2d_multisample(int64_t target, int64_t samples, int64_t format, int64_t width, int64_t height, int64_t fixed) {
    glTexImage2DMultisample((GLenum)target, (GLsizei)samples, (GLenum)format, (GLsizei)width, (GLsizei)height, (GLboolean)fixed);
}

// ── Sync Objects (GL 3.2+) ──────────────────────────────────

static inline int64_t gorget_gl_fence_sync(void) {
    GLsync s = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0);
    return (int64_t)(uintptr_t)s;
}
static inline void gorget_gl_delete_sync(int64_t sync) {
    glDeleteSync((GLsync)(uintptr_t)sync);
}
static inline int64_t gorget_gl_client_wait_sync(int64_t sync, int64_t flags, int64_t timeout_ns) {
    GLenum r = glClientWaitSync((GLsync)(uintptr_t)sync, (GLbitfield)flags, (GLuint64)timeout_ns);
    return (int64_t)r;
}
static inline void gorget_gl_wait_sync(int64_t sync) {
    glWaitSync((GLsync)(uintptr_t)sync, 0, GL_TIMEOUT_IGNORED);
}

#endif // !__APPLE__

// ── Pixel Store (GL 1.0+) ──────────────────────────────────

static inline void gorget_gl_pixel_store_i(int64_t pname, int64_t param) {
    glPixelStorei((GLenum)pname, (GLint)param);
}

// ── Compressed Textures ─────────────────────────────────────

static inline void gorget_gl_compressed_tex_image_2d(int64_t target, int64_t level, int64_t format, int64_t width, int64_t height, GorgetArray* data) {
    if (!data || !data->data) return;
    glCompressedTexImage2D((GLenum)target, (GLint)level, (GLenum)format, (GLsizei)width, (GLsizei)height, 0, (GLsizei)data->len, data->data);
}
static inline void gorget_gl_copy_tex_image_2d(int64_t target, int64_t level, int64_t format, int64_t x, int64_t y, int64_t width, int64_t height) {
    glCopyTexImage2D((GLenum)target, (GLint)level, (GLenum)format, (GLint)x, (GLint)y, (GLsizei)width, (GLsizei)height, 0);
}

// ── Texture Download ────────────────────────────────────────

static inline void gorget_gl_get_tex_image(int64_t target, int64_t level, int64_t format, int64_t type, GorgetArray* data) {
    if (!data || !data->data) return;
    glGetTexImage((GLenum)target, (GLint)level, (GLenum)format, (GLenum)type, data->data);
}

#ifndef __APPLE__
// ── Program Binary (GL 4.1+) ────────────────────────────────

static inline int64_t gorget_gl_get_program_binary(int64_t program, GorgetArray* data) {
    if (!data || !data->data) return 0;
    GLenum fmt = 0;
    GLsizei len = 0;
    glGetProgramBinary((GLuint)program, (GLsizei)data->len, &len, &fmt, data->data);
    data->len = len;
    return (int64_t)fmt;
}
static inline void gorget_gl_program_binary(int64_t program, int64_t format, GorgetArray* data) {
    if (!data || !data->data) return;
    glProgramBinary((GLuint)program, (GLenum)format, data->data, (GLsizei)data->len);
}

// ── Timer Queries (GL 3.3+) ─────────────────────────────────

static inline void gorget_gl_query_counter(int64_t id, int64_t target) {
    glQueryCounter((GLuint)id, (GLenum)target);
}
static inline int64_t gorget_gl_get_query_result_i64(int64_t id) {
    GLint64 result = 0;
    glGetQueryObjecti64v((GLuint)id, GL_QUERY_RESULT, &result);
    return (int64_t)result;
}

#endif // !__APPLE__

// ── Typed State Getters ─────────────────────────────────────

static inline double gorget_gl_get_float(int64_t pname) {
    GLfloat val = 0.0f;
    glGetFloatv((GLenum)pname, &val);
    return (double)val;
}

// ── Object Query Functions ──────────────────────────────────

static inline int64_t gorget_gl_is_enabled(int64_t cap) { return (int64_t)glIsEnabled((GLenum)cap); }
static inline int64_t gorget_gl_is_texture(int64_t tex) { return (int64_t)glIsTexture((GLuint)tex); }
static inline int64_t gorget_gl_is_buffer(int64_t buf) { return (int64_t)glIsBuffer((GLuint)buf); }
static inline int64_t gorget_gl_is_shader(int64_t s) { return (int64_t)glIsShader((GLuint)s); }
static inline int64_t gorget_gl_is_program(int64_t p) { return (int64_t)glIsProgram((GLuint)p); }
static inline int64_t gorget_gl_is_framebuffer(int64_t fb) { return (int64_t)glIsFramebuffer((GLuint)fb); }
