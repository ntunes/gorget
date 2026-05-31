
#ifdef __APPLE__
#import <Metal/Metal.h>
#import <QuartzCore/CAMetalLayer.h>
#include <SDL2/SDL.h>
#include <SDL2/SDL_syswm.h>

// ── Helper: SDL Metal view creation (uses SDL's built-in support) ──
// SDL_Metal_CreateView / SDL_Metal_GetLayer

static int64_t gorget_sdl_metal_create_view(int64_t window_h) {
    @autoreleasepool {
        SDL_Window* window = (SDL_Window*)(intptr_t)window_h;
        SDL_MetalView view = SDL_Metal_CreateView(window);
        return (int64_t)(intptr_t)view;
    }
}

static int64_t gorget_sdl_metal_get_layer(int64_t view_h) {
    @autoreleasepool {
        SDL_MetalView view = (SDL_MetalView)(intptr_t)view_h;
        CAMetalLayer* layer = (__bridge CAMetalLayer*)SDL_Metal_GetLayer(view);
        return (int64_t)(intptr_t)layer;
    }
}

// ── Device ──────────────────────────────────────────────────

static int64_t gorget_metal_create_device(void) {
    @autoreleasepool {
        id<MTLDevice> device = MTLCreateSystemDefaultDevice();
        // MTLCreateSystemDefaultDevice returns +1
        return (int64_t)(intptr_t)device;
    }
}

static Str gorget_metal_device_name(int64_t device_h) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        NSString* name = device.name;
        const char* utf8 = [name UTF8String];
        // Return an owned Gorget Str; copy the data since NSString may be autoreleased
        return gorget_str_own_region(utf8, strlen(utf8));
    }
}

static bool gorget_metal_device_supports_family(int64_t device_h, int64_t family) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        if (@available(macOS 10.15, *)) {
            return [device supportsFamily:(MTLGPUFamily)family];
        }
        return false;
    }
}

// ── Command Queue ───────────────────────────────────────────

static int64_t gorget_metal_create_command_queue(int64_t device_h) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        id<MTLCommandQueue> queue = [device newCommandQueue];
        // newCommandQueue returns +1
        return (int64_t)(intptr_t)queue;
    }
}

// ── Command Buffer ──────────────────────────────────────────

static int64_t gorget_metal_create_command_buffer(int64_t queue_h) {
    @autoreleasepool {
        id<MTLCommandQueue> queue = (__bridge id<MTLCommandQueue>)(void*)(intptr_t)queue_h;
        id<MTLCommandBuffer> cb = [queue commandBuffer];
        // commandBuffer returns autoreleased — retain for handle ownership
        [(id)cb retain];
        return (int64_t)(intptr_t)cb;
    }
}

static void gorget_metal_command_buffer_present(int64_t cb_h, int64_t drawable_h) {
    @autoreleasepool {
        id<MTLCommandBuffer> cb = (__bridge id<MTLCommandBuffer>)(void*)(intptr_t)cb_h;
        id<CAMetalDrawable> drawable = (__bridge id<CAMetalDrawable>)(void*)(intptr_t)drawable_h;
        [cb presentDrawable:drawable];
    }
}

static void gorget_metal_command_buffer_commit(int64_t cb_h) {
    @autoreleasepool {
        id<MTLCommandBuffer> cb = (__bridge id<MTLCommandBuffer>)(void*)(intptr_t)cb_h;
        [cb commit];
    }
}

static void gorget_metal_command_buffer_wait(int64_t cb_h) {
    @autoreleasepool {
        id<MTLCommandBuffer> cb = (__bridge id<MTLCommandBuffer>)(void*)(intptr_t)cb_h;
        [cb waitUntilCompleted];
    }
}

// ── Buffers ─────────────────────────────────────────────────

static MTLResourceOptions mtl_resource_options(int64_t storage_mode) {
    switch (storage_mode) {
        case 0: return MTLResourceStorageModeShared;
        case 1: return MTLResourceStorageModeManaged;
        case 2: return MTLResourceStorageModePrivate;
        default: return MTLResourceStorageModeShared;
    }
}

static int64_t gorget_metal_create_buffer(int64_t device_h, int64_t length, int64_t storage_mode) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        id<MTLBuffer> buf = [device newBufferWithLength:(NSUInteger)length
                                               options:mtl_resource_options(storage_mode)];
        return (int64_t)(intptr_t)buf;
    }
}

static int64_t gorget_metal_create_buffer_with_data(int64_t device_h, const GorgetArray* data, int64_t length, int64_t storage_mode) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        const void* bytes = data ? data->data : NULL;
        NSUInteger len = (NSUInteger)(length > 0 ? length : (data ? (int64_t)data->len : 0));
        id<MTLBuffer> buf = [device newBufferWithBytes:bytes
                                                length:len
                                               options:mtl_resource_options(storage_mode)];
        return (int64_t)(intptr_t)buf;
    }
}

static int64_t gorget_metal_buffer_contents(int64_t buffer_h) {
    @autoreleasepool {
        id<MTLBuffer> buf = (__bridge id<MTLBuffer>)(void*)(intptr_t)buffer_h;
        return (int64_t)(intptr_t)[buf contents];
    }
}

static int64_t gorget_metal_buffer_length(int64_t buffer_h) {
    @autoreleasepool {
        id<MTLBuffer> buf = (__bridge id<MTLBuffer>)(void*)(intptr_t)buffer_h;
        return (int64_t)[buf length];
    }
}

static void gorget_metal_buffer_did_modify_range(int64_t buffer_h, int64_t offset, int64_t length) {
    @autoreleasepool {
        id<MTLBuffer> buf = (__bridge id<MTLBuffer>)(void*)(intptr_t)buffer_h;
        [buf didModifyRange:NSMakeRange((NSUInteger)offset, (NSUInteger)length)];
    }
}

// ── Textures ────────────────────────────────────────────────

static int64_t gorget_metal_create_texture_2d(int64_t device_h, int64_t width, int64_t height, int64_t format, int64_t usage, int64_t storage_mode) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        MTLTextureDescriptor* desc = [MTLTextureDescriptor texture2DDescriptorWithPixelFormat:(MTLPixelFormat)format
                                                                                        width:(NSUInteger)width
                                                                                       height:(NSUInteger)height
                                                                                    mipmapped:NO];
        desc.usage = (MTLTextureUsage)usage;
        desc.storageMode = (MTLStorageMode)storage_mode;
        id<MTLTexture> tex = [device newTextureWithDescriptor:desc];
        return (int64_t)(intptr_t)tex;
    }
}

static int64_t gorget_metal_create_texture_2d_mipmapped(int64_t device_h, int64_t width, int64_t height, int64_t format, int64_t mip_levels, int64_t usage, int64_t storage_mode) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        MTLTextureDescriptor* desc = [[MTLTextureDescriptor alloc] init];
        desc.textureType = MTLTextureType2D;
        desc.pixelFormat = (MTLPixelFormat)format;
        desc.width = (NSUInteger)width;
        desc.height = (NSUInteger)height;
        desc.mipmapLevelCount = (NSUInteger)mip_levels;
        desc.usage = (MTLTextureUsage)usage;
        desc.storageMode = (MTLStorageMode)storage_mode;
        id<MTLTexture> tex = [device newTextureWithDescriptor:desc];
        [desc release];
        return (int64_t)(intptr_t)tex;
    }
}

static void gorget_metal_texture_upload(int64_t texture_h, int64_t x, int64_t y, int64_t width, int64_t height, const GorgetArray* data, int64_t bytes_per_row) {
    @autoreleasepool {
        id<MTLTexture> tex = (__bridge id<MTLTexture>)(void*)(intptr_t)texture_h;
        MTLRegion region = MTLRegionMake2D((NSUInteger)x, (NSUInteger)y, (NSUInteger)width, (NSUInteger)height);
        [tex replaceRegion:region
               mipmapLevel:0
                 withBytes:(data ? data->data : NULL)
               bytesPerRow:(NSUInteger)bytes_per_row];
    }
}

static void gorget_metal_texture_upload_mip(int64_t texture_h, int64_t mip_level, int64_t x, int64_t y, int64_t width, int64_t height, const GorgetArray* data, int64_t bytes_per_row) {
    @autoreleasepool {
        id<MTLTexture> tex = (__bridge id<MTLTexture>)(void*)(intptr_t)texture_h;
        MTLRegion region = MTLRegionMake2D((NSUInteger)x, (NSUInteger)y, (NSUInteger)width, (NSUInteger)height);
        [tex replaceRegion:region
               mipmapLevel:(NSUInteger)mip_level
                 withBytes:(data ? data->data : NULL)
               bytesPerRow:(NSUInteger)bytes_per_row];
    }
}

static int64_t gorget_metal_texture_width(int64_t texture_h) {
    @autoreleasepool {
        id<MTLTexture> tex = (__bridge id<MTLTexture>)(void*)(intptr_t)texture_h;
        return (int64_t)tex.width;
    }
}

static int64_t gorget_metal_texture_height(int64_t texture_h) {
    @autoreleasepool {
        id<MTLTexture> tex = (__bridge id<MTLTexture>)(void*)(intptr_t)texture_h;
        return (int64_t)tex.height;
    }
}

// ── Samplers ────────────────────────────────────────────────

static int64_t gorget_metal_create_sampler(int64_t device_h, int64_t min_filter, int64_t mag_filter, int64_t mip_filter, int64_t addr_s, int64_t addr_t) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        MTLSamplerDescriptor* desc = [[MTLSamplerDescriptor alloc] init];
        desc.minFilter = (MTLSamplerMinMagFilter)min_filter;
        desc.magFilter = (MTLSamplerMinMagFilter)mag_filter;
        desc.mipFilter = (MTLSamplerMipFilter)mip_filter;
        desc.sAddressMode = (MTLSamplerAddressMode)addr_s;
        desc.tAddressMode = (MTLSamplerAddressMode)addr_t;
        id<MTLSamplerState> sampler = [device newSamplerStateWithDescriptor:desc];
        [desc release];
        return (int64_t)(intptr_t)sampler;
    }
}

// ── Shaders / Library ───────────────────────────────────────

static int64_t gorget_metal_create_library(int64_t device_h, const char* source) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        NSString* src = [NSString stringWithUTF8String:source];
        NSError* error = nil;
        MTLCompileOptions* opts = [[MTLCompileOptions alloc] init];
        id<MTLLibrary> lib = [device newLibraryWithSource:src options:opts error:&error];
        [opts release];
        if (error && !lib) {
            fprintf(stderr, "Metal shader compile error: %s\n",
                    [[error localizedDescription] UTF8String]);
        }
        return (int64_t)(intptr_t)lib;
    }
}

static int64_t gorget_metal_create_library_from_data(int64_t device_h, const GorgetArray* data) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        if (!data || !data->data) return 0;
        dispatch_data_t ddata = dispatch_data_create(data->data, data->len,
                                                      dispatch_get_main_queue(),
                                                      DISPATCH_DATA_DESTRUCTOR_DEFAULT);
        NSError* error = nil;
        id<MTLLibrary> lib = [device newLibraryWithData:ddata error:&error];
        // dispatch_data_t is ARC-managed under ARC, manual release under MRC
        // Under MRC, dispatch_release is needed; under ARC it's automatic
        #if !__has_feature(objc_arc)
        dispatch_release(ddata);
        #endif
        if (error && !lib) {
            fprintf(stderr, "Metal library load error: %s\n",
                    [[error localizedDescription] UTF8String]);
        }
        return (int64_t)(intptr_t)lib;
    }
}

static int64_t gorget_metal_library_function(int64_t library_h, const char* name) {
    @autoreleasepool {
        id<MTLLibrary> lib = (__bridge id<MTLLibrary>)(void*)(intptr_t)library_h;
        NSString* fn_name = [NSString stringWithUTF8String:name];
        id<MTLFunction> func = [lib newFunctionWithName:fn_name];
        if (!func) {
            fprintf(stderr, "Metal: function '%s' not found in library\n", name);
        }
        return (int64_t)(intptr_t)func;
    }
}

// ── Vertex Descriptor ───────────────────────────────────────

static int64_t gorget_metal_create_vertex_descriptor(void) {
    @autoreleasepool {
        MTLVertexDescriptor* desc = [MTLVertexDescriptor vertexDescriptor];
        // vertexDescriptor returns autoreleased — retain
        [(id)desc retain];
        return (int64_t)(intptr_t)desc;
    }
}

static void gorget_metal_vertex_desc_set_attribute(int64_t desc_h, int64_t index, int64_t format, int64_t offset, int64_t buffer_index) {
    @autoreleasepool {
        MTLVertexDescriptor* desc = (__bridge MTLVertexDescriptor*)(void*)(intptr_t)desc_h;
        desc.attributes[index].format = (MTLVertexFormat)format;
        desc.attributes[index].offset = (NSUInteger)offset;
        desc.attributes[index].bufferIndex = (NSUInteger)buffer_index;
    }
}

static void gorget_metal_vertex_desc_set_layout(int64_t desc_h, int64_t index, int64_t stride, int64_t step_function) {
    @autoreleasepool {
        MTLVertexDescriptor* desc = (__bridge MTLVertexDescriptor*)(void*)(intptr_t)desc_h;
        desc.layouts[index].stride = (NSUInteger)stride;
        desc.layouts[index].stepFunction = (MTLVertexStepFunction)step_function;
    }
}

// ── Render Pipeline State ───────────────────────────────────

static int64_t gorget_metal_create_render_pipeline(int64_t device_h, int64_t vert_fn_h, int64_t frag_fn_h, int64_t vert_desc_h, int64_t color_fmt, int64_t depth_fmt) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        id<MTLFunction> vert = (__bridge id<MTLFunction>)(void*)(intptr_t)vert_fn_h;
        id<MTLFunction> frag = (__bridge id<MTLFunction>)(void*)(intptr_t)frag_fn_h;
        MTLVertexDescriptor* vdesc = vert_desc_h ? (__bridge MTLVertexDescriptor*)(void*)(intptr_t)vert_desc_h : nil;

        MTLRenderPipelineDescriptor* desc = [[MTLRenderPipelineDescriptor alloc] init];
        desc.vertexFunction = vert;
        desc.fragmentFunction = frag;
        if (vdesc) desc.vertexDescriptor = vdesc;
        desc.colorAttachments[0].pixelFormat = (MTLPixelFormat)color_fmt;
        desc.depthAttachmentPixelFormat = (MTLPixelFormat)depth_fmt;

        NSError* error = nil;
        id<MTLRenderPipelineState> pso = [device newRenderPipelineStateWithDescriptor:desc error:&error];
        [desc release];
        if (error && !pso) {
            fprintf(stderr, "Metal pipeline error: %s\n",
                    [[error localizedDescription] UTF8String]);
        }
        return (int64_t)(intptr_t)pso;
    }
}

static int64_t gorget_metal_create_render_pipeline_blended(int64_t device_h, int64_t vert_fn_h, int64_t frag_fn_h, int64_t vert_desc_h, int64_t color_fmt, int64_t depth_fmt, int64_t src_rgb, int64_t dst_rgb, int64_t src_a, int64_t dst_a) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        id<MTLFunction> vert = (__bridge id<MTLFunction>)(void*)(intptr_t)vert_fn_h;
        id<MTLFunction> frag = (__bridge id<MTLFunction>)(void*)(intptr_t)frag_fn_h;
        MTLVertexDescriptor* vdesc = vert_desc_h ? (__bridge MTLVertexDescriptor*)(void*)(intptr_t)vert_desc_h : nil;

        MTLRenderPipelineDescriptor* desc = [[MTLRenderPipelineDescriptor alloc] init];
        desc.vertexFunction = vert;
        desc.fragmentFunction = frag;
        if (vdesc) desc.vertexDescriptor = vdesc;
        desc.colorAttachments[0].pixelFormat = (MTLPixelFormat)color_fmt;
        desc.colorAttachments[0].blendingEnabled = YES;
        desc.colorAttachments[0].sourceRGBBlendFactor = (MTLBlendFactor)src_rgb;
        desc.colorAttachments[0].destinationRGBBlendFactor = (MTLBlendFactor)dst_rgb;
        desc.colorAttachments[0].sourceAlphaBlendFactor = (MTLBlendFactor)src_a;
        desc.colorAttachments[0].destinationAlphaBlendFactor = (MTLBlendFactor)dst_a;
        desc.colorAttachments[0].rgbBlendOperation = MTLBlendOperationAdd;
        desc.colorAttachments[0].alphaBlendOperation = MTLBlendOperationAdd;
        desc.depthAttachmentPixelFormat = (MTLPixelFormat)depth_fmt;

        NSError* error = nil;
        id<MTLRenderPipelineState> pso = [device newRenderPipelineStateWithDescriptor:desc error:&error];
        [desc release];
        if (error && !pso) {
            fprintf(stderr, "Metal pipeline error: %s\n",
                    [[error localizedDescription] UTF8String]);
        }
        return (int64_t)(intptr_t)pso;
    }
}

// ── Depth/Stencil State ─────────────────────────────────────

static int64_t gorget_metal_create_depth_stencil(int64_t device_h, int64_t compare_fn, int64_t depth_write) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        MTLDepthStencilDescriptor* desc = [[MTLDepthStencilDescriptor alloc] init];
        desc.depthCompareFunction = (MTLCompareFunction)compare_fn;
        desc.depthWriteEnabled = (depth_write != 0);
        id<MTLDepthStencilState> state = [device newDepthStencilStateWithDescriptor:desc];
        [desc release];
        return (int64_t)(intptr_t)state;
    }
}

// ── Render Pass Descriptor ──────────────────────────────────

static int64_t gorget_metal_create_render_pass_desc(void) {
    @autoreleasepool {
        MTLRenderPassDescriptor* desc = [MTLRenderPassDescriptor renderPassDescriptor];
        // renderPassDescriptor returns autoreleased — retain
        [(id)desc retain];
        return (int64_t)(intptr_t)desc;
    }
}

static void gorget_metal_render_pass_set_color(int64_t desc_h, int64_t index, int64_t texture_h, int64_t load_action, int64_t store_action, double r, double g, double b, double a) {
    @autoreleasepool {
        MTLRenderPassDescriptor* desc = (__bridge MTLRenderPassDescriptor*)(void*)(intptr_t)desc_h;
        id<MTLTexture> tex = (__bridge id<MTLTexture>)(void*)(intptr_t)texture_h;
        desc.colorAttachments[index].texture = tex;
        desc.colorAttachments[index].loadAction = (MTLLoadAction)load_action;
        desc.colorAttachments[index].storeAction = (MTLStoreAction)store_action;
        desc.colorAttachments[index].clearColor = MTLClearColorMake(r, g, b, a);
    }
}

static void gorget_metal_render_pass_set_depth(int64_t desc_h, int64_t texture_h, int64_t load_action, int64_t store_action, double clear_depth) {
    @autoreleasepool {
        MTLRenderPassDescriptor* desc = (__bridge MTLRenderPassDescriptor*)(void*)(intptr_t)desc_h;
        id<MTLTexture> tex = texture_h ? (__bridge id<MTLTexture>)(void*)(intptr_t)texture_h : nil;
        desc.depthAttachment.texture = tex;
        desc.depthAttachment.loadAction = (MTLLoadAction)load_action;
        desc.depthAttachment.storeAction = (MTLStoreAction)store_action;
        desc.depthAttachment.clearDepth = clear_depth;
    }
}

// ── Render Command Encoder ──────────────────────────────────

static int64_t gorget_metal_create_render_encoder(int64_t cb_h, int64_t pass_desc_h) {
    @autoreleasepool {
        id<MTLCommandBuffer> cb = (__bridge id<MTLCommandBuffer>)(void*)(intptr_t)cb_h;
        MTLRenderPassDescriptor* desc = (__bridge MTLRenderPassDescriptor*)(void*)(intptr_t)pass_desc_h;
        id<MTLRenderCommandEncoder> enc = [cb renderCommandEncoderWithDescriptor:desc];
        // renderCommandEncoderWithDescriptor returns autoreleased — retain
        [(id)enc retain];
        return (int64_t)(intptr_t)enc;
    }
}

static void gorget_metal_encoder_set_pipeline(int64_t enc_h, int64_t pso_h) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        id<MTLRenderPipelineState> pso = (__bridge id<MTLRenderPipelineState>)(void*)(intptr_t)pso_h;
        [enc setRenderPipelineState:pso];
    }
}

static void gorget_metal_encoder_set_vertex_buffer(int64_t enc_h, int64_t buf_h, int64_t offset, int64_t index) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        id<MTLBuffer> buf = (__bridge id<MTLBuffer>)(void*)(intptr_t)buf_h;
        [enc setVertexBuffer:buf offset:(NSUInteger)offset atIndex:(NSUInteger)index];
    }
}

static void gorget_metal_encoder_set_vertex_bytes(int64_t enc_h, const GorgetArray* data, int64_t length, int64_t index) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        NSUInteger len = (NSUInteger)(length > 0 ? length : (data ? (int64_t)data->len : 0));
        [enc setVertexBytes:(data ? data->data : NULL)
                     length:len
                    atIndex:(NSUInteger)index];
    }
}

static void gorget_metal_encoder_set_fragment_buffer(int64_t enc_h, int64_t buf_h, int64_t offset, int64_t index) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        id<MTLBuffer> buf = (__bridge id<MTLBuffer>)(void*)(intptr_t)buf_h;
        [enc setFragmentBuffer:buf offset:(NSUInteger)offset atIndex:(NSUInteger)index];
    }
}

static void gorget_metal_encoder_set_fragment_bytes(int64_t enc_h, const GorgetArray* data, int64_t length, int64_t index) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        NSUInteger len = (NSUInteger)(length > 0 ? length : (data ? (int64_t)data->len : 0));
        [enc setFragmentBytes:(data ? data->data : NULL)
                       length:len
                      atIndex:(NSUInteger)index];
    }
}

static void gorget_metal_encoder_set_fragment_texture(int64_t enc_h, int64_t tex_h, int64_t index) {
    @autoreleasepool {
        // Guard: Metal texture handles are ObjC pointers — must be valid heap addresses.
        // On ARM64 macOS, heap starts well above 0x10000. Values like 0, 1, or small
        // integers indicate data corruption (e.g. a boolean or tag leaking into a handle slot).
        if ((uintptr_t)tex_h < 0x10000u) {
            fprintf(stderr, "gorget: invalid Metal texture handle %lld (0x%llx) at index %lld — skipping bind\n",
                    (long long)tex_h, (unsigned long long)tex_h, (long long)index);
            return;
        }
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        id<MTLTexture> tex = (__bridge id<MTLTexture>)(void*)(intptr_t)tex_h;
        [enc setFragmentTexture:tex atIndex:(NSUInteger)index];
    }
}

static void gorget_metal_encoder_set_fragment_sampler(int64_t enc_h, int64_t sampler_h, int64_t index) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        id<MTLSamplerState> sampler = (__bridge id<MTLSamplerState>)(void*)(intptr_t)sampler_h;
        [enc setFragmentSamplerState:sampler atIndex:(NSUInteger)index];
    }
}

static void gorget_metal_encoder_set_depth_stencil(int64_t enc_h, int64_t state_h) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        id<MTLDepthStencilState> state = (__bridge id<MTLDepthStencilState>)(void*)(intptr_t)state_h;
        [enc setDepthStencilState:state];
    }
}

static void gorget_metal_encoder_set_cull_mode(int64_t enc_h, int64_t mode) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        [enc setCullMode:(MTLCullMode)mode];
    }
}

static void gorget_metal_encoder_set_front_facing(int64_t enc_h, int64_t winding) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        [enc setFrontFacingWinding:(MTLWinding)winding];
    }
}

static void gorget_metal_encoder_set_fill_mode(int64_t enc_h, int64_t mode) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        [enc setTriangleFillMode:(MTLTriangleFillMode)mode];
    }
}

static void gorget_metal_encoder_set_viewport(int64_t enc_h, double x, double y, double w, double h, double near, double far) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        MTLViewport vp = { x, y, w, h, near, far };
        [enc setViewport:vp];
    }
}

static void gorget_metal_encoder_set_scissor(int64_t enc_h, int64_t x, int64_t y, int64_t w, int64_t h) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        MTLScissorRect rect = { (NSUInteger)x, (NSUInteger)y, (NSUInteger)w, (NSUInteger)h };
        [enc setScissorRect:rect];
    }
}

static void gorget_metal_encoder_set_blend_color(int64_t enc_h, double r, double g, double b, double a) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        [enc setBlendColorRed:(float)r green:(float)g blue:(float)b alpha:(float)a];
    }
}

static void gorget_metal_encoder_set_stencil_ref(int64_t enc_h, int64_t ref_val) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        [enc setStencilReferenceValue:(uint32_t)ref_val];
    }
}

static void gorget_metal_encoder_draw_primitives(int64_t enc_h, int64_t prim_type, int64_t start, int64_t count) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        [enc drawPrimitives:(MTLPrimitiveType)prim_type
                vertexStart:(NSUInteger)start
                vertexCount:(NSUInteger)count];
    }
}

static void gorget_metal_encoder_draw_primitives_instanced(int64_t enc_h, int64_t prim_type, int64_t start, int64_t count, int64_t instance_count) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        [enc drawPrimitives:(MTLPrimitiveType)prim_type
                vertexStart:(NSUInteger)start
                vertexCount:(NSUInteger)count
              instanceCount:(NSUInteger)instance_count];
    }
}

static void gorget_metal_encoder_draw_indexed(int64_t enc_h, int64_t prim_type, int64_t index_count, int64_t index_type, int64_t idx_buf_h, int64_t index_offset) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        id<MTLBuffer> idx_buf = (__bridge id<MTLBuffer>)(void*)(intptr_t)idx_buf_h;
        [enc drawIndexedPrimitives:(MTLPrimitiveType)prim_type
                        indexCount:(NSUInteger)index_count
                         indexType:(MTLIndexType)index_type
                       indexBuffer:idx_buf
                 indexBufferOffset:(NSUInteger)index_offset];
    }
}

static void gorget_metal_encoder_draw_indexed_instanced(int64_t enc_h, int64_t prim_type, int64_t index_count, int64_t index_type, int64_t idx_buf_h, int64_t index_offset, int64_t instance_count) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        id<MTLBuffer> idx_buf = (__bridge id<MTLBuffer>)(void*)(intptr_t)idx_buf_h;
        [enc drawIndexedPrimitives:(MTLPrimitiveType)prim_type
                        indexCount:(NSUInteger)index_count
                         indexType:(MTLIndexType)index_type
                       indexBuffer:idx_buf
                 indexBufferOffset:(NSUInteger)index_offset
                     instanceCount:(NSUInteger)instance_count];
    }
}

static void gorget_metal_encoder_end(int64_t enc_h) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        [enc endEncoding];
    }
}

// ── Blit Command Encoder ────────────────────────────────────

static int64_t gorget_metal_create_blit_encoder(int64_t cb_h) {
    @autoreleasepool {
        id<MTLCommandBuffer> cb = (__bridge id<MTLCommandBuffer>)(void*)(intptr_t)cb_h;
        id<MTLBlitCommandEncoder> enc = [cb blitCommandEncoder];
        // blitCommandEncoder returns autoreleased — retain
        [(id)enc retain];
        return (int64_t)(intptr_t)enc;
    }
}

static void gorget_metal_blit_generate_mipmaps(int64_t enc_h, int64_t tex_h) {
    @autoreleasepool {
        id<MTLBlitCommandEncoder> enc = (__bridge id<MTLBlitCommandEncoder>)(void*)(intptr_t)enc_h;
        id<MTLTexture> tex = (__bridge id<MTLTexture>)(void*)(intptr_t)tex_h;
        [enc generateMipmapsForTexture:tex];
    }
}

static void gorget_metal_blit_end(int64_t enc_h) {
    @autoreleasepool {
        id<MTLBlitCommandEncoder> enc = (__bridge id<MTLBlitCommandEncoder>)(void*)(intptr_t)enc_h;
        [enc endEncoding];
    }
}

// Convenience: generate mipmaps in one shot (creates blit encoder, generates, ends, commits)
static void gorget_metal_generate_mipmaps(int64_t cb_h, int64_t tex_h) {
    @autoreleasepool {
        id<MTLCommandBuffer> cb = (__bridge id<MTLCommandBuffer>)(void*)(intptr_t)cb_h;
        id<MTLTexture> tex = (__bridge id<MTLTexture>)(void*)(intptr_t)tex_h;
        id<MTLBlitCommandEncoder> enc = [cb blitCommandEncoder];
        [enc generateMipmapsForTexture:tex];
        [enc endEncoding];
    }
}

// ── CAMetalLayer Operations ─────────────────────────────────

static void gorget_metal_layer_set_device(int64_t layer_h, int64_t device_h) {
    @autoreleasepool {
        CAMetalLayer* layer = (__bridge CAMetalLayer*)(void*)(intptr_t)layer_h;
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        layer.device = device;
    }
}

static void gorget_metal_layer_set_pixel_format(int64_t layer_h, int64_t format) {
    @autoreleasepool {
        CAMetalLayer* layer = (__bridge CAMetalLayer*)(void*)(intptr_t)layer_h;
        layer.pixelFormat = (MTLPixelFormat)format;
    }
}

static void gorget_metal_layer_set_drawable_size(int64_t layer_h, int64_t width, int64_t height) {
    @autoreleasepool {
        CAMetalLayer* layer = (__bridge CAMetalLayer*)(void*)(intptr_t)layer_h;
        layer.drawableSize = CGSizeMake((CGFloat)width, (CGFloat)height);
    }
}

static int64_t gorget_metal_layer_next_drawable(int64_t layer_h) {
    @autoreleasepool {
        CAMetalLayer* layer = (__bridge CAMetalLayer*)(void*)(intptr_t)layer_h;
        id<CAMetalDrawable> drawable = [layer nextDrawable];
        if (!drawable) {
            fprintf(stderr, "Metal: nextDrawable returned nil (GPU stall or surface lost)\n");
            return 0;
        }
        // nextDrawable returns autoreleased — retain
        [(id)drawable retain];
        return (int64_t)(intptr_t)drawable;
    }
}

static int64_t gorget_metal_drawable_texture(int64_t drawable_h) {
    @autoreleasepool {
        id<CAMetalDrawable> drawable = (__bridge id<CAMetalDrawable>)(void*)(intptr_t)drawable_h;
        id<MTLTexture> tex = drawable.texture;
        // texture property returns unretained — retain for handle
        [(id)tex retain];
        return (int64_t)(intptr_t)tex;
    }
}

// ── Release ─────────────────────────────────────────────────

static void gorget_metal_release(int64_t obj_h) {
    if (obj_h == 0) return;
    @autoreleasepool {
        id obj = (__bridge id)(void*)(intptr_t)obj_h;
        [obj release];
    }
}

// ── Convenience: begin_frame ────────────────────────────────
// Gets the next drawable from the layer and returns its handle.
// Equivalent to metal_layer_next_drawable but named for gpu.gg compat.

static int64_t gorget_metal_begin_frame(int64_t layer_h) {
    return gorget_metal_layer_next_drawable(layer_h);
}

// ── Autorelease Pool (per-frame memory management) ──────
// Call push at frame start, pop at frame end. Drains autoreleased objects.

static int64_t gorget_metal_autorelease_pool_push(void) {
    // objc_autoreleasePoolPush returns an opaque token
    extern void* objc_autoreleasePoolPush(void);
    void* pool = objc_autoreleasePoolPush();
    return (int64_t)(intptr_t)pool;
}

static void gorget_metal_autorelease_pool_pop(int64_t pool_h) {
    extern void objc_autoreleasePoolPop(void* pool);
    objc_autoreleasePoolPop((void*)(intptr_t)pool_h);
}

// ── Triple Buffering (dispatch semaphore) ────────────────

#include <dispatch/dispatch.h>

static int64_t gorget_metal_semaphore_create(int64_t value) {
    dispatch_semaphore_t sem = dispatch_semaphore_create((long)value);
    return (int64_t)(intptr_t)sem;
}

static void gorget_metal_semaphore_wait(int64_t sem_h) {
    dispatch_semaphore_t sem = (dispatch_semaphore_t)(intptr_t)sem_h;
    dispatch_semaphore_wait(sem, DISPATCH_TIME_FOREVER);
}

static void gorget_metal_semaphore_signal(int64_t sem_h) {
    dispatch_semaphore_t sem = (dispatch_semaphore_t)(intptr_t)sem_h;
    dispatch_semaphore_signal(sem);
}

// addCompletedHandler: signals a dispatch semaphore when GPU finishes
static void gorget_metal_command_buffer_on_complete(int64_t cb_h, int64_t sem_h) {
    @autoreleasepool {
        id<MTLCommandBuffer> cb = (__bridge id<MTLCommandBuffer>)(void*)(intptr_t)cb_h;
        dispatch_semaphore_t sem = (dispatch_semaphore_t)(intptr_t)sem_h;
        [cb addCompletedHandler:^(id<MTLCommandBuffer> _Nonnull buffer) {
            (void)buffer;
            dispatch_semaphore_signal(sem);
        }];
    }
}

// ── Stencil Attachment ──────────────────────────────────

static void gorget_metal_render_pass_set_stencil(int64_t desc_h, int64_t texture_h, int64_t load_action, int64_t store_action, int64_t clear_stencil) {
    @autoreleasepool {
        MTLRenderPassDescriptor* desc = (__bridge MTLRenderPassDescriptor*)(void*)(intptr_t)desc_h;
        id<MTLTexture> tex = texture_h ? (__bridge id<MTLTexture>)(void*)(intptr_t)texture_h : nil;
        desc.stencilAttachment.texture = tex;
        desc.stencilAttachment.loadAction = (MTLLoadAction)load_action;
        desc.stencilAttachment.storeAction = (MTLStoreAction)store_action;
        desc.stencilAttachment.clearStencil = (uint32_t)clear_stencil;
    }
}

static int64_t gorget_metal_create_render_pipeline_with_stencil(int64_t device_h, int64_t vert_fn_h, int64_t frag_fn_h, int64_t vert_desc_h, int64_t color_fmt, int64_t depth_fmt, int64_t stencil_fmt) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        id<MTLFunction> vert = (__bridge id<MTLFunction>)(void*)(intptr_t)vert_fn_h;
        id<MTLFunction> frag = (__bridge id<MTLFunction>)(void*)(intptr_t)frag_fn_h;
        MTLVertexDescriptor* vdesc = vert_desc_h ? (__bridge MTLVertexDescriptor*)(void*)(intptr_t)vert_desc_h : nil;

        MTLRenderPipelineDescriptor* desc = [[MTLRenderPipelineDescriptor alloc] init];
        desc.vertexFunction = vert;
        desc.fragmentFunction = frag;
        if (vdesc) desc.vertexDescriptor = vdesc;
        desc.colorAttachments[0].pixelFormat = (MTLPixelFormat)color_fmt;
        desc.depthAttachmentPixelFormat = (MTLPixelFormat)depth_fmt;
        desc.stencilAttachmentPixelFormat = (MTLPixelFormat)stencil_fmt;

        NSError* error = nil;
        id<MTLRenderPipelineState> pso = [device newRenderPipelineStateWithDescriptor:desc error:&error];
        [desc release];
        if (error && !pso) {
            fprintf(stderr, "Metal pipeline error: %s\n",
                    [[error localizedDescription] UTF8String]);
        }
        return (int64_t)(intptr_t)pso;
    }
}

// ── Depth Bias (shadow acne prevention) ─────────────────

static void gorget_metal_encoder_set_depth_bias(int64_t enc_h, double depth_bias, double slope_scale, double clamp) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        [enc setDepthBias:(float)depth_bias slopeScale:(float)slope_scale clamp:(float)clamp];
    }
}

// ── Vertex-stage Texture/Sampler ────────────────────────

static void gorget_metal_encoder_set_vertex_texture(int64_t enc_h, int64_t tex_h, int64_t index) {
    @autoreleasepool {
        if ((uintptr_t)tex_h < 0x10000u) {
            fprintf(stderr, "gorget: invalid Metal vertex texture handle %lld (0x%llx) at index %lld — skipping\n",
                    (long long)tex_h, (unsigned long long)tex_h, (long long)index);
            return;
        }
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        id<MTLTexture> tex = (__bridge id<MTLTexture>)(void*)(intptr_t)tex_h;
        [enc setVertexTexture:tex atIndex:(NSUInteger)index];
    }
}

static void gorget_metal_encoder_set_vertex_sampler(int64_t enc_h, int64_t sampler_h, int64_t index) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        id<MTLSamplerState> sampler = (__bridge id<MTLSamplerState>)(void*)(intptr_t)sampler_h;
        [enc setVertexSamplerState:sampler atIndex:(NSUInteger)index];
    }
}

// ── Command Buffer Status/Error ─────────────────────────

static int64_t gorget_metal_command_buffer_status(int64_t cb_h) {
    @autoreleasepool {
        id<MTLCommandBuffer> cb = (__bridge id<MTLCommandBuffer>)(void*)(intptr_t)cb_h;
        return (int64_t)cb.status;
    }
}

static Str gorget_metal_command_buffer_error(int64_t cb_h) {
    @autoreleasepool {
        id<MTLCommandBuffer> cb = (__bridge id<MTLCommandBuffer>)(void*)(intptr_t)cb_h;
        NSError* error = cb.error;
        if (!error) {
            return GORGET_EMPTY_STR;
        }
        const char* utf8 = [[error localizedDescription] UTF8String];
        return gorget_str_own_region(utf8, strlen(utf8));
    }
}

// ── MSAA Support ────────────────────────────────────────

static int64_t gorget_metal_create_render_pipeline_msaa(int64_t device_h, int64_t vert_fn_h, int64_t frag_fn_h, int64_t vert_desc_h, int64_t color_fmt, int64_t depth_fmt, int64_t sample_count) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        id<MTLFunction> vert = (__bridge id<MTLFunction>)(void*)(intptr_t)vert_fn_h;
        id<MTLFunction> frag = (__bridge id<MTLFunction>)(void*)(intptr_t)frag_fn_h;
        MTLVertexDescriptor* vdesc = vert_desc_h ? (__bridge MTLVertexDescriptor*)(void*)(intptr_t)vert_desc_h : nil;

        MTLRenderPipelineDescriptor* desc = [[MTLRenderPipelineDescriptor alloc] init];
        desc.vertexFunction = vert;
        desc.fragmentFunction = frag;
        if (vdesc) desc.vertexDescriptor = vdesc;
        desc.colorAttachments[0].pixelFormat = (MTLPixelFormat)color_fmt;
        desc.depthAttachmentPixelFormat = (MTLPixelFormat)depth_fmt;
        desc.sampleCount = (NSUInteger)sample_count;

        NSError* error = nil;
        id<MTLRenderPipelineState> pso = [device newRenderPipelineStateWithDescriptor:desc error:&error];
        [desc release];
        if (error && !pso) {
            fprintf(stderr, "Metal MSAA pipeline error: %s\n",
                    [[error localizedDescription] UTF8String]);
        }
        return (int64_t)(intptr_t)pso;
    }
}

static int64_t gorget_metal_create_texture_2d_msaa(int64_t device_h, int64_t width, int64_t height, int64_t format, int64_t sample_count, int64_t usage, int64_t storage_mode) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        MTLTextureDescriptor* desc = [[MTLTextureDescriptor alloc] init];
        desc.textureType = MTLTextureType2DMultisample;
        desc.pixelFormat = (MTLPixelFormat)format;
        desc.width = (NSUInteger)width;
        desc.height = (NSUInteger)height;
        desc.sampleCount = (NSUInteger)sample_count;
        desc.usage = (MTLTextureUsage)usage;
        desc.storageMode = (MTLStorageMode)storage_mode;
        id<MTLTexture> tex = [device newTextureWithDescriptor:desc];
        [desc release];
        return (int64_t)(intptr_t)tex;
    }
}

// ── Compute Pipeline ────────────────────────────────────

static int64_t gorget_metal_create_compute_pipeline(int64_t device_h, int64_t function_h) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        id<MTLFunction> fn = (__bridge id<MTLFunction>)(void*)(intptr_t)function_h;
        NSError* error = nil;
        id<MTLComputePipelineState> pso = [device newComputePipelineStateWithFunction:fn error:&error];
        if (!pso) {
            NSLog(@"Metal compute pipeline error: %@", error);
            return 0;
        }
        return (int64_t)(intptr_t)pso; // +1 from new, caller owns
    }
}

static int64_t gorget_metal_create_compute_encoder(int64_t cmd_buf_h) {
    @autoreleasepool {
        id<MTLCommandBuffer> cb = (__bridge id<MTLCommandBuffer>)(void*)(intptr_t)cmd_buf_h;
        id<MTLComputeCommandEncoder> enc = [cb computeCommandEncoder];
        [(id)enc retain]; // autoreleased
        return (int64_t)(intptr_t)enc;
    }
}

static void gorget_metal_compute_set_pipeline(int64_t enc_h, int64_t pipeline_h) {
    @autoreleasepool {
        id<MTLComputeCommandEncoder> enc = (__bridge id<MTLComputeCommandEncoder>)(void*)(intptr_t)enc_h;
        id<MTLComputePipelineState> pso = (__bridge id<MTLComputePipelineState>)(void*)(intptr_t)pipeline_h;
        [enc setComputePipelineState:pso];
    }
}

static void gorget_metal_compute_set_buffer(int64_t enc_h, int64_t buf_h, int64_t offset, int64_t index) {
    @autoreleasepool {
        id<MTLComputeCommandEncoder> enc = (__bridge id<MTLComputeCommandEncoder>)(void*)(intptr_t)enc_h;
        id<MTLBuffer> buf = (__bridge id<MTLBuffer>)(void*)(intptr_t)buf_h;
        [enc setBuffer:buf offset:(NSUInteger)offset atIndex:(NSUInteger)index];
    }
}

static void gorget_metal_compute_set_bytes(int64_t enc_h, const GorgetArray* data, int64_t index) {
    @autoreleasepool {
        id<MTLComputeCommandEncoder> enc = (__bridge id<MTLComputeCommandEncoder>)(void*)(intptr_t)enc_h;
        if (data && data->data) {
            [enc setBytes:data->data length:(NSUInteger)data->len atIndex:(NSUInteger)index];
        }
    }
}

static void gorget_metal_compute_set_texture(int64_t enc_h, int64_t tex_h, int64_t index) {
    @autoreleasepool {
        id<MTLComputeCommandEncoder> enc = (__bridge id<MTLComputeCommandEncoder>)(void*)(intptr_t)enc_h;
        id<MTLTexture> tex = (__bridge id<MTLTexture>)(void*)(intptr_t)tex_h;
        [enc setTexture:tex atIndex:(NSUInteger)index];
    }
}

static void gorget_metal_compute_dispatch_threadgroups(int64_t enc_h, int64_t gx, int64_t gy, int64_t gz, int64_t tx, int64_t ty, int64_t tz) {
    @autoreleasepool {
        id<MTLComputeCommandEncoder> enc = (__bridge id<MTLComputeCommandEncoder>)(void*)(intptr_t)enc_h;
        MTLSize groups = MTLSizeMake((NSUInteger)gx, (NSUInteger)gy, (NSUInteger)gz);
        MTLSize threads = MTLSizeMake((NSUInteger)tx, (NSUInteger)ty, (NSUInteger)tz);
        [enc dispatchThreadgroups:groups threadsPerThreadgroup:threads];
    }
}

static void gorget_metal_compute_dispatch_threads(int64_t enc_h, int64_t gx, int64_t gy, int64_t gz, int64_t tx, int64_t ty, int64_t tz) {
    @autoreleasepool {
        id<MTLComputeCommandEncoder> enc = (__bridge id<MTLComputeCommandEncoder>)(void*)(intptr_t)enc_h;
        MTLSize grid = MTLSizeMake((NSUInteger)gx, (NSUInteger)gy, (NSUInteger)gz);
        MTLSize threads = MTLSizeMake((NSUInteger)tx, (NSUInteger)ty, (NSUInteger)tz);
        [enc dispatchThreads:grid threadsPerThreadgroup:threads];
    }
}

static void gorget_metal_compute_end(int64_t enc_h) {
    @autoreleasepool {
        id<MTLComputeCommandEncoder> enc = (__bridge id<MTLComputeCommandEncoder>)(void*)(intptr_t)enc_h;
        [enc endEncoding];
    }
}

static int64_t gorget_metal_compute_thread_execution_width(int64_t pipeline_h) {
    @autoreleasepool {
        id<MTLComputePipelineState> pso = (__bridge id<MTLComputePipelineState>)(void*)(intptr_t)pipeline_h;
        return (int64_t)[pso threadExecutionWidth];
    }
}

static int64_t gorget_metal_compute_max_threads_per_threadgroup(int64_t pipeline_h) {
    @autoreleasepool {
        id<MTLComputePipelineState> pso = (__bridge id<MTLComputePipelineState>)(void*)(intptr_t)pipeline_h;
        return (int64_t)[pso maxTotalThreadsPerThreadgroup];
    }
}

static void gorget_metal_compute_set_sampler(int64_t enc_h, int64_t sampler_h, int64_t index) {
    @autoreleasepool {
        id<MTLComputeCommandEncoder> enc = (__bridge id<MTLComputeCommandEncoder>)(void*)(intptr_t)enc_h;
        id<MTLSamplerState> sampler = (__bridge id<MTLSamplerState>)(void*)(intptr_t)sampler_h;
        [enc setSamplerState:sampler atIndex:(NSUInteger)index];
    }
}

// ── GPU Synchronization ─────────────────────────────────

static int64_t gorget_metal_create_event(int64_t device_h) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        id<MTLEvent> event = [device newEvent];
        return (int64_t)(intptr_t)event; // +1 from new
    }
}

static void gorget_metal_command_buffer_encode_signal_event(int64_t cmd_buf_h, int64_t event_h, int64_t value) {
    @autoreleasepool {
        id<MTLCommandBuffer> cb = (__bridge id<MTLCommandBuffer>)(void*)(intptr_t)cmd_buf_h;
        id<MTLEvent> event = (__bridge id<MTLEvent>)(void*)(intptr_t)event_h;
        [cb encodeSignalEvent:event value:(uint64_t)value];
    }
}

static void gorget_metal_command_buffer_encode_wait_event(int64_t cmd_buf_h, int64_t event_h, int64_t value) {
    @autoreleasepool {
        id<MTLCommandBuffer> cb = (__bridge id<MTLCommandBuffer>)(void*)(intptr_t)cmd_buf_h;
        id<MTLEvent> event = (__bridge id<MTLEvent>)(void*)(intptr_t)event_h;
        [cb encodeWaitForEvent:event value:(uint64_t)value];
    }
}

static int64_t gorget_metal_create_shared_event(int64_t device_h) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        id<MTLSharedEvent> event = [device newSharedEvent];
        return (int64_t)(intptr_t)event; // +1 from new
    }
}

static int64_t gorget_metal_shared_event_signaled_value(int64_t event_h) {
    @autoreleasepool {
        id<MTLSharedEvent> event = (__bridge id<MTLSharedEvent>)(void*)(intptr_t)event_h;
        return (int64_t)[event signaledValue];
    }
}

static int64_t gorget_metal_create_fence(int64_t device_h) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        id<MTLFence> fence = [device newFence];
        return (int64_t)(intptr_t)fence; // +1 from new
    }
}

static void gorget_metal_encoder_wait_for_fence(int64_t enc_h, int64_t fence_h) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        id<MTLFence> fence = (__bridge id<MTLFence>)(void*)(intptr_t)fence_h;
        [enc waitForFence:fence beforeStages:MTLRenderStageFragment];
    }
}

static void gorget_metal_encoder_update_fence(int64_t enc_h, int64_t fence_h) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        id<MTLFence> fence = (__bridge id<MTLFence>)(void*)(intptr_t)fence_h;
        [enc updateFence:fence afterStages:MTLRenderStageFragment];
    }
}

static void gorget_metal_encoder_memory_barrier(int64_t enc_h, int64_t scope) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        [enc memoryBarrierWithScope:(MTLBarrierScope)scope afterStages:MTLRenderStageFragment beforeStages:MTLRenderStageVertex];
    }
}

// ── Blit Encoder Operations ─────────────────────────────

static void gorget_metal_blit_copy_buffer_to_buffer(int64_t enc_h, int64_t src_h, int64_t src_offset, int64_t dst_h, int64_t dst_offset, int64_t size) {
    @autoreleasepool {
        id<MTLBlitCommandEncoder> enc = (__bridge id<MTLBlitCommandEncoder>)(void*)(intptr_t)enc_h;
        id<MTLBuffer> src = (__bridge id<MTLBuffer>)(void*)(intptr_t)src_h;
        id<MTLBuffer> dst = (__bridge id<MTLBuffer>)(void*)(intptr_t)dst_h;
        [enc copyFromBuffer:src sourceOffset:(NSUInteger)src_offset toBuffer:dst destinationOffset:(NSUInteger)dst_offset size:(NSUInteger)size];
    }
}

static void gorget_metal_blit_copy_texture_to_texture(int64_t enc_h, int64_t src_h, int64_t src_slice, int64_t src_level, int64_t dst_h, int64_t dst_slice, int64_t dst_level, int64_t sx, int64_t sy, int64_t sw, int64_t sh) {
    @autoreleasepool {
        id<MTLBlitCommandEncoder> enc = (__bridge id<MTLBlitCommandEncoder>)(void*)(intptr_t)enc_h;
        id<MTLTexture> src = (__bridge id<MTLTexture>)(void*)(intptr_t)src_h;
        id<MTLTexture> dst = (__bridge id<MTLTexture>)(void*)(intptr_t)dst_h;
        [enc copyFromTexture:src sourceSlice:(NSUInteger)src_slice sourceLevel:(NSUInteger)src_level
             sourceOrigin:MTLOriginMake((NSUInteger)sx, (NSUInteger)sy, 0)
             sourceSize:MTLSizeMake((NSUInteger)sw, (NSUInteger)sh, 1)
             toTexture:dst destinationSlice:(NSUInteger)dst_slice destinationLevel:(NSUInteger)dst_level
             destinationOrigin:MTLOriginMake(0, 0, 0)];
    }
}

static void gorget_metal_blit_copy_buffer_to_texture(int64_t enc_h, int64_t buf_h, int64_t buf_offset, int64_t bytes_per_row, int64_t bytes_per_image, int64_t tex_h, int64_t slice, int64_t level, int64_t w, int64_t h, int64_t d) {
    @autoreleasepool {
        id<MTLBlitCommandEncoder> enc = (__bridge id<MTLBlitCommandEncoder>)(void*)(intptr_t)enc_h;
        id<MTLBuffer> buf = (__bridge id<MTLBuffer>)(void*)(intptr_t)buf_h;
        id<MTLTexture> tex = (__bridge id<MTLTexture>)(void*)(intptr_t)tex_h;
        [enc copyFromBuffer:buf sourceOffset:(NSUInteger)buf_offset
             sourceBytesPerRow:(NSUInteger)bytes_per_row sourceBytesPerImage:(NSUInteger)bytes_per_image
             sourceSize:MTLSizeMake((NSUInteger)w, (NSUInteger)h, (NSUInteger)d)
             toTexture:tex destinationSlice:(NSUInteger)slice destinationLevel:(NSUInteger)level
             destinationOrigin:MTLOriginMake(0, 0, 0)];
    }
}

static void gorget_metal_blit_copy_texture_to_buffer(int64_t enc_h, int64_t tex_h, int64_t slice, int64_t level, int64_t buf_h, int64_t buf_offset, int64_t bytes_per_row, int64_t bytes_per_image, int64_t w, int64_t h, int64_t d) {
    @autoreleasepool {
        id<MTLBlitCommandEncoder> enc = (__bridge id<MTLBlitCommandEncoder>)(void*)(intptr_t)enc_h;
        id<MTLTexture> tex = (__bridge id<MTLTexture>)(void*)(intptr_t)tex_h;
        id<MTLBuffer> buf = (__bridge id<MTLBuffer>)(void*)(intptr_t)buf_h;
        [enc copyFromTexture:tex sourceSlice:(NSUInteger)slice sourceLevel:(NSUInteger)level
             sourceOrigin:MTLOriginMake(0, 0, 0)
             sourceSize:MTLSizeMake((NSUInteger)w, (NSUInteger)h, (NSUInteger)d)
             toBuffer:buf destinationOffset:(NSUInteger)buf_offset
             destinationBytesPerRow:(NSUInteger)bytes_per_row destinationBytesPerImage:(NSUInteger)bytes_per_image];
    }
}

static void gorget_metal_blit_fill_buffer(int64_t enc_h, int64_t buf_h, int64_t offset, int64_t length, int64_t value) {
    @autoreleasepool {
        id<MTLBlitCommandEncoder> enc = (__bridge id<MTLBlitCommandEncoder>)(void*)(intptr_t)enc_h;
        id<MTLBuffer> buf = (__bridge id<MTLBuffer>)(void*)(intptr_t)buf_h;
        NSRange range = NSMakeRange((NSUInteger)offset, (NSUInteger)length);
        [enc fillBuffer:buf range:range value:(uint8_t)value];
    }
}

static void gorget_metal_blit_synchronize_resource(int64_t enc_h, int64_t resource_h) {
    @autoreleasepool {
#if TARGET_OS_OSX
        id<MTLBlitCommandEncoder> enc = (__bridge id<MTLBlitCommandEncoder>)(void*)(intptr_t)enc_h;
        id<MTLResource> resource = (__bridge id<MTLResource>)(void*)(intptr_t)resource_h;
        [enc synchronizeResource:resource];
#else
        (void)enc_h; (void)resource_h; // no-op on iOS (unified memory)
#endif
    }
}

// ── Indirect Drawing ────────────────────────────────────

static void gorget_metal_encoder_draw_primitives_indirect(int64_t enc_h, int64_t prim_type, int64_t indirect_buf_h, int64_t offset) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        id<MTLBuffer> buf = (__bridge id<MTLBuffer>)(void*)(intptr_t)indirect_buf_h;
        [enc drawPrimitives:(MTLPrimitiveType)prim_type indirectBuffer:buf indirectBufferOffset:(NSUInteger)offset];
    }
}

static void gorget_metal_encoder_draw_indexed_indirect(int64_t enc_h, int64_t prim_type, int64_t index_count, int64_t index_type, int64_t index_buf_h, int64_t index_offset, int64_t indirect_buf_h, int64_t indirect_offset) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        id<MTLBuffer> idx_buf = (__bridge id<MTLBuffer>)(void*)(intptr_t)index_buf_h;
        id<MTLBuffer> ind_buf = (__bridge id<MTLBuffer>)(void*)(intptr_t)indirect_buf_h;
        [enc drawIndexedPrimitives:(MTLPrimitiveType)prim_type indexCount:(NSUInteger)index_count
             indexType:(MTLIndexType)index_type indexBuffer:idx_buf indexBufferOffset:(NSUInteger)index_offset
             indirectBuffer:ind_buf indirectBufferOffset:(NSUInteger)indirect_offset];
    }
}

// ── Additional Texture Types ────────────────────────────

static int64_t gorget_metal_create_texture_cube(int64_t device_h, int64_t format, int64_t size, int64_t mipmaps, int64_t usage) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        MTLTextureDescriptor* desc = [MTLTextureDescriptor textureCubeDescriptorWithPixelFormat:(MTLPixelFormat)format size:(NSUInteger)size mipmapped:(mipmaps > 1)];
        if (mipmaps > 1) desc.mipmapLevelCount = (NSUInteger)mipmaps;
        desc.usage = (MTLTextureUsage)usage;
        id<MTLTexture> tex = [device newTextureWithDescriptor:desc];
        return (int64_t)(intptr_t)tex; // +1 from new
    }
}

static int64_t gorget_metal_create_texture_2d_array(int64_t device_h, int64_t format, int64_t width, int64_t height, int64_t array_length, int64_t mipmaps, int64_t usage) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        MTLTextureDescriptor* desc = [[MTLTextureDescriptor alloc] init];
        desc.textureType = MTLTextureType2DArray;
        desc.pixelFormat = (MTLPixelFormat)format;
        desc.width = (NSUInteger)width;
        desc.height = (NSUInteger)height;
        desc.arrayLength = (NSUInteger)array_length;
        desc.mipmapLevelCount = (NSUInteger)(mipmaps > 0 ? mipmaps : 1);
        desc.usage = (MTLTextureUsage)usage;
        id<MTLTexture> tex = [device newTextureWithDescriptor:desc];
        return (int64_t)(intptr_t)tex; // +1 from new
    }
}

static int64_t gorget_metal_create_texture_3d(int64_t device_h, int64_t format, int64_t width, int64_t height, int64_t depth, int64_t mipmaps, int64_t usage) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        MTLTextureDescriptor* desc = [[MTLTextureDescriptor alloc] init];
        desc.textureType = MTLTextureType3D;
        desc.pixelFormat = (MTLPixelFormat)format;
        desc.width = (NSUInteger)width;
        desc.height = (NSUInteger)height;
        desc.depth = (NSUInteger)depth;
        desc.mipmapLevelCount = (NSUInteger)(mipmaps > 0 ? mipmaps : 1);
        desc.usage = (MTLTextureUsage)usage;
        id<MTLTexture> tex = [device newTextureWithDescriptor:desc];
        return (int64_t)(intptr_t)tex; // +1 from new
    }
}

static int64_t gorget_metal_create_texture_view(int64_t tex_h, int64_t format, int64_t tex_type, int64_t mip_start, int64_t mip_count, int64_t slice_start, int64_t slice_count) {
    @autoreleasepool {
        id<MTLTexture> tex = (__bridge id<MTLTexture>)(void*)(intptr_t)tex_h;
        NSRange mip_range = NSMakeRange((NSUInteger)mip_start, (NSUInteger)mip_count);
        NSRange slice_range = NSMakeRange((NSUInteger)slice_start, (NSUInteger)slice_count);
        id<MTLTexture> view = [tex newTextureViewWithPixelFormat:(MTLPixelFormat)format
                                   textureType:(MTLTextureType)tex_type
                                   levels:mip_range slices:slice_range];
        return (int64_t)(intptr_t)view; // +1 from new
    }
}

// ── Resource Heaps ──────────────────────────────────────

static int64_t gorget_metal_create_heap(int64_t device_h, int64_t size, int64_t storage_mode) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        MTLHeapDescriptor* desc = [[MTLHeapDescriptor alloc] init];
        desc.size = (NSUInteger)size;
        desc.storageMode = (MTLStorageMode)storage_mode;
        desc.cpuCacheMode = MTLCPUCacheModeDefaultCache;
        id<MTLHeap> heap = [device newHeapWithDescriptor:desc];
        [desc release];
        return (int64_t)(intptr_t)heap; // +1 from new
    }
}

static int64_t gorget_metal_heap_create_buffer(int64_t heap_h, int64_t length, int64_t storage_mode) {
    @autoreleasepool {
        id<MTLHeap> heap = (__bridge id<MTLHeap>)(void*)(intptr_t)heap_h;
        id<MTLBuffer> buf = [heap newBufferWithLength:(NSUInteger)length
                                             options:mtl_resource_options(storage_mode)];
        return (int64_t)(intptr_t)buf; // +1 from new, nil if heap is full
    }
}

static int64_t gorget_metal_heap_create_texture(int64_t heap_h, int64_t width, int64_t height, int64_t format, int64_t usage, int64_t storage_mode) {
    @autoreleasepool {
        id<MTLHeap> heap = (__bridge id<MTLHeap>)(void*)(intptr_t)heap_h;
        MTLTextureDescriptor* desc = [MTLTextureDescriptor texture2DDescriptorWithPixelFormat:(MTLPixelFormat)format
                                                                                        width:(NSUInteger)width
                                                                                       height:(NSUInteger)height
                                                                                    mipmapped:NO];
        desc.usage = (MTLTextureUsage)usage;
        desc.storageMode = (MTLStorageMode)storage_mode;
        id<MTLTexture> tex = [heap newTextureWithDescriptor:desc];
        return (int64_t)(intptr_t)tex; // +1 from new, nil if heap is full
    }
}

static int64_t gorget_metal_heap_used_size(int64_t heap_h) {
    @autoreleasepool {
        id<MTLHeap> heap = (__bridge id<MTLHeap>)(void*)(intptr_t)heap_h;
        return (int64_t)[heap usedSize];
    }
}

static int64_t gorget_metal_heap_current_allocated_size(int64_t heap_h) {
    @autoreleasepool {
        id<MTLHeap> heap = (__bridge id<MTLHeap>)(void*)(intptr_t)heap_h;
        return (int64_t)[heap currentAllocatedSize];
    }
}

static int64_t gorget_metal_heap_max_available_size(int64_t heap_h, int64_t alignment) {
    @autoreleasepool {
        id<MTLHeap> heap = (__bridge id<MTLHeap>)(void*)(intptr_t)heap_h;
        return (int64_t)[heap maxAvailableSizeWithAlignment:(NSUInteger)alignment];
    }
}

// ── Argument Buffers (Bindless Resources) ───────────────

static int64_t gorget_metal_create_argument_encoder(int64_t fn_h, int64_t buffer_index) {
    @autoreleasepool {
        id<MTLFunction> fn = (__bridge id<MTLFunction>)(void*)(intptr_t)fn_h;
        id<MTLArgumentEncoder> enc = [fn newArgumentEncoderWithBufferIndex:(NSUInteger)buffer_index];
        return (int64_t)(intptr_t)enc; // +1 from new
    }
}

static int64_t gorget_metal_argument_encoder_encoded_length(int64_t enc_h) {
    @autoreleasepool {
        id<MTLArgumentEncoder> enc = (__bridge id<MTLArgumentEncoder>)(void*)(intptr_t)enc_h;
        return (int64_t)[enc encodedLength];
    }
}

static void gorget_metal_argument_encoder_set_argument_buffer(int64_t enc_h, int64_t buf_h, int64_t offset) {
    @autoreleasepool {
        id<MTLArgumentEncoder> enc = (__bridge id<MTLArgumentEncoder>)(void*)(intptr_t)enc_h;
        id<MTLBuffer> buf = (__bridge id<MTLBuffer>)(void*)(intptr_t)buf_h;
        [enc setArgumentBuffer:buf offset:(NSUInteger)offset];
    }
}

static void gorget_metal_argument_encoder_set_buffer(int64_t enc_h, int64_t buf_h, int64_t offset, int64_t index) {
    @autoreleasepool {
        id<MTLArgumentEncoder> enc = (__bridge id<MTLArgumentEncoder>)(void*)(intptr_t)enc_h;
        id<MTLBuffer> buf = (__bridge id<MTLBuffer>)(void*)(intptr_t)buf_h;
        [enc setBuffer:buf offset:(NSUInteger)offset atIndex:(NSUInteger)index];
    }
}

static void gorget_metal_argument_encoder_set_texture(int64_t enc_h, int64_t tex_h, int64_t index) {
    @autoreleasepool {
        id<MTLArgumentEncoder> enc = (__bridge id<MTLArgumentEncoder>)(void*)(intptr_t)enc_h;
        id<MTLTexture> tex = (__bridge id<MTLTexture>)(void*)(intptr_t)tex_h;
        [enc setTexture:tex atIndex:(NSUInteger)index];
    }
}

static void gorget_metal_argument_encoder_set_sampler(int64_t enc_h, int64_t sampler_h, int64_t index) {
    @autoreleasepool {
        id<MTLArgumentEncoder> enc = (__bridge id<MTLArgumentEncoder>)(void*)(intptr_t)enc_h;
        id<MTLSamplerState> sampler = (__bridge id<MTLSamplerState>)(void*)(intptr_t)sampler_h;
        [enc setSamplerState:sampler atIndex:(NSUInteger)index];
    }
}

static void gorget_metal_encoder_use_resource(int64_t enc_h, int64_t resource_h, int64_t usage) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        id<MTLResource> resource = (__bridge id<MTLResource>)(void*)(intptr_t)resource_h;
        [enc useResource:resource usage:(MTLResourceUsage)usage];
    }
}

static void gorget_metal_encoder_use_heap(int64_t enc_h, int64_t heap_h) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        id<MTLHeap> heap = (__bridge id<MTLHeap>)(void*)(intptr_t)heap_h;
        [enc useHeap:heap];
    }
}

// ── Parallel Render Command Encoder ─────────────────────

static int64_t gorget_metal_create_parallel_render_encoder(int64_t cmd_buf_h, int64_t pass_desc_h) {
    @autoreleasepool {
        id<MTLCommandBuffer> cb = (__bridge id<MTLCommandBuffer>)(void*)(intptr_t)cmd_buf_h;
        MTLRenderPassDescriptor* pass = (__bridge MTLRenderPassDescriptor*)(void*)(intptr_t)pass_desc_h;
        id<MTLParallelRenderCommandEncoder> enc = [cb parallelRenderCommandEncoderWithDescriptor:pass];
        [(id)enc retain];
        return (int64_t)(intptr_t)enc;
    }
}

static int64_t gorget_metal_parallel_encoder_make_render_encoder(int64_t parallel_enc_h) {
    @autoreleasepool {
        id<MTLParallelRenderCommandEncoder> penc = (__bridge id<MTLParallelRenderCommandEncoder>)(void*)(intptr_t)parallel_enc_h;
        id<MTLRenderCommandEncoder> enc = [penc renderCommandEncoder];
        [(id)enc retain];
        return (int64_t)(intptr_t)enc;
    }
}

static void gorget_metal_parallel_encoder_end(int64_t parallel_enc_h) {
    @autoreleasepool {
        id<MTLParallelRenderCommandEncoder> penc = (__bridge id<MTLParallelRenderCommandEncoder>)(void*)(intptr_t)parallel_enc_h;
        [penc endEncoding];
    }
}

// ── Visibility Result Buffer (Occlusion Queries) ────────

static void gorget_metal_render_pass_set_visibility_result_buffer(int64_t desc_h, int64_t buffer_h) {
    @autoreleasepool {
        MTLRenderPassDescriptor* desc = (__bridge MTLRenderPassDescriptor*)(void*)(intptr_t)desc_h;
        id<MTLBuffer> buf = (__bridge id<MTLBuffer>)(void*)(intptr_t)buffer_h;
        desc.visibilityResultBuffer = buf;
    }
}

static void gorget_metal_encoder_set_visibility_result_mode(int64_t enc_h, int64_t mode, int64_t offset) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
        [enc setVisibilityResultMode:(MTLVisibilityResultMode)mode offset:(NSUInteger)offset];
    }
}

// ── Full Stencil State (Separate front/back) ────────────

static int64_t gorget_metal_create_depth_stencil_full(int64_t device_h, int64_t depth_compare, int64_t depth_write,
    int64_t front_compare, int64_t front_read_mask, int64_t front_write_mask, int64_t front_sfail, int64_t front_dfail, int64_t front_dspass,
    int64_t back_compare, int64_t back_read_mask, int64_t back_write_mask, int64_t back_sfail, int64_t back_dfail, int64_t back_dspass) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        MTLDepthStencilDescriptor* desc = [[MTLDepthStencilDescriptor alloc] init];
        desc.depthCompareFunction = (MTLCompareFunction)depth_compare;
        desc.depthWriteEnabled = (depth_write != 0);

        MTLStencilDescriptor* front = [[MTLStencilDescriptor alloc] init];
        front.stencilCompareFunction = (MTLCompareFunction)front_compare;
        front.readMask = (uint32_t)front_read_mask;
        front.writeMask = (uint32_t)front_write_mask;
        front.stencilFailureOperation = (MTLStencilOperation)front_sfail;
        front.depthFailureOperation = (MTLStencilOperation)front_dfail;
        front.depthStencilPassOperation = (MTLStencilOperation)front_dspass;
        desc.frontFaceStencil = front;

        MTLStencilDescriptor* back = [[MTLStencilDescriptor alloc] init];
        back.stencilCompareFunction = (MTLCompareFunction)back_compare;
        back.readMask = (uint32_t)back_read_mask;
        back.writeMask = (uint32_t)back_write_mask;
        back.stencilFailureOperation = (MTLStencilOperation)back_sfail;
        back.depthFailureOperation = (MTLStencilOperation)back_dfail;
        back.depthStencilPassOperation = (MTLStencilOperation)back_dspass;
        desc.backFaceStencil = back;

        id<MTLDepthStencilState> state = [device newDepthStencilStateWithDescriptor:desc];
        [front release];
        [back release];
        [desc release];
        return (int64_t)(intptr_t)state; // +1 from new
    }
}

// ── MRT Render Pipeline ─────────────────────────────────

static int64_t gorget_metal_create_render_pipeline_mrt(int64_t device_h, int64_t vertex_fn_h, int64_t fragment_fn_h,
    int64_t vertex_desc_h, int64_t num_color_attachments, const GorgetArray* color_formats, int64_t depth_format) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        id<MTLFunction> vertFn = (__bridge id<MTLFunction>)(void*)(intptr_t)vertex_fn_h;
        id<MTLFunction> fragFn = (__bridge id<MTLFunction>)(void*)(intptr_t)fragment_fn_h;
        MTLVertexDescriptor* vertDesc = vertex_desc_h ? (__bridge MTLVertexDescriptor*)(void*)(intptr_t)vertex_desc_h : nil;

        MTLRenderPipelineDescriptor* desc = [[MTLRenderPipelineDescriptor alloc] init];
        desc.vertexFunction = vertFn;
        desc.fragmentFunction = fragFn;
        if (vertDesc) desc.vertexDescriptor = vertDesc;
        desc.depthAttachmentPixelFormat = (MTLPixelFormat)depth_format;

        int n = (int)num_color_attachments;
        if (n > 8) n = 8;
        const int64_t* fmts = color_formats ? (const int64_t*)color_formats->data : NULL;
        for (int i = 0; i < n; i++) {
            desc.colorAttachments[i].pixelFormat = (MTLPixelFormat)(fmts ? fmts[i] : 80);
        }

        NSError* error = nil;
        id<MTLRenderPipelineState> pso = [device newRenderPipelineStateWithDescriptor:desc error:&error];
        [desc release];
        if (error) {
            NSLog(@"MRT pipeline error: %@", error);
            return 0;
        }
        return (int64_t)(intptr_t)pso; // +1 from new
    }
}

static void gorget_metal_render_pass_set_color_texture(int64_t desc_h, int64_t index, int64_t tex_h) {
    @autoreleasepool {
        MTLRenderPassDescriptor* desc = (__bridge MTLRenderPassDescriptor*)(void*)(intptr_t)desc_h;
        id<MTLTexture> tex = (__bridge id<MTLTexture>)(void*)(intptr_t)tex_h;
        desc.colorAttachments[(NSUInteger)index].texture = tex;
        desc.colorAttachments[(NSUInteger)index].loadAction = MTLLoadActionClear;
        desc.colorAttachments[(NSUInteger)index].storeAction = MTLStoreActionStore;
    }
}

// ── Indirect Command Buffers (GPU-Driven Rendering) ─────

static int64_t gorget_metal_create_indirect_command_buffer(int64_t device_h, int64_t max_commands, int64_t inherit_pipeline, int64_t inherit_buffers) {
    @autoreleasepool {
        if (@available(macOS 10.14, *)) {
            id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
            MTLIndirectCommandBufferDescriptor* desc = [[MTLIndirectCommandBufferDescriptor alloc] init];
            desc.commandTypes = MTLIndirectCommandTypeDraw | MTLIndirectCommandTypeDrawIndexed;
            desc.inheritPipelineState = (inherit_pipeline != 0);
            desc.inheritBuffers = (inherit_buffers != 0);
            desc.maxVertexBufferBindCount = 8;
            desc.maxFragmentBufferBindCount = 4;
            id<MTLIndirectCommandBuffer> icb = [device newIndirectCommandBufferWithDescriptor:desc maxCommandCount:(NSUInteger)max_commands options:0];
            [desc release];
            return (int64_t)(intptr_t)icb; // +1 from new
        }
        return 0;
    }
}

static int64_t gorget_metal_icb_indirect_render_command(int64_t icb_h, int64_t index) {
    @autoreleasepool {
        if (@available(macOS 10.14, *)) {
            id<MTLIndirectCommandBuffer> icb = (__bridge id<MTLIndirectCommandBuffer>)(void*)(intptr_t)icb_h;
            id<MTLIndirectRenderCommand> cmd = [icb indirectRenderCommandAtIndex:(NSUInteger)index];
            return (int64_t)(intptr_t)cmd; // not retained — owned by ICB
        }
        return 0;
    }
}

static void gorget_metal_indirect_render_set_pipeline(int64_t cmd_h, int64_t pipeline_h) {
    @autoreleasepool {
        if (@available(macOS 10.14, *)) {
            id<MTLIndirectRenderCommand> cmd = (__bridge id<MTLIndirectRenderCommand>)(void*)(intptr_t)cmd_h;
            id<MTLRenderPipelineState> pso = (__bridge id<MTLRenderPipelineState>)(void*)(intptr_t)pipeline_h;
            [cmd setRenderPipelineState:pso];
        }
    }
}

static void gorget_metal_indirect_render_set_vertex_buffer(int64_t cmd_h, int64_t buf_h, int64_t offset, int64_t index) {
    @autoreleasepool {
        if (@available(macOS 10.14, *)) {
            id<MTLIndirectRenderCommand> cmd = (__bridge id<MTLIndirectRenderCommand>)(void*)(intptr_t)cmd_h;
            id<MTLBuffer> buf = (__bridge id<MTLBuffer>)(void*)(intptr_t)buf_h;
            [cmd setVertexBuffer:buf offset:(NSUInteger)offset atIndex:(NSUInteger)index];
        }
    }
}

static void gorget_metal_indirect_render_set_fragment_buffer(int64_t cmd_h, int64_t buf_h, int64_t offset, int64_t index) {
    @autoreleasepool {
        if (@available(macOS 10.14, *)) {
            id<MTLIndirectRenderCommand> cmd = (__bridge id<MTLIndirectRenderCommand>)(void*)(intptr_t)cmd_h;
            id<MTLBuffer> buf = (__bridge id<MTLBuffer>)(void*)(intptr_t)buf_h;
            [cmd setFragmentBuffer:buf offset:(NSUInteger)offset atIndex:(NSUInteger)index];
        }
    }
}

static void gorget_metal_indirect_render_draw_primitives(int64_t cmd_h, int64_t prim_type, int64_t start, int64_t count, int64_t instance_count, int64_t base_instance) {
    @autoreleasepool {
        if (@available(macOS 10.14, *)) {
            id<MTLIndirectRenderCommand> cmd = (__bridge id<MTLIndirectRenderCommand>)(void*)(intptr_t)cmd_h;
            [cmd drawPrimitives:(MTLPrimitiveType)prim_type vertexStart:(NSUInteger)start vertexCount:(NSUInteger)count instanceCount:(NSUInteger)instance_count baseInstance:(NSUInteger)base_instance];
        }
    }
}

static void gorget_metal_indirect_render_draw_indexed(int64_t cmd_h, int64_t prim_type, int64_t index_count, int64_t index_type, int64_t index_buf_h, int64_t index_offset, int64_t instance_count, int64_t base_vertex, int64_t base_instance) {
    @autoreleasepool {
        if (@available(macOS 10.14, *)) {
            id<MTLIndirectRenderCommand> cmd = (__bridge id<MTLIndirectRenderCommand>)(void*)(intptr_t)cmd_h;
            id<MTLBuffer> idx_buf = (__bridge id<MTLBuffer>)(void*)(intptr_t)index_buf_h;
            [cmd drawIndexedPrimitives:(MTLPrimitiveType)prim_type indexCount:(NSUInteger)index_count indexType:(MTLIndexType)index_type indexBuffer:idx_buf indexBufferOffset:(NSUInteger)index_offset instanceCount:(NSUInteger)instance_count baseVertex:(NSInteger)base_vertex baseInstance:(NSUInteger)base_instance];
        }
    }
}

static void gorget_metal_indirect_render_reset(int64_t cmd_h) {
    @autoreleasepool {
        if (@available(macOS 10.14, *)) {
            id<MTLIndirectRenderCommand> cmd = (__bridge id<MTLIndirectRenderCommand>)(void*)(intptr_t)cmd_h;
            [cmd reset];
        }
    }
}

static void gorget_metal_encoder_execute_commands_in_buffer(int64_t enc_h, int64_t icb_h, int64_t start, int64_t count) {
    @autoreleasepool {
        if (@available(macOS 10.14, *)) {
            id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)enc_h;
            id<MTLIndirectCommandBuffer> icb = (__bridge id<MTLIndirectCommandBuffer>)(void*)(intptr_t)icb_h;
            NSRange range = NSMakeRange((NSUInteger)start, (NSUInteger)count);
            [enc executeCommandsInBuffer:icb withRange:range];
        }
    }
}

// ── Sampler with LOD ────────────────────────────────────

static int64_t gorget_metal_create_sampler_with_lod(int64_t device_h, int64_t min_filter, int64_t mag_filter, int64_t mip_filter, int64_t address_s, int64_t address_t, double lod_min, double lod_max, int64_t max_anisotropy) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        MTLSamplerDescriptor* desc = [[MTLSamplerDescriptor alloc] init];
        desc.minFilter = (MTLSamplerMinMagFilter)min_filter;
        desc.magFilter = (MTLSamplerMinMagFilter)mag_filter;
        desc.mipFilter = (MTLSamplerMipFilter)mip_filter;
        desc.sAddressMode = (MTLSamplerAddressMode)address_s;
        desc.tAddressMode = (MTLSamplerAddressMode)address_t;
        desc.lodMinClamp = (float)lod_min;
        desc.lodMaxClamp = (float)lod_max;
        desc.maxAnisotropy = (NSUInteger)max_anisotropy;
        id<MTLSamplerState> sampler = [device newSamplerStateWithDescriptor:desc];
        [desc release];
        return (int64_t)(intptr_t)sampler; // +1 from new
    }
}

// ── Texture Read-back ───────────────────────────────────

static GorgetArray gorget_metal_texture_get_bytes(int64_t tex_h, int64_t x, int64_t y, int64_t w, int64_t h, int64_t bytes_per_row) {
    @autoreleasepool {
        id<MTLTexture> tex = (__bridge id<MTLTexture>)(void*)(intptr_t)tex_h;
        NSUInteger row_bytes = (NSUInteger)bytes_per_row;
        NSUInteger total = row_bytes * (NSUInteger)h;
        GorgetArray arr = gorget_array_new(1);
        uint8_t* buf = (uint8_t*)GORGET_ALLOC(total);
        MTLRegion region = MTLRegionMake2D((NSUInteger)x, (NSUInteger)y, (NSUInteger)w, (NSUInteger)h);
        [tex getBytes:buf bytesPerRow:row_bytes fromRegion:region mipmapLevel:0];
        arr.data = buf;
        arr.len = total;
        arr.cap = total;
        return arr;
    }
}

// ── Buffer Write from CPU ───────────────────────────────

static void gorget_metal_buffer_write(int64_t buf_h, int64_t offset, const GorgetArray* data) {
    @autoreleasepool {
        id<MTLBuffer> buf = (__bridge id<MTLBuffer>)(void*)(intptr_t)buf_h;
        if (!data || !data->data) return;
        void* dst = (uint8_t*)[buf contents] + (NSUInteger)offset;
        memcpy(dst, data->data, data->len);
    }
}

// ── Device Queries ──────────────────────────────────────

static int64_t gorget_metal_device_max_buffer_length(int64_t device_h) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        return (int64_t)[device maxBufferLength];
    }
}

static int64_t gorget_metal_device_max_threads_per_threadgroup(int64_t device_h) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        MTLSize s = [device maxThreadsPerThreadgroup];
        // Return the product (total max threads)
        return (int64_t)(s.width * s.height * s.depth);
    }
}

static bool gorget_metal_device_has_unified_memory(int64_t device_h) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        if (@available(macOS 10.15, *)) {
            return [device hasUnifiedMemory];
        }
        return false;
    }
}

static int64_t gorget_metal_device_recommended_max_working_set_size(int64_t device_h) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
#if TARGET_OS_OSX
        return (int64_t)[device recommendedMaxWorkingSetSize];
#else
        (void)device;
        return 0; // Not available on iOS
#endif
    }
}

// ── CAMetalLayer Configuration ──────────────────────────

static void gorget_metal_layer_set_vsync(int64_t layer_h, int64_t enabled) {
    @autoreleasepool {
        CAMetalLayer* layer = (__bridge CAMetalLayer*)(void*)(intptr_t)layer_h;
        layer.displaySyncEnabled = (enabled != 0);
    }
}

static void gorget_metal_layer_set_maximum_drawable_count(int64_t layer_h, int64_t count) {
    @autoreleasepool {
        CAMetalLayer* layer = (__bridge CAMetalLayer*)(void*)(intptr_t)layer_h;
        layer.maximumDrawableCount = (NSUInteger)count;
    }
}

static void gorget_metal_layer_set_framebuffer_only(int64_t layer_h, int64_t flag) {
    @autoreleasepool {
        CAMetalLayer* layer = (__bridge CAMetalLayer*)(void*)(intptr_t)layer_h;
        layer.framebufferOnly = (flag != 0);
    }
}

// ══════════════════════════════════════════════════════════════
// Metal Tier 3 — Ray Tracing, Mesh Shaders, Binary Archives
// ══════════════════════════════════════════════════════════════

// ── Ray Tracing ─────────────────────────────────────────────

#if __has_include(<Metal/MTLAccelerationStructure.h>)

static int64_t gorget_metal_create_accel_struct_desc(int64_t type) {
    @autoreleasepool {
        if (type == 0) {
            MTLPrimitiveAccelerationStructureDescriptor* desc = [MTLPrimitiveAccelerationStructureDescriptor descriptor];
            return (int64_t)(intptr_t)(__bridge_retained void*)desc;
        } else {
            MTLInstanceAccelerationStructureDescriptor* desc = [MTLInstanceAccelerationStructureDescriptor descriptor];
            return (int64_t)(intptr_t)(__bridge_retained void*)desc;
        }
    }
}

static void gorget_metal_accel_desc_set_geometry(int64_t desc_h, int64_t geometry_h, int64_t count) {
    @autoreleasepool {
        MTLPrimitiveAccelerationStructureDescriptor* desc = (__bridge MTLPrimitiveAccelerationStructureDescriptor*)(void*)(intptr_t)desc_h;
        id geom = (__bridge id)(void*)(intptr_t)geometry_h;
        desc.geometryDescriptors = @[geom];
    }
}

static int64_t gorget_metal_create_triangle_geom_desc(int64_t vb_h, int64_t v_offset, int64_t v_stride, int64_t tri_count, int64_t ib_h, int64_t idx_type) {
    @autoreleasepool {
        MTLAccelerationStructureTriangleGeometryDescriptor* desc = [MTLAccelerationStructureTriangleGeometryDescriptor descriptor];
        desc.vertexBuffer = (__bridge id<MTLBuffer>)(void*)(intptr_t)vb_h;
        desc.vertexBufferOffset = (NSUInteger)v_offset;
        desc.vertexStride = (NSUInteger)v_stride;
        desc.triangleCount = (NSUInteger)tri_count;
        if (ib_h != 0) {
            desc.indexBuffer = (__bridge id<MTLBuffer>)(void*)(intptr_t)ib_h;
            desc.indexType = (MTLIndexType)idx_type;
        }
        return (int64_t)(intptr_t)(__bridge_retained void*)desc;
    }
}

static int64_t gorget_metal_accel_structure_sizes(int64_t device_h, int64_t desc_h) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        MTLAccelerationStructureDescriptor* desc = (__bridge MTLAccelerationStructureDescriptor*)(void*)(intptr_t)desc_h;
        MTLAccelerationStructureSizes sizes = [device accelerationStructureSizesWithDescriptor:desc];
        return (int64_t)sizes.accelerationStructureSize;
    }
}

static int64_t gorget_metal_create_accel_structure(int64_t device_h, int64_t size) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        MTLAccelerationStructureDescriptor* desc = [MTLPrimitiveAccelerationStructureDescriptor descriptor];
        id<MTLAccelerationStructure> accel = [device newAccelerationStructureWithSize:(NSUInteger)size];
        return (int64_t)(intptr_t)(__bridge_retained void*)accel;
    }
}

static void gorget_metal_build_accel_structure(int64_t encoder_h, int64_t desc_h, int64_t accel_h, int64_t scratch_h, int64_t scratch_offset) {
    @autoreleasepool {
        id<MTLAccelerationStructureCommandEncoder> encoder = (__bridge id<MTLAccelerationStructureCommandEncoder>)(void*)(intptr_t)encoder_h;
        MTLAccelerationStructureDescriptor* desc = (__bridge MTLAccelerationStructureDescriptor*)(void*)(intptr_t)desc_h;
        id<MTLAccelerationStructure> accel = (__bridge id<MTLAccelerationStructure>)(void*)(intptr_t)accel_h;
        id<MTLBuffer> scratch = (__bridge id<MTLBuffer>)(void*)(intptr_t)scratch_h;
        [encoder buildAccelerationStructure:accel descriptor:desc scratchBuffer:scratch scratchBufferOffset:(NSUInteger)scratch_offset];
    }
}

static void gorget_metal_refit_accel_structure(int64_t encoder_h, int64_t desc_h, int64_t source_h, int64_t dest_h, int64_t scratch_h, int64_t scratch_offset) {
    @autoreleasepool {
        id<MTLAccelerationStructureCommandEncoder> encoder = (__bridge id<MTLAccelerationStructureCommandEncoder>)(void*)(intptr_t)encoder_h;
        MTLAccelerationStructureDescriptor* desc = (__bridge MTLAccelerationStructureDescriptor*)(void*)(intptr_t)desc_h;
        id<MTLAccelerationStructure> source = (__bridge id<MTLAccelerationStructure>)(void*)(intptr_t)source_h;
        id<MTLAccelerationStructure> dest = (__bridge id<MTLAccelerationStructure>)(void*)(intptr_t)dest_h;
        id<MTLBuffer> scratch = (__bridge id<MTLBuffer>)(void*)(intptr_t)scratch_h;
        [encoder refitAccelerationStructure:source descriptor:desc destination:dest scratchBuffer:scratch scratchBufferOffset:(NSUInteger)scratch_offset];
    }
}

static int64_t gorget_metal_create_instance_accel_desc(int64_t instance_buffer_h, int64_t instance_count) {
    @autoreleasepool {
        MTLInstanceAccelerationStructureDescriptor* desc = [MTLInstanceAccelerationStructureDescriptor descriptor];
        desc.instanceDescriptorBuffer = (__bridge id<MTLBuffer>)(void*)(intptr_t)instance_buffer_h;
        desc.instanceCount = (NSUInteger)instance_count;
        return (int64_t)(intptr_t)(__bridge_retained void*)desc;
    }
}

static int64_t gorget_metal_create_intersection_fn_table(int64_t pipeline_h, int64_t count) {
    @autoreleasepool {
        id<MTLComputePipelineState> pipeline = (__bridge id<MTLComputePipelineState>)(void*)(intptr_t)pipeline_h;
        MTLIntersectionFunctionTableDescriptor* desc = [MTLIntersectionFunctionTableDescriptor intersectionFunctionTableDescriptor];
        desc.functionCount = (NSUInteger)count;
        id<MTLIntersectionFunctionTable> table = [pipeline newIntersectionFunctionTableWithDescriptor:desc];
        return (int64_t)(intptr_t)(__bridge_retained void*)table;
    }
}

static void gorget_metal_intersection_fn_table_set_fn(int64_t table_h, int64_t index, int64_t pipeline_h, const char* fn_name) {
    (void)table_h; (void)index; (void)pipeline_h; (void)fn_name;
    // Complex setup — requires visible function handle lookup
}

static int64_t gorget_metal_cmd_buf_accel_encoder(int64_t cmd_buf_h) {
    @autoreleasepool {
        id<MTLCommandBuffer> cmd_buf = (__bridge id<MTLCommandBuffer>)(void*)(intptr_t)cmd_buf_h;
        id<MTLAccelerationStructureCommandEncoder> encoder = [cmd_buf accelerationStructureCommandEncoder];
        return (int64_t)(intptr_t)(__bridge_retained void*)encoder;
    }
}

static void gorget_metal_accel_encoder_end(int64_t encoder_h) {
    @autoreleasepool {
        id<MTLAccelerationStructureCommandEncoder> encoder = (__bridge id<MTLAccelerationStructureCommandEncoder>)(void*)(intptr_t)encoder_h;
        [encoder endEncoding];
    }
}

static void gorget_metal_render_enc_set_accel_struct(int64_t encoder_h, int64_t index, int64_t accel_h) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> encoder = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)encoder_h;
        id<MTLAccelerationStructure> accel = (__bridge id<MTLAccelerationStructure>)(void*)(intptr_t)accel_h;
        [encoder setFragmentAccelerationStructure:accel atBufferIndex:(NSUInteger)index];
    }
}

static void gorget_metal_render_enc_set_intersection_fn_table(int64_t encoder_h, int64_t index, int64_t table_h) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> encoder = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)encoder_h;
        id<MTLIntersectionFunctionTable> table = (__bridge id<MTLIntersectionFunctionTable>)(void*)(intptr_t)table_h;
        [encoder setFragmentIntersectionFunctionTable:table atBufferIndex:(NSUInteger)index];
    }
}

static void gorget_metal_compute_enc_set_accel_struct(int64_t encoder_h, int64_t index, int64_t accel_h) {
    @autoreleasepool {
        id<MTLComputeCommandEncoder> encoder = (__bridge id<MTLComputeCommandEncoder>)(void*)(intptr_t)encoder_h;
        id<MTLAccelerationStructure> accel = (__bridge id<MTLAccelerationStructure>)(void*)(intptr_t)accel_h;
        [encoder setAccelerationStructure:accel atBufferIndex:(NSUInteger)index];
    }
}

#else
// Stubs for older SDKs
static int64_t gorget_metal_create_accel_struct_desc(int64_t t) { (void)t; return 0; }
static void gorget_metal_accel_desc_set_geometry(int64_t a, int64_t b, int64_t c) { (void)a;(void)b;(void)c; }
static int64_t gorget_metal_create_triangle_geom_desc(int64_t a, int64_t b, int64_t c, int64_t d, int64_t e, int64_t f) { (void)a;(void)b;(void)c;(void)d;(void)e;(void)f; return 0; }
static int64_t gorget_metal_accel_structure_sizes(int64_t a, int64_t b) { (void)a;(void)b; return 0; }
static int64_t gorget_metal_create_accel_structure(int64_t a, int64_t b) { (void)a;(void)b; return 0; }
static void gorget_metal_build_accel_structure(int64_t a, int64_t b, int64_t c, int64_t d, int64_t e) { (void)a;(void)b;(void)c;(void)d;(void)e; }
static void gorget_metal_refit_accel_structure(int64_t a, int64_t b, int64_t c, int64_t d, int64_t e, int64_t f) { (void)a;(void)b;(void)c;(void)d;(void)e;(void)f; }
static int64_t gorget_metal_create_instance_accel_desc(int64_t a, int64_t b) { (void)a;(void)b; return 0; }
static int64_t gorget_metal_create_intersection_fn_table(int64_t a, int64_t b) { (void)a;(void)b; return 0; }
static void gorget_metal_intersection_fn_table_set_fn(int64_t a, int64_t b, int64_t c, const char* d) { (void)a;(void)b;(void)c;(void)d; }
static int64_t gorget_metal_cmd_buf_accel_encoder(int64_t a) { (void)a; return 0; }
static void gorget_metal_accel_encoder_end(int64_t a) { (void)a; }
static void gorget_metal_render_enc_set_accel_struct(int64_t a, int64_t b, int64_t c) { (void)a;(void)b;(void)c; }
static void gorget_metal_render_enc_set_intersection_fn_table(int64_t a, int64_t b, int64_t c) { (void)a;(void)b;(void)c; }
static void gorget_metal_compute_enc_set_accel_struct(int64_t a, int64_t b, int64_t c) { (void)a;(void)b;(void)c; }
#endif

// ── Mesh Shaders (Metal 3) ─────────────────────────────────

#if __has_include(<Metal/MTLMeshRenderPipelineDescriptor.h>) || (__MAC_OS_X_VERSION_MAX_ALLOWED >= 130000)

static int64_t gorget_metal_create_mesh_pipeline(int64_t device_h, const char* obj_fn, const char* mesh_fn, const char* frag_fn, int64_t library_h, int64_t pixel_format) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        id<MTLLibrary> library = (__bridge id<MTLLibrary>)(void*)(intptr_t)library_h;
        MTLMeshRenderPipelineDescriptor* desc = [[MTLMeshRenderPipelineDescriptor alloc] init];
        desc.objectFunction = [library newFunctionWithName:[NSString stringWithUTF8String:obj_fn]];
        desc.meshFunction = [library newFunctionWithName:[NSString stringWithUTF8String:mesh_fn]];
        desc.fragmentFunction = [library newFunctionWithName:[NSString stringWithUTF8String:frag_fn]];
        desc.colorAttachments[0].pixelFormat = (MTLPixelFormat)pixel_format;
        NSError* error = nil;
        MTLRenderPipelineReflection* reflection = nil;
        id<MTLRenderPipelineState> pipeline = [device newRenderPipelineStateWithMeshDescriptor:desc options:0 reflection:&reflection error:&error];
        [desc release];
        if (error) { fprintf(stderr, "gorget: mesh pipeline error: %s\n", [[error localizedDescription] UTF8String]); return 0; }
        return (int64_t)(intptr_t)(__bridge_retained void*)pipeline;
    }
}

static void gorget_metal_render_enc_draw_mesh_tg(int64_t encoder_h, int64_t mx, int64_t my, int64_t mz, int64_t mtx, int64_t mty, int64_t mtz, int64_t otx, int64_t oty, int64_t otz) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> encoder = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)encoder_h;
        [encoder drawMeshThreadgroups:MTLSizeMake((NSUInteger)mx, (NSUInteger)my, (NSUInteger)mz)
              threadsPerObjectThreadgroup:MTLSizeMake((NSUInteger)otx, (NSUInteger)oty, (NSUInteger)otz)
                threadsPerMeshThreadgroup:MTLSizeMake((NSUInteger)mtx, (NSUInteger)mty, (NSUInteger)mtz)];
    }
}

#else
static int64_t gorget_metal_create_mesh_pipeline(int64_t a, const char* b, const char* c, const char* d, int64_t e, int64_t f) { (void)a;(void)b;(void)c;(void)d;(void)e;(void)f; return 0; }
static void gorget_metal_render_enc_draw_mesh_tg(int64_t a, int64_t b, int64_t c, int64_t d, int64_t e, int64_t f, int64_t g, int64_t h, int64_t i, int64_t j) { (void)a;(void)b;(void)c;(void)d;(void)e;(void)f;(void)g;(void)h;(void)i;(void)j; }
#endif

// ── Binary Archives (Metal 2.3+) ───────────────────────────

static int64_t gorget_metal_create_binary_archive(int64_t device_h, const char* url_str) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        MTLBinaryArchiveDescriptor* desc = [[MTLBinaryArchiveDescriptor alloc] init];
        if (url_str && strlen(url_str) > 0) {
            NSURL* url = [NSURL fileURLWithPath:[NSString stringWithUTF8String:url_str]];
            desc.url = url;
        }
        NSError* error = nil;
        id<MTLBinaryArchive> archive = [device newBinaryArchiveWithDescriptor:desc error:&error];
        [desc release];
        if (error) { fprintf(stderr, "gorget: binary archive error: %s\n", [[error localizedDescription] UTF8String]); return 0; }
        return (int64_t)(intptr_t)(__bridge_retained void*)archive;
    }
}

static int64_t gorget_metal_binary_archive_add_pipeline(int64_t archive_h, int64_t desc_h) {
    @autoreleasepool {
        id<MTLBinaryArchive> archive = (__bridge id<MTLBinaryArchive>)(void*)(intptr_t)archive_h;
        MTLRenderPipelineDescriptor* desc = (__bridge MTLRenderPipelineDescriptor*)(void*)(intptr_t)desc_h;
        NSError* error = nil;
        BOOL ok = [archive addRenderPipelineFunctionsWithDescriptor:desc error:&error];
        return ok ? 1 : 0;
    }
}

static int64_t gorget_metal_binary_archive_serialize(int64_t archive_h, const char* url_str) {
    @autoreleasepool {
        id<MTLBinaryArchive> archive = (__bridge id<MTLBinaryArchive>)(void*)(intptr_t)archive_h;
        NSURL* url = [NSURL fileURLWithPath:[NSString stringWithUTF8String:url_str]];
        NSError* error = nil;
        BOOL ok = [archive serializeToURL:url error:&error];
        return ok ? 1 : 0;
    }
}

// ── Visible Functions / Function Pointers (Metal 2.3+) ──────

static int64_t gorget_metal_create_visible_fn_table(int64_t pipeline_h, int64_t count) {
    @autoreleasepool {
        id<MTLComputePipelineState> pipeline = (__bridge id<MTLComputePipelineState>)(void*)(intptr_t)pipeline_h;
        MTLVisibleFunctionTableDescriptor* desc = [MTLVisibleFunctionTableDescriptor visibleFunctionTableDescriptor];
        desc.functionCount = (NSUInteger)count;
        id<MTLVisibleFunctionTable> table = [pipeline newVisibleFunctionTableWithDescriptor:desc];
        return (int64_t)(intptr_t)(__bridge_retained void*)table;
    }
}

static void gorget_metal_visible_fn_table_set_fn(int64_t table_h, int64_t index, int64_t pipeline_h, const char* fn_name) {
    @autoreleasepool {
        id<MTLVisibleFunctionTable> table = (__bridge id<MTLVisibleFunctionTable>)(void*)(intptr_t)table_h;
        id<MTLComputePipelineState> pipeline = (__bridge id<MTLComputePipelineState>)(void*)(intptr_t)pipeline_h;
        id<MTLFunctionHandle> handle = [pipeline functionHandleWithFunction:nil];
        // Note: full implementation needs library function lookup by name
        (void)fn_name; (void)handle;
        [table setFunction:handle atIndex:(NSUInteger)index];
    }
}

static void gorget_metal_render_enc_set_visible_fn_table(int64_t encoder_h, int64_t index, int64_t table_h) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> encoder = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)encoder_h;
        id<MTLVisibleFunctionTable> table = (__bridge id<MTLVisibleFunctionTable>)(void*)(intptr_t)table_h;
        [encoder setFragmentVisibleFunctionTable:table atBufferIndex:(NSUInteger)index];
    }
}

static void gorget_metal_compute_enc_set_visible_fn_table(int64_t encoder_h, int64_t index, int64_t table_h) {
    @autoreleasepool {
        id<MTLComputeCommandEncoder> encoder = (__bridge id<MTLComputeCommandEncoder>)(void*)(intptr_t)encoder_h;
        id<MTLVisibleFunctionTable> table = (__bridge id<MTLVisibleFunctionTable>)(void*)(intptr_t)table_h;
        [encoder setVisibleFunctionTable:table atBufferIndex:(NSUInteger)index];
    }
}

// ── Texture Views ──────────────────────────────────────────

static int64_t gorget_metal_texture_new_view(int64_t texture_h, int64_t pixel_format, int64_t texture_type, int64_t levels, int64_t num_levels, int64_t slices, int64_t num_slices) {
    @autoreleasepool {
        id<MTLTexture> texture = (__bridge id<MTLTexture>)(void*)(intptr_t)texture_h;
        id<MTLTexture> view = [texture newTextureViewWithPixelFormat:(MTLPixelFormat)pixel_format
                                                        textureType:(MTLTextureType)texture_type
                                                             levels:NSMakeRange((NSUInteger)levels, (NSUInteger)num_levels)
                                                             slices:NSMakeRange((NSUInteger)slices, (NSUInteger)num_slices)];
        return (int64_t)(intptr_t)(__bridge_retained void*)view;
    }
}

static void gorget_metal_make_aliasable(int64_t resource_h) {
    @autoreleasepool {
        id<MTLResource> resource = (__bridge id<MTLResource>)(void*)(intptr_t)resource_h;
        [resource makeAliasable];
    }
}

// ── Fence / Event Synchronization ──────────────────────────
// gorget_metal_create_fence — already defined above
// gorget_metal_create_event — already defined above
// gorget_metal_create_shared_event — already defined above

static void gorget_metal_render_enc_update_fence(int64_t encoder_h, int64_t fence_h, int64_t stages) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> encoder = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)encoder_h;
        id<MTLFence> fence = (__bridge id<MTLFence>)(void*)(intptr_t)fence_h;
        [encoder updateFence:fence afterStages:(MTLRenderStages)stages];
    }
}

static void gorget_metal_render_enc_wait_fence(int64_t encoder_h, int64_t fence_h, int64_t stages) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> encoder = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)encoder_h;
        id<MTLFence> fence = (__bridge id<MTLFence>)(void*)(intptr_t)fence_h;
        [encoder waitForFence:fence beforeStages:(MTLRenderStages)stages];
    }
}

static void gorget_metal_cmd_buf_signal_event(int64_t cmd_buf_h, int64_t event_h, int64_t value) {
    @autoreleasepool {
        id<MTLCommandBuffer> cmd_buf = (__bridge id<MTLCommandBuffer>)(void*)(intptr_t)cmd_buf_h;
        id<MTLEvent> event = (__bridge id<MTLEvent>)(void*)(intptr_t)event_h;
        [cmd_buf encodeSignalEvent:event value:(uint64_t)value];
    }
}

static void gorget_metal_cmd_buf_wait_event(int64_t cmd_buf_h, int64_t event_h, int64_t value) {
    @autoreleasepool {
        id<MTLCommandBuffer> cmd_buf = (__bridge id<MTLCommandBuffer>)(void*)(intptr_t)cmd_buf_h;
        id<MTLEvent> event = (__bridge id<MTLEvent>)(void*)(intptr_t)event_h;
        [cmd_buf encodeWaitForEvent:event value:(uint64_t)value];
    }
}

// ── Timestamps & Counters (Metal 2.3+) ─────────────────────

static int64_t gorget_metal_create_counter_sample_buffer(int64_t device_h, int64_t count) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        MTLCounterSampleBufferDescriptor* desc = [[MTLCounterSampleBufferDescriptor alloc] init];
        desc.sampleCount = (NSUInteger)count;
        desc.storageMode = MTLStorageModePrivate;
        NSError* error = nil;
        id<MTLCounterSampleBuffer> buffer = [device newCounterSampleBufferWithDescriptor:desc error:&error];
        [desc release];
        if (error) return 0;
        return (int64_t)(intptr_t)(__bridge_retained void*)buffer;
    }
}

static int64_t gorget_metal_device_sample_timestamps(int64_t device_h) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        MTLTimestamp cpu_ts = 0, gpu_ts = 0;
        [device sampleTimestamps:&cpu_ts gpuTimestamp:&gpu_ts];
        return (int64_t)gpu_ts;
    }
}

// ── Dynamic Libraries (Metal 2.3+) ─────────────────────────

static int64_t gorget_metal_create_dynamic_library(int64_t device_h, int64_t library_h) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        id<MTLLibrary> library = (__bridge id<MTLLibrary>)(void*)(intptr_t)library_h;
        NSError* error = nil;
        id<MTLDynamicLibrary> dyn = [device newDynamicLibrary:library error:&error];
        if (error) { fprintf(stderr, "gorget: dynamic library error: %s\n", [[error localizedDescription] UTF8String]); return 0; }
        return (int64_t)(intptr_t)(__bridge_retained void*)dyn;
    }
}

// ── Residency Sets (Metal 3+) ──────────────────────────────

static int64_t gorget_metal_create_residency_set(int64_t device_h) {
    (void)device_h;
    return 0; // Requires Metal 3 + macOS 15 / iOS 18
}

static void gorget_metal_residency_set_add(int64_t set_h, int64_t resource_h) {
    (void)set_h; (void)resource_h;
}

static void gorget_metal_residency_set_commit(int64_t set_h) {
    (void)set_h;
}

// ── Sampler with Compare Function (shadow mapping) ──────────

static int64_t gorget_metal_create_sampler_with_compare(int64_t device_h, int64_t min_filter, int64_t mag_filter, int64_t addr_s, int64_t addr_t, int64_t compare_fn) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        MTLSamplerDescriptor* desc = [[MTLSamplerDescriptor alloc] init];
        desc.minFilter = (MTLSamplerMinMagFilter)min_filter;
        desc.magFilter = (MTLSamplerMinMagFilter)mag_filter;
        desc.sAddressMode = (MTLSamplerAddressMode)addr_s;
        desc.tAddressMode = (MTLSamplerAddressMode)addr_t;
        desc.compareFunction = (MTLCompareFunction)compare_fn;
        id<MTLSamplerState> sampler = [device newSamplerStateWithDescriptor:desc];
        return (int64_t)(intptr_t)(__bridge_retained void*)sampler;
    }
}

// ── Debug Groups ────────────────────────────────────────────

static void gorget_metal_encoder_push_debug_group(int64_t encoder_h, const char* label) {
    @autoreleasepool {
        id<MTLCommandEncoder> enc = (__bridge id<MTLCommandEncoder>)(void*)(intptr_t)encoder_h;
        NSString* ns_label = [NSString stringWithUTF8String:label];
        [enc pushDebugGroup:ns_label];
    }
}
static void gorget_metal_encoder_pop_debug_group(int64_t encoder_h) {
    @autoreleasepool {
        id<MTLCommandEncoder> enc = (__bridge id<MTLCommandEncoder>)(void*)(intptr_t)encoder_h;
        [enc popDebugGroup];
    }
}
static void gorget_metal_encoder_insert_debug_signpost(int64_t encoder_h, const char* label) {
    @autoreleasepool {
        id<MTLCommandEncoder> enc = (__bridge id<MTLCommandEncoder>)(void*)(intptr_t)encoder_h;
        NSString* ns_label = [NSString stringWithUTF8String:label];
        [enc insertDebugSignpost:ns_label];
    }
}
static void gorget_metal_cmd_buf_push_debug_group(int64_t cmd_buf_h, const char* label) {
    @autoreleasepool {
        id<MTLCommandBuffer> buf = (__bridge id<MTLCommandBuffer>)(void*)(intptr_t)cmd_buf_h;
        NSString* ns_label = [NSString stringWithUTF8String:label];
        [buf pushDebugGroup:ns_label];
    }
}
static void gorget_metal_cmd_buf_pop_debug_group(int64_t cmd_buf_h) {
    @autoreleasepool {
        id<MTLCommandBuffer> buf = (__bridge id<MTLCommandBuffer>)(void*)(intptr_t)cmd_buf_h;
        [buf popDebugGroup];
    }
}

// ── MRT (Multiple Render Target) ────────────────────────────

static void gorget_metal_render_pass_set_color_attachment(int64_t desc_h, int64_t index, int64_t texture_h, int64_t load_action, int64_t store_action, double r, double g, double b, double a) {
    @autoreleasepool {
        MTLRenderPassDescriptor* desc = (__bridge MTLRenderPassDescriptor*)(void*)(intptr_t)desc_h;
        MTLRenderPassColorAttachmentDescriptor* ca = desc.colorAttachments[index];
        ca.texture = (__bridge id<MTLTexture>)(void*)(intptr_t)texture_h;
        ca.loadAction = (MTLLoadAction)load_action;
        ca.storeAction = (MTLStoreAction)store_action;
        ca.clearColor = MTLClearColorMake(r, g, b, a);
    }
}
static void gorget_metal_pipeline_set_color_attachment_format(int64_t desc_h, int64_t index, int64_t format) {
    @autoreleasepool {
        MTLRenderPipelineDescriptor* desc = (__bridge MTLRenderPipelineDescriptor*)(void*)(intptr_t)desc_h;
        desc.colorAttachments[index].pixelFormat = (MTLPixelFormat)format;
    }
}
static void gorget_metal_pipeline_set_color_attachment_blending(int64_t desc_h, int64_t index, int64_t enabled, int64_t src_rgb, int64_t dst_rgb, int64_t src_alpha, int64_t dst_alpha) {
    @autoreleasepool {
        MTLRenderPipelineDescriptor* desc = (__bridge MTLRenderPipelineDescriptor*)(void*)(intptr_t)desc_h;
        MTLRenderPipelineColorAttachmentDescriptor* ca = desc.colorAttachments[index];
        ca.blendingEnabled = (BOOL)enabled;
        ca.sourceRGBBlendFactor = (MTLBlendFactor)src_rgb;
        ca.destinationRGBBlendFactor = (MTLBlendFactor)dst_rgb;
        ca.sourceAlphaBlendFactor = (MTLBlendFactor)src_alpha;
        ca.destinationAlphaBlendFactor = (MTLBlendFactor)dst_alpha;
    }
}

// ── Device Feature Queries ──────────────────────────────────
// gorget_metal_device_supports_family — already defined above
// gorget_metal_device_name — already defined above

static int64_t gorget_metal_device_registry_id(int64_t device_h) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        return (int64_t)device.registryID;
    }
}
static int64_t gorget_metal_device_current_allocated_size(int64_t device_h) {
    @autoreleasepool {
        id<MTLDevice> device = (__bridge id<MTLDevice>)(void*)(intptr_t)device_h;
        return (int64_t)device.currentAllocatedSize;
    }
}

// ── Full Stencil Configuration ──────────────────────────────
// gorget_metal_create_depth_stencil_full — already defined above

static void gorget_metal_encoder_set_stencil_front_back_ref(int64_t encoder_h, int64_t front_ref, int64_t back_ref) {
    @autoreleasepool {
        id<MTLRenderCommandEncoder> enc = (__bridge id<MTLRenderCommandEncoder>)(void*)(intptr_t)encoder_h;
        [enc setStencilFrontReferenceValue:(uint32_t)front_ref backReferenceValue:(uint32_t)back_ref];
    }
}

#endif /* __APPLE__ */
