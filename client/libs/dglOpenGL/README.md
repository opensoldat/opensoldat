# Delphi / Pascal OpenGL header translation

- OpenGL version 4.6
- Copyright (C) DGL-OpenGL-Portteam -  All Rights Reserved

# Supported environments and targets
- (Win32) Delphi 4 and up
- (Win32, Win64) Delphi XE2
- (Win32, Win64, Linux, MacOSX) FreePascal (2.2.6 and up)

# Obtained through
- GitHub repository - https://github.com/SaschaWillems/dglOpenGL
- Delphi OpenGL Community(DGL) - www.delphigl.com

# Credits
- Converted and maintained by DGL's OpenGL-Portteam :
    - Sascha Willems             - http://www.saschawillems.de
    - Steffen Xonna (Lossy eX)   - http://www.dev-center.de
- Additional input :
    - Andrey Gruzdev (Mac OS X patch for XE2 / FPC)
    - Lars Middendorf
    - Martin Waldegger (Mars)
    - Benjamin Rosseaux (BeRo)
- Additional thanks:
    - sigsegv (libdl.so)

# License
> You may retrieve the latest version of this file at the Delphi OpenGL
> Community home page, located at http://www.delphigl.com/
>
>  This Source Code Form is subject to the terms of the Mozilla Public License,
>  v. 2.0. If a copy of the MPL was not distributed with this file,
>  You can obtain one at http://mozilla.org/MPL/2.0/.
>
> Software distributed under the License is distributed on an
> "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
> implied. See the License for the specific language governing
> rights and limitations under the License.
>
> Note : If you want to use our header for projects whose licensing is not compatible with the MPL, just ask for a changed license!

# Old History
**Refer to the commits for recent changes**

# Version 1.0    
- Initial Release

# Version 1.1    
- Added PPointer in Tpyessection for compatiblity with Delphi versions lower than 7                                    (SW)
Added a function named RaiseLastOSError including a comment
- on how to make it run under Delphi versions lower than 7 (SW)
- Added some data types according to the GL-Syntax         (SW)

# Version 1.2    
- Fixed some problems with getting the addresses of some
- Extensions (e.g. glTexImage3D) where the EXT/ARB did work
- but not the core-functions                               (SW)

# Version 1.3    
- A second call to ReadimplementationProperties won't
- revert to the default libs anymore                       (MW)
- Libraries now will be released if necessary              (MW)

# Version 1.3a   
- Small fixes for glSlang-functions                        (SW)

# Version 1.3b   
- Fixed a small bug with GL_ARB_shader_objects, that lead to that extension not loaded correctly              (SW)

# Version 1.3c   
- more GL 1.5 compliance by FOG_COORD_xx and
- ARB less VBO and occlusion query routines                (MW)

# Version 1.3d   
- Fixed linebreaks (should now be corrected under D5)      (SW)

# Version 1.4    
- Changed header to correspond to the OpenGL-Shading
- Language specification 1.10 :
- Added new GL_SAMPLER_*-Constants
- Added Constant GL_SHADING_LANGUAGE_VERSION_ARB
- Added Constant GL_FRAGMENT_SHADER_DERIVATIVE_HINT_ARB
- Added Constant GL_MAX_FRAGMENT_UNIFORM_COMPONENTS_ARB  (SW)

# Version 1.4a   
- Fixed a missing stdcall for glBindAttribLocationARB      (SW)

# Version 1.4b   
- Fixed declaration for glUniform*(f/i)vARB (added count)  (MW)
- glCompileShaderARB changed from function to procedure    (MW)

# Version 1.5    
- Added support for FreePascal                             (BR)
- Added type TGLVectorf3/TGLVector3f                       (SW)

# Version 1.6    
- Added Extension GL_EXT_framebuffer_object                (SX)

# Version 1.7    
- Added Extension GL_ARB_fragment_program_shadow           (SX)
- Added Extension GL_ARB_draw_buffers                      (SX)
- Added Extension GL_ARB_texture_rectangle                 (SX)
- Added Extension GL_ARB_color_buffer_float                (SX)
- Added Extension GL_ARB_half_float_pixel                  (SX)
- Added Extension GL_ARB_texture_float                     (SX)
- Added Extension GL_ARB_pixel_buffer_object               (SX)
- Added Extension GL_EXT_depth_bounds_test                 (SX)
- Added Extension GL_EXT_texture_mirror_clamp              (SX)
- Added Extension GL_EXT_blend_equation_separate           (SX)
- Added Extension GL_EXT_pixel_buffer_object               (SX)
- Added Extension GL_EXT_texture_compression_dxt1          (SX)
- Added Extension GL_NV_fragment_program_option            (SX)
- Added Extension GL_NV_fragment_program2                  (SX)
- Added Extension GL_NV_vertex_program2_option             (SX)
- Added Extension GL_NV_vertex_program3                    (SX)

# Version 1.8    
- Added explicit delegate type definitions                 (LM)
- Added .Net 1.1 Support                                   (LM)
- Added .Net overloaded functions                          (LM)
- Added delayed extension loading and stubs                (LM)
- Added automatic InitOpenGL call in CreateRenderingContext(LM)
- Added extra Read_* function                              (LM)

# Version 2.0    
- fixed some Problem with version string and damn drivers.
- String 1.15 identified as OpenGL 1.5 not as OpenGL 1.1   (SX)
- Removed unexisting extension GL_ARB_texture_mirror_repeat(SX)
- Added Extension WGL_ARB_pixel_format_float               (SX)
- Added Extension GL_EXT_stencil_clear_tag                 (SX)
- Added Extension GL_EXT_texture_rectangle                 (SX)
- Added Extension GL_EXT_texture_edge_clamp                (SX)
- Some 1.5 Core Consts added (now completed)               (SX)
- gluProject need pointer for not .net                     (SX)
- gluUnProject need pointer for not .net                   (SX)
- wglUseFontOutlines* need pointer for not .net            (SX)
- wglSwapMultipleBuffers need pointer for not .net         (SX)
- Bug with wglGetExtensionsStringEXT removed			   (SX)
- different type for .net                                  (SX)
- Added OpenGL 2.0 Core                                    (SX)

# Version 2.0.1  
- fixed some problems with glGetActiveAttrib in 2.0 Core   (SX)
- fixes some problems with gluProject                      (SX)
- fixes some problems with gluUnProject                    (SX)
- fixes some problems with gluTessVertex                   (SX)
- fixes some problems with gluLoadSamplingMatrices         (SX)

# Version 2.1    
- Removed .NET Support                                     (SX)
- Better support for Linux                                 (SX)
- Better Codeformation                                     (SX)
- Added some more Vector/Matrix types                      (SX)
- Added OpenGL 2.1 Core                                    (SX)
- Added Extension GL_EXT_packed_depth_stencil              (SX)
- Added Extension GL_EXT_texture_sRGB                      (SX)
- Added Extension GL_EXT_framebuffer_blit                  (SX)
- Added Extension GL_EXT_framebuffer_multisample           (SX)
- Added Extension GL_EXT_timer_query                       (SX)
- Added Extension GL_EXT_gpu_program_parameters            (SX)
- Added Extension GL_EXT_bindable_uniform                  (SX)
- Added Extension GL_EXT_draw_buffers2                     (SX)
- Added Extension GL_EXT_draw_instanced                    (SX)
- Added Extension GL_EXT_framebuffer_sRGB                  (SX)
- Added Extension GL_EXT_geometry_shader4                  (SX)
- Added Extension GL_EXT_gpu_shader4                       (SX)
- Added Extension GL_EXT_packed_float                      (SX)
- Added Extension GL_EXT_texture_array                     (SX)
- Added Extension GL_EXT_texture_buffer_object             (SX)
- Added Extension GL_EXT_texture_compression_latc          (SX)
- Added Extension GL_EXT_texture_compression_rgtc          (SX)
- Added Extension GL_EXT_texture_integer                   (SX)
- Added Extension GL_EXT_texture_shared_exponent           (SX)
- Added Extension GL_NV_depth_buffer_float                 (SX)
- Added Extension GL_NV_fragment_program4                  (SX)
- Added Extension GL_NV_framebuffer_multisample_coverage   (SX)
- Added Extension GL_NV_geometry_program4                  (SX)
- Added Extension GL_NV_gpu_program4                       (SX)
- Added Extension GL_NV_parameter_buffer_object            (SX)
- Added Extension GL_NV_transform_feedback                 (SX)
- Added Extension GL_NV_vertex_program4                    (SX)

# Version 3.0    
- fixed some const of GL_EXT_texture_shared_exponent       (SX)
- possible better support for mac                          (SX)
- Added OpenGL 3.0 Core                                    (SX)
- Added Extension GL_ARB_depth_buffer_float                (SX)
- Added Extension GL_ARB_draw_instanced                    (SX)
- Added Extension GL_ARB_framebuffer_object                (SX)
- Added Extension GL_ARB_framebuffer_sRGB                  (SX)
- Added Extension GL_ARB_geometry_shader4                  (SX)
- Added Extension GL_ARB_half_float_vertex                 (SX)
- Added Extension GL_ARB_instanced_arrays                  (SX)
- Added Extension GL_ARB_map_buffer_range                  (SX)
- Added Extension GL_ARB_texture_buffer_object             (SX)
- Added Extension GL_ARB_texture_compression_rgtc          (SX)
- Added Extension GL_ARB_texture_rg                        (SX)
- Added Extension GL_ARB_vertex_array_object               (SX)
- Added Extension GL_NV_conditional_render                 (SX)
- Added Extension GL_NV_present_video                      (SX)
- Added Extension GL_EXT_transform_feedback                (SX)
- Added Extension GL_EXT_direct_state_access               (SX)
- Added Extension GL_EXT_vertex_array_bgra                 (SX)
- Added Extension GL_EXT_texture_swizzle                   (SX)
- Added Extension GL_NV_explicit_multisample               (SX)
- Added Extension GL_NV_transform_feedback2                (SX)
- Added Extension WGL_ARB_create_context                   (SX)
- Added Extension WGL_NV_present_video                     (SX)
- Added Extension WGL_NV_video_out                         (SX)
- Added Extension WGL_NV_swap_group                        (SX)
- Added Extension WGL_NV_gpu_affinity                      (SX)
- Added define DGL_TINY_HEADER to suppress automatic function loading                                         (SX)
- glProcedure renamed to dglGetProcAddress and now it's
- visible from outside the unit to custom load functions   (SX)
- dglCheckExtension added to check if an extension exists  (SX)
- Read_GL_ARB_buffer_object renamed to Read_GL_ARB_vertex_buffer_object                         (SX)

# Version 3.0.1  
- fixed an problem with fpc                                (SX)

# Version 3.0.2  
- fixed an problem with WGL_ARB_create_context             (SX)

# Version 3.2    
- Functions from GL_VERSION_3_0 where updated              (SX)
- Functions from GL_ARB_map_buffer_range where updated     (SX)
- Functions from GL_NV_present_video where added           (SX)
- Added consts of GL_ARB_instanced_arrays                  (SX)
- Defines to identify Delphi was changed (prevent for feature maintenance)                                     (SX)
- Added Extension GL_ATI_meminfo                           (SX)
- Added Extension GL_AMD_performance_monitor               (SX)
- Added Extension GL_AMD_texture_texture4                  (SX)
- Added Extension GL_AMD_vertex_shader_tesselator          (SX)
- Added Extension GL_EXT_provoking_vertex                  (SX)
- Added Extension WGL_AMD_gpu_association                  (SX)
- Added OpenGL 3.1 Core                                    (SX)
- All deprecated stuff can be disabled if you undef the define DGL_DEPRECATED                                    (SX)
- Added Extension GL_ARB_uniform_buffer_object             (SX)
- Added Extension GL_ARB_compatibility                     (SX)
- Added Extension GL_ARB_copy_buffer                       (SX)
- Added Extension GL_ARB_shader_texture_lod                (SX)
- Remove function from GL_NV_present_video                 (SX)
- Added Extension WGL_3DL_stereo_control                   (SX)
- Added Extension GL_EXT_texture_snorm                     (SX)
- Added Extension GL_AMD_draw_buffers_blend                (SX)
- Added Extension GL_APPLE_texture_range                   (SX)
- Added Extension GL_APPLE_float_pixels                    (SX)
- Added Extension GL_APPLE_vertex_program_evaluators       (SX)
- Added Extension GL_APPLE_aux_depth_stencil               (SX)
- Added Extension GL_APPLE_object_purgeable                (SX)
- Added Extension GL_APPLE_row_bytes                       (SX)
- Added OpenGL 3.2 Core                                    (SX)
- Added Extension GL_ARB_depth_clamp                       (SX)
- Added Extension GL_ARB_draw_elements_base_vertex         (SX)
- Added Extension GL_ARB_fragment_coord_conventions        (SX)
- Added Extension GL_ARB_provoking_vertex                  (SX)
- Added Extension GL_ARB_seamless_cube_map                 (SX)
- Added Extension GL_ARB_sync                              (SX)
- Added Extension GL_ARB_texture_multisample               (SX)
- Added Extension GL_ARB_vertex_array_bgra                 (SX)
- Added Extension GL_ARB_draw_buffers_blend                (SX)
- Added Extension GL_ARB_sample_shading                    (SX)
- Added Extension GL_ARB_texture_cube_map_array            (SX)
- Added Extension GL_ARB_texture_gather                    (SX)
- Added Extension GL_ARB_texture_query_lod                 (SX)
- Added Extension WGL_ARB_create_context_profile           (SX)
- Added GLX Core up to Version 1.4                         (SX)
- Added Extension GLX_ARB_multisample                      (SX)
- Added Extension GLX_ARB_fbconfig_float                   (SX)
- Added Extension GLX_ARB_get_proc_address                 (SX)
- Added Extension GLX_ARB_create_context                   (SX)
- Added Extension GLX_ARB_create_context_profile           (SX)
- Added Extension GLX_EXT_visual_info                      (SX)
- Added Extension GLX_EXT_visual_rating                    (SX)
- Added Extension GLX_EXT_import_context                   (SX)
- Added Extension GLX_EXT_fbconfig_packed_float            (SX)
- Added Extension GLX_EXT_framebuffer_sRGB                 (SX)
- Added Extension GLX_EXT_texture_from_pixmap              (SX)

# Version 3.2.1  
- Fixed some problems with Delphi < 6                      (SX)

# Version 3.2.2  
- Added Extension GL_APPLE_rgb_422                         (SX)
- Added Extension GL_EXT_separate_shader_objects           (SX)
- Added Extension GL_NV_video_capture                      (SX)
- Added Extension GL_NV_copy_image                         (SX)
- Added Extension GL_NV_parameter_buffer_object2           (SX)
- Added Extension GL_NV_shader_buffer_load                 (SX)
- Added Extension GL_NV_vertex_buffer_unified_memory       (SX)
- Added Extension GL_NV_texture_barrier                    (SX)
- Variable GL_EXT_texture_snorm will be filled             (SX)
- Variable GL_APPLE_row_bytes will be filled               (SX)
- Added Extension WGL_NV_video_capture                     (SX)
- Added Extension WGL_NV_copy_image                        (SX)
- WGL_NV_video_out now named WGL_NV_video_output           (SX)
- Added Extension GLX_EXT_swap_control                     (SX)

# Version 3.2.3  
- Fixed an Problem with glGetAttribLocation                (SX)
- Added const GL_UNIFORM_BUFFER_EXT                        (SX)
- Functions of GL_NV_texture_barrier now will be loaded    (SX)

# Version 4.0    
- Changes on Extension GL_ARB_texture_gather               (SX)
- Changes on Extension GL_NV_shader_buffer_load            (SX)
- Added OpenGL 3.3 Core                                    (SX)
- Added OpenGL 4.0 Core                                    (SX)
- Added Extension GL_AMD_shader_stencil_export             (SX)
- Added Extension GL_AMD_seamless_cubemap_per_texture      (SX)
- Added Extension GL_ARB_shading_language_include          (SX)
- Added Extension GL_ARB_texture_compression_bptc          (SX)
- Added Extension GL_ARB_blend_func_extended               (SX)
- Added Extension GL_ARB_explicit_attrib_location          (SX)
- Added Extension GL_ARB_occlusion_query2                  (SX)
- Added Extension GL_ARB_sampler_objects                   (SX)
- Added Extension GL_ARB_shader_bit_encoding               (SX)
- Added Extension GL_ARB_texture_rgb10_a2ui                (SX)
- Added Extension GL_ARB_texture_swizzle                   (SX)
- Added Extension GL_ARB_timer_query                       (SX)
- Added Extension GL_ARB_vertex_type_2_10_10_10_rev        (SX)
- Added Extension GL_ARB_draw_indirect                     (SX)
- Added Extension GL_ARB_gpu_shader5                       (SX)
- Added Extension GL_ARB_gpu_shader_fp64                   (SX)
- Added Extension GL_ARB_shader_subroutine                 (SX)
- Added Extension GL_ARB_tessellation_shader               (SX)
- Added Extension GL_ARB_texture_buffer_object_rgb32       (SX)
- Added Extension GL_ARB_transform_feedback2               (SX)
- Added Extension GL_ARB_transform_feedback3               (SX)

# Version 4.1   
- Possible fix some strange linux behavior                 (SX)
- All function uses GL instead of TGL types                (SX)
- GL_AMD_vertex_shader_tesselator will be read now         (SX)
- GL_AMD_draw_buffers_blend will be read now               (SX)
- Changes on glStencilFuncSeparate (GL_2_0)                (SX)
- Changes on GL_VERSION_3_2                                (SX)
- Changes on GL_VERSION_3_3                                (SX)
- Changes on GL_VERSION_4_0                                (SX)
- Changes on GL_ARB_sample_shading                         (SX)
- Changes on GL_ARB_texture_cube_map_array                 (SX)
- Changes on GL_ARB_gpu_shader5                            (SX)
- Changes on GL_ARB_transform_feedback3                    (SX)
- Changes on GL_ARB_sampler_objects                        (SX)
- Changes on GL_ARB_gpu_shader_fp64                        (SX)
- Changes on GL_APPLE_element_array                        (SX)
- Changes on GL_APPLE_vertex_array_range                   (SX)
- Changes on GL_NV_transform_feedback                      (SX)
- Changes on GL_NV_vertex_buffer_unified_memory            (SX)
- Changes on GL_EXT_multi_draw_arrays                      (SX)
- Changes on GL_EXT_direct_state_access                    (SX)
- Changes on GL_AMD_performance_monitor                    (SX)
- Changes on GL_AMD_seamless_cubemap_per_texture           (SX)
- Changes on GL_EXT_geometry_shader4                       (SX)
- Added OpenGL 4.1 Core                                    (SX)
- Added Extension GL_ARB_ES2_compatibility                 (SX)
- Added Extension GL_ARB_get_program_binary                (SX)
- Added Extension GL_ARB_separate_shader_objects           (SX)
- Added Extension GL_ARB_shader_precision                  (SX)
- Added Extension GL_ARB_vertex_attrib_64bit               (SX)
- Added Extension GL_ARB_viewport_array                    (SX)
- Added Extension GL_ARB_cl_event                          (SX)
- Added Extension GL_ARB_debug_output                      (SX)
- Added Extension GL_ARB_robustness                        (SX)
- Added Extension GL_ARB_shader_stencil_export             (SX)
- Added Extension GL_AMD_conservative_depth                (SX)
- Added Extension GL_EXT_shader_image_load_store           (SX)
- Added Extension GL_EXT_vertex_attrib_64bit               (SX)
- Added Extension GL_NV_gpu_program5                       (SX)
- Added Extension GL_NV_gpu_shader5                        (SX)
- Added Extension GL_NV_shader_buffer_store                (SX)
- Added Extension GL_NV_tessellation_program5              (SX)
- Added Extension GL_NV_vertex_attrib_integer_64bit        (SX)
- Added Extension GL_NV_multisample_coverage               (SX)
- Added Extension GL_AMD_name_gen_delete                   (SX)
- Added Extension GL_AMD_debug_output                      (SX)
- Added Extension GL_NV_vdpau_interop                      (SX)
- Added Extension GL_AMD_transform_feedback3_lines_triangles (SX)
- Added Extension GL_AMD_depth_clamp_separate              (SX)
- Added Extension GL_EXT_texture_sRGB_decode               (SX)
- Added Extension WGL_ARB_framebuffer_sRGB                 (SX)
- Added Extension WGL_ARB_create_context_robustness        (SX)
- Added Extension WGL_EXT_create_context_es2_profile       (SX)
- Added Extension WGL_NV_multisample_coverage              (SX)
- Added Extension GLX_ARB_vertex_buffer_object             (SX)
- Added Extension GLX_ARB_framebuffer_sRGB                 (SX)
- Added Extension GLX_ARB_create_context_robustness        (SX)
- Added Extension GLX_EXT_create_context_es2_profile       (SX)

# Version 4.1a   
- Fix for dglGetProcAddress with FPC and linux (def param) (SW)

# Version 4.2    
- Added OpenGL 4.2 Core                                    (SW)
- Added Extension GL_ARB_base_instance                     (SW)
- Added Extension GL_ARB_shading_language_420pack          (SW)
- Added Extension GL_ARB_transform_feedback_instanced      (SW)
- Added Extension GL_ARB_compressed_texture_pixel_storage  (SW)
- Added Extension GL_ARB_conservative_depth                (SW)
- Added Extension GL_ARB_internalformat_query              (SW)
- Added Extension GL_ARB_map_buffer_alignment              (SW)
- Added Extension GL_ARB_shader_atomic_counters            (SW)
- Added Extension GL_ARB_shader_image_load_store           (SW)
- Added Extension GL_ARB_shading_language_packing          (SW)
- Added Extension GL_ARB_texture_storage                   (SW)
- Added Extension WGL_NV_DX_interop                        (SW)
- Added Define for WGL_EXT_create_context_es2_profile      (SW)

# Version 4.2a   
- Added Mac OS X patch by Andrey Gruzdev                   (SW)

# Version 4.3    
- Added OpenGL 4.3 Core                                    (SW)
- Added GL_ARB_arrays_of_arrays		                     (SW)
- Added GL_ARB_fragment_layer_viewport                     (SW)
- Added GL_ARB_shader_image_size                           (SW)
- Added GL_ARB_ES3_compatibility                           (SW)
- Added GL_ARB_clear_buffer_object                         (SW)
- Added GL_ARB_compute_shader                              (SW)
- Added GL_ARB_copy_image                                  (SW)
- Added GL_KHR_debug                                       (SW)
- Added GL_ARB_explicit_uniform_location,                  (SW)
- Added GL_ARB_framebuffer_no_attachments                  (SW)
- Added GL_ARB_internalformat_query2                       (SW)
- Added GL_ARB_invalidate_subdata                          (SW)
- Added GL_ARB_multi_draw_indirect                         (SW)
- Added GL_ARB_program_interface_query                     (SW)
- Added GL_ARB_robust_buffer_access_behavior               (SW)
- Added GL_ARB_shader_storage_buffer_object                (SW)
- Added GL_ARB_stencil_texturing                           (SW)
- Added GL_ARB_texture_buffer_range                        (SW)
- Added GL_ARB_texture_query_levels                        (SW)
- Added GL_ARB_texture_storage_multisample                 (SW)
- Added GL_ARB_texture_view                                (SW)
- Added GL_ARB_vertex_attrib_binding                       (SW)
- Added new vendor-specific extensions                     (SW)
- Added GL_NV_path_rendering                               (SW)
- Added GL_AMD_pinned_memory                               (SW)
- Added GL_AMD_stencil_operation_extended                  (SW)
- Added GL_AMD_vertex_shader_viewport_index                (SW)
- Added GL_AMD_vertex_shader_layer                         (SW)
- Added GL_NV_bindless_texture                             (SW)
- Added GL_NV_shader_atomic_float                          (SW)
- Added GL_AMD_query_buffer_object                         (SW)
- Added CreateRenderingContextVersion                      (SW)

# Version 4.4    
- Added OpenGL 4.4 Core                                    (SW)
- Added ARB_buffer_storage                                 (SW)
- Added ARB_clear_texture extension                        (SW)
- Added ARB_enhanced_layouts extension                     (SW)
- Added ARB_multi_bind extension                           (SW)
- Added ARB_query_buffer_object extension                  (SW)
- Added ARB_texture_mirror_clamp_to_edge extension         (SW)
- Added ARB_texture_stencil8 extension                     (SW)
- Added ARB_vertex_type_10f_11f_11f_rev extension          (SW)
- Added MAX_VERTEX_ATTRIB_STRIDE stat                      (SW)
- Added missing functions for GL_EXT_direct_state_access   (SW)
- GL3.0+ uses non-deprecated way of getting extensions
- (thanks to frenK)                                        (SW)
- Added missing cdecl for TglXGetVisualFromFBConfig        (SW)

# Version 4.5    
- Added OpenGL 4.5 Core                                    (SW)
- Added GL_ARB_ES3_1_compatibility                         (SW)
- Added GL_ARB_clip_control                                (SW)
- Added GL_ARB_conditional_render_inverted                 (SW)
- Added GL_ARB_cull_distance                               (SW)
- Added GL_ARB_derivative_control                          (SW)
- Added GL_ARB_direct_state_access                         (SW)
- Added GL_ARB_get_texture_sub_image                       (SW)
- Added GL_ARB_shader_texture_image_samples                (SW)
- Added GL_ARB_texture_barrier                             (SW)
- Added GL_KHR_blend_equation_advanced                     (SW)
- Added GL_KHR_blend_equation_advanced_coherent            (SW)
- Added GL_KHR_context_flush_control                       (SW)
- Added GL_KHR_robustness                                  (SW)
- Added GL_KHR_robust_buffer_access_behavior               (SW)
- Added GLX_ARB_context_flush_control                      (SW)
- Added WGL_ARB_context_flush_control                      (SW)

# Version 4.5a
- Changed declarations of GL_TRUE/GL_FALSE for compatbility with ByteBool (SW)
- Changed declarations of GLU_TRUE/GLU_FALSE for compatbility with ByteBool (SW)
- Added Exception mask settings for 64-Bit (SetExceptionMask) (SW)
- Changed 64-Bit detection for FPC
- Added TVector3f for backwards compatibility

# Version 4.5b
- Added missing constant GL_PRIMITIVE_RESTART_FOR_PATCHES_SUPPORTED (SW)
- Added missing constant GL_TEXTURE_BUFFER_BINDING (SW)
- Added missing extension GL_NV_conservative_raster (SW)
- Added recently announced OpenGL extensions :
  - Added GL_ARB_ES3_2_compatibility (SW)
  - Added GL_ARB_fragment_shader_interlock (SW)
  - Added GL_ARB_gpu_shader_int64 (SW)
  - Added GL_ARB_parallel_shader_compile (SW)
  - Added GL_ARB_post_depth_coverage (SW)
  - Added GL_ARB_sample_locations (SW)
  - Added GL_ARB_shader_atomic_counter_ops (SW)
  - Added GL_ARB_shader_ballot (SW)
  - Added GL_ARB_shader_clock (SW)
  - Added GL_ARB_shader_viewport_layer_array (SW)
  - Added GL_ARB_sparse_texture2 (SW)
  - Added GL_ARB_sparse_texture_clamp (SW)
  - Added GL_KHR_no_error (SW)
  - Added GL_NV_conservative_raster_dilate (SW)
  - Added GL_OVR_multiview (SW)
  - Added GL_OVR_multiview2 (SW)
  - Added GL_INTEL_framebuffer_CMAA (SW)
