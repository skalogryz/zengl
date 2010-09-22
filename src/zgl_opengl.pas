{
 *  Copyright © Kemka Andrey aka Andru
 *  mail: dr.andru@gmail.com
 *  site: http://andru-kun.inf.ua
 *
 *  This file is part of ZenGL.
 *
 *  ZenGL is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as
 *  published by the Free Software Foundation, either version 3 of
 *  the License, or (at your option) any later version.
 *
 *  ZenGL is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with ZenGL. If not, see http://www.gnu.org/licenses/
}
unit zgl_opengl;

{$I zgl_config.cfg}

interface
uses
  {$IFDEF LINUX}
  X, XUtil,
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF DARWIN}
  MacOSAll,
  {$ENDIF}
  zgl_opengl_all;

const
  TARGET_SCREEN  = 1;
  TARGET_TEXTURE = 2;

function  gl_Create : Boolean;
procedure gl_Destroy;
procedure gl_ResetState;
procedure gl_LoadEx;

var
  ogl_LoadEx     : Boolean;
  ogl_zDepth     : Byte;
  ogl_Stencil    : Byte;
  ogl_FSAA       : Byte;
  ogl_Anisotropy : Byte;
  ogl_FOVY       : Single = 45;
  ogl_zNear      : Single = 0.1;
  ogl_zFar       : Single = 100;
  ogl_MTexActive : array[ 0..8 ] of Boolean;
  ogl_MTexture   : array[ 0..8 ] of LongWord;

  ogl_Mode   : Integer = 3; // 2D/3D Modes
  ogl_Target : Integer = TARGET_SCREEN;

  ogl_Width  : Integer;
  ogl_Height : Integer;
  ogl_ClipX  : Integer;
  ogl_ClipY  : Integer;
  ogl_ClipW  : Integer;
  ogl_ClipH  : Integer;
  ogl_ClipR  : Integer;

  ogl_Extensions    : AnsiString;
  ogl_3DAccelerator : Boolean;
  ogl_CanVSync      : Boolean;
  ogl_CanCompressA  : Boolean;
  ogl_CanCompressE  : Boolean;
  ogl_CanARB        : Boolean; // ARBvp/ARBfp шейдеры
  ogl_CanGLSL       : Boolean; // GLSL шейдеры
  ogl_CanVBO        : Boolean;
  ogl_CanFBO        : Boolean;
  ogl_CanPBuffer    : Boolean;
  ogl_MaxLights     : Integer;
  ogl_MaxTexSize    : Integer;
  ogl_MaxAnisotropy : Integer;
  ogl_MaxTexLevels  : Integer;
  ogl_Separate      : Boolean;

  {$IFDEF LINUX}
  oglx_Extensions : AnsiString;
  ogl_PBufferMode : Integer;
  ogl_Context     : GLXContext;
  ogl_VisualInfo  : PXVisualInfo;
  ogl_Attr        : array[ 0..31 ] of Integer;
  {$ENDIF}
  {$IFDEF WINDOWS}
  ogl_Context : HGLRC;
  ogl_fAttr   : array[ 0..1  ] of Single = ( 0, 0 );
  ogl_iAttr   : array[ 0..31 ] of Integer;
  ogl_Format  : Integer;
  ogl_Formats : LongWord;
  {$ENDIF}
  {$IFDEF DARWIN}
  ogl_Device   : GDHandle;
  ogl_Context  : TAGLContext;
  ogl_Format   : TAGLPixelFormat;
  ogl_Attr     : array[ 0..31 ] of LongWord;
  {$ENDIF}

implementation
uses
  zgl_application,
  zgl_screen,
  zgl_window,
  zgl_log,
  zgl_utils;

function gl_Create;
  {$IFDEF WINDOWS}
  var
    i               : Integer;
    pixelFormat     : Integer;
    pixelFormatDesc : TPixelFormatDescriptor;
  {$ENDIF}
  {$IFDEF DARWIN}
  var
    i : Integer;
  {$ENDIF}
begin
  Result := FALSE;

  if not InitGL() Then
    begin
      log_Add( 'Cannot load GL library' );
      exit;
    end;

{$IFDEF LINUX}
  ogl_Context := glXCreateContext( scr_Display, ogl_VisualInfo, 0, TRUE );
  if not Assigned( ogl_Context ) Then
    begin
      ogl_Context := glXCreateContext( scr_Display, ogl_VisualInfo, 0, FALSE );
      if not Assigned( ogl_Context ) Then
        begin
          u_Error( 'Cannot create OpenGL context' );
          exit;
        end;
    end;

  if not glXMakeCurrent( scr_Display, wnd_Handle, ogl_Context ) Then
    begin
      u_Error( 'Cannot set current OpenGL context' );
      exit;
    end;
{$ENDIF}
{$IFDEF WINDOWS}
  if ogl_Context <> 0 Then
    wglDeleteContext( ogl_Context );

  if ogl_Format = 0 Then
    begin
      FillChar( pixelFormatDesc, SizeOf( TPixelFormatDescriptor ), 0 );
      with pixelFormatDesc do
        begin
          nSize        := SizeOf( TPIXELFORMATDESCRIPTOR );
          nVersion     := 1;
          dwFlags      := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
          iPixelType   := PFD_TYPE_RGBA;
          cColorBits   := 32;
          cDepthBits   := 24;
          cStencilBits := ogl_Stencil;
          iLayerType   := PFD_MAIN_PLANE;
        end;
      pixelFormat := ChoosePixelFormat( wnd_DC, @pixelFormatDesc );
    end else
      pixelFormat := ogl_Format;

  if not SetPixelFormat( wnd_DC, pixelFormat, @pixelFormatDesc ) Then
    begin
      u_Error( 'Cannot set pixel format' );
      exit;
    end;

  ogl_Context := wglCreateContext( wnd_DC );
  if ( ogl_Context = 0 ) Then
    begin
      u_Error( 'Cannot create OpenGL context' );
      exit;
    end;
  if not wnd_First Then log_Add( 'Create OpenGL Context' );

  if not wglMakeCurrent( wnd_DC, ogl_Context ) Then
    begin
      u_Error( 'Cannot set current OpenGL context' );
      exit;
    end;
  if not wnd_First Then log_Add( 'Make Current OpenGL Context' );

  if ogl_Format = 0 Then
    wglChoosePixelFormatARB := gl_GetProc( 'wglChoosePixelFormatARB' );
  if ( not Assigned( wglChoosePixelFormatARB ) ) and ( ogl_Format = 0 ) Then
    begin
      wnd_First := FALSE;
      ogl_Format := pixelFormat;
      gl_Destroy();
      wnd_Destroy();
      wnd_Create( wnd_Width, wnd_Height );
      Result := gl_Create();
      exit;
    end;
  if ( ogl_Format = 0 ) and ( Assigned( wglChoosePixelFormatARB ) ) and ( not app_InitToHandle ) Then
    begin
      ogl_zDepth := 24;

      repeat
        FillChar( ogl_iAttr[ 0 ], length( ogl_iAttr ) * 4, 0 );
        ogl_iAttr[ 0  ] := WGL_ACCELERATION_ARB;
        ogl_iAttr[ 1  ] := WGL_FULL_ACCELERATION_ARB;
        ogl_iAttr[ 2  ] := WGL_DRAW_TO_WINDOW_ARB;
        ogl_iAttr[ 3  ] := GL_TRUE;
        ogl_iAttr[ 4  ] := WGL_SUPPORT_OPENGL_ARB;
        ogl_iAttr[ 5  ] := GL_TRUE;
        ogl_iAttr[ 6  ] := WGL_DOUBLE_BUFFER_ARB;
        ogl_iAttr[ 7  ] := GL_TRUE;
        ogl_iAttr[ 8  ] := WGL_PIXEL_TYPE_ARB;
        ogl_iAttr[ 9  ] := WGL_TYPE_RGBA_ARB;
        ogl_iAttr[ 10 ] := WGL_COLOR_BITS_ARB;
        ogl_iAttr[ 11 ] := 24;
        ogl_iAttr[ 12 ] := WGL_RED_BITS_ARB;
        ogl_iAttr[ 13 ] := 8;
        ogl_iAttr[ 14 ] := WGL_GREEN_BITS_ARB;
        ogl_iAttr[ 15 ] := 8;
        ogl_iAttr[ 16 ] := WGL_BLUE_BITS_ARB;
        ogl_iAttr[ 17 ] := 8;
        ogl_iAttr[ 18 ] := WGL_ALPHA_BITS_ARB;
        ogl_iAttr[ 19 ] := 8;
        ogl_iAttr[ 20 ] := WGL_DEPTH_BITS_ARB;
        ogl_iAttr[ 21 ] := ogl_zDepth;
        i := 22;
        if ogl_Stencil > 0 Then
          begin
            ogl_iAttr[ i     ] := WGL_STENCIL_BITS_ARB;
            ogl_iAttr[ i + 1 ] := ogl_Stencil;
            INC( i, 2 );
          end;
        if ogl_FSAA > 0 Then
          begin
            ogl_iAttr[ i     ] := WGL_SAMPLE_BUFFERS_ARB;
            ogl_iAttr[ i + 1 ] := GL_TRUE;
            ogl_iAttr[ i + 2 ] := WGL_SAMPLES_ARB;
            ogl_iAttr[ i + 3 ] := ogl_FSAA;
          end;

        log_Add( 'wglChoosePixelFormatARB: zDepth = ' + u_IntToStr( ogl_zDepth ) + '; ' + 'stencil = ' + u_IntToStr( ogl_Stencil ) + '; ' + 'fsaa = ' + u_IntToStr( ogl_FSAA )  );
        wglChoosePixelFormatARB( wnd_DC, @ogl_iAttr, @ogl_fAttr, 1, @ogl_Format, @ogl_Formats );
        if ( ogl_Format = 0 ) and ( ogl_zDepth < 16 ) Then
          begin
            if ogl_FSAA <= 0 Then
              break
            else
              begin
                ogl_zDepth := 24;
                DEC( ogl_FSAA, 2 );
              end;
          end else
            DEC( ogl_zDepth, 8 );
      until ogl_Format <> 0;

      if ogl_Format = 0 Then
        begin
          ogl_zDepth := 24;
          log_Add( 'ChoosePixelFormat: zDepth = ' + u_IntToStr( ogl_zDepth ) + '; ' + 'stencil = ' + u_IntToStr( ogl_Stencil )  );
          ogl_Format := PixelFormat;
        end;
      wnd_First := FALSE;
      gl_Destroy();
      wnd_Destroy();
      wnd_Create( wnd_Width, wnd_Height );
      Result := gl_Create();
      exit;
    end;

  if pixelFormat = 0 Then
    begin
      u_Error( 'Cannot choose pixel format' );
      exit;
    end;
{$ENDIF}
{$IFDEF DARWIN}
  if not InitAGL() Then
    begin
      log_Add( 'Cannot load AGL library' );
      exit;
    end;

  ogl_zDepth := 24;
  repeat
    FillChar( ogl_Attr[ 0 ], length( ogl_Attr ) * 4, AGL_NONE );
    ogl_Attr[ 0  ] := AGL_RGBA;
    ogl_Attr[ 1  ] := AGL_RED_SIZE;
    ogl_Attr[ 2  ] := 8;
    ogl_Attr[ 3  ] := AGL_GREEN_SIZE;
    ogl_Attr[ 4  ] := 8;
    ogl_Attr[ 5  ] := AGL_BLUE_SIZE;
    ogl_Attr[ 6  ] := 8;
    ogl_Attr[ 7  ] := AGL_ALPHA_SIZE;
    ogl_Attr[ 8  ] := 8;
    ogl_Attr[ 9  ] := AGL_DOUBLEBUFFER;
    ogl_Attr[ 10 ] := AGL_DEPTH_SIZE;
    ogl_Attr[ 11 ] := ogl_zDepth;
    i := 12;
    if ogl_Stencil > 0 Then
      begin
        ogl_Attr[ i     ] := AGL_STENCIL_SIZE;
        ogl_Attr[ i + 1 ] := ogl_Stencil;
        INC( i, 2 );
      end;
    if ogl_FSAA > 0 Then
      begin
        ogl_Attr[ i     ] := AGL_SAMPLE_BUFFERS_ARB;
        ogl_Attr[ i + 1 ] := 1;
        ogl_Attr[ i + 2 ] := AGL_SAMPLES_ARB;
        ogl_Attr[ i + 3 ] := ogl_FSAA;
        INC( i, 4 );
      end;

    log_Add( 'aglChoosePixelFormat: zDepth = ' + u_IntToStr( ogl_zDepth ) + '; ' + 'stencil = ' + u_IntToStr( ogl_Stencil ) + '; ' + 'fsaa = ' + u_IntToStr( ogl_FSAA ) );
    DMGetGDeviceByDisplayID( DisplayIDType( scr_Display ), ogl_Device, FALSE );
    ogl_Format := aglChoosePixelFormat( @ogl_Device, 1, @ogl_Attr[ 0 ] );
    if ( not Assigned( ogl_Format ) and ( ogl_zDepth = 1 ) ) Then
      begin
        if ogl_FSAA = 0 Then
          break
        else
          begin
            ogl_zDepth := 24;
            DEC( ogl_FSAA, 2 );
          end;
      end else
        if not Assigned( ogl_Format ) Then DEC( ogl_zDepth, 8 );
  if ogl_zDepth = 0 Then ogl_zDepth := 1;
  until Assigned( ogl_Format );

  if not Assigned( ogl_Format ) Then
    begin
      u_Error( 'Cannot choose pixel format.' );
      exit;
    end;

  ogl_Context := aglCreateContext( ogl_Format, nil );
  if not Assigned( ogl_Context ) Then
    begin
      u_Error( 'Cannot create OpenGL context' );
      exit;
    end;
  if aglSetDrawable( ogl_Context, GetWindowPort( wnd_Handle ) ) = GL_FALSE Then
    begin
      u_Error( 'Cannot set Drawable' );
      exit;
    end;
  if aglSetCurrentContext( ogl_Context ) = GL_FALSE Then
    begin
      u_Error( 'Cannot set current OpenGL context' );
      exit;
    end;
  aglDestroyPixelFormat( ogl_Format );
{$ENDIF}

{$IFDEF LINUX}
  ogl_3DAccelerator := glGetString( GL_RENDERER ) <> 'Software Rasterizer';
{$ENDIF}
{$IFDEF WINDOWS}
  ogl_3DAccelerator := glGetString( GL_RENDERER ) <> 'GDI Generic';
{$ENDIF}
{$IFDEF DARWIN}
  ogl_3DAccelerator := glGetString( GL_RENDERER ) <> 'Apple Software Renderer';
{$ENDIF}
  if not ogl_3DAccelerator Then
    u_Warning( 'Cannot find 3D-accelerator! Application run in software-mode, it''s very slow' );

  log_Add( 'GL_VERSION: ' + glGetString( GL_VERSION ) );
  log_Add( 'GL_RENDERER: ' + glGetString( GL_RENDERER ) );

  gl_LoadEx();
  gl_ResetState();

  Result := TRUE;
end;

procedure gl_ResetState;
begin
  glHint( GL_LINE_SMOOTH_HINT,            GL_NICEST );
  glHint( GL_POLYGON_SMOOTH_HINT,         GL_NICEST );
  glHint( GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST );
  glHint( GL_FOG_HINT,                    GL_DONT_CARE );
  glHint( GL_SHADE_MODEL,                 GL_NICEST );
  glShadeModel( GL_SMOOTH );

  glClearColor( 0, 0, 0, 0 );

  glDepthFunc ( GL_LEQUAL );
  glClearDepth( 1.0 );

  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
  glAlphaFunc( GL_GREATER, 0 );

  if ogl_Separate Then
    begin
      glBlendEquationEXT( GL_FUNC_ADD_EXT );
      glBlendFuncSeparateEXT( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA, GL_ONE, GL_ONE_MINUS_SRC_ALPHA );
      glBlendEquationSeparateEXT( GL_FUNC_ADD_EXT, GL_FUNC_ADD_EXT );
    end;

  glDisable( GL_BLEND );
  glDisable( GL_ALPHA_TEST );
  glDisable( GL_DEPTH_TEST );
  glDisable( GL_TEXTURE_2D );
  glEnable ( GL_NORMALIZE );
end;

procedure gl_Destroy;
begin
{$IFDEF LINUX}
  if not glXMakeCurrent( scr_Display, None, nil ) Then
    u_Error( 'Cannot release current OpenGL context');

  glXDestroyContext( scr_Display, ogl_Context );
{$ENDIF}
{$IFDEF WINDOWS}
  if not wglMakeCurrent( wnd_DC, 0 ) Then
    u_Error( 'Cannot release current OpenGL context' );

  wglDeleteContext( ogl_Context );
{$ENDIF}
{$IFDEF DARWIN}
  if aglSetCurrentContext( nil ) = GL_FALSE Then
    u_Error( 'Cannot release current OpenGL context' );

  aglDestroyContext( ogl_Context );
  FreeAGL();
{$ENDIF}

  FreeGL();
end;

procedure gl_LoadEx;
  {$IFDEF LINUX}
  var
    i, j : Integer;
  {$ENDIF}
begin
  if ogl_LoadEx Then
    exit
  else
    ogl_LoadEx := TRUE;

  ogl_Extensions := glGetString( GL_EXTENSIONS );

  // Texture size
  glGetIntegerv( GL_MAX_TEXTURE_SIZE, @ogl_MaxTexSize );
  log_Add( 'GL_MAX_TEXTURE_SIZE: ' + u_IntToStr( ogl_MaxTexSize ) );

  ogl_CanCompressA := gl_IsSupported( 'GL_ARB_texture_compression', ogl_Extensions );
  log_Add( 'GL_ARB_TEXTURE_COMPRESSION: ' + u_BoolToStr( ogl_CanCompressA ) );
  ogl_CanCompressE := gl_IsSupported( 'GL_EXT_texture_compression_s3tc', ogl_Extensions );
  log_Add( 'GL_EXT_TEXTURE_COMPRESSION_S3TC: ' + u_BoolToStr( ogl_CanCompressE ) );

  gl_Vertex2f  := @glVertex2f;
  gl_Vertex2fv := @glVertex2fv;

  // Multitexturing
  gl_TexCoord2f  := @glTexCoord2f;
  gl_TexCoord2fv := @glTexCoord2fv;
  glGetIntegerv( GL_MAX_TEXTURE_UNITS_ARB, @ogl_MaxTexLevels );
  log_Add( 'GL_MAX_TEXTURE_UNITS_ARB: ' + u_IntToStr( ogl_MaxTexLevels ) );
  glMultiTexCoord2fARB := gl_GetProc( 'glMultiTexCoord2f' );
  if Assigned( glMultiTexCoord2fARB ) Then
    begin
      glMultiTexCoord2fvARB    := gl_GetProc( 'glMultiTexCoord2fv'    );
      glActiveTextureARB       := gl_GetProc( 'glActiveTexture'       );
      glClientActiveTextureARB := gl_GetProc( 'glClientActiveTexture' );
    end else
      begin
        // Это конечно извращенство, но лень потом проверять везде "ogl_MaxTexLevels > 0" :)
        glActiveTextureARB       := @glEnable;
        glClientActiveTextureARB := @glEnable;
      end;

  // Anisotropy
  glGetIntegerv( GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, @ogl_MaxAnisotropy );
  ogl_Anisotropy := ogl_MaxAnisotropy;
  log_Add( 'GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT: ' + u_IntToStr( ogl_MaxAnisotropy ) );

  glBlendFuncSeparateEXT     := gl_GetProc( 'glBlendFuncSeparate' );
  glBlendEquationEXT         := gl_GetProc( 'glBlendEquation' );
  glBlendEquationSeparateEXT := gl_GetProc( 'glBlendEquationSeparate' );
  ogl_Separate := Assigned( glBlendFuncSeparateEXT ) and Assigned( glBlendEquationEXT ) and Assigned( glBlendEquationSeparateEXT ) and
                  gl_IsSupported( 'GL_EXT_blend_func_separate', ogl_Extensions ) and
                  gl_IsSupported( 'GL_EXT_blend_equation_separate', ogl_Extensions );
  log_Add( 'GL_EXT_BLEND_FUNC_SEPARATE: ' + u_BoolToStr( ogl_Separate ) );

  // VBO
  glBindBufferARB := gl_GetProc( 'glBindBuffer' );
  if Assigned( glBindBufferARB ) Then
    begin
      ogl_CanVBO                := TRUE;
      glDeleteBuffersARB        := gl_GetProc( 'glDeleteBuffers'        );
      glGenBuffersARB           := gl_GetProc( 'glGenBuffers'           );
      glIsBufferARB             := gl_GetProc( 'glIsBuffer'             );
      glBufferDataARB           := gl_GetProc( 'glBufferData'           );
      glBufferSubDataARB        := gl_GetProc( 'glBufferSubData'        );
      glMapBufferARB            := gl_GetProc( 'glMapBuffer'            );
      glUnmapBufferARB          := gl_GetProc( 'glUnmapBuffer'          );
      glGetBufferParameterivARB := gl_GetProc( 'glGetBufferParameteriv' );
    end else
      ogl_CanVBO := FALSE;
  log_Add( 'GL_ARB_VERTEX_BUFFER_OBJECT: ' + u_BoolToStr( ogl_CanVBO ) );

  // FBO
  glBindRenderbufferEXT := gl_GetProc( 'glBindRenderbuffer' );
  if Assigned( glBindRenderbufferEXT ) Then
    begin
      ogl_CanFBO                   := TRUE;
      glIsRenderbufferEXT          := gl_GetProc( 'glIsRenderbuffer'          );
      glDeleteRenderbuffersEXT     := gl_GetProc( 'glDeleteRenderbuffers'     );
      glGenRenderbuffersEXT        := gl_GetProc( 'glGenRenderbuffers'        );
      glRenderbufferStorageEXT     := gl_GetProc( 'glRenderbufferStorage'     );
      glIsFramebufferEXT           := gl_GetProc( 'glIsFramebuffer'           );
      glBindFramebufferEXT         := gl_GetProc( 'glBindFramebuffer'         );
      glDeleteFramebuffersEXT      := gl_GetProc( 'glDeleteFramebuffers'      );
      glGenFramebuffersEXT         := gl_GetProc( 'glGenFramebuffers'         );
      glCheckFramebufferStatusEXT  := gl_GetProc( 'glCheckFramebufferStatus'  );
      glFramebufferTexture2DEXT    := gl_GetProc( 'glFramebufferTexture2D'    );
      glFramebufferRenderbufferEXT := gl_GetProc( 'glFramebufferRenderbuffer' );
    end else
      ogl_CanFBO := FALSE;
   log_Add( 'GL_EXT_FRAMEBUFFER_OBJECT: ' + u_BoolToStr( ogl_CanFBO ) );

  // PBUFFER
{$IFDEF LINUX}
  oglx_Extensions := glXQueryServerString( scr_Display, scr_Default, GLX_EXTENSIONS );
  glXQueryVersion( scr_Display, i, j );
  if ( i * 10 + j >= 13 ) Then
    ogl_PBufferMode := 1
  else
    if gl_IsSupported( 'GLX_SGIX_fbconfig', oglx_Extensions ) and gl_IsSupported( 'GLX_SGIX_pbuffer', oglx_Extensions ) Then
        ogl_PBufferMode := 2
    else
      ogl_PBufferMode := 0;
  ogl_CanPBuffer := ogl_PbufferMode <> 0;
  if ogl_PBufferMode = 2 Then
    log_Add( 'GLX_SGIX_PBUFFER: TRUE' )
  else
    log_Add( 'GLX_PBUFFER: ' + u_BoolToStr( ogl_CanPBuffer ) );

  case ogl_PBufferMode of
    1:
      begin
        glXGetVisualFromFBConfig := gl_GetProc( 'glXGetVisualFromFBConfig' );
        glXChooseFBConfig        := gl_GetProc( 'glXChooseFBConfig' );
        glXCreatePbuffer         := gl_GetProc( 'glXCreatePbuffer' );
        glXDestroyPbuffer        := gl_GetProc( 'glXDestroyPbuffer' );
      end;
    2:
      begin
        glXGetVisualFromFBConfig := gl_GetProc( 'glXGetVisualFromFBConfigSGIX' );
        glXChooseFBConfig        := gl_GetProc( 'glXChooseFBConfigSGIX' );
        glXCreateGLXPbufferSGIX  := gl_GetProc( 'glXCreateGLXPbufferSGIX' );
        glXDestroyGLXPbufferSGIX := gl_GetProc( 'glXDestroyGLXPbufferSGIX' );
      end;
  end;
{$ENDIF}
{$IFDEF WINDOWS}
  wglCreatePbufferARB := gl_GetProc( 'wglCreatePbuffer' );
  if Assigned( wglCreatePbufferARB ) and Assigned( wglChoosePixelFormatARB ) Then
    begin
      ogl_CanPBuffer         := TRUE;
      wglGetPbufferDCARB     := gl_GetProc( 'wglGetPbufferDC'     );
      wglReleasePbufferDCARB := gl_GetProc( 'wglReleasePbufferDC' );
      wglDestroyPbufferARB   := gl_GetProc( 'wglDestroyPbuffer'   );
    end else
      ogl_CanPBuffer := FALSE;
  log_Add( 'WGL_PBUFFER: ' + u_BoolToStr( ogl_CanPBuffer ) );
{$ENDIF}
{$IFDEF DARWIN}
  ogl_CanPBuffer := Assigned( aglCreatePBuffer );
  log_Add( 'AGL_PBUFFER: ' + u_BoolToStr( ogl_CanPBuffer ) );
{$ENDIF}

  // WaitVSync
{$IFDEF LINUX}
  glXSwapIntervalSGI := gl_GetProc( 'glXSwapIntervalSGI' );
  ogl_CanVSync       := Assigned( glXSwapIntervalSGI );
{$ENDIF}
{$IFDEF WINDOWS}
  wglSwapIntervalEXT := gl_GetProc( 'wglSwapInterval' );
  ogl_CanVSync       := Assigned( wglSwapIntervalEXT );
{$ENDIF}
{$IFDEF DARWIN}
  if aglSetInt( ogl_Context, AGL_SWAP_INTERVAL, 1 ) = GL_TRUE Then
    ogl_CanVSync := TRUE
  else
    ogl_CanVSync := FALSE;
  aglSetInt( ogl_Context, AGL_SWAP_INTERVAL, Byte( scr_VSync ) );
{$ENDIF}
  if ogl_CanVSync Then
    scr_SetVSync( scr_VSync );
  log_Add( 'Support WaitVSync: ' + u_BoolToStr( ogl_CanVSync ) );
end;

end.
