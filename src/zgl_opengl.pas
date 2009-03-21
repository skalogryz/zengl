{
 * Copyright © Kemka Andrey aka Andru
 * mail: dr.andru@gmail.com
 * site: http://andru-kun.ru
 *
 * This file is part of ZenGL
 *
 * ZenGL is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * ZenGL is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
}
unit zgl_opengl;

{$I zgl_config.cfg}

interface
uses
  {$IFDEF LINUX}
  X, XLib, XUtil,
  {$ENDIF}
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  {$IFDEF DARWIN}
  MacOSAll,
  {$ENDIF}
  zgl_opengl_all
  ;

function  gl_Create : Boolean;
procedure gl_Destroy;
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
  ogl_MTexture   : array[ 0..8 ] of DWORD;

  ogl_Mode : WORD = 3; // 2D/3D Modes

  ogl_X      : WORD;
  ogl_Y      : WORD;
  ogl_Width  : WORD;
  ogl_Height : WORD;
  ogl_CropX  : WORD;
  ogl_CropY  : WORD;
  ogl_CropW  : WORD;
  ogl_CropH  : WORD;

  ogl_Extensions    : String;
  ogl_3DAccelerator : Boolean;
  ogl_CanVSync      : Boolean;
  ogl_CanCompress   : Boolean;
  ogl_CanARB        : Boolean; // ARBvp/ARBfp шейдеры
  ogl_CanGLSL       : Boolean; // GLSL шейдеры
  ogl_CanVBO        : Boolean;
  ogl_CanFBO        : Boolean;
  ogl_CanPBuffer    : Boolean;
  ogl_MaxLights     : Integer;
  ogl_MaxTexSize    : Integer;
  ogl_MaxAnisotropy : Integer;
  ogl_MaxTexLevels  : Integer;

  {$IFDEF LINUX}
  ogl_Context    : GLXContext;
  ogl_VisualInfo : PXVisualInfo;
  ogl_Attr       : array[ 0..31 ] of DWORD;
  {$ENDIF}
  {$IFDEF WIN32}
  ogl_Context : HGLRC;
  ogl_fAttr   : array[ 0..1  ] of Single = ( 0, 0 );
  ogl_iAttr   : array[ 0..31 ] of Integer;
  ogl_Format  : Integer;
  ogl_Formats : DWORD;
  {$ENDIF}
  {$IFDEF DARWIN}
  ogl_Device   : GDHandle;
  ogl_Context  : TAGLContext;
  ogl_Format   : TAGLPixelFormat;
  ogl_Attr     : array[ 0..31 ] of DWORD;
  {$ENDIF}

implementation
uses
  zgl_application,
  zgl_screen,
  zgl_window,
  zgl_log,
  zgl_utils;

function gl_Create;
  {$IFDEF WIN32}
  var
    PixelFormat     : Integer;
    PixelFormatDesc : TPixelFormatDescriptor;

    ga, gf : LongWord;
    i      : LongWord;
  {$ENDIF}
  {$IFDEF DARWIN}
  var
    i : Integer;
  {$ENDIF}
begin
  Result := FALSE;

  if not InitGL Then
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
{$IFDEF WIN32}
  if ogl_Context <> 0 Then
    wglDeleteContext( ogl_Context );

  FillChar( PixelFormatDesc, SizeOf( TPixelFormatDescriptor ), 0 );

  if ogl_Format = 0 Then
    begin
      with PixelFormatDesc do
        begin
          nSize        := SizeOf( TPIXELFORMATDESCRIPTOR );
          nVersion     := 1;
          dwFlags      := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
          iPixelType   := PFD_TYPE_RGBA;
          cColorBits   := scr_BPP;
          cAlphaBits   := 8;
          cDepthBits   := 24;
          cStencilBits := 0;
          iLayerType   := PFD_MAIN_PLANE;
        end;
      PixelFormat := ChoosePixelFormat( wnd_DC, @PixelFormatDesc );
    end else
      PixelFormat := ogl_Format;

  if not SetPixelFormat( wnd_DC, PixelFormat, @PixelFormatDesc ) Then
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
  log_Add( 'Create OpenGL Context' );

  if not wglMakeCurrent( wnd_DC, ogl_Context ) Then
    begin
      u_Error( 'Cannot set current OpenGL context' );
      exit;
    end;
  log_Add( 'Make Current OpenGL Context' );

  gf := PixelFormatDesc.dwFlags and PFD_GENERIC_FORMAT;
  ga := PixelFormatDesc.dwFlags and PFD_GENERIC_ACCELERATED;

  ogl_3DAccelerator := gf and ( not ga ) = 0;
  if not ogl_3DAccelerator Then
    u_Warning( 'Cannot find 3D-accelerator! Application run in software-mode, it''s very slow' );

  if ogl_Format = 0 Then
  {$IFDEF USE_WINEHACK}
    wglChoosePixelFormatARB := gl_GetProc( 'wglChoosePixelFormatARB' );
  {$ELSE}
    wglChoosePixelFormatARB := gl_GetProc( 'wglChoosePixelFormat' );
  {$ENDIF}
  if ( ogl_Format = 0 ) and ( Assigned( wglChoosePixelFormatARB ) ) and ( not app_InitToHandle ) Then
    begin
      ogl_zDepth := 24;

      repeat
        ogl_iAttr[ 0 ] := WGL_ACCELERATION_ARB;
        ogl_iAttr[ 1 ] := WGL_FULL_ACCELERATION_ARB;
        ogl_iAttr[ 2 ] := WGL_DRAW_TO_WINDOW_ARB;
        ogl_iAttr[ 3 ] := GL_TRUE;
        ogl_iAttr[ 4 ] := WGL_SUPPORT_OPENGL_ARB;
        ogl_iAttr[ 5 ] := GL_TRUE;
        ogl_iAttr[ 6 ] := WGL_DOUBLE_BUFFER_ARB;
        ogl_iAttr[ 7 ] := GL_TRUE;
        ogl_iAttr[ 8 ] := WGL_DEPTH_BITS_ARB;
        ogl_iAttr[ 9 ] := ogl_zDepth;
        i := 10;
        if ogl_Stencil > 0 Then
          begin
            ogl_iAttr[ i     ] := WGL_STENCIL_BITS_ARB;
            ogl_iAttr[ i + 1 ] := ogl_Stencil;
            INC( i, 2 );
          end;
        ogl_iAttr[ i     ] := WGL_COLOR_BITS_ARB;
        ogl_iAttr[ i + 1 ] := scr_BPP;
        ogl_iAttr[ i + 2 ] := WGL_ALPHA_BITS_ARB;
        ogl_iAttr[ i + 3 ] := 8;
        INC( i, 4 );
        if ogl_FSAA > 0 Then
          begin
            ogl_iAttr[ i     ] := WGL_SAMPLE_BUFFERS_ARB;
            ogl_iAttr[ i + 1 ] := GL_TRUE;
            ogl_iAttr[ i + 2 ] := WGL_SAMPLES_ARB;
            ogl_iAttr[ i + 3 ] := ogl_FSAA;
            INC( i, 4 );
          end;
        ogl_iAttr[ i     ] := 0;
        ogl_iAttr[ i + 1 ] := 0;

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

      if ogl_Format <> 0 Then
        begin
          gl_Destroy;
          wnd_Destroy;
          wnd_Create( wnd_Width, wnd_Height );
          Result := gl_Create;
          exit;
        end;
    end;

  if PixelFormat = 0 Then
    begin
      u_Error( 'Cannot choose pixel format' );
      exit;
    end;
{$ENDIF}
{$IFDEF DARWIN}
  if not InitAGL Then
    begin
      log_Add( 'Cannot load AGL library' );
      exit;
    end;

  ogl_zDepth := 24;
  repeat
    ogl_Attr[ 0 ]  := AGL_RGBA;
    ogl_Attr[ 1 ]  := AGL_RED_SIZE;
    ogl_Attr[ 2 ]  := 1;
    ogl_Attr[ 3 ]  := AGL_GREEN_SIZE;
    ogl_Attr[ 4 ]  := 1;
    ogl_Attr[ 5 ]  := AGL_BLUE_SIZE;
    ogl_Attr[ 6 ]  := 1;
    ogl_Attr[ 7 ]  := AGL_ALPHA_SIZE;
    ogl_Attr[ 8 ]  := 1;
    ogl_Attr[ 9 ]  := AGL_DOUBLEBUFFER;
    ogl_Attr[ 10 ] := AGL_DEPTH_SIZE;
    ogl_Attr[ 11 ] := ogl_zDepth;
    ogl_Attr[ 12 ] := AGL_FULLSCREEN;
    i := 13;
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
        ogl_Attr[ i     ] := AGL_SAMPLES_ARB;
        ogl_Attr[ i + 1 ] := ogl_FSAA;
        INC( i, 2 );
      end;
    ogl_Attr[ i ] := AGL_NONE;

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
  if wnd_FullScreen Then
    begin
      if aglSetFullScreen( ogl_Context, scr_Width, scr_Height, scr_Refresh, 0 ) = GL_FALSE Then
        begin
          u_Error( 'Cannot set FullScreen' );
          exit;
        end;
    end else
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

  ogl_3DAccelerator := glGetString( GL_RENDERER ) <> 'Apple Software Renderer';
  if not ogl_3DAccelerator Then
    u_Warning( 'Cannot find 3D-accelerator! Application run in software-mode, it''s very slow' );
{$ENDIF}
  log_Add( 'GL_VERSION: ' + glGetString( GL_VERSION ) );
  log_Add( 'GL_RENDERER: ' + glGetString( GL_RENDERER ) );

  gl_LoadEx;

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

  glDisable( GL_BLEND );
  glDisable( GL_ALPHA_TEST );
  glDisable( GL_DEPTH_TEST );
  glDisable( GL_TEXTURE_2D );
  glEnable ( GL_NORMALIZE );

  Result := TRUE;
end;

procedure gl_Destroy;
begin
{$IFDEF LINUX}
  if not glXMakeCurrent( scr_Display, None, nil ) Then
    u_Error( 'Cannot release current OpenGL context');

  glXDestroyContext( scr_Display, ogl_Context );
  glXWaitGL;
{$ENDIF}
{$IFDEF WIN32}
  if not wglMakeCurrent( wnd_DC, 0 ) Then
    u_Error( 'Cannot release current OpenGL context' );

  wglDeleteContext( ogl_Context );
{$ENDIF}
{$IFDEF DARWIN}
  if aglSetCurrentContext( nil ) = GL_FALSE Then
    u_Error( 'Cannot release current OpenGL context' );

  aglDestroyContext( ogl_Context );
  FreeAGL;
{$ENDIF}

  FreeGL;
end;

procedure gl_LoadEx;
begin
  if ogl_LoadEx Then
    exit
  else
    ogl_LoadEx := TRUE;

  ogl_Extensions := glGetString( GL_EXTENSIONS );

  // Texture size
  glGetIntegerv( GL_MAX_TEXTURE_SIZE, @ogl_MaxTexSize );
  log_Add( 'GL_MAX_TEXTURE_SIZE: ' + u_IntToStr( ogl_MaxTexSize ) );

  ogl_CanCompress := gl_IsSupported( 'GL_ARB_texture_compression', ogl_Extensions );
  log_Add( 'GL_ARB_TEXTURE_COMPRESSION: ' + u_BoolToStr( ogl_CanCompress ) );

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

  {glGetIntegerv( GL_MAX_LIGHTS, @ogl_MaxLights );
  log_Add( 'GL_MAX_LIGHTS: ' + u_IntToStr( ogl_MaxLights ) );
  glLightModeli( GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE );
  glLightModeli( GL_LIGHT_MODEL_LOCAL_VIEWER, GL_FALSE );}

  {for i := 0 to ogl_MaxLights do
    begin
      glLightfv( GL_LIGHT0 + i, GL_AMBIENT,  @matAMBIENT );
      glLightfv( GL_LIGHT0 + i, GL_DIFFUSE,  @matDIFFUSE );
      glLightfv( GL_LIGHT0 + i, GL_SPECULAR, @matSPECULAR );
      glLightfv( GL_LIGHT0 + i, GL_EMISSION, @matEMISSION );
      glLightf ( GL_LIGHT0 + i, GL_SHININESS, matSHININESS );
    end;}

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
  glBindRenderbufferEXT := gl_GetProc( 'glBindRenderbufferEXT' );
  if Assigned( glBindRenderbufferEXT ) Then
    begin
      ogl_CanFBO                   := TRUE;
      glIsRenderbufferEXT          := gl_GetProc( 'glIsRenderbufferEXT'          );
      glDeleteRenderbuffersEXT     := gl_GetProc( 'glDeleteRenderbuffersEXT'     );
      glGenRenderbuffersEXT        := gl_GetProc( 'glGenRenderbuffersEXT'        );
      glRenderbufferStorageEXT     := gl_GetProc( 'glRenderbufferStorageEXT'     );
      glIsFramebufferEXT           := gl_GetProc( 'glIsFramebufferEXT'           );
      glBindFramebufferEXT         := gl_GetProc( 'glBindFramebufferEXT'         );
      glDeleteFramebuffersEXT      := gl_GetProc( 'glDeleteFramebuffersEXT'      );
      glGenFramebuffersEXT         := gl_GetProc( 'glGenFramebuffersEXT'         );
      glCheckFramebufferStatusEXT  := gl_GetProc( 'glCheckFramebufferStatusEXT'  );
      glFramebufferTexture2DEXT    := gl_GetProc( 'glFramebufferTexture2DEXT'    );
      glFramebufferRenderbufferEXT := gl_GetProc( 'glFramebufferRenderbufferEXT' );
    end else
      ogl_CanFBO := FALSE;
   log_Add( 'GL_EXT_FRAMEBUFFER_OBJECT: ' + u_BoolToStr( ogl_CanFBO ) );

  // PBUFFER
{$IFDEF WIN32}
  wglCreatePbufferARB := gl_GetProc( 'wglCreatePbuffer' );
  if Assigned( wglCreatePbufferARB ) and Assigned( wglChoosePixelFormatARB ) Then
    begin
      ogl_CanPBuffer         := TRUE;
      wglGetPbufferDCARB     := gl_GetProc( 'wglGetPbufferDC'     );
      wglReleasePbufferDCARB := gl_GetProc( 'wglReleasePbufferDC' );
      wglDestroyPbufferARB   := gl_GetProc( 'wglDestroyPbuffer'   );
    end else
      ogl_CanPBuffer := FALSE;
  log_Add( 'WGL_ARB_PBUFFER: ' + u_BoolToStr( ogl_CanPBuffer ) );
{$ENDIF}

  {glActiveStencilFaceEXT := wglGetProcAddress( 'glActiveStencilFaceEXT' );}

  // WaitVSync
{$IFDEF LINUX}
  glXGetVideoSyncSGI  := gl_GetProc( 'glXGetVideoSyncSGI' );
  if Assigned( glXGetVideoSyncSGI ) Then
    begin
      ogl_CanVSync        := TRUE;
      glXWaitVideoSyncSGI := gl_GetProc( 'glXWaitVideoSyncSGI' );
    end else
      ogl_CanVSync := FALSE;
{$ENDIF}
{$IFDEF WIN32}
  wglGetSwapIntervalEXT := gl_GetProc( 'wglGetSwapIntervalEXT' );
  if Assigned( wglGetSwapIntervalEXT ) Then
    begin
      ogl_CanVSync := TRUE;
      wglSwapIntervalEXT := gl_GetProc( 'wglSwapIntervalEXT' );
    end else
      ogl_CanVSync := FALSE;
{$ENDIF}
{$IFDEF DARWIN}
  if aglSetInt( ogl_Context, AGL_SWAP_INTERVAL, 1 ) = GL_TRUE Then
    ogl_CanVSync := TRUE
  else
    ogl_CanVSync := FALSE;
  aglSetInt( ogl_Context, AGL_SWAP_INTERVAL, Byte( scr_VSync ) );
{$ENDIF}
  log_Add( 'Support WaitVSync: ' + u_BoolToStr( ogl_CanVSync ) );
end;

end.