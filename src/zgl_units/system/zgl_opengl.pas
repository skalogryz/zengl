{
 * Copyright © Kemka Andrey aka Andru
 * mail: dr.andru@gmail.com
 * site: http://andru.2x4.ru
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
}
unit zgl_opengl;

{$I define.inc}

interface
uses
  GL, GLExt,
  {$IFDEF LINUX}
  GLX, X,
  {$ENDIF}
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}

  zgl_const,
  zgl_global_var,
  zgl_log,

  Utils;

function  gl_Create : Boolean;
procedure gl_Destroy;
procedure gl_LoadEx;

procedure Set2DMode; extdecl;
procedure Set3DMode( FOVY : Single ); extdecl;
procedure SetCurrentMode;

procedure gl_MTexCoord2f( U, V : Single ); extdecl;
procedure gl_MTexCoord2fv( Coord : Pointer ); extdecl;

const
{$IFDEF LINUX}
  libGLU = 'libGLU.so.1';
{$ENDIF}
{$IFDEF WIN32}
  libGLU = 'glu32.dll';
{$ENDIF}
procedure gluPerspective(fovy, aspect, zNear, zFar: GLdouble); extdecl; external libGLU;
function gluBuild2DMipmaps(target: GLenum; components, width, height: GLint; format, atype: GLenum; const data: Pointer): Integer; extdecl; external libGLU;

var
  LoadEx : Boolean;
  
  gl_TexCoord2f  : procedure( U, V : Single ); extdecl;
  gl_TexCoord2fv : procedure( Coord : Pointer ); extdecl;
  gl_Vertex2f    : procedure( X, Y : Single ); extdecl;
  gl_Vertex2fv   : procedure( const v : PSingle ); extdecl;

implementation
uses
  zgl_window;

function gl_Create;
  {$IFDEF WIN32}
  var
    PixelFormat     : Integer;
    PixelFormatDesc : TPixelFormatDescriptor;
    
    ga, gf : DWORD;
    i, j : DWORD;
  {$ENDIF}
begin
  Result := FALSE;
  
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
    wglChoosePixelFormatARB := wglGetProcAddress( 'wglChoosePixelFormatARB' );
  if ( ogl_Format = 0 ) and ( Assigned( wglChoosePixelFormatARB ) ) Then
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
          Result := gl_Create();
          exit;
        end;
    end;

  if PixelFormat = 0 Then
    begin
      u_Error( 'Cannot choose pixel format' );
      exit;
    end;
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
end;

procedure gl_LoadEx;
begin
  if LoadEx Then
    exit
  else
    LoadEx := TRUE;
    
  // Texture size
  glGetIntegerv( GL_MAX_TEXTURE_SIZE, @ogl_MaxTexSize );
  log_Add( 'GL_MAX_TEXTURE_SIZE: ' + u_IntToStr( ogl_MaxTexSize ) );

  ogl_CanCompress := glext_ExtensionSupported( 'GL_ARB_texture_compression', glGetString( GL_EXTENSIONS ) );
  log_Add( 'GL_ARB_TEXTURE_COMPRESSION: ' + u_BoolToStr( ogl_CanCompress ) );
  
  gl_Vertex2f  := @glVertex2f;
  gl_Vertex2fv := @glVertex2fv;
    
  // Multitexturing
  gl_TexCoord2f  := @glTexCoord2f;
  gl_TexCoord2fv := @glTexCoord2fv;
  glGetIntegerv( GL_MAX_TEXTURE_UNITS_ARB, @ogl_MaxTexLevels );
  log_Add( 'GL_MAX_TEXTURE_UNITS_ARB: ' + u_IntToStr( ogl_MaxTexLevels ) );
  glMultiTexCoord2fARB := wglGetProcAddress( 'glMultiTexCoord2fARB' );
  if Assigned( glMultiTexCoord2fARB ) Then
    begin
      glMultiTexCoord2fvARB    := wglGetProcAddress( 'glMultiTexCoord2fvARB'    );
      glActiveTextureARB       := wglGetProcAddress( 'glActiveTextureARB'       );
      glClientActiveTextureARB := wglGetProcAddress( 'glClientActiveTextureARB' );
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
  
  glGetIntegerv( GL_MAX_LIGHTS, @ogl_MaxLights );
  log_Add( 'GL_MAX_LIGHTS: ' + u_IntToStr( ogl_MaxLights ) );
  glLightModeli( GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE );
  glLightModeli( GL_LIGHT_MODEL_LOCAL_VIEWER, GL_FALSE );

  {for i := 0 to ogl_MaxLights do
    begin
      glLightfv( GL_LIGHT0 + i, GL_AMBIENT,  @matAMBIENT );
      glLightfv( GL_LIGHT0 + i, GL_DIFFUSE,  @matDIFFUSE );
      glLightfv( GL_LIGHT0 + i, GL_SPECULAR, @matSPECULAR );
      glLightfv( GL_LIGHT0 + i, GL_EMISSION, @matEMISSION );
      glLightf ( GL_LIGHT0 + i, GL_SHININESS, matSHININESS );
    end;}
    
  // VBO
  glBindBufferARB := wglGetProcAddress( 'glBindBufferARB' );
  if Assigned( glBindBufferARB ) Then
    begin
      ogl_CanVBO         := TRUE;
      glDeleteBuffersARB := wglGetProcAddress( 'glDeleteBuffersARB' );
      glGenBuffersARB    := wglGetProcAddress( 'glGenBuffersARB'    );
      glIsBufferARB      := wglGetProcAddress( 'glIsBufferARB'      );
      glBufferDataARB    := wglGetProcAddress( 'glBufferDataARB'    );
      glBufferSubDataARB := wglGetProcAddress( 'glBufferSubDataARB' );
    end else
      ogl_CanVBO := FALSE;
  log_Add( 'GL_ARB_VERTEX_BUFFER_OBJECT: ' + u_BoolToStr( ogl_CanVBO ) );
    
  // FBO
  glBindRenderbufferEXT := wglGetProcAddress( 'glBindRenderbufferEXT' );
  if Assigned( glBindRenderbufferEXT ) Then
    begin
      ogl_CanFBO                   := TRUE;
      glIsRenderbufferEXT          := wglGetProcAddress( 'glIsRenderbufferEXT'          );
      glDeleteRenderbuffersEXT     := wglGetProcAddress( 'glDeleteRenderbuffersEXT'     );
      glGenRenderbuffersEXT        := wglGetProcAddress( 'glGenRenderbuffersEXT'        );
      glRenderbufferStorageEXT     := wglGetProcAddress( 'glRenderbufferStorageEXT'     );
      glIsFramebufferEXT           := wglGetProcAddress( 'glIsFramebufferEXT'           );
      glBindFramebufferEXT         := wglGetProcAddress( 'glBindFramebufferEXT'         );
      glDeleteFramebuffersEXT      := wglGetProcAddress( 'glDeleteFramebuffersEXT'      );
      glGenFramebuffersEXT         := wglGetProcAddress( 'glGenFramebuffersEXT'         );
      glCheckFramebufferStatusEXT  := wglGetProcAddress( 'glCheckFramebufferStatusEXT'  );
      glFramebufferTexture2DEXT    := wglGetProcAddress( 'glFramebufferTexture2DEXT'    );
      glFramebufferRenderbufferEXT := wglGetProcAddress( 'glFramebufferRenderbufferEXT' );
    end else
      ogl_CanFBO := FALSE;
   log_Add( 'GL_EXT_FRAMEBUFFER_OBJECT: ' + u_BoolToStr( ogl_CanFBO ) );
    
  // PBUFFER
  {$IFDEF WIN32}
  wglCreatePbufferARB := wglGetProcAddress( 'wglCreatePbufferARB' );
  if Assigned( wglCreatePbufferARB ) and Assigned( wglChoosePixelFormatARB ) Then
    begin
      ogl_CanPBuffer         := TRUE;
      wglGetPbufferDCARB     := wglGetProcAddress( 'wglGetPbufferDCARB'     );
      wglReleasePbufferDCARB := wglGetProcAddress( 'wglReleasePbufferDCARB' );
      wglDestroyPbufferARB   := wglGetProcAddress( 'wglDestroyPbufferARB'   );
    end else
      ogl_CanPBuffer := FALSE;
  log_Add( 'WGL_ARB_PBUFFER: ' + u_BoolToStr( ogl_CanPBuffer ) );
  {$ENDIF}

    
  // WaitVSync
{$IFDEF LINUX}
  glXGetVideoSyncSGI  := wglGetProcAddress( 'glXGetVideoSyncSGI' );
  if Assigned( glXGetVideoSyncSGI ) Then
    begin
      ogl_CanVSync        := TRUE;
      glXWaitVideoSyncSGI := wglGetProcAddress( 'glXWaitVideoSyncSGI' );
    end else
      ogl_CanVSync := FALSE;
{$ENDIF}
{$IFDEF WIN32}
  wglGetSwapIntervalEXT := wglGetProcAddress( 'wglGetSwapIntervalEXT' );
  if Assigned( wglGetSwapIntervalEXT ) Then
    begin
      ogl_CanVSync := TRUE;
      wglSwapIntervalEXT := wglGetProcAddress( 'wglSwapIntervalEXT' );
    end else
      ogl_CanVSync := FALSE;
      
   wglChoosePixelFormatARB := wglGetProcAddress( 'wglChoosePixelFormatARB' );
{$ENDIF}
  log_Add( 'Support WaitVSync: ' + u_BoolToStr( ogl_CanVSync ) );
end;

procedure Set2DMode;
begin
  ogl_Mode := 2;
  
  glDisable( GL_DEPTH_TEST );
  glMatrixMode( GL_PROJECTION );
  glLoadIdentity;
  if app_Flags and CORRECT_RESOLUTION > 0 Then
    glOrtho( 0, ogl_Width - scr_AddCX * 2 / scr_ResCX, ogl_Height - scr_AddCY * 2 / scr_ResCY, 0, 0, 1 )
  else
    glOrtho( 0, wnd_Width, wnd_Height, 0, 0, 1 );
  glMatrixMode( GL_MODELVIEW );
  glLoadIdentity;
  if app_Flags and CORRECT_RESOLUTION > 0 Then
    glViewPort( scr_AddCX, scr_AddCY, wnd_Width - scr_AddCX * 2, wnd_Height - scr_AddCY * 2 )
  else
    glViewPort( 0, 0, wnd_Width, wnd_Height );
end;

procedure Set3DMode;
begin
  ogl_Mode := 3;
  ogl_FOVY := FOVY;

  glColor4ub( 255, 255, 255, 255 );

  glEnable( GL_DEPTH_TEST );
  glMatrixMode( GL_PROJECTION );
  glLoadIdentity;
  gluPerspective( ogl_FOVY, ogl_Width / ogl_Height, ogl_zNear, ogl_zFar );
  glMatrixMode( GL_MODELVIEW );
  glLoadIdentity;
  glViewPort( 0, 0, wnd_Width, wnd_Height );
end;

procedure SetCurrentMode;
begin
  if ogl_Mode = 2 Then
    Set2DMode
  else
    Set3DMode( ogl_FOVY );
end;

procedure gl_MTexCoord2f;
  var
    i : Integer;
begin
  for i := 0 to ogl_MaxTexLevels do
    if ogl_MTexActive[ i ] Then
      glMultiTexCoord2fARB( GL_TEXTURE0_ARB + i, U, V );
end;

procedure gl_MTexCoord2fv;
  var
    i : Integer;
begin
  for i := 0 to ogl_MaxTexLevels do
    if ogl_MTexActive[ i ] Then
      glMultiTexCoord2fvARB( GL_TEXTURE0_ARB + i, Coord )
end;

end.
