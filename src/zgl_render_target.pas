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
unit zgl_render_target;

{$I zgl_config.cfg}

interface
uses
  {$IFDEF LINUX}
  X, XLib, XUtil,
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF DARWIN}
  MacOSAll,
  {$ENDIF}
  zgl_opengl,
  zgl_opengl_all,
  zgl_textures;

const
  RT_TYPE_PBUFFER = 0;
  RT_TYPE_FBO     = 1;

  RT_DEFAULT      = $00;
  RT_FULL_SCREEN  = $01;
  RT_USE_DEPTH    = $02;
  RT_CLEAR_COLOR  = $04;
  RT_CLEAR_DEPTH  = $08;

{$IFDEF LINUX}
type
  zglPPBuffer = ^zglTPBuffer;
  zglTPBuffer = record
    Handle  : Integer;
    Context : GLXContext;
    PBuffer : GLXPBuffer;
end;
{$ENDIF}
{$IFDEF WINDOWS}
type
  zglPPBuffer = ^zglTPBuffer;
  zglTPBuffer = record
    Handle : THandle;
    DC     : HDC;
    RC     : HGLRC;
end;
{$ENDIF}
{$IFDEF DARWIN}
type
  zglPPBuffer = ^zglTPBuffer;
  zglTPBuffer = record
    Context : TAGLContext;
    PBuffer : TAGLPbuffer;
end;
{$ENDIF}

type
  zglPFBO = ^zglTFBO;
  zglTFBO = record
    FrameBuffer  : LongWord;
    RenderBuffer : LongWord;
end;

type
  zglPRenderTarget = ^zglTRenderTarget;
  zglTRenderTarget = record
    _type      : Byte;
    Handle     : Pointer;
    Surface    : zglPTexture;
    Flags      : Byte;

    prev, next : zglPRenderTarget;
end;

type
  zglPRenderTargetManager = ^zglTRenderTargetManager;
  zglTRenderTargetManager = record
    Count : LongWord;
    First : zglTRenderTarget;
end;

type
  zglTRenderCallback = procedure( Data : Pointer );

function  rtarget_Add( const Surface : zglPTexture; const Flags : Byte ) : zglPRenderTarget;
procedure rtarget_Del( var Target : zglPRenderTarget );
procedure rtarget_Set( const Target : zglPRenderTarget );
procedure rtarget_DrawIn( const Target : zglPRenderTarget; const RenderCallback : zglTRenderCallback; const Data : Pointer );

var
  managerRTarget : zglTRenderTargetManager;

implementation
uses
  zgl_application,
  zgl_main,
  zgl_window,
  zgl_screen,
  zgl_opengl_simple,
  zgl_render_2d,
  zgl_sprite_2d,
  zgl_log;

var
  lRTarget : zglPRenderTarget;
  lGLW     : Integer;
  lGLH     : Integer;
  lClipW   : Integer;
  lClipH   : Integer;
  lResCX   : Single;
  lResCY   : Single;

function rtarget_Add;
  var
    i, _type : Integer;
    pFBO     : zglPFBO;
    pPBuffer : zglPPBuffer;
{$IFDEF LINUX}
    n            : Integer;
    fbconfig     : GLXFBConfig;
    visualinfo   : PXVisualInfo;
    pbufferiAttr : array[ 0..15 ] of Integer;
    fbconfigAttr : array[ 0..31 ] of Integer;
{$ENDIF}
{$IFDEF WINDOWS}
    pbufferiAttr : array[ 0..31 ] of Integer;
    pbufferfAttr : array[ 0..15 ] of Single;
    pixelFormat  : array[ 0..63 ] of Integer;
    nPixelFormat : LongWord;
{$ENDIF}
{$IFDEF DARWIN}
    pbufferdAttr : array[ 0..31 ] of LongWord;
    pixelFormat  : TAGLPixelFormat;
{$ENDIF}
begin
  Result := @managerRTarget.First;
  while Assigned( Result.next ) do
    Result := Result.next;

  zgl_GetMem( Pointer( Result.next ), SizeOf( zglTRenderTarget ) );

  _type := RT_TYPE_FBO;

  // GeForce FX sucks: http://www.opengl.org/wiki/Common_Mistakes#Render_To_Texture
  if gl_IsSupported( 'GeForce FX', ogl_Renderer ) and ( Flags and RT_USE_DEPTH > 0 ) Then
    _type := RT_TYPE_PBUFFER;

  if not ogl_CanFBO Then
    if ogl_CanPBuffer Then
      _type := RT_TYPE_PBUFFER
    else
      exit;

  case _type of
    {$IFDEF LINUX}
    RT_TYPE_PBUFFER:
      begin
        zgl_GetMem( Result.next.Handle, SizeOf( zglTPBuffer ) );
        pPBuffer := Result.next.Handle;

        FillChar( pbufferiAttr[ 0 ], 16 * 4, None );
        FillChar( fbconfigAttr[ 0 ], 32 * 4, None );
        fbconfigAttr[ 0  ] := GLX_DRAWABLE_TYPE;
        fbconfigAttr[ 1  ] := GLX_PBUFFER_BIT;
        fbconfigAttr[ 2  ] := GLX_DOUBLEBUFFER;
        fbconfigAttr[ 3  ] := GL_TRUE;
        fbconfigAttr[ 4  ] := GLX_RENDER_TYPE;
        fbconfigAttr[ 5  ] := GLX_RGBA_BIT;
        fbconfigAttr[ 6  ] := GLX_RED_SIZE;
        fbconfigAttr[ 7  ] := 8;
        fbconfigAttr[ 8  ] := GLX_GREEN_SIZE;
        fbconfigAttr[ 9  ] := 8;
        fbconfigAttr[ 10 ] := GLX_BLUE_SIZE;
        fbconfigAttr[ 11 ] := 8;
        fbconfigAttr[ 12 ] := GLX_ALPHA_SIZE;
        fbconfigAttr[ 13 ] := 8;
        fbconfigAttr[ 14 ] := GLX_DEPTH_SIZE;
        fbconfigAttr[ 15 ] := ogl_zDepth;
        i := 16;
        if ogl_Stencil > 0 Then
          begin
            fbconfigAttr[ i     ] := GLX_STENCIL_SIZE;
            fbconfigAttr[ i + 1 ] := ogl_Stencil;
            INC( i, 2 );
          end;
        if ogl_FSAA > 0 Then
          begin
            fbconfigAttr[ i     ] := GLX_SAMPLES_SGIS;
            fbconfigAttr[ i + 1 ] := ogl_FSAA;
          end;

        fbconfig := glXChooseFBConfig( scr_Display, scr_Default, @fbconfigAttr[ 0 ], @n );
        if not Assigned( fbconfig ) Then
          begin
            log_Add( 'PBuffer: failed to choose GLXFBConfig' );
            ogl_CanPBuffer := FALSE;
            exit;
          end else
            pPBuffer.Handle := PInteger( fbconfig )^;

        case ogl_PBufferMode of
          1:
            begin
              pbufferiAttr[ 0 ] := GLX_PBUFFER_WIDTH;
              pbufferiAttr[ 1 ] := Round( Surface.Width / Surface.U );
              pbufferiAttr[ 2 ] := GLX_PBUFFER_HEIGHT;
              pbufferiAttr[ 3 ] := Round( Surface.Height / Surface.V );
              pbufferiAttr[ 4 ] := GLX_PRESERVED_CONTENTS;
              pbufferiAttr[ 5 ] := GL_TRUE;
              pbufferiAttr[ 6 ] := GLX_LARGEST_PBUFFER;
              pbufferiAttr[ 7 ] := GL_TRUE;

              pPBuffer.PBuffer := glXCreatePbuffer( scr_Display, pPBuffer.Handle, @pbufferiAttr[ 0 ] );
            end;
          2:
            begin
              pbufferiAttr[ 0 ] := GLX_PRESERVED_CONTENTS;
              pbufferiAttr[ 1 ] := GL_TRUE;
              pbufferiAttr[ 2 ] := GLX_LARGEST_PBUFFER;
              pbufferiAttr[ 3 ] := GL_TRUE;

              pPBuffer.PBuffer := glXCreateGLXPbufferSGIX( scr_Display, pPBuffer.Handle, Surface.Width, Surface.Height, @pbufferiAttr[ 0 ] );
            end;
        end;

        if pPBuffer.PBuffer = 0 Then
          begin
            log_Add( 'PBuffer: failed to create GLXPBuffer' );
            ogl_CanPBuffer := FALSE;
            exit;
          end;

        visualinfo := glXGetVisualFromFBConfig( scr_Display, pPBuffer.Handle );
        if not Assigned( visualinfo ) Then
          begin
            log_Add( 'PBuffer: failed to choose Visual' );
            ogl_CanPBuffer := FALSE;
            exit;
          end;

        pPBuffer.Context := glXCreateContext( scr_Display, visualinfo, ogl_Context, TRUE );
        XFree( fbconfig );
        XFree( visualinfo );
        if pPBuffer.Context = nil Then
          begin
            log_Add( 'PBuffer: failed to create GLXContext' );
            ogl_CanPBuffer := FALSE;
            exit;
          end;

        glXMakeCurrent( scr_Display, pPBuffer.PBuffer, pPBuffer.Context );
        gl_ResetState();
        Set2DMode();
        glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );
        ssprite2d_Draw( Surface, 0, ogl_Height - Surface.Height, ogl_Width - ( ogl_Width - Surface.Width ), ogl_Height - ( ogl_Height - Surface.Height ), 0, 255 );
        glXMakeCurrent( scr_Display, wnd_Handle, ogl_Context );
      end;
    {$ENDIF}
    {$IFDEF WINDOWS}
    RT_TYPE_PBUFFER:
      begin
        zgl_GetMem( Result.next.Handle, SizeOf( zglTPBuffer ) );
        pPBuffer := Result.next.Handle;

        FillChar( pbufferiAttr[ 0 ], 32 * 4, 0 );
        FillChar( pbufferfAttr[ 0 ], 16 * 4, 0 );
        pbufferiAttr[ 0  ] := WGL_DRAW_TO_PBUFFER_ARB;
        pbufferiAttr[ 1  ] := GL_TRUE;
        pbufferiAttr[ 2  ] := WGL_DOUBLE_BUFFER_ARB;
        pbufferiAttr[ 3  ] := GL_TRUE;
        pbufferiAttr[ 4  ] := WGL_COLOR_BITS_ARB;
        pbufferiAttr[ 5  ] := 24;
        pbufferiAttr[ 6  ] := WGL_RED_BITS_ARB;
        pbufferiAttr[ 7  ] := 8;
        pbufferiAttr[ 8  ] := WGL_GREEN_BITS_ARB;
        pbufferiAttr[ 9  ] := 8;
        pbufferiAttr[ 10 ] := WGL_BLUE_BITS_ARB;
        pbufferiAttr[ 11 ] := 8;
        pbufferiAttr[ 12 ] := WGL_ALPHA_BITS_ARB;
        pbufferiAttr[ 13 ] := 8;
        pbufferiAttr[ 14 ] := WGL_DEPTH_BITS_ARB;
        pbufferiAttr[ 15 ] := ogl_zDepth;
        i := 16;
        if ogl_Stencil > 0 Then
          begin
            pbufferiAttr[ i     ] := WGL_STENCIL_BITS_ARB;
            pbufferiAttr[ i + 1 ] := ogl_Stencil;
            INC( i, 2 );
          end;
        if ogl_FSAA > 0 Then
          begin
            pbufferiAttr[ i     ] := WGL_SAMPLE_BUFFERS_ARB;
            pbufferiAttr[ i + 1 ] := GL_TRUE;
            pbufferiAttr[ i + 2 ] := WGL_SAMPLES_ARB;
            pbufferiAttr[ i + 3 ] := ogl_FSAA;
          end;

        wglChoosePixelFormatARB( wnd_DC, @pbufferiAttr[ 0 ], @pbufferfAttr[ 0 ], 64, @pixelFormat, @nPixelFormat );

        pPBuffer.Handle := wglCreatePbufferARB( wnd_DC, pixelFormat[ 0 ], Round( Surface.Width / Surface.U ), Round( Surface.Height / Surface.V ), nil );
        if pPBuffer.Handle <> 0 Then
          begin
            pPBuffer.DC := wglGetPbufferDCARB( pPBuffer.Handle );
            pPBuffer.RC := wglCreateContext( pPBuffer.DC );
            if pPBuffer.RC = 0 Then
              begin
                log_Add( 'PBuffer: RC create - Error' );
                ogl_CanPBuffer := FALSE;
                exit;
              end;
            wglShareLists( ogl_Context, pPBuffer.RC );
          end else
            begin
              log_Add( 'PBuffer: wglCreatePbufferARB - failed' );
              ogl_CanPBuffer := FALSE;
              exit;
            end;
        wglMakeCurrent( pPBuffer.DC, pPBuffer.RC );
        gl_ResetState();
        Set2DMode();
        glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );
        ssprite2d_Draw( Surface, 0, ogl_Height - Surface.Height, ogl_Width - ( ogl_Width - Surface.Width ), ogl_Height - ( ogl_Height - Surface.Height ), 0, 255 );
        wglMakeCurrent( wnd_DC, ogl_Context );
      end;
    {$ENDIF}
    {$IFDEF DARWIN}
    RT_TYPE_PBUFFER:
      begin
        zgl_GetMem( Result.next.Handle, SizeOf( zglTPBuffer ) );
        pPBuffer := Result.next.Handle;

        FillChar( pbufferdAttr[ 0 ], 32 * 4, AGL_NONE );
        pbufferdAttr[ 0  ] := AGL_DOUBLEBUFFER;
        pbufferdAttr[ 1  ] := AGL_RGBA;
        pbufferdAttr[ 2  ] := GL_TRUE;
        pbufferdAttr[ 3  ] := AGL_RED_SIZE;
        pbufferdAttr[ 4  ] := 8;
        pbufferdAttr[ 5  ] := AGL_GREEN_SIZE;
        pbufferdAttr[ 6  ] := 8;
        pbufferdAttr[ 7  ] := AGL_BLUE_SIZE;
        pbufferdAttr[ 8  ] := 8;
        pbufferdAttr[ 9  ] := AGL_ALPHA_SIZE;
        pbufferdAttr[ 10 ] := 8;
        pbufferdAttr[ 11 ] := AGL_DEPTH_SIZE;
        pbufferdAttr[ 12 ] := ogl_zDepth;
        i := 13;
        if ogl_Stencil > 0 Then
          begin
            pbufferdAttr[ i     ] := AGL_STENCIL_SIZE;
            pbufferdAttr[ i + 1 ] := ogl_Stencil;
            INC( i, 2 );
          end;
        if ogl_FSAA > 0 Then
          begin
            pbufferdAttr[ i     ] := AGL_SAMPLE_BUFFERS_ARB;
            pbufferdAttr[ i + 1 ] := 1;
            pbufferdAttr[ i + 2 ] := AGL_SAMPLES_ARB;
            pbufferdAttr[ i + 3 ] := ogl_FSAA;
          end;

        DMGetGDeviceByDisplayID( DisplayIDType( scr_Display ), ogl_Device, FALSE );
        pixelFormat := aglChoosePixelFormat( @ogl_Device, 1, @pbufferdAttr[ 0 ] );
        if not Assigned( pixelFormat ) Then
          begin
            log_Add( 'PBuffer: aglChoosePixelFormat - failed' );
            ogl_CanPBuffer := FALSE;
            exit;
          end;

        pPBuffer.Context := aglCreateContext( pixelFormat, ogl_Context );
        if not Assigned( pPBuffer.Context ) Then
          begin
            log_Add( 'PBuffer: aglCreateContext - failed' );
            ogl_CanPBuffer := FALSE;
            exit;
          end;
        aglDestroyPixelFormat( pixelFormat );

        if aglCreatePBuffer( Surface.Width, Surface.Height, GL_TEXTURE_2D, GL_RGBA, 0, @pPBuffer.PBuffer ) = GL_FALSE Then
          begin
            log_Add( 'PBuffer: aglCreatePBuffer - failed' );
            ogl_CanPBuffer := FALSE;
            exit;
          end;
      end;
    {$ENDIF}
    RT_TYPE_FBO:
      begin
        zgl_GetMem( Result.next.Handle, SizeOf( zglTFBO ) );
        pFBO := Result.next.Handle;

        glGenFramebuffersEXT( 1, @pFBO.FrameBuffer );
        glBindFramebufferEXT( GL_FRAMEBUFFER_EXT, pFBO.FrameBuffer );
        if glIsFrameBufferEXT( pFBO.FrameBuffer ) = GL_FALSE Then
          begin
            log_Add( 'FBO: Gen FrameBuffer - Error' );
            exit;
          end;

        glGenRenderbuffersEXT( 1, @pFBO.RenderBuffer );
        glBindRenderbufferEXT( GL_RENDERBUFFER_EXT, pFBO.RenderBuffer );
        if glIsRenderBufferEXT( pFBO.RenderBuffer ) = GL_FALSE Then
          begin
            log_Add( 'FBO: Gen RenderBuffer - Error' );
            exit;
          end;

        glRenderbufferStorageEXT( GL_RENDERBUFFER_EXT, GL_RGBA, Round( Surface.Width / Surface.U ), Round( Surface.Height / Surface.V ) );
        if Flags and RT_USE_DEPTH > 0 Then
          begin
            case ogl_zDepth of
              24: glRenderbufferStorageEXT( GL_RENDERBUFFER_EXT, GL_DEPTH_COMPONENT24, Round( Surface.Width / Surface.U ), Round( Surface.Height / Surface.V ) );
              32: glRenderbufferStorageEXT( GL_RENDERBUFFER_EXT, GL_DEPTH_COMPONENT32, Round( Surface.Width / Surface.U ), Round( Surface.Height / Surface.V ) );
            else
              glRenderbufferStorageEXT( GL_RENDERBUFFER_EXT, GL_DEPTH_COMPONENT16, Round( Surface.Width / Surface.U ), Round( Surface.Height / Surface.V ) );
            end;
            glFramebufferRenderbufferEXT( GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, pFBO.RenderBuffer );
          end;

        glFramebufferTexture2DEXT( GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, 0, 0 );
        glBindFramebufferEXT( GL_FRAMEBUFFER_EXT, 0 );
      end;
  end;
  Result.next._type   := _type;
  Result.next.Surface := Surface;
  Result.next.Flags   := Flags;

  Result.next.prev := Result;
  Result.next.next := nil;
  Result := Result.next;
  INC( managerRTarget.Count );
end;

procedure rtarget_Del;
begin
  if not Assigned( Target ) Then exit;

  tex_Del( Target.Surface );

  case Target._type of
    RT_TYPE_FBO:
      begin
        if glIsRenderBufferEXT( zglPFBO( Target.Handle ).RenderBuffer ) = GL_TRUE Then
          glDeleteRenderbuffersEXT( 1, @zglPFBO( Target.Handle ).RenderBuffer );
        if glIsFramebufferEXT( zglPFBO( Target.Handle ).FrameBuffer ) = GL_TRUE Then
          glDeleteFramebuffersEXT( 1, @zglPFBO( Target.Handle ).FrameBuffer );
      end;
    RT_TYPE_PBUFFER:
      begin
        {$IFDEF LINUX}
        case ogl_PBufferMode of
          1: glXDestroyPbuffer( scr_Display, zglPPBuffer( Target.Handle ).PBuffer );
          2: glXDestroyGLXPbufferSGIX( scr_Display, zglPPBuffer( Target.Handle ).PBuffer );
        end;
        {$ENDIF}
        {$IFDEF WINDOWS}
        if zglPPBuffer( Target.Handle ).RC <> 0 Then
          wglDeleteContext( zglPPBuffer( Target.Handle ).RC );
        if zglPPBuffer( Target.Handle ).DC <> 0 Then
          wglReleasePbufferDCARB( zglPPBuffer( Target.Handle ).Handle, zglPPBuffer( Target.Handle ).DC );
        if zglPPBuffer( Target.Handle ).Handle <> 0 Then
          wglDestroyPbufferARB( zglPPBuffer( Target.Handle ).Handle );
        {$ENDIF}
        {$IFDEF DARWIN}
        aglDestroyContext( zglPPBuffer( Target.Handle ).Context );
        aglDestroyPBuffer( zglPPBuffer( Target.Handle ).PBuffer );
        {$ENDIF}
      end;
  end;

  if Assigned( Target.prev ) Then
    Target.prev.next := Target.next;
  if Assigned( Target.next ) Then
    Target.next.prev := Target.prev;

  if Assigned( Target.Handle ) Then
    FreeMemory( Target.Handle );
  FreeMemory( Target );
  Target := nil;

  DEC( managerRTarget.Count );
end;

procedure rtarget_Set;
begin
  batch2d_Flush();

  if Assigned( Target ) Then
    begin
      lRTarget   := Target;
      lGLW       := ogl_Width;
      lGLH       := ogl_Height;
      lClipW     := ogl_ClipW;
      lClipH     := ogl_ClipH;
      lResCX     := scr_ResCX;
      lResCY     := scr_ResCY;
      ogl_Target := TARGET_TEXTURE;

      case Target._type of
        RT_TYPE_PBUFFER:
          begin
            {$IFDEF LINUX}
            glXMakeCurrent( scr_Display, zglPPBuffer( Target.Handle ).PBuffer, zglPPBuffer( Target.Handle ).Context );
            {$ENDIF}
            {$IFDEF WINDOWS}
            wglMakeCurrent( zglPPBuffer( Target.Handle ).DC, zglPPBuffer( Target.Handle ).RC );
            {$ENDIF}
            {$IFDEF DARWIN}
            aglSetCurrentContext( zglPPBuffer( Target.Handle ).Context );
            aglSetPBuffer( zglPPBuffer( Target.Handle ).Context, zglPPBuffer( Target.Handle ).PBuffer, 0, 0, aglGetVirtualScreen( ogl_Context ) );
            SetCurrentMode;
            {$ENDIF}
          end;
        RT_TYPE_FBO:
          begin
            glBindFramebufferEXT( GL_FRAMEBUFFER_EXT, zglPFBO( Target.Handle ).FrameBuffer );
            glFramebufferTexture2DEXT( GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, Target.Surface.ID, 0 );
          end;
      end;

      if Target.Flags and RT_FULL_SCREEN > 0 Then
        glViewport( 0, 0, Target.Surface.Width, Target.Surface.Height )
      else
        begin
          ogl_Width  := Target.Surface.Width;
          ogl_Height := Target.Surface.Height;
          ogl_ClipX  := 0;
          ogl_ClipY  := 0;
          ogl_ClipW  := ogl_Width;
          ogl_ClipH  := ogl_Height;
          scr_ResCX  := 1;
          scr_ResCY  := 1;
          SetCurrentMode();
        end;

      if Target.Flags and RT_CLEAR_COLOR > 0 Then
        glClear( GL_COLOR_BUFFER_BIT );
      if Target.Flags and RT_CLEAR_DEPTH > 0 Then
        glClear( GL_DEPTH_BUFFER_BIT );
    end else
      if Assigned( lRTarget ) Then
        begin
          case lRTarget._type of
            RT_TYPE_PBUFFER:
              begin
                glEnable( GL_TEXTURE_2D );
                glBindTexture( GL_TEXTURE_2D, lRTarget.Surface.ID );
                glCopyTexSubImage2D( GL_TEXTURE_2D, 0, 0, 0, 0, 0, lRTarget.Surface.Width, lRTarget.Surface.Height );
                glDisable( GL_TEXTURE_2D );

                {$IFDEF LINUX}
                glXMakeCurrent( scr_Display, wnd_Handle, ogl_Context );
                {$ENDIF}
                {$IFDEF WINDOWS}
                wglMakeCurrent( wnd_DC, ogl_Context );
                {$ENDIF}
                {$IFDEF DARWIN}
                aglSwapBuffers( zglPPBuffer( lRTarget.Handle ).Context );
                aglSetCurrentContext( ogl_Context );
                {$ENDIF}
              end;
            RT_TYPE_FBO:
              begin
                glFramebufferTexture2DEXT( GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, 0, 0 );
                glBindFramebufferEXT( GL_FRAMEBUFFER_EXT, 0 );
              end;
          end;

          ogl_Target := TARGET_SCREEN;
          if lRTarget.Flags and RT_FULL_SCREEN > 0 Then
            scr_SetViewPort()
          else
            begin
              ogl_Width  := lGLW;
              ogl_Height := lGLH;
              ogl_ClipW  := lClipW;
              ogl_ClipH  := lClipH;
              scr_ResCX  := lResCX;
              scr_ResCY  := lResCY;
              SetCurrentMode();
            end;

          lRTarget   := nil;
      end;
end;

procedure rtarget_DrawIn;
begin
  if ogl_Separate Then
    begin
      rtarget_Set( Target );
      RenderCallback( Data );
      rtarget_Set( nil );
    end else
      begin
        rtarget_Set( Target );

        glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
        glColorMask( GL_TRUE, GL_TRUE, GL_TRUE, GL_FALSE );
        RenderCallback( Data );
        batch2d_Flush();

        glBlendFunc( GL_ONE, GL_ONE_MINUS_SRC_ALPHA );
        glColorMask( GL_FALSE, GL_FALSE, GL_FALSE, GL_TRUE );
        RenderCallback( Data );
        batch2d_Flush();

        rtarget_Set( nil );

        glColorMask( GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE );
        glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
      end;
end;

end.
