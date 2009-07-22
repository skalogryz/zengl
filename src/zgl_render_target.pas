{
 * Copyright © Kemka Andrey aka Andru
 * mail: dr.andru@gmail.com
 * site: http://andru-kun.inf.ua
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
unit zgl_render_target;

{$I zgl_config.cfg}

interface
uses
  {$IFDEF LINUX}
  X, XLib, XUtil,
  {$ENDIF}
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  zgl_opengl,
  zgl_opengl_all,
  zgl_textures;

const
  RT_TYPE_SIMPLE  = 0;
  RT_TYPE_FBO     = 1;
  RT_TYPE_PBUFFER = 2;
  RT_FULL_SCREEN  = $01;
  RT_CLEAR_SCREEN = $02;

type
  zglPFBO = ^zglTFBO;
  zglTFBO = record
    FrameBuffer  : DWORD;
    RenderBuffer : DWORD;
end;

{$IFDEF LINUX}
type
  zglPPBuffer = ^zglTPBuffer;
  zglTPBuffer = record
    Handle  : Integer;
    PBuffer : GLXPBuffer;
end;
{$ENDIF}
{$IFDEF WIN32}
type
  zglPPBuffer = ^zglTPBuffer;
  zglTPBuffer = record
    Handle : THandle;
    DC     : HDC;
    RC     : HGLRC;
end;
{$ENDIF}

type
  zglPRenderTarget = ^zglTRenderTarget;
  zglTRenderTarget = record
    rtType     : Byte;
    Handle     : Pointer;
    Surface    : zglPTexture;
    Flags      : Byte;

    Prev, Next : zglPRenderTarget;
end;

type
  zglPRenderTargetManager = ^zglTRenderTargetManager;
  zglTRenderTargetManager = record
    Count : DWORD;
    First : zglTRenderTarget;
end;

function  rtarget_Add( rtType : Byte; const Surface : zglPTexture; const Flags : Byte ) : zglPRenderTarget;
procedure rtarget_Del( var Target : zglPRenderTarget );
procedure rtarget_Set( const Target : zglPRenderTarget );

var
  managerRTarget : zglTRenderTargetManager;

implementation
uses
  zgl_main,
  zgl_window,
  zgl_screen,
  zgl_render_2d,
  zgl_log;

var
  lRTarget : zglPRenderTarget;
  lMode : Integer;

function rtarget_Add;
  var
    pFBO : zglPFBO;
{$IFDEF LINUX}
    i, n : Integer;
    fbconfig : GLXFBConfig;
    visualinfo : PXVisualInfo;
    pPBuffer : zglPPBuffer;
    PBufferiAttr : array[ 0..8 ] of Integer;
{$ENDIF}
{$IFDEF WIN32}
    pPBuffer     : zglPPBuffer;
    PBufferiAttr : array[ 0..15 ] of Integer;
    PBufferfAttr : array[ 0..1 ] of Single;
    PixelFormat  : Integer;
    nPixelFormat : DWORD;
{$ENDIF}
begin
  Result := @managerRTarget.First;
  while Assigned( Result.Next ) do
    Result := Result.Next;

  zgl_GetMem( Pointer( Result.Next ), SizeOf( zglTRenderTarget ) );

  if ( not ogl_CanFBO ) and ( rtType = RT_TYPE_FBO ) Then
    {$IFDEF LINUX}
    if ogl_CanPBuffer Then
      rtType := RT_TYPE_PBUFFER
    else
    {$ENDIF}
    {$IFDEF WIN32}
    if ogl_CanPBuffer Then
      rtType := RT_TYPE_PBUFFER
    else
    {$ENDIF}
      rtType := RT_TYPE_SIMPLE;

  if ( not ogl_CanPBuffer ) and ( rtType = RT_TYPE_PBUFFER ) Then
    if ogl_CanFBO Then
      rtType := RT_TYPE_FBO
    else
      rtType := RT_TYPE_SIMPLE;

  case rtType of
    RT_TYPE_SIMPLE: Result.Next.Handle := nil;
    RT_TYPE_FBO:
      begin
        zgl_GetMem( Result.Next.Handle, SizeOf( zglTFBO ) );
        pFBO := Result.Next.Handle;

        glGenFramebuffersEXT( 1, @pFBO.FrameBuffer );
        glBindFramebufferEXT( GL_FRAMEBUFFER_EXT, pFBO.FrameBuffer );
        if glIsFrameBufferEXT( pFBO.FrameBuffer ) = GL_TRUE Then
          log_Add( 'FBO: Gen FrameBuffer - Success' )
        else
          begin
            log_Add( 'FBO: Gen FrameBuffer - Error' );
            exit;
          end;

        glGenRenderbuffersEXT( 1, @pFBO.RenderBuffer );
        glBindRenderbufferEXT( GL_RENDERBUFFER_EXT, pFBO.RenderBuffer );
        if glIsRenderBufferEXT( pFBO.RenderBuffer ) = GL_TRUE Then
          log_Add( 'FBO: Gen RenderBuffer - Success' )
        else
          begin
            log_Add( 'FBO: Gen RenderBuffer - Error' );
            exit;
          end;

        case ogl_zDepth of
          24: glRenderbufferStorageEXT( GL_RENDERBUFFER_EXT, GL_DEPTH_COMPONENT24, Surface.Width, Surface.Height );
          32: glRenderbufferStorageEXT( GL_RENDERBUFFER_EXT, GL_DEPTH_COMPONENT32, Surface.Width, Surface.Height );
        else
          glRenderbufferStorageEXT( GL_RENDERBUFFER_EXT, GL_DEPTH_COMPONENT16, Surface.Width, Surface.Height );
        end;
        glFramebufferRenderbufferEXT( GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, pFBO.RenderBuffer );
        glFramebufferTexture2DEXT( GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, 0, 0 );
        glBindFramebufferEXT( GL_FRAMEBUFFER_EXT, 0 );
      end;
    {$IFDEF LINUX}
    RT_TYPE_PBUFFER:
      begin
        zgl_GetMem( Result.Next.Handle, SizeOf( zglTPBuffer ) );
        pPBuffer := Result.Next.Handle;

        PBufferiAttr[ 0 ] := GLX_PBUFFER_WIDTH;
        PBufferiAttr[ 1 ] := Surface.Width;
        PBufferiAttr[ 2 ] := GLX_PBUFFER_HEIGHT;
        PBufferiAttr[ 3 ] := Surface.Height;
        PBufferiAttr[ 4 ] := GLX_PRESERVED_CONTENTS;
        PBufferiAttr[ 5 ] := GL_TRUE;
        PBufferiAttr[ 6 ] := None;

        fbconfig := glXGetFBConfigs( scr_Display, scr_Default, @n );
        for i := 0 to n - 1 do
          begin
            visualinfo := glXGetVisualFromFBConfig( scr_Display, PInteger( fbconfig + i * 4 )^ );
            if visualinfo.visualid = ogl_VisualInfo.visualid Then
              begin
                pPBuffer.Handle := PInteger( fbconfig + i * 4 )^;
                break;
              end;
          end;
        XFree( fbconfig );
        XFree( visualinfo );

        pPBuffer.PBuffer := glXCreatePbuffer( scr_Display, pPBuffer.Handle, @PBufferiAttr[ 0 ] );
        if pPBuffer.PBuffer = 0 Then
          begin
            log_Add( 'PBuffer: glXCreatePbuffer - fail' );
            ogl_CanPBuffer := FALSE;
            exit;
          end;
      end;
    {$ENDIF}
    {$IFDEF WIN32}
    RT_TYPE_PBUFFER:
      begin
        zgl_GetMem( Result.Next.Handle, SizeOf( zglTPBuffer ) );
        pPBuffer := Result.Next.Handle;

        PBufferiAttr[ 0 ]  := WGL_DRAW_TO_PBUFFER_ARB;
        PBufferiAttr[ 1 ]  := GL_TRUE;
        PBufferiAttr[ 2 ]  := WGL_COLOR_BITS_ARB;
        PBufferiAttr[ 3 ]  := scr_BPP;
        PBufferiAttr[ 4 ]  := WGL_ALPHA_BITS_ARB;
        PBufferiAttr[ 5 ]  := 8;
        PBufferiAttr[ 6 ]  := WGL_DEPTH_BITS_ARB;
        PBufferiAttr[ 7 ]  := ogl_zDepth;
        PBufferiAttr[ 8 ]  := WGL_PBUFFER_WIDTH_ARB;
        PBufferiAttr[ 9 ]  := Surface.Width;
        PBufferiAttr[ 10 ] := WGL_PBUFFER_HEIGHT_ARB;
        PBufferiAttr[ 11 ] := Surface.Height;
        PBufferiAttr[ 12 ] := 0;

        PBufferfAttr[ 0 ] := 0;
        PBufferfAttr[ 1 ] := 0;

        wglChoosePixelFormatARB( wnd_DC, @PBufferiAttr, @PBufferfAttr, 1, @PixelFormat, @nPixelFormat );
        pPBuffer.Handle := wglCreatePbufferARB( wnd_DC, PixelFormat, ogl_Width, ogl_Height, nil );

        pPBuffer.DC := wglGetPbufferDCARB( pPBuffer.Handle );
        pPBuffer.RC := wglCreateContext( pPBuffer.DC );

        wglMakeCurrent( pPBuffer.Handle, pPBuffer.RC );
        wglShareLists( wnd_DC, pPBuffer.RC );
        wglMakeCurrent( wnd_DC, ogl_Context );

        if pPBuffer.RC = 0 Then
          begin
            log_Add( 'PBuffer: RC create - Error' );
            ogl_CanPBuffer := FALSE;
            exit;
          end else
            log_Add( 'PBuffer: RC create - Success' );
      end;
    {$ENDIF}
  end;
  Result.Next.rtType  := rtType;
  Result.Next.Surface := Surface;
  Result.Next.Flags   := Flags;

  Result.Next.Prev := Result;
  Result.Next.Next := nil;
  Result := Result.Next;
  INC( managerRTarget.Count );
end;

procedure rtarget_Del;
begin
  if not Assigned( Target ) Then exit;

  case Target.rtType of
    RT_TYPE_FBO:
      begin
        if glIsRenderBufferEXT( zglPFBO( Target.Handle ).RenderBuffer ) = GL_TRUE Then
          glDeleteRenderbuffersEXT( 1, @zglPFBO( Target.Handle ).RenderBuffer );
        if glIsRenderBufferEXT( zglPFBO( Target.Handle ).FrameBuffer ) = GL_TRUE Then
          glDeleteFramebuffersEXT( 1, @zglPFBO( Target.Handle ).FrameBuffer );
      end;
  {$IFDEF WIN32}
    RT_TYPE_PBUFFER:
      begin
        if zglPPBuffer( Target.Handle ).RC <> 0 Then
          wglDeleteContext( zglPPBuffer( Target.Handle ).RC );
        if zglPPBuffer( Target.Handle ).DC <> 0 Then
          wglReleasePbufferDCARB( zglPPBuffer( Target.Handle ).Handle, zglPPBuffer( Target.Handle ).DC );
        if zglPPBuffer( Target.Handle ).Handle <> 0 Then
          wglDestroyPbufferARB( zglPPBuffer( Target.Handle ).Handle );
      end;
  {$ENDIF}
  end;

  if Assigned( Target.Prev ) Then
    Target.Prev.Next := Target.Next;
  if Assigned( Target.Next ) Then
    Target.Next.Prev := Target.Prev;

  if Assigned( Target.Handle ) Then
    FreeMemory( Target.Handle );
  FreeMemory( Target );
  DEC( managerRTarget.Count );

  Target := nil;
end;

procedure rtarget_Set;
begin
  batch2d_Flush;

  if Assigned( Target ) Then
    begin
      lRTarget := Target;
      lMode := ogl_Mode;
      ogl_Mode := 1;

      case Target.rtType of
        RT_TYPE_SIMPLE:
          begin
          end;
        RT_TYPE_FBO:
          begin
            glBindFramebufferEXT( GL_FRAMEBUFFER_EXT, zglPFBO( Target.Handle ).FrameBuffer );
            glFramebufferTexture2DEXT( GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, Target.Surface.ID, 0 );
          end;
        {$IFDEF LINUX}
        RT_TYPE_PBUFFER:
          begin
            glXMakeCurrent( scr_Display, zglPPBuffer( Target.Handle ).PBuffer, ogl_Context );
          end;
        {$ENDIF}
        {$IFDEF WIN32}
        RT_TYPE_PBUFFER:
          begin
            wglMakeCurrent( zglPPBuffer( Target.Handle ).Handle, zglPPBuffer( Target.Handle ).RC );
          end;
        {$ENDIF}
      end;

      if Target.Flags and RT_FULL_SCREEN > 0 Then
        glViewport( 0, 0, Target.Surface.Width, Target.Surface.Height )
      else
        glViewport( 0, -( ogl_Height - Target.Surface.Height - scr_AddCY - ( scr_SubCY - scr_AddCY ) ),
                    ogl_Width - scr_AddCX - ( scr_SubCX - scr_AddCX ), ogl_Height - scr_AddCY - ( scr_SubCY - scr_AddCY ) );

      if ( Target.Flags and RT_CLEAR_SCREEN > 0 ) then
        glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );
    end else
      begin
        case lRTarget.rtType of
          RT_TYPE_SIMPLE, RT_TYPE_PBUFFER:
            begin
              glEnable( GL_TEXTURE_2D );
              tex_Filter( lRTarget.Surface, lRTarget.Flags );
              glBindTexture( GL_TEXTURE_2D, lRTarget.Surface.ID );

              glCopyTexSubImage2D( GL_TEXTURE_2D, 0, 0, 0, 0, 0, lRTarget.Surface.Width, lRTarget.Surface.Height );

              glDisable( GL_TEXTURE_2D );
            end;
          RT_TYPE_FBO:
            begin
              glFramebufferTexture2DEXT( GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, 0, 0 );
              glBindFramebufferEXT( GL_FRAMEBUFFER_EXT, 0 );
            end;
        end;

        case lRTarget.rtType of
          {$IFDEF LINUX}
          RT_TYPE_PBUFFER:
            begin
              glXMakeCurrent( scr_Display, wnd_Handle, ogl_Context );
            end;
          {$ENDIF}
          {$IFDEF WIN32}
          RT_TYPE_PBUFFER:
            begin
              wglMakeCurrent( wnd_DC, ogl_Context );
            end;
          {$ENDIF}
        end;

        ogl_Mode := lMode;
        scr_SetViewPort;
        if ( lRTarget.rtType = RT_TYPE_SIMPLE ) and ( lRTarget.Flags and RT_CLEAR_SCREEN > 0 ) Then
          glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );
      end;
end;

end.
