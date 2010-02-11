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
unit zgl_screen;

{$I zgl_config.cfg}

interface
uses
  {$IFDEF LINUX}
  X, XLib, XUtil, xf86vmode, UnixType,
  {$ENDIF}
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  {$IFDEF DARWIN}
  MacOSAll,
  {$ENDIF}
  zgl_types,
  zgl_opengl_all;

const
  REFRESH_MAXIMUM = 0;
  REFRESH_DEFAULT = 1;

procedure scr_Init;
function  scr_Create : Boolean;
procedure scr_GetResList;
procedure scr_Destroy;
procedure scr_Reset;
procedure scr_Clear;
procedure scr_Flush;

procedure scr_SetOptions( const Width, Height, BPP, Refresh : WORD; const FullScreen, VSync : Boolean );
procedure scr_CorrectResolution( const Width, Height : WORD );
procedure scr_SetViewPort;
procedure scr_SetVSync( const VSync : Boolean );
procedure scr_SetFSAA( const FSAA : Byte );

{$IFDEF LINUX}
function XOpenIM(para1:PDisplay; para2:PXrmHashBucketRec; para3:Pchar; para4:Pchar):PXIM;cdecl;external;
function XCreateIC(para1 : PXIM; para2 : array of const):PXIC;cdecl;external;
{$ENDIF}

type
  zglPResolitionList = ^zglTResolutionList;
  zglTResolutionList = record
    Count  : Integer;
    Width  : array of Integer;
    Height : array of Integer;
end;

var
  scr_Width   : Integer = 800;
  scr_Height  : Integer = 600;
  scr_BPP     : Integer = 32;
  scr_Refresh : Integer;
  scr_VSync   : Boolean;
  scr_ResList : zglTResolutionList;
  scr_Initialized : Boolean;
  scr_Changing : Boolean;

  // Resolution Correct
  scr_ResW  : Integer;
  scr_ResH  : Integer;
  scr_ResCX : Single  = 1;
  scr_ResCY : Single  = 1;
  scr_AddCX : Integer = 0;
  scr_AddCY : Integer = 0;
  scr_SubCX : Integer = 0;
  scr_SubCY : Integer = 0;

  {$IFDEF LINUX}
  scr_Display   : PDisplay;
  scr_Default   : cint;
  scr_Settings  : TXF86VidModeModeInfo;
  scr_Desktop   : TXF86VidModeModeInfo;
  scr_ModeCount : DWORD;
  scr_ModeList  : PPXF86VidModeModeInfo;
  {$ENDIF}
  {$IFDEF WIN32}
  scr_Settings : DEVMODE;
  scr_Desktop  : DEVMODE;
  {$ENDIF}
  {$IFDEF DARWIN}
  scr_Display  : CGDirectDisplayID;
  scr_Desktop  : CFDictionaryRef;
  scr_DesktopW : Integer;
  scr_DesktopH : Integer;
  scr_Settings : CFDictionaryRef;
  scr_EraseClr : RGBColor;
  scr_Restore  : MacOSAll.Ptr;
  {$ENDIF}

implementation
uses
  zgl_main,
  zgl_application,
  zgl_window,
  zgl_opengl,
  zgl_opengl_simple,
  zgl_camera_2d,
  zgl_log,
  zgl_utils;

{$IFDEF WIN32}
function GetDisplayColors : Integer;
  var
    tHDC: hdc;
begin
  tHDC := GetDC( 0 );
  Result := GetDeviceCaps( tHDC, BITSPIXEL ) * GetDeviceCaps( tHDC, PLANES );
  ReleaseDC( 0, tHDC );
end;

function GetDisplayRefresh : Integer;
  var
    tHDC: hdc;
begin
  tHDC := GetDC( 0 );
  Result := GetDeviceCaps( tHDC, VREFRESH );
  ReleaseDC( 0, tHDC );
end;
{$ENDIF}

procedure scr_Init;
  {$IFDEF LINUX}
  var
    i, j : Integer;
  {$ENDIF}
begin
  scr_Initialized := TRUE;
{$IFDEF LINUX}
  log_Init;

  if Assigned( scr_Display ) Then
    XCloseDisplay( scr_Display );

  scr_Display := XOpenDisplay( nil );
  if not Assigned( scr_Display ) Then
    begin
      u_Error( 'Cannot connect to X server.' );
      exit;
    end;

  scr_Default := DefaultScreen( scr_Display );

  if XF86VidModeQueryExtension( scr_Display, @i, @j ) = 0 Then
    begin
      u_Error( 'XF86VidMode Extension not found' );
      exit;
    end else log_Add( 'XF86VidMode Extension - ok' );
  XF86VidModeGetAllModeLines( scr_Display, scr_Default, @scr_ModeCount, @scr_ModeList );
  XF86VidModeGetModeLine( scr_Display, scr_Default, @scr_Desktop.dotclock, PXF86VidModeModeLine( Pointer( @scr_Desktop ) + SizeOf( scr_Desktop.dotclock ) ) );
{$ENDIF}
{$IFDEF WIN32}
  with scr_Desktop do
    begin
      dmSize             := SizeOf( DEVMODE );
      dmPelsWidth        := GetSystemMetrics( SM_CXSCREEN );
      dmPelsHeight       := GetSystemMetrics( SM_CYSCREEN );
      dmBitsPerPel       := GetDisplayColors;
      dmDisplayFrequency := GetDisplayRefresh;
      dmFields           := DM_PELSWIDTH or DM_PELSHEIGHT or DM_BITSPERPEL or DM_DISPLAYFREQUENCY;
    end;
{$ENDIF}
{$IFDEF DARWIN}
  scr_Display  := CGMainDisplayID;
  scr_Desktop  := CGDisplayCurrentMode( scr_Display );
  scr_DesktopW := CGDisplayPixelsWide( scr_Display );
  scr_DesktopH := CGDisplayPixelsHigh( scr_Display );
{$ENDIF}
end;

function scr_Create;
  {$IFDEF LINUX}
  var
    i, j : Integer;
  {$ENDIF}
begin
  Result := FALSE;
{$IFDEF LINUX}
  scr_Init;

  if not glXQueryExtension( scr_Display, i, j ) Then
    begin
      u_Error( 'GLX Extension not found' );
      exit;
    end else log_Add( 'GLX Extension - ok' );

  app_XIM := XOpenIM( scr_Display, nil, nil, nil );
  if not Assigned( app_XIM ) Then
    log_Add( 'XOpenIM - Fail' )
  else
    log_Add( 'XOpenIM - ok' );

  app_XIC := XCreateIC( app_XIM, [ XNInputStyle, XIMPreeditNothing or XIMStatusNothing, 0 ] );
  if not Assigned( app_XIC ) Then
    log_Add( 'XCreateIC - Fail' )
  else
    log_Add( 'XCreateIC - ok' );

  ogl_zDepth := 24;
  repeat
    ogl_Attr[ 0 ]  := GLX_RGBA;
    ogl_Attr[ 1 ]  := GL_TRUE;
    ogl_Attr[ 2 ]  := GLX_RED_SIZE;
    ogl_Attr[ 3 ]  := 8;
    ogl_Attr[ 4 ]  := GLX_GREEN_SIZE;
    ogl_Attr[ 5 ]  := 8;
    ogl_Attr[ 6 ]  := GLX_BLUE_SIZE;
    ogl_Attr[ 7 ]  := 8;
    ogl_Attr[ 8 ]  := GLX_ALPHA_SIZE;
    ogl_Attr[ 9 ]  := 8;
    ogl_Attr[ 10 ] := GLX_DOUBLEBUFFER;
    ogl_Attr[ 11 ] := GL_TRUE;
    ogl_Attr[ 12 ] := GLX_DEPTH_SIZE;
    ogl_Attr[ 13 ] := ogl_zDepth;
    i := 14;
    if ogl_Stencil > 0 Then
      begin
        ogl_Attr[ i     ] := GLX_STENCIL_SIZE;
        ogl_Attr[ i + 1 ] := ogl_Stencil;
        INC( i, 2 );
      end;
    if ogl_FSAA > 0 Then
        begin
          ogl_Attr[ i     ] := GLX_SAMPLES_SGIS;
          ogl_Attr[ i + 1 ] := ogl_FSAA;
          INC( i, 2 );
        end;
    ogl_Attr[ i ] := None;

    log_Add( 'glXChooseVisual: zDepth = ' + u_IntToStr( ogl_zDepth ) + '; ' + 'stencil = ' + u_IntToStr( ogl_Stencil ) + '; ' + 'fsaa = ' + u_IntToStr( ogl_FSAA )  );
    ogl_VisualInfo := glXChooseVisual( scr_Display, scr_Default, @ogl_Attr[ 0 ] );
    if ( not Assigned( ogl_VisualInfo ) and ( ogl_zDepth = 1 ) ) Then
      begin
        if ogl_FSAA = 0 Then
          break
        else
          begin
            ogl_zDepth := 24;
            DEC( ogl_FSAA, 2 );
          end;
      end else
        if not Assigned( ogl_VisualInfo ) Then DEC( ogl_zDepth, 8 );
  if ogl_zDepth = 0 Then ogl_zDepth := 1;
  until Assigned( ogl_VisualInfo );

  if not Assigned( ogl_VisualInfo ) Then
    begin
      u_Error( 'Cannot choose pixel format.' );
      exit;
    end;

  ogl_zDepth := ogl_VisualInfo.depth;

  wnd_Root := RootWindow( scr_Display, ogl_VisualInfo.screen );
{$ENDIF}
{$IFDEF WIN32}
  scr_Init;
{$ENDIF}
{$IFDEF DARWIN}
  scr_Init;
{$ENDIF}
  log_Add( 'Current mode: ' + u_IntToStr( zgl_Get( DESKTOP_WIDTH ) ) + ' x ' + u_IntToStr( zgl_Get( DESKTOP_HEIGHT ) ) );
  scr_GetResList;
  Result := TRUE;
end;

procedure scr_GetResList;
  var
    i : Integer;
  {$IFDEF LINUX}
    tmp_Settings : TXF86VidModeModeInfo;
  {$ENDIF}
  {$IFDEF WIN32}
    tmp_Settings : DEVMODE;
  {$ENDIF}
  function Already( Width, Height : Integer ) : Boolean;
    var
      j : Integer;
  begin
    Result := FALSE;
    for j := 0 to scr_ResList.Count - 1 do
      if ( scr_ResList.Width[ j ] = Width ) and ( scr_ResList.Height[ j ] = Height ) Then Result := TRUE;
  end;
begin
{$IFDEF LINUX}
  for i := 0 to scr_ModeCount - 1 do
    begin
      tmp_Settings := PXF86VidModeModeInfo( Ptr( scr_ModeList^ ) + i * SizeOf( TXF86VidModeModeInfo ) )^;
      if not Already( tmp_Settings.hdisplay, tmp_Settings.vdisplay ) Then
        begin
          INC( scr_ResList.Count );
          SetLength( scr_ResList.Width, scr_ResList.Count );
          SetLength( scr_ResList.Height, scr_ResList.Count );
          scr_ResList.Width[ scr_ResList.Count - 1 ]  := tmp_Settings.hdisplay;
          scr_ResList.Height[ scr_ResList.Count - 1 ] := tmp_Settings.vdisplay;
        end;
    end;
{$ENDIF}
{$IFDEF WIN32}
  i := 0;
  while EnumDisplaySettings( nil, i, tmp_Settings ) <> FALSE do
    begin
      if not Already( tmp_Settings.dmPelsWidth, tmp_Settings.dmPelsHeight ) Then
        begin
          INC( scr_ResList.Count );
          SetLength( scr_ResList.Width, scr_ResList.Count );
          SetLength( scr_ResList.Height, scr_ResList.Count );
          scr_ResList.Width[ scr_ResList.Count - 1 ]  := tmp_Settings.dmPelsWidth;
          scr_ResList.Height[ scr_ResList.Count - 1 ] := tmp_Settings.dmPelsHeight;
        end;
      INC( i );
    end;
{$ENDIF}
end;

procedure scr_Destroy;
begin
{$IFDEF LINUX}
  scr_Reset;
  XFree( scr_ModeList );
  glXWaitX;
{$ENDIF}
{$IFDEF WIN32}
  scr_Reset;
{$ENDIF}
{$IFDEF DARWIN}
  scr_Reset;
{$ENDIF}
end;

procedure scr_Reset;
begin
{$IFDEF LINUX}
  XF86VidModeSwitchToMode( scr_Display, scr_Default, @scr_Desktop );
  XF86VidModeSetViewPort( scr_Display, scr_Default, 0, 0 );
  XUngrabKeyboard( scr_Display, CurrentTime );
  XUngrabPointer( scr_Display, CurrentTime );
  glXWaitX;
{$ENDIF}
{$IFDEF WIN32}
  ChangeDisplaySettings( DEVMODE( nil^ ), 0 );
{$ENDIF}
{$IFDEF DARWIN}
  CGDisplaySwitchToMode( scr_Display, scr_Desktop );
  //CGDisplayRelease( scr_Display );
{$ENDIF}
end;

procedure scr_Clear;
begin
  glClear( GL_COLOR_BUFFER_BIT   * Byte( app_Flags and COLOR_BUFFER_CLEAR > 0 ) or
           GL_DEPTH_BUFFER_BIT   * Byte( app_Flags and DEPTH_BUFFER_CLEAR > 0 ) or
           GL_STENCIL_BUFFER_BIT * Byte( app_Flags and STENCIL_BUFFER_CLEAR > 0 ) );
end;

procedure scr_Flush;
  var
    sync : LongWord;
begin
{$IFDEF LINUX}
  if scr_VSync Then
    begin
      if ogl_CanVSync2 Then
        glXSwapIntervalSGI( 1 )
      else
        if ogl_CanVSync Then
          begin
            glXGetVideoSyncSGI( sync );
            glXWaitVideoSyncSGI( 2, ( sync + 1 ) mod 2, sync );
          end;
      glFinish;
    end;

  glXSwapBuffers( scr_Display, wnd_Handle );
{$ENDIF}
{$IFDEF WIN32}
  if ogl_CanVSync Then
    begin
      sync := wglGetSwapIntervalEXT;
      if scr_VSync Then
        begin
          if sync = 0 Then
            wglSwapIntervalEXT( 1 );
          glFinish;
        end else
          if sync <> 0 Then
            wglSwapIntervalEXT( 0 );
    end;

  SwapBuffers( wnd_DC );
{$ENDIF}
{$IFDEF DARWIN}
//  glFinish;
  aglSwapBuffers( ogl_Context );
{$ENDIF}
end;

procedure scr_SetOptions;
  var
  {$IFDEF LINUX}
    modeToSet : Integer;
  {$ENDIF}
  {$IFDEF WIN32}
    i : Integer;
    r : Integer;
  {$ENDIF}
  {$IFDEF DARWIN}
    b  : Integer;
    dW : SInt16;
    dH : SInt16;
    nw : WindowRef;
  {$ENDIF}
begin
  scr_Changing   := TRUE;
  ogl_Width      := Width;
  ogl_Height     := Height;
  wnd_Width      := Width;
  wnd_Height     := Height;
  scr_Width      := Width;
  scr_Height     := Height;
  scr_BPP        := BPP;
  wnd_FullScreen := FullScreen;
  scr_Vsync      := VSync;
  if not app_Initialized Then exit;
  scr_SetVSync( scr_VSync );

  if Height >= zgl_Get( DESKTOP_HEIGHT ) Then
    wnd_FullScreen := TRUE;
  if wnd_FullScreen Then
    begin
      scr_Width  := Width;
      scr_Height := Height;
      scr_BPP    := BPP;
    end else
      begin
        scr_Width  := zgl_Get( DESKTOP_WIDTH );
        scr_Height := zgl_Get( DESKTOP_HEIGHT );
        {$IFDEF LINUX}
        scr_BPP := BPP;
        {$ENDIF}
        {$IFDEF WIN32}
        scr_BPP     := GetDisplayColors;
        scr_Refresh := GetDisplayRefresh;
        {$ENDIF}
      end;
{$IFDEF LINUX}
  for modeToSet := 0 to scr_ModeCount - 1 do
    begin
      scr_Settings := PXF86VidModeModeInfo( Ptr( scr_ModeList^ ) + modeToSet * SizeOf( TXF86VidModeModeInfo ) )^;
      if ( scr_Settings.hdisplay = scr_Width ) and ( scr_Settings.vdisplay = scr_Height ) Then break;
    end;
  if ( scr_Settings.hdisplay <> scr_Width ) or ( scr_Settings.vdisplay <> scr_Height ) Then
    begin
      log_Add( 'Cannot find mode to set...' );
      exit;
    end;

  if ( wnd_FullScreen ) and
     ( scr_Settings.hdisplay <> scr_Desktop.hDisplay ) and
     ( scr_Settings.vdisplay <> scr_Desktop.vDisplay ) Then
    begin
      XF86VidModeSwitchToMode( scr_Display, scr_Default, @scr_Settings );
      XF86VidModeSetViewPort( scr_Display, scr_Default, 0, 0 );
    end else
      begin
        scr_Reset;
        XMapWindow( scr_Display, wnd_Handle );
      end;
{$ENDIF}
{$IFDEF WIN32}
  if wnd_FullScreen Then
    begin
      i := 0;
      r := 0;
      while EnumDisplaySettings( nil, i, scr_Settings ) <> FALSE do
        with scr_Settings do
          begin
            dmSize   := SizeOf( DEVMODE );
            dmFields := DM_PELSWIDTH or DM_PELSHEIGHT or DM_BITSPERPEL or DM_DISPLAYFREQUENCY;
            if ( dmPelsWidth  = scr_Width  ) and
               ( dmPelsHeight = scr_Height ) and
               ( dmBitsPerPel = scr_BPP    ) and
               ( dmDisplayFrequency > r    ) and
               ( dmDisplayFrequency <= scr_Desktop.dmDisplayFrequency ) Then
              begin
                if ( ChangeDisplaySettings( scr_Settings, CDS_TEST or CDS_FULLSCREEN ) = DISP_CHANGE_SUCCESSFUL ) Then
                  r := dmDisplayFrequency
                else
                  break;
              end;
            INC( i );
          end;

      with scr_Settings do
        begin
          dmSize := SizeOf( DEVMODE );
          if scr_Refresh = REFRESH_MAXIMUM Then scr_Refresh := r;
          if scr_Refresh = REFRESH_DEFAULT Then scr_Refresh := 0;

          dmPelsWidth        := scr_Width;
          dmPelsHeight       := scr_Height;
          dmBitsPerPel       := scr_BPP;
          dmDisplayFrequency := scr_Refresh;
          dmFields           := DM_PELSWIDTH or DM_PELSHEIGHT or DM_BITSPERPEL or DM_DISPLAYFREQUENCY;
        end;

      if ChangeDisplaySettings( scr_Settings, CDS_TEST or CDS_FULLSCREEN ) <> DISP_CHANGE_SUCCESSFUL Then
        begin
          u_Warning( 'Cannot set fullscreen mode.' );
          wnd_FullScreen := FALSE;
        end else
          ChangeDisplaySettings( scr_Settings, CDS_FULLSCREEN )
    end else
      scr_Reset;
{$ENDIF}
{$IFDEF DARWIN}
  //CGDisplayCapture( scr_Display );
  if scr_Refresh <> 0 Then
    begin
      scr_Settings := CGDisplayBestModeForParametersAndRefreshRate( scr_Display,
                                                                    scr_BPP,
                                                                    scr_Width, scr_Height,
                                                                    scr_Refresh,
                                                                    b );
      scr_Refresh := b;
    end;
  if scr_Refresh = 0 Then
    scr_Settings := CGDisplayBestModeForParameters( scr_Display, scr_BPP, scr_Width, scr_Height, b );

  if b = 1 Then
    CGDisplaySwitchToMode( scr_Display, scr_Settings )
  else
    begin
      u_Warning( 'Cannot set fullscreen mode.' );
      wnd_FullScreen := FALSE;
    end;

  if wnd_FullScreen Then
    HideMenuBar
  else
    ShowMenuBar;
{$ENDIF}
  if wnd_FullScreen Then
    log_Add( 'Set screen options: ' + u_IntToStr( scr_Width ) + ' x ' + u_IntToStr( scr_Height ) + ' x ' + u_IntToStr( scr_BPP ) + 'bpp fullscreen' )
  else
    log_Add( 'Set screen options: ' + u_IntToStr( wnd_Width ) + ' x ' + u_IntToStr( wnd_Height ) + ' x ' + u_IntToStr( scr_BPP ) + 'bpp windowed' );
  if app_Work Then
    wnd_Update;
end;

procedure scr_CorrectResolution;
begin
  scr_ResW  := Width;
  scr_ResH  := Height;
  scr_ResCX := wnd_Width  / Width;
  scr_ResCY := wnd_Height / Height;

  if scr_ResCX < scr_ResCY Then
    begin
      scr_AddCX := 0;
      scr_AddCY := Round( ( wnd_Height - Height * scr_ResCX ) / 2 );
      scr_ResCY := scr_ResCX;
    end else
      begin
        scr_AddCX := Round( ( wnd_Width - Width * scr_ResCY ) / 2 );
        scr_AddCY := 0;
        scr_ResCX := scr_ResCY;
      end;

  if app_Flags and CORRECT_HEIGHT = 0 Then
    begin
      scr_ResCY := wnd_Height / Height;
      scr_AddCY := 0;
    end;
  if app_Flags and CORRECT_WIDTH = 0 Then
    begin
      scr_ResCX := wnd_Width / Width;
      scr_AddCX := 0;
    end;

  ogl_Width  := Round( wnd_Width / scr_ResCX );
  ogl_Height := Round( wnd_Height / scr_ResCY );
  scr_SubCX  := ogl_Width - Width;
  scr_SubCY  := ogl_Height - Height;
  SetCurrentMode;

  cam2dZoomX := cam2dGlobal.Zoom.X;
  cam2dZoomY := cam2dGlobal.Zoom.Y;
  cam2dSX    := Round( -ogl_Width / 2 + scr_AddCX / scr_ResCX );
  cam2dSY    := Round( -ogl_Height / 2 + scr_AddCY / scr_ResCY );
  ogl_CropR  := Round( sqrt( sqr( ogl_CropW / scr_ResCX / cam2dZoomX ) + sqr( ogl_CropH / scr_ResCY / cam2dZoomY ) ) ) div 2;
end;

procedure scr_SetViewPort;
begin
  if ( ogl_Mode <> 2 ) and ( ogl_Mode <> 3 ) Then exit;

  if ( app_Flags and CORRECT_RESOLUTION > 0 ) and ( ogl_Mode = 2 ) Then
    begin
      ogl_CropX := 0;
      ogl_CropY := 0;
      ogl_CropW := wnd_Width - scr_AddCX * 2;
      ogl_CropH := wnd_Height - scr_AddCY * 2;
      glViewPort( scr_AddCX, scr_AddCY, ogl_CropW, ogl_CropH );
    end else
      begin
        ogl_CropX := 0;
        ogl_CropY := 0;
        ogl_CropW := wnd_Width;
        ogl_CropH := wnd_Height;
        glViewPort( 0, 0, ogl_CropW, ogl_CropH );
      end;
end;

procedure scr_SetVSync;
begin
  scr_VSync := VSync;
{$IFDEF DARWIN}
  if Assigned( ogl_Context ) Then
    aglSetInt( ogl_Context, AGL_SWAP_INTERVAL, Byte( scr_VSync ) );
{$ENDIF}
end;

procedure scr_SetFSAA;
begin
  if ogl_FSAA = FSAA Then exit;
  ogl_FSAA := FSAA;

{$IFDEF LINUX}
  XFree( scr_ModeList );
  scr_Destroy;
  scr_Create;
{$ENDIF}

  gl_Destroy;
  wnd_Update;
  gl_Create;
  if ogl_FSAA <> 0 Then
    log_Add( 'Set FSAA: ' + u_IntToStr( ogl_FSAA ) + 'x' )
  else
    log_Add( 'Set FSAA: off' );
end;

end.
