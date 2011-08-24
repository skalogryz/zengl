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
unit zgl_screen;

{$I zgl_config.cfg}
{$IFDEF iOS}
  {$modeswitch objectivec1}
{$ENDIF}

interface
uses
  {$IFDEF LINUX}
  X, XLib, XRandr, UnixType
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows
  {$ENDIF}
  {$IFDEF MACOSX}
  MacOSAll
  {$ENDIF}
  {$IFDEF iOS}
  iPhoneAll
  {$ENDIF}
  ;

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

procedure scr_SetWindowedMode;
procedure scr_SetOptions( Width, Height, Refresh : Word; FullScreen, VSync : Boolean );
procedure scr_CorrectResolution( Width, Height : Word );
procedure scr_SetViewPort;
procedure scr_SetVSync( VSync : Boolean );
procedure scr_SetFSAA( FSAA : Byte );
procedure scr_ReadPixels( var pData : Pointer; X, Y, Width, Height : Word );

{$IFDEF LINUX}
function XOpenIM(para1:PDisplay; para2:PXrmHashBucketRec; para3:Pchar; para4:Pchar):PXIM;cdecl;external;
function XCloseIM(im : PXIM) : TStatus;cdecl;external;
function XCreateIC(para1 : PXIM; para2 : array of const):PXIC;cdecl;external;
procedure XDestroyIC(ic : PXIC);cdecl;external;
{$ENDIF}
{$IFDEF WINDOWS}
const
  MONITOR_DEFAULTTOPRIMARY = $00000001;

type
  HMONITOR = THANDLE;
  MONITORINFOEX = record
    cbSize    : LongWord;
    rcMonitor : TRect;
    rcWork    : TRect;
    dwFlags   : LongWord;
    szDevice  : array[ 0..CCHDEVICENAME - 1 ] of WideChar;
  end;
{$ENDIF}
{$IFDEF WINDESKTOP}
function MonitorFromWindow( hwnd : HWND; dwFlags : LongWord ) : THandle; stdcall; external 'user32.dll';
function GetMonitorInfoW( monitor : HMONITOR; var moninfo : MONITORINFOEX ) : BOOL; stdcall; external 'user32.dll';
{$ENDIF}
{$IFDEF WINCE}
function MonitorFromWindow( hwnd : HWND; dwFlags : LongWord ) : THandle; stdcall; external 'coredll.dll';
function GetMonitorInfoW( monitor : HMONITOR; var moninfo : MONITORINFOEX ) : BOOL; stdcall; external 'coredll.dll' name 'GetMonitorInfo';
function ChangeDisplaySettingsExW( lpszDeviceName : PWideChar; lpDevMode : DEVMODEW; handle : HWND; dwflags : DWORD; lParam : Pointer ) : LongInt; stdcall; external 'coredll.dll' name 'ChangeDisplaySettingsEx';
{$ENDIF}

type
  zglPResolutionList = ^zglTResolutionList;
  zglTResolutionList = record
    Count  : Integer;
    Width  : array of Integer;
    Height : array of Integer;
end;

var
  scrWidth       : Integer = 800;
  scrHeight      : Integer = 600;
  scrRefresh     : Integer;
  scrVSync       : Boolean;
  scrResList     : zglTResolutionList;
  scrInitialized : Boolean;
  scrChanging    : Boolean;

  // Resolution Correct
  scrResW  : Integer;
  scrResH  : Integer;
  scrResCX : Single  = 1;
  scrResCY : Single  = 1;
  scrAddCX : Integer = 0;
  scrAddCY : Integer = 0;
  scrSubCX : Integer = 0;
  scrSubCY : Integer = 0;

  {$IFDEF LINUX}
  scrDisplay   : PDisplay;
  scrDefault   : cint;
  scrSettings  : Pointer;
  scrDesktop   : LongInt;
  scrCurrent   : LongInt;
  scrModeCount : LongInt;
  scrModeList  : PXRRScreenSize;
  {$ENDIF}
  {$IFDEF WINDOWS}
  scrSettings : DEVMODEW;
  scrDesktop  : DEVMODEW;
  scrMonitor  : HMONITOR;
  scrMonInfo  : MONITORINFOEX;
  {$ENDIF}
  {$IFDEF MACOSX}
  scrDisplay   : CGDirectDisplayID;
  scrDesktop   : CFDictionaryRef;
  scrDesktopW  : Integer;
  scrDesktopH  : Integer;
  scrSettings  : CFDictionaryRef;
  scrModeCount : CFIndex;
  scrModeList  : CFArrayRef;
  {$ENDIF}
  {$IFDEF iOS}
  scrDisplayLink  : CADisplayLink;
  scrCurrentModeW : Integer;
  scrCurrentModeH : Integer;
  scrDesktopW     : Integer;
  scrDesktopH     : Integer;
  scrOrientation  : UIInterfaceOrientation;
  scrAngle        : Integer;
  scrCanLandscape : Boolean = TRUE;
  scrCanPortrait  : Boolean = TRUE;
  {$ENDIF}

implementation
uses
  zgl_main,
  zgl_application,
  zgl_window,
  {$IFNDEF USE_GLES}
  zgl_opengl,
  zgl_opengl_all,
  {$ELSE}
  zgl_opengles,
  zgl_opengles_all,
  {$ENDIF}
  zgl_opengl_simple,
  zgl_render_2d,
  zgl_camera_2d,
  zgl_log,
  zgl_utils;

{$IFDEF WINDOWS}
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
    rotation : Word;
  {$ENDIF}
begin
{$IFDEF LINUX}
  log_Init();

  if Assigned( scrDisplay ) Then
    XCloseDisplay( scrDisplay );

  scrDisplay := XOpenDisplay( nil );
  if not Assigned( scrDisplay ) Then
    begin
      u_Error( 'Cannot connect to X server.' );
      exit;
    end;

  scrDefault := DefaultScreen( scrDisplay );
  wndRoot    := DefaultRootWindow( scrDisplay );

  scrModeList := XRRSizes( scrDisplay, XRRRootToScreen( scrDisplay, wndRoot ), @scrModeCount );
  scrSettings := XRRGetScreenInfo( scrDisplay, wndRoot );
  scrDesktop  := XRRConfigCurrentConfiguration( scrSettings, @rotation );
{$ENDIF}
{$IFDEF WINDOWS}
  scrMonitor := MonitorFromWindow( wndHandle, MONITOR_DEFAULTTOPRIMARY );
  FillChar( scrMonInfo, SizeOf( MONITORINFOEX ), 0 );
  scrMonInfo.cbSize := SizeOf( MONITORINFOEX );
  GetMonitorInfoW( scrMonitor, scrMonInfo );

  FillChar( scrDesktop, SizeOf( DEVMODEW ), 0 );
  with scrDesktop do
    begin
      dmSize             := SizeOf( DEVMODEW );
      dmPelsWidth        := GetSystemMetrics( SM_CXSCREEN );
      dmPelsHeight       := GetSystemMetrics( SM_CYSCREEN );
      dmBitsPerPel       := GetDisplayColors();
      dmDisplayFrequency := GetDisplayRefresh();
      dmFields           := DM_PELSWIDTH or DM_PELSHEIGHT or DM_BITSPERPEL or DM_DISPLAYFREQUENCY;
    end;
{$ENDIF}
{$IFDEF MACOSX}
  scrDisplay  := CGMainDisplayID();
  scrDesktop  := CGDisplayCurrentMode( scrDisplay );
  scrDesktopW := CGDisplayPixelsWide( scrDisplay );
  scrDesktopH := CGDisplayPixelsHigh( scrDisplay );

  scrModeList  := CGDisplayAvailableModes( scrDisplay );
  scrModeCount := CFArrayGetCount( scrModeList );
{$ENDIF}
{$IFDEF iOS}
  app_InitPool();
  scrOrientation := UIApplication.sharedApplication.statusBarOrientation();
  wndPortrait    := scrCanPortrait;

  if ( UIDevice.currentDevice.systemVersion.floatValue() >= 3.2 ) Then
    begin
      // magic...
      {$IFNDEF iPhoneSim}
      scrCurrentModeW := Round( UIScreen.mainScreen.currentMode.size.height );
      scrCurrentModeH := Round( UIScreen.mainScreen.currentMode.size.width );
      {$ELSE}
      scrCurrentModeW := Round( UIScreen.mainScreen.currentMode.size.width );
      scrCurrentModeH := Round( UIScreen.mainScreen.currentMode.size.height );
      {$ENDIF}
    end else
      begin
        scrCurrentModeW := Round( UIScreen.mainScreen.bounds.size.width );
        scrCurrentModeH := Round( UIScreen.mainScreen.bounds.size.height );
      end;

  if scrCanPortrait Then
    begin
      scrDesktopW := scrCurrentModeW;
      scrDesktopH := scrCurrentModeH;
    end else
      begin
        scrDesktopW := scrCurrentModeH;
        scrDesktopH := scrCurrentModeW;
      end;
{$ENDIF}
  scrInitialized := TRUE;
end;

function scr_Create : Boolean;
begin
  Result := FALSE;

  scr_Init();
{$IFDEF LINUX}
  if DefaultDepth( scrDisplay, scrDefault ) < 24 Then
    begin
      u_Error( 'DefaultDepth not set to 24-bit.' );
      zgl_Exit();
      exit;
    end;

  appXIM := XOpenIM( scrDisplay, nil, nil, nil );
  if not Assigned( appXIM ) Then
    log_Add( 'XOpenIM - Fail' )
  else
    log_Add( 'XOpenIM - ok' );

  appXIC := XCreateIC( appXIM, [ XNInputStyle, XIMPreeditNothing or XIMStatusNothing, 0 ] );
  if not Assigned( appXIC ) Then
    log_Add( 'XCreateIC - Fail' )
  else
    log_Add( 'XCreateIC - ok' );
{$ENDIF}
{$IFDEF WINDOWS}
  if ( not wndFullScreen ) and ( scrDesktop.dmBitsPerPel <> 32 ) Then
    scr_SetWindowedMode();
{$ENDIF}
{$IFDEF MACOSX}
  if CGDisplayBitsPerPixel( scrDisplay ) <> 32 Then
    begin
      u_Error( 'Desktop not set to 32-bit mode.' );
      zgl_Exit();
      exit;
    end;
{$ENDIF}
  log_Add( 'Current mode: ' + u_IntToStr( zgl_Get( DESKTOP_WIDTH ) ) + ' x ' + u_IntToStr( zgl_Get( DESKTOP_HEIGHT ) ) );
  scr_GetResList();
  Result := TRUE;
end;

procedure scr_GetResList;
  var
    i : Integer;
  {$IFDEF LINUX}
    tmpSettings : PXRRScreenSize;
  {$ENDIF}
  {$IFDEF WINDOWS}
    tmpSettings : DEVMODEW;
  {$ENDIF}
  {$IFDEF MACOSX}
    tmpSettings   : UnivPtr;
    width, height : Integer;
  {$ENDIF}
  function Already( Width, Height : Integer ) : Boolean;
    var
      j : Integer;
  begin
    Result := FALSE;
    for j := 0 to scrResList.Count - 1 do
      if ( scrResList.Width[ j ] = Width ) and ( scrResList.Height[ j ] = Height ) Then Result := TRUE;
  end;
begin
{$IFDEF LINUX}
  tmpSettings := scrModeList;
  for i := 0 to scrModeCount - 1 do
    begin
      if not Already( tmpSettings.Width, tmpSettings.Height ) Then
        begin
          INC( scrResList.Count );
          SetLength( scrResList.Width, scrResList.Count );
          SetLength( scrResList.Height, scrResList.Count );
          scrResList.Width[ scrResList.Count - 1 ]  := tmpSettings.Width;
          scrResList.Height[ scrResList.Count - 1 ] := tmpSettings.Height;
        end;
      INC( tmpSettings );
    end;
{$ENDIF}
{$IFDEF WINDOWS}
  i := 0;
  FillChar( tmpSettings, SizeOf( DEVMODEW ), 0 );
  tmpSettings.dmSize := SizeOf( DEVMODEW );
  while EnumDisplaySettingsW( scrMonInfo.szDevice, i, tmpSettings ) <> FALSE do
    begin
      if not Already( tmpSettings.dmPelsWidth, tmpSettings.dmPelsHeight ) Then
        begin
          INC( scrResList.Count );
          SetLength( scrResList.Width, scrResList.Count );
          SetLength( scrResList.Height, scrResList.Count );
          scrResList.Width[ scrResList.Count - 1 ]  := tmpSettings.dmPelsWidth;
          scrResList.Height[ scrResList.Count - 1 ] := tmpSettings.dmPelsHeight;
        end;
      INC( i );
    end;
{$ENDIF}
{$IFDEF MACOSX}
  tmpSettings := scrModeList;
  for i := 0 to scrModeCount - 1 do
    begin
      tmpSettings := CFArrayGetValueAtIndex( scrModeList, i );
      CFNumberGetValue( CFDictionaryGetValue( tmpSettings, CFSTRP('Width') ), kCFNumberIntType, @width );
      CFNumberGetValue( CFDictionaryGetValue( tmpSettings, CFSTRP('Height') ), kCFNumberIntType, @height );
      if not Already( width, height ) Then
        begin
          INC( scrResList.Count );
          SetLength( scrResList.Width, scrResList.Count );
          SetLength( scrResList.Height, scrResList.Count );
          scrResList.Width[ scrResList.Count - 1 ]  := width;
          scrResList.Height[ scrResList.Count - 1 ] := height;
        end;
    end;
{$ENDIF}
{$IFDEF iOS}
  scrResList.Count := 1;
  SetLength( scrResList.Width, 1 );
  SetLength( scrResList.Height, 1 );
  scrResList.Width[ 0 ] := scrDesktopW;
  scrResList.Height[ 0 ] := scrDesktopH;
{$ENDIF}
end;

procedure scr_Destroy;
begin
  scr_Reset();
  {$IFDEF LINUX}
  XRRFreeScreenConfigInfo( scrSettings );

  XDestroyIC( appXIC );
  XCloseIM( appXIM );
  {$ENDIF}

  scrResList.Count := 0;
  SetLength( scrResList.Width, 0 );
  SetLength( scrResList.Height, 0 );

  scrInitialized := FALSE;
end;

procedure scr_Reset;
begin
  scrChanging := TRUE;
{$IFDEF LINUX}
  XRRSetScreenConfig( scrDisplay, scrSettings, wndRoot, scrDesktop, 1, 0 );
{$ENDIF}
{$IFDEF WINDOWS}
  ChangeDisplaySettingsExW( scrMonInfo.szDevice, {$IFDEF WINDESKTOP}DEVMODEW( nil^ ){$ELSE}scrDesktop{$ENDIF}, 0, CDS_FULLSCREEN, nil );
{$ENDIF}
{$IFDEF MACOSX}
  CGDisplaySwitchToMode( scrDisplay, scrDesktop );
  //CGDisplayRelease( scrDisplay );
{$ENDIF}
end;

procedure scr_Clear;
begin
  batch2d_Flush();
  glClear( GL_COLOR_BUFFER_BIT * Byte( appFlags and COLOR_BUFFER_CLEAR > 0 ) or GL_DEPTH_BUFFER_BIT * Byte( appFlags and DEPTH_BUFFER_CLEAR > 0 ) or
           GL_STENCIL_BUFFER_BIT * Byte( appFlags and STENCIL_BUFFER_CLEAR > 0 ) );
end;

procedure scr_Flush;
begin
  batch2d_Flush();
{$IFNDEF USE_GLES}
  {$IFDEF LINUX}
  glXSwapBuffers( scrDisplay, wndHandle );
  {$ENDIF}
  {$IFDEF WINDOWS}
  SwapBuffers( wndDC );
  {$ENDIF}
  {$IFDEF MACOSX}
  aglSwapBuffers( oglContext );
  {$ENDIF}
{$ELSE}
  {$IFNDEF iOS}
  eglSwapBuffers( oglDisplay, oglSurface );
  {$ELSE}
  eglContext.presentRenderbuffer( GL_RENDERBUFFER );
  {$ENDIF}
{$ENDIF}
end;

procedure scr_SetWindowedMode;
  {$IFDEF WINDOWS}
  var
    settings : DEVMODEW;
  {$ENDIF}
begin
  {$IFDEF LINUX}
  scr_Reset();
  XMapWindow( scrDisplay, wndHandle );
  {$ENDIF}
  {$IFDEF WINDOWS}
  if scrDesktop.dmBitsPerPel <> 32 Then
    begin
      settings              := scrDesktop;
      settings.dmBitsPerPel := 32;

      if ChangeDisplaySettingsExW( scrMonInfo.szDevice, settings, 0, CDS_TEST, nil ) <> DISP_CHANGE_SUCCESSFUL Then
        begin
          u_Error( 'Desktop doesn''t support 32-bit color mode.' );
          zgl_Exit();
        end else
          ChangeDisplaySettingsExW( scrMonInfo.szDevice, settings, 0, CDS_FULLSCREEN, nil );
    end else
      scr_Reset();
  {$ENDIF}
  {$IFDEF MACOSX}
  scr_Reset();
  ShowMenuBar();
  {$ENDIF}
end;

procedure scr_SetOptions( Width, Height, Refresh : Word; FullScreen, VSync : Boolean );
  {$IFDEF LINUX}
  var
    modeToSet : Integer;
    mode      : PXRRScreenSize;
  {$ENDIF}
  {$IFDEF WINDOWS}
  var
    i : Integer;
    r : Integer;
  {$ENDIF}
  {$IFDEF MACOSX}
  var
    b : Integer;
  {$ENDIF}
begin
  scrChanging   := TRUE;
  oglWidth      := Width;
  oglHeight     := Height;
  oglTargetW    := Width;
  oglTargetH    := Height;
  wndWidth      := Width;
  wndHeight     := Height;
  wndFullScreen := FullScreen;
  scrVsync      := VSync;

  if Height >= zgl_Get( DESKTOP_HEIGHT ) Then
    wndFullScreen := TRUE;
  if wndFullScreen Then
    begin
      scrWidth  := Width;
      scrHeight := Height;
    end else
      begin
        scrWidth  := zgl_Get( DESKTOP_WIDTH );
        scrHeight := zgl_Get( DESKTOP_HEIGHT );
        {$IFDEF WINDOWS}
        scrRefresh := GetDisplayRefresh();
        {$ENDIF}
      end;

  if not appInitialized Then exit;
  scr_SetVSync( scrVSync );
{$IFDEF LINUX}
  if wndFullScreen Then
    begin
      scrCurrent := -1;
      mode       := scrModeList;

      for modeToSet := 0 to scrModeCount - 1 do
        if ( mode.Width = scrWidth ) and ( mode.Height = scrHeight ) Then
          begin
            scrCurrent := modeToSet;
            break;
          end else
            INC( mode );

      if scrCurrent = -1 Then
        begin
          u_Warning( 'Cannot set fullscreen mode.' );
          scrCurrent    := scrDesktop;
          wndFullScreen := FALSE;
        end;
      XRRSetScreenConfig( scrDisplay, scrSettings, wndRoot, scrCurrent, 1, 0 );
    end else
      scr_SetWindowedMode();
{$ENDIF}
{$IFDEF WINDOWS}
  if wndFullScreen Then
    begin
      i := 0;
      r := 0;
      FillChar( scrSettings, SizeOf( DEVMODEW ), 0 );
      scrSettings.dmSize := SizeOf( DEVMODEW );
      while EnumDisplaySettingsW( scrMonInfo.szDevice, i, scrSettings ) <> FALSE do
        with scrSettings do
          begin
            dmFields := DM_PELSWIDTH or DM_PELSHEIGHT or DM_BITSPERPEL or DM_DISPLAYFREQUENCY;
            if ( dmPelsWidth = scrWidth  ) and ( dmPelsHeight = scrHeight ) and ( dmBitsPerPel = 32 ) and ( dmDisplayFrequency > r ) and
               ( dmDisplayFrequency <= scrDesktop.dmDisplayFrequency ) Then
              begin
                if ChangeDisplaySettingsExW( scrMonInfo.szDevice, scrSettings, 0, CDS_TEST, nil ) = DISP_CHANGE_SUCCESSFUL Then
                  r := dmDisplayFrequency
                else
                  break;
              end;
            INC( i );
          end;

      with scrSettings do
        begin
          if scrRefresh = REFRESH_MAXIMUM Then scrRefresh := r;
          if scrRefresh = REFRESH_DEFAULT Then scrRefresh := 0;

          dmPelsWidth        := scrWidth;
          dmPelsHeight       := scrHeight;
          dmBitsPerPel       := 32;
          dmDisplayFrequency := scrRefresh;
          dmFields           := DM_PELSWIDTH or DM_PELSHEIGHT or DM_BITSPERPEL or DM_DISPLAYFREQUENCY;
        end;

      if ChangeDisplaySettingsExW( scrMonInfo.szDevice, scrSettings, 0, CDS_TEST, nil ) <> DISP_CHANGE_SUCCESSFUL Then
        begin
          u_Warning( 'Cannot set fullscreen mode.' );
          wndFullScreen := FALSE;
        end else
          ChangeDisplaySettingsExW( scrMonInfo.szDevice, scrSettings, 0, CDS_FULLSCREEN, nil );
    end else
      scr_SetWindowedMode();
{$ENDIF}
{$IFDEF MACOSX}
  if wndFullScreen Then
    begin
      //CGDisplayCapture( scrDisplay );
      if scrRefresh <> 0 Then
        begin
          scrSettings := CGDisplayBestModeForParametersAndRefreshRate( scrDisplay, 32, scrWidth, scrHeight, scrRefresh, b );
          scrRefresh  := b;
        end;
      if scrRefresh = 0 Then
        scrSettings := CGDisplayBestModeForParameters( scrDisplay, 32, scrWidth, scrHeight, b );

      if b = 1 Then
        CGDisplaySwitchToMode( scrDisplay, scrSettings )
      else
        begin
          u_Warning( 'Cannot set fullscreen mode.' );
          wndFullScreen := FALSE;
        end;

      HideMenuBar();
    end else
      scr_SetWindowedMode();
{$ENDIF}
  if wndFullScreen Then
    log_Add( 'Screen options changed to: ' + u_IntToStr( scrWidth ) + ' x ' + u_IntToStr( scrHeight ) + ' fullscreen' )
  else
    log_Add( 'Screen options changed to: ' + u_IntToStr( wndWidth ) + ' x ' + u_IntToStr( wndHeight ) + ' windowed' );
  if appWork Then
    wnd_Update();
end;

procedure scr_CorrectResolution( Width, Height : Word );
begin
  scrResW  := Width;
  scrResH  := Height;
  scrResCX := wndWidth  / Width;
  scrResCY := wndHeight / Height;

  if scrResCX < scrResCY Then
    begin
      scrAddCX := 0;
      scrAddCY := Round( ( wndHeight - Height * scrResCX ) / 2 );
      scrResCY := scrResCX;
    end else
      begin
        scrAddCX := Round( ( wndWidth - Width * scrResCY ) / 2 );
        scrAddCY := 0;
        scrResCX := scrResCY;
      end;

  if appFlags and CORRECT_HEIGHT = 0 Then
    begin
      scrResCY := wndHeight / Height;
      scrAddCY := 0;
    end;
  if appFlags and CORRECT_WIDTH = 0 Then
    begin
      scrResCX := wndWidth / Width;
      scrAddCX := 0;
    end;

  oglWidth  := Round( wndWidth / scrResCX );
  oglHeight := Round( wndHeight / scrResCY );
  scrSubCX  := oglWidth - Width;
  scrSubCY  := oglHeight - Height;
  SetCurrentMode();
end;

procedure scr_SetViewPort;
begin
  if oglTarget = TARGET_SCREEN Then
    begin
      if ( appFlags and CORRECT_RESOLUTION > 0 ) and ( oglMode = 2 ) Then
        begin
          oglClipX := 0;
          oglClipY := 0;
          oglClipW := wndWidth - scrAddCX * 2;
          oglClipH := wndHeight - scrAddCY * 2;
          {$IFNDEF iOS}
          glViewPort( scrAddCX, scrAddCY, oglClipW, oglClipH );
          {$ELSE}
          if ( wndPortrait ) or ( not scrCanPortrait ) Then
            glViewPort( scrAddCX, scrAddCY, oglClipW, oglClipH )
          else
            glViewPort( scrAddCY, scrAddCX, oglClipH, oglClipW );
          {$ENDIF}
        end else
          begin
            oglClipX := 0;
            oglClipY := 0;
            oglClipW := wndWidth;
            oglClipH := wndHeight;
            {$IFNDEF iOS}
            glViewPort( 0, 0, oglClipW, oglClipH );
            {$ELSE}
            if ( wndPortrait ) or ( not scrCanPortrait ) Then
              glViewPort( 0, 0, oglClipW, oglClipH )
            else
              glViewPort( 0, 0, oglClipH, oglClipW );
            {$ENDIF}
          end;
    end else
      begin
        oglClipX := 0;
        oglClipY := 0;
        oglClipW := oglWidth;
        oglClipH := oglHeight;
        glViewPort( 0, 0, oglTargetW, oglTargetH );
      end;
end;

procedure scr_SetVSync( VSync : Boolean );
begin
  scrVSync := VSync;
{$IFNDEF USE_GLES}
  {$IFDEF LINUX}
  if oglCanVSync Then
    glXSwapIntervalSGI( Integer( scrVSync ) );
  {$ENDIF}
  {$IFDEF WINDOWS}
  if oglCanVSync Then
    wglSwapInterval( Integer( scrVSync ) );
  {$ENDIF}
  {$IFDEF MACOSX}
  if Assigned( oglContext ) Then
    aglSetInt( oglContext, AGL_SWAP_INTERVAL, Byte( scrVSync ) );
  {$ENDIF}
{$ELSE}
  {$IFNDEF iOS}
  if oglCanVSync Then
    eglSwapInterval( oglDisplay, Integer( scrVSync ) );
  {$ELSE}
  {$ENDIF}
{$ENDIF}
end;

procedure scr_SetFSAA( FSAA : Byte );
begin
  if oglFSAA = FSAA Then exit;
  oglFSAA := FSAA;

  gl_Destroy();
  log_Add( 'Start reinitialization of OpenGL' );

  gl_Create();
  wnd_Destroy();
  wnd_Create( wndWidth, wndHeight );
  wnd_SetCaption( wndCaption );
  gl_Initialize();
  if oglFSAA <> 0 Then
    log_Add( 'FSAA changed to: ' + u_IntToStr( oglFSAA ) + 'x' )
  else
    log_Add( 'FSAA changed to: off' );
end;

procedure scr_ReadPixels( var pData : Pointer; X, Y, Width, Height : Word );
begin
  batch2d_Flush();
  GetMem( pData, Width * Height * 4 );
  glReadPixels( X, oglClipH - Height - Y, Width, Height, GL_RGBA, GL_UNSIGNED_BYTE, pData );
end;

end.
