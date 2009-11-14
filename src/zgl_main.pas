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
unit zgl_main;

{$I zgl_config.cfg}

interface
uses
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  {$IFDEF DARWIN}
  MacOSAll,
  {$ENDIF}
  zgl_types;

const
  cs_ZenGL = 'ZenGL 0.1.40';

  // zgl_Reg
  SYS_LOAD               = $000001;
  SYS_DRAW               = $000002;
  SYS_UPDATE             = $000003;
  SYS_EXIT               = $000004;
  SYS_ACTIVATE           = $000005;
  TEX_FORMAT_EXTENSION   = $000010;
  TEX_FORMAT_FILE_LOADER = $000011;
  TEX_FORMAT_MEM_LOADER  = $000012;
  SND_FORMAT_EXTENSION   = $000020;
  SND_FORMAT_FILE_LOADER = $000021;
  SND_FORMAT_MEM_LOADER  = $000022;
  SND_FORMAT_DECODER     = $000023;
  WIDGET_TYPE_ID         = $000030;
  WIDGET_FILL_DESC       = $000031;
  WIDGET_ONDRAW          = $000032;
  WIDGET_ONPROC          = $000033;

  // zgl_Get
  SYS_FPS         = 1;
  APP_PAUSED      = 2;
  APP_DIRECTORY   = 3;
  USR_HOMEDIR     = 4;
  LOG_FILENAME    = 5;
  ZGL_VERSION     = 6;
  SCR_ADD_X       = 7;
  SCR_ADD_Y       = 8;
  DESKTOP_WIDTH   = 9;
  DESKTOP_HEIGHT  = 10;
  RESOLUTION_LIST = 11;
  MANAGER_TIMER   = 12;
  MANAGER_TEXTURE = 13;
  MANAGER_FONT    = 14;
  MANAGER_RTARGET = 15;
  MANAGER_SOUND   = 16;
  MANAGER_GUI     = 17;

  // zgl_Enable/zgl_Disable
  COLOR_BUFFER_CLEAR    = $000001;
  DEPTH_BUFFER          = $000002;
  DEPTH_BUFFER_CLEAR    = $000004;
  DEPTH_MASK            = $000008;
  STENCIL_BUFFER_CLEAR  = $000010;
  CORRECT_RESOLUTION    = $000020;
  CORRECT_WIDTH         = $000040;
  CORRECT_HEIGHT        = $000080;
  APP_USE_AUTOPAUSE     = $000100;
  APP_USE_LOG           = $000200;
  APP_USE_ENGLISH_INPUT = $000400;
  APP_USE_UTF8          = $000800;
  WND_USE_AUTOCENTER    = $001000;
  SND_CAN_PLAY          = $002000;
  SND_CAN_PLAY_FILE     = $004000;
  CROP_INVISIBLE        = $008000;

procedure zgl_Init( const FSAA : Byte = 0; const StencilBits : Byte = 0 );
{$IFDEF WIN32}
procedure zgl_InitToHandle( const Handle : DWORD; const FSAA : Byte = 0; const StencilBits : Byte = 0 );
{$ENDIF}
procedure zgl_Destroy;
procedure zgl_Exit;
procedure zgl_Reg( const What : DWORD; const UserData : Pointer );
function  zgl_Get( const What : DWORD ) : Ptr;
procedure zgl_GetSysDir;
procedure zgl_GetMem( var Mem : Pointer; const Size : DWORD );
procedure zgl_Enable( const What : DWORD );
procedure zgl_Disable( const What : DWORD );

implementation
uses
  zgl_application,
  zgl_screen,
  zgl_window,
  zgl_opengl,
  zgl_opengl_all,
  zgl_opengl_simple,
  zgl_timers,
  zgl_log,
  zgl_textures,
  zgl_render_target,
  zgl_font,
  zgl_gui_main,
  zgl_sound,
  zgl_utils;

procedure zgl_GetSysDir;
{$IFDEF LINUX}
begin
  app_WorkDir := './';

  app_UsrHomeDir := getenv( 'HOME' ) + '/';
{$ENDIF}
{$IFDEF WIN32}
var
  FL, FP : PAnsiChar;
  S      : AnsiString;
  t      : array[ 0..MAX_PATH - 1 ] of AnsiChar;
begin
  wnd_INST := GetModuleHandle( nil );
  GetMem( FL, 65535 );
  GetMem( FP, 65535 );
  GetModuleFileNameA( wnd_INST, FL, 65535 );
  GetFullPathNameA( FL, 65535, FP, FL );
  S := copy( AnsiString( FP ), 1, length( FP ) - length( FL ) );
  app_WorkDir := PAnsiChar( S );
  FL := nil;
  FP := nil;

  GetEnvironmentVariableA( 'APPDATA', t, MAX_PATH );
  app_UsrHomeDir := t;
  app_UsrHomeDir := app_UsrHomeDir + '\';
{$ENDIF}
{$IFDEF DARWIN}
var
  appBundle   : CFBundleRef;
  appCFURLRef : CFURLRef;
  appCFString : CFStringRef;
  appPath     : array[ 0..8191 ] of AnsiChar;
begin
  appBundle   := CFBundleGetMainBundle;
  appCFURLRef := CFBundleCopyBundleURL( appBundle );
  appCFString := CFURLCopyFileSystemPath( appCFURLRef, kCFURLPOSIXPathStyle );
  CFStringGetFileSystemRepresentation( appCFString, @appPath[ 0 ], 8192 );
  app_WorkDir := appPath + '/';

  app_UsrHomeDir := getenv( 'HOME' ) + '/';
{$ENDIF}
  app_GetSysDirs := TRUE;
end;

procedure zgl_Init;
begin
  zgl_GetSysDir;
  log_Init;

  ogl_FSAA    := FSAA;
  ogl_Stencil := StencilBits;
  if not scr_Create Then exit;

  app_Initialized := TRUE;
  if wnd_Height >= zgl_Get( DESKTOP_HEIGHT ) Then
    wnd_FullScreen := TRUE;
  {$IFDEF LINUX}
  if ( wnd_FullScreen ) and ( ( wnd_Width <> zgl_Get( DESKTOP_WIDTH ) ) and ( wnd_Height <> zgl_Get( DESKTOP_HEIGHT ) ) ) Then
    scr_SetOptions( wnd_Width, wnd_Height, scr_BPP, scr_Refresh, wnd_FullScreen, scr_VSync );
  {$ENDIF}

  if not wnd_Create( wnd_Width, wnd_Height ) Then exit;
  if not gl_Create Then exit;
  wnd_SetCaption( wnd_Caption );
  app_Work := TRUE;

  Set2DMode;
  wnd_ShowCursor( app_ShowCursor );

  app_MainLoop;
  zgl_Destroy;
end;

{$IFDEF WIN32}
procedure zgl_InitToHandle;
begin
  zgl_GetSysDir;
  log_Init;

  ogl_FSAA    := FSAA;
  ogl_Stencil := StencilBits;

  if not scr_Create Then exit;
  app_InitToHandle := TRUE;
  wnd_Handle := Handle;
  wnd_DC := GetDC( wnd_Handle );
  if not gl_Create Then exit;
  wnd_SetCaption( wnd_Caption );
  app_Work := TRUE;

  Set2DMode;
  wnd_ShowCursor( app_ShowCursor );

  app_MainLoop;
  zgl_Destroy;
end;
{$ENDIF}

procedure zgl_Destroy;
  var
    i : Integer;
    p : Pointer;
begin
  scr_Destroy;

  log_Add( 'Timers to free: ' + u_IntToStr( managerTimer.Count ) );
  while managerTimer.Count > 1 do
    begin
      p := managerTimer.First.Next;
      timer_Del( zglPTimer( p ) );
    end;

  log_Add( 'Render Targets to free: ' + u_IntToStr( managerRTarget.Count ) );
  while managerRTarget.Count > 1 do
    begin
      p := managerRTarget.First.Next;
      rtarget_Del( zglPRenderTarget( p ) );
    end;

  log_Add( 'Textures to free: ' + u_IntToStr( managerTexture.Count.Items ) );
  while managerTexture.Count.Items > 1 do
    begin
      p := managerTexture.First.Next;
      tex_Del( zglPTexture( p ) );
    end;

  log_Add( 'Fonts to free: ' + u_IntToStr( managerFont.Count ) );
  while managerFont.Count > 1 do
    begin
      p := managerFont.First.Next;
      font_Del( zglPFont( p ) );
    end;

  log_Add( 'Sounds to free: ' + u_IntToStr( managerSound.Count.Items ) );
  while managerSound.Count.Items > 1 do
    begin
      p := managerSound.First.Next;
      snd_Del( zglPSound( p ) );
    end;

  for i := 1 to SND_MAX do
    snd_StopFile( i );
  snd_Free;

  if app_WorkTime <> 0 Then
    log_Add( 'Average FPS: ' + u_IntToStr( Round( app_FPSAll / app_WorkTime ) ) );

  gl_Destroy;
  if not app_InitToHandle Then wnd_Destroy;

  app_PExit;
  log_Add( 'End' );
  log_Close;
end;

procedure zgl_Exit;
begin
  app_Work := FALSE;
end;

procedure zgl_Reg;
  var
    i : Integer;
begin
  case What of
    // Callback
    SYS_LOAD:
      begin
        app_PLoad := UserData;
        if not Assigned( UserData ) Then app_PLoad := zero;
      end;
    SYS_DRAW:
      begin
        app_PDraw := UserData;
        if not Assigned( UserData ) Then app_PDraw := zero;
      end;
    SYS_UPDATE:
      begin
        app_PUpdate := UserData;
        if not Assigned( UserData ) Then app_PUpdate := zerou;
      end;
    SYS_EXIT:
      begin
        app_PExit := UserData;
        if not Assigned( UserData ) Then app_PExit := zero;
      end;
    SYS_ACTIVATE:
      begin
        app_PActivate := UserData;
        if not Assigned( UserData ) Then app_PActivate := zeroa;
      end;
    // Textures
    TEX_FORMAT_EXTENSION:
      begin
        SetLength( managerTexture.Formats, managerTexture.Count.Formats + 1 );
        managerTexture.Formats[ managerTexture.Count.Formats ].Extension := u_StrUp( String( PChar( UserData ) ) );
      end;
    TEX_FORMAT_FILE_LOADER:
      begin
        managerTexture.Formats[ managerTexture.Count.Formats ].FileLoader := UserData;
      end;
    TEX_FORMAT_MEM_LOADER:
      begin
        managerTexture.Formats[ managerTexture.Count.Formats ].MemLoader := UserData;
        INC( managerTexture.Count.Formats );
      end;
    // Sound
    SND_FORMAT_EXTENSION:
      begin
        SetLength( managerSound.Formats, managerSound.Count.Formats + 1 );
        managerSound.Formats[ managerSound.Count.Formats ].Extension := u_StrUp( String( PChar( UserData ) ) );
        managerSound.Formats[ managerSound.Count.Formats ].Decoder   := nil;
      end;
    SND_FORMAT_FILE_LOADER:
      begin
        managerSound.Formats[ managerSound.Count.Formats ].FileLoader := UserData;
      end;
    SND_FORMAT_MEM_LOADER:
      begin
        managerSound.Formats[  managerSound.Count.Formats ].MemLoader := UserData;
        INC( managerSound.Count.Formats );
      end;
    SND_FORMAT_DECODER:
      begin
        for i := 0 to managerSound.Count.Formats - 1 do
          if managerSound.Formats[ i ].Extension = zglPSoundDecoder( UserData ).Ext Then
            managerSound.Formats[ i ].Decoder := UserData;
      end;
    // GUI
    WIDGET_TYPE_ID:
      begin
        if DWORD( UserData ) > length( managerGUI.Types ) Then
          begin
            SetLength( managerGUI.Types, length( managerGUI.Types ) + 1 );
            managerGUI.Types[ length( managerGUI.Types ) - 1 ]._type := DWORD( UserData );
            widgetTLast := length( managerGUI.Types ) - 1;
          end else
            widgetTLast := DWORD( UserData );
      end;
    WIDGET_FILL_DESC:
      begin
        managerGUI.Types[ widgetTLast ].FillDesc := UserData;
      end;
    WIDGET_ONDRAW:
      begin
        managerGUI.Types[ widgetTLast ].OnDraw := UserData;
      end;
    WIDGET_ONPROC:
      begin
        managerGUI.Types[ widgetTLast ].OnProc := UserData;
      end;
  end;
end;

function zgl_Get;
begin
  if ( What = APP_DIRECTORY ) or ( What = USR_HOMEDIR ) Then
    if not app_GetSysDirs Then zgl_GetSysDir;

  if ( What = DESKTOP_WIDTH ) or ( What = DESKTOP_HEIGHT ) Then
    if not scr_Initialized Then scr_Init;

  case What of
    SYS_FPS: Result := app_FPS;
    APP_PAUSED: Result := Byte( app_Pause );
    APP_DIRECTORY: Result := Ptr( PAnsiChar( app_WorkDir ) );
    USR_HOMEDIR: Result := Ptr( PAnsiChar( app_UsrHomeDir ) );
    LOG_FILENAME: Result := Ptr( @logfile );
    //ZGL_VERSION: Result := cv_version;
    SCR_ADD_X: Result := scr_AddCX;
    SCR_ADD_Y: Result := scr_AddCY;
    DESKTOP_WIDTH:
    {$IFDEF LINUX}
      Result := scr_Desktop.hdisplay;
    {$ENDIF}
    {$IFDEF WIN32}
      Result := scr_Desktop.dmPelsWidth;
    {$ENDIF}
    {$IFDEF DARWIN}
      Result := scr_DesktopW;
    {$ENDIF}
    DESKTOP_HEIGHT:
    {$IFDEF LINUX}
      Result := scr_Desktop.vdisplay;
    {$ENDIF}
    {$IFDEF WIN32}
      Result := scr_Desktop.dmPelsHeight;
    {$ENDIF}
    {$IFDEF DARWIN}
      Result := scr_DesktopH;
    {$ENDIF}
    RESOLUTION_LIST: Result := Ptr( @scr_ResList );

    // Managers
    MANAGER_TIMER:   Result := Ptr( @managerTimer );
    MANAGER_TEXTURE: Result := Ptr( @managerTexture );
    MANAGER_FONT:    Result := Ptr( @managerFont );
    MANAGER_RTARGET: Result := Ptr( @managerRTarget );
    MANAGER_SOUND:   Result := Ptr( @managerSound );
    MANAGER_GUI:     Result := Ptr( @managerGUI );
  end;
end;

procedure zgl_GetMem;
begin
  if Size > 0 Then
    begin
      GetMem( Mem, Size );
      FillChar( Mem^, Size, 0 );
    end else
      begin
        FreeMem( Mem );
        Mem := nil;
      end;
end;

procedure zgl_Enable;
begin
  app_Flags := app_Flags or What;

  if What and DEPTH_BUFFER > 0 Then
    glEnable( GL_DEPTH_TEST );

  if What and DEPTH_MASK > 0 Then
    glDepthMask( GL_TRUE );

  if What and CORRECT_RESOLUTION > 0 Then
    app_Flags := app_Flags or CORRECT_WIDTH or CORRECT_HEIGHT;

  if What and APP_USE_AUTOPAUSE > 0 Then
    app_AutoPause := TRUE;

  if What and APP_USE_LOG > 0 Then
    app_Log := TRUE;

  if What and APP_USE_UTF8 > 0 Then
    begin
      if SizeOf( Char ) = 1 Then
        font_GetCID := font_GetUTF8ID
      else
        if SizeOf( Char ) = 2 Then
          font_GetCID := font_GetUTF16ID;
    end;

  if What and SND_CAN_PLAY > 0 Then
    sndCanPlay := TRUE;

  if What and SND_CAN_PLAY_FILE > 0 Then
    sndCanPlayFile := TRUE;
end;

procedure zgl_Disable;
begin
  if app_Flags and What > 0 Then
    app_Flags := app_Flags xor What;

  if What and DEPTH_BUFFER > 0 Then
    glDisable( GL_DEPTH_TEST );

  if What and DEPTH_MASK > 0 Then
    glDepthMask( GL_FALSE );

  if What and CORRECT_RESOLUTION > 0 Then
    begin
      scr_ResCX := 1;
      scr_ResCY := 1;
      scr_AddCX := 0;
      scr_AddCY := 0;
      scr_SubCX := 0;
      scr_SubCY := 0;
    end;

  if What and APP_USE_AUTOPAUSE > 0 Then
    app_AutoPause := FALSE;

  if What and APP_USE_LOG > 0 Then
    app_Log := FALSE;

  if What and APP_USE_UTF8 > 0 Then
    font_GetCID := font_GetCP1251ID;

  if What and SND_CAN_PLAY > 0 Then
    sndCanPlay := FALSE;

  if What and SND_CAN_PLAY_FILE > 0 Then
    sndCanPlayFile := FALSE;
end;

end.
