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
unit zgl_main;

{$I zgl_config.cfg}

interface
uses
  {$IFDEF LINUX}
  BaseUnix, X, XRandr,
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF DARWIN}
  BaseUnix, MacOSAll,
  {$ENDIF}
  zgl_types;

const
  cs_ZenGL = 'ZenGL 0.2 RC6';

  // zgl_Reg
  SYS_APP_INIT           = $000001;
  SYS_APP_LOOP           = $000002;
  SYS_LOAD               = $000003;
  SYS_DRAW               = $000004;
  SYS_UPDATE             = $000005;
  SYS_EXIT               = $000006;
  SYS_ACTIVATE           = $000007;
  TEX_FORMAT_EXTENSION   = $000010;
  TEX_FORMAT_FILE_LOADER = $000011;
  TEX_FORMAT_MEM_LOADER  = $000012;
  TEX_CURRENT_EFFECT     = $000013;
  SND_FORMAT_EXTENSION   = $000020;
  SND_FORMAT_FILE_LOADER = $000021;
  SND_FORMAT_MEM_LOADER  = $000022;
  SND_FORMAT_DECODER     = $000023;
  WIDGET_TYPE_ID         = $000030;
  WIDGET_FILL_DESC       = $000031;
  WIDGET_ONDRAW          = $000032;
  WIDGET_ONPROC          = $000033;
  WIDGET_ONFREEDESC      = $000034;
  WIDGET_ONFREEDATA      = $000035;

  // zgl_Get
  SYS_FPS         = 1;
  APP_PAUSED      = 2;
  APP_DIRECTORY   = 3;
  APP_WND_HANDLE  = 4;
  APP_OGL_CONTEXT = 5;
  USR_HOMEDIR     = 6;
  LOG_FILENAME    = 7;
  ZGL_VERSION     = 8;
  SCR_ADD_X       = 9;
  SCR_ADD_Y       = 10;
  DESKTOP_WIDTH   = 11;
  DESKTOP_HEIGHT  = 12;
  RESOLUTION_LIST = 13;
  MANAGER_TIMER   = 14;
  MANAGER_TEXTURE = 15;
  MANAGER_ATLAS   = 16;
  MANAGER_FONT    = 17;
  MANAGER_RTARGET = 18;
  MANAGER_SOUND   = 19;
  MANAGER_GUI     = 20;

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
  CLIP_INVISIBLE        = $008000;

procedure zgl_Init( const FSAA : Byte = 0; const StencilBits : Byte = 0 );
procedure zgl_InitToHandle( const Handle : Ptr; const FSAA : Byte = 0; const StencilBits : Byte = 0 );
procedure zgl_Destroy;
procedure zgl_Exit;
procedure zgl_Reg( const What : LongWord; const UserData : Pointer );
function  zgl_Get( const What : LongWord ) : Ptr;
procedure zgl_GetSysDir;
procedure zgl_GetMem( var Mem : Pointer; const Size : LongWord );
procedure zgl_FreeMem( var Mem : Pointer );
procedure zgl_FreeStr( var Str : String );
procedure zgl_Enable( const What : LongWord );
procedure zgl_Disable( const What : LongWord );

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
  zgl_render_2d,
  zgl_textures,
  {$IFDEF USE_TEXTURE_ATLAS}
  zgl_texture_atlas,
  {$ENDIF}
  zgl_render_target,
  zgl_font,
  {$IFDEF USE_SOUND}
  zgl_sound,
  {$ENDIF}
  zgl_utils;

procedure zgl_Init( const FSAA : Byte = 0; const StencilBits : Byte = 0 );
begin
  zgl_GetSysDir();
  log_Init();

  ogl_FSAA    := FSAA;
  ogl_Stencil := StencilBits;
  if not scr_Create() Then exit;

  app_Initialized := TRUE;
  if wnd_Height >= zgl_Get( DESKTOP_HEIGHT ) Then
    wnd_FullScreen := TRUE;

  if not wnd_Create( wnd_Width, wnd_Height ) Then exit;
  if not gl_Create() Then exit;
  wnd_SetCaption( wnd_Caption );
  app_Work := TRUE;

  Set2DMode();
  wnd_ShowCursor( app_ShowCursor );

  {$IFDEF LINUX}
  scr_SetOptions( wnd_Width, wnd_Height, scr_Refresh, wnd_FullScreen, scr_VSync );
  {$ENDIF}

  app_PInit();
  app_PLoop();
  zgl_Destroy();
end;

procedure zgl_InitToHandle( const Handle : Ptr; const FSAA : Byte = 0; const StencilBits : Byte = 0 );
begin
  zgl_GetSysDir();
  log_Init();

  ogl_FSAA    := FSAA;
  ogl_Stencil := StencilBits;

  if not scr_Create() Then exit;
  app_InitToHandle := TRUE;
  {$IFDEF LINUX}
  wnd_Handle := TWindow( Handle );
  {$ENDIF}
  {$IFDEF DARWIN}
  wnd_Handle := WindowRef( Handle );
  {$ENDIF}
  {$IFDEF WINDOWS}
  wnd_Handle := HWND( Handle );
  wnd_DC     := GetDC( wnd_Handle );
  {$ENDIF}
  if not gl_Create() Then exit;
  wnd_SetCaption( wnd_Caption );
  app_Work := TRUE;

  Set2DMode();
  wnd_ShowCursor( app_ShowCursor );

  app_PInit();
  app_PLoop();
  zgl_Destroy();
end;

procedure zgl_Destroy;
  var
    i : Integer;
    p : Pointer;
begin
  if app_WorkTime <> 0 Then
    log_Add( 'Average FPS: ' + u_IntToStr( Round( app_FPSAll / app_WorkTime ) ) );

  app_PExit();
  scr_Destroy();

  log_Add( 'Timers to free: ' + u_IntToStr( managerTimer.Count ) );
  while managerTimer.Count > 0 do
    begin
      p := managerTimer.First.next;
      timer_Del( zglPTimer( p ) );
    end;

  log_Add( 'Render Targets to free: ' + u_IntToStr( managerRTarget.Count ) );
  while managerRTarget.Count > 0 do
    begin
      p := managerRTarget.First.next;
      rtarget_Del( zglPRenderTarget( p ) );
    end;

  {$IFDEF USE_TEXTURE_ATLAS}
  log_Add( 'Atlases to free: ' + u_IntToStr( managerAtlas.Count ) );
  while managerAtlas.Count > 0 do
    begin
      p := managerAtlas.First.next;
      atlas_Del( zglPAtlas( p ) );
    end;
  {$ENDIF}

  log_Add( 'Textures to free: ' + u_IntToStr( managerTexture.Count.Items ) );
  while managerTexture.Count.Items > 0 do
    begin
      p := managerTexture.First.next;
      tex_Del( zglPTexture( p ) );
    end;

  log_Add( 'Fonts to free: ' + u_IntToStr( managerFont.Count ) );
  while managerFont.Count > 0 do
    begin
      p := managerFont.First.next;
      font_Del( zglPFont( p ) );
    end;

  {$IFDEF USE_SOUND}
  log_Add( 'Sounds to free: ' + u_IntToStr( managerSound.Count.Items ) );
  while managerSound.Count.Items > 0 do
    begin
      p := managerSound.First.next;
      snd_Del( zglPSound( p ) );
    end;

  for i := 1 to SND_MAX do
    snd_StopFile( i );
  snd_Free();
  {$ENDIF}

  gl_Destroy();
  if not app_InitToHandle Then wnd_Destroy();

  log_Add( 'End' );
  log_Close();
end;

procedure zgl_Exit;
begin
  app_Work := FALSE;
end;

procedure zgl_Reg( const What : LongWord; const UserData : Pointer );
  var
    i : Integer;
begin
  case What of
    // Callback
    SYS_APP_INIT:
      begin
        app_PInit := UserData;
        if not Assigned( UserData ) Then app_PInit := app_Init;
      end;
    SYS_APP_LOOP:
      begin
        app_PLoop := UserData;
        if not Assigned( UserData ) Then app_PLoop := app_MainLoop;
      end;
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
    TEX_CURRENT_EFFECT:
      begin
        tex_CalcCustomEffect := UserData;
        if not Assigned( tex_CalcCustomEffect ) Then tex_CalcCustomEffect := zeroce;
      end;
    // Sound
    {$IFDEF USE_SOUND}
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
    {$ENDIF}
  end;
end;

function zgl_Get( const What : LongWord ) : Ptr;
begin
  if ( What = APP_DIRECTORY ) or ( What = USR_HOMEDIR ) Then
    if not app_GetSysDirs Then zgl_GetSysDir();

  if ( What = DESKTOP_WIDTH ) or ( What = DESKTOP_HEIGHT ) Then
    if not scr_Initialized Then scr_Init();

  case What of
    SYS_FPS: Result := app_FPS;
    APP_PAUSED: Result := Byte( app_Pause );
    APP_DIRECTORY: Result := Ptr( PAnsiChar( app_WorkDir ) );
    APP_WND_HANDLE: Result := Ptr( wnd_Handle );
    APP_OGL_CONTEXT: Result := Ptr( ogl_Context );
    USR_HOMEDIR: Result := Ptr( PAnsiChar( app_UsrHomeDir ) );
    LOG_FILENAME: Result := Ptr( @logfile );
    //ZGL_VERSION: Result := cv_version;
    SCR_ADD_X: Result := scr_AddCX;
    SCR_ADD_Y: Result := scr_AddCY;
    DESKTOP_WIDTH:
    {$IFDEF LINUX}
      Result := PXRRScreenSize( scr_ModeList + scr_Desktop * SizeOf( PXRRScreenSize ) ).width;
    {$ENDIF}
    {$IFDEF WINDOWS}
      Result := scr_Desktop.dmPelsWidth;
    {$ENDIF}
    {$IFDEF DARWIN}
      Result := scr_DesktopW;
    {$ENDIF}
    DESKTOP_HEIGHT:
    {$IFDEF LINUX}
      Result := PXRRScreenSize( scr_ModeList + scr_Desktop * SizeOf( PXRRScreenSize ) ).height;
    {$ENDIF}
    {$IFDEF WINDOWS}
      Result := scr_Desktop.dmPelsHeight;
    {$ENDIF}
    {$IFDEF DARWIN}
      Result := scr_DesktopH;
    {$ENDIF}
    RESOLUTION_LIST: Result := Ptr( @scr_ResList );

    // Managers
    MANAGER_TIMER:   Result := Ptr( @managerTimer );
    MANAGER_TEXTURE: Result := Ptr( @managerTexture );
    {$IFDEF USE_TEXTURE_ATLAS}
    MANAGER_ATLAS:   Result := Ptr( @managerAtlas );
    {$ENDIF}
    MANAGER_FONT:    Result := Ptr( @managerFont );
    MANAGER_RTARGET: Result := Ptr( @managerRTarget );
    {$IFDEF USE_SOUND}
    MANAGER_SOUND:   Result := Ptr( @managerSound );
    {$ENDIF}
  end;
end;

procedure zgl_GetSysDir;
{$IFDEF LINUX}
begin
  app_WorkDir := './';

  app_UsrHomeDir := FpGetEnv( 'HOME' ) + '/';
{$ENDIF}
{$IFDEF WINDOWS}
var
  buffer : PAnsiChar;
  fn, fp : PAnsiChar;
  s      : AnsiString;
  t      : array[ 0..MAX_PATH - 1 ] of AnsiChar;
begin
  wnd_INST := GetModuleHandle( nil );
  GetMem( buffer, 65535 );
  GetMem( fn, 65535 );
  GetModuleFileNameA( wnd_INST, fn, 65535 );
  GetFullPathNameA( fn, 65535, buffer, fp );
  s := copy( AnsiString( buffer ), 1, length( buffer ) - length( fp ) );
  app_WorkDir := PAnsiChar( s );

  GetEnvironmentVariableA( 'APPDATA', t, MAX_PATH );
  app_UsrHomeDir := t;
  app_UsrHomeDir := app_UsrHomeDir + '\';

  FreeMem( buffer );
  FreeMem( fn );
{$ENDIF}
{$IFDEF DARWIN}
var
  appBundle   : CFBundleRef;
  appCFURLRef : CFURLRef;
  appCFString : CFStringRef;
  appPath     : array[ 0..8191 ] of AnsiChar;
begin
  appBundle   := CFBundleGetMainBundle();
  appCFURLRef := CFBundleCopyBundleURL( appBundle );
  appCFString := CFURLCopyFileSystemPath( appCFURLRef, kCFURLPOSIXPathStyle );
  CFStringGetFileSystemRepresentation( appCFString, @appPath[ 0 ], 8192 );
  app_WorkDir := appPath + '/';

  app_UsrHomeDir := FpGetEnv( 'HOME' ) + '/';
{$ENDIF}
  app_GetSysDirs := TRUE;
end;

procedure zgl_GetMem( var Mem : Pointer; const Size : LongWord );
begin
  if Size > 0 Then
    begin
      GetMem( Mem, Size );
      FillChar( Mem^, Size, 0 );
    end else
      Mem := nil;
end;

procedure zgl_FreeMem( var Mem : Pointer );
begin
  FreeMem( Mem );
  Mem := nil;
end;

procedure zgl_FreeStr( var Str : String );
begin
  Str := '';
end;

procedure zgl_Enable( const What : LongWord );
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
      {$IFNDEF FPC}
      else
        font_GetCID := font_GetUTF16ID;
      {$ENDIF}
    end;

  {$IFDEF USE_SOUND}
  if What and SND_CAN_PLAY > 0 Then
    sndCanPlay := TRUE;

  if What and SND_CAN_PLAY_FILE > 0 Then
    sndCanPlayFile := TRUE;
  {$ENDIF}

  if What and CLIP_INVISIBLE > 0 Then
    render2d_Clip := TRUE;
end;

procedure zgl_Disable( const What : LongWord );
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

  {$IFDEF USE_SOUND}
  if What and SND_CAN_PLAY > 0 Then
    sndCanPlay := FALSE;

  if What and SND_CAN_PLAY_FILE > 0 Then
    sndCanPlayFile := FALSE;
  {$ENDIF}

  if What and CLIP_INVISIBLE > 0 Then
    render2d_Clip := FALSE;
end;

end.
