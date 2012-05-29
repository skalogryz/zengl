{
 *  Copyright © Kemka Andrey aka Andru
 *  mail: dr.andru@gmail.com
 *  site: http://zengl.org
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
  Windows,
  zgl_types;

const
  cs_ZenGL    = 'ZenGL 0.2.9';
  cs_Date     = '2012.05.29';
  cv_major    = 0;
  cv_minor    = 2;
  cv_revision = 9;

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

  // zgl_Get
  ZENGL_VERSION           = 1;
  ZENGL_VERSION_STRING    = 2;
  ZENGL_VERSION_DATE      = 3;

  DIRECTORY_APPLICATION   = 101;
  DIRECTORY_HOME          = 102;

  LOG_FILENAME            = 203;

  DESKTOP_WIDTH           = 300;
  DESKTOP_HEIGHT          = 301;
  RESOLUTION_LIST         = 302;

  WINDOW_HANDLE           = 400;
  WINDOW_X                = 401;
  WINDOW_Y                = 402;
  WINDOW_WIDTH            = 403;
  WINDOW_HEIGHT           = 404;

  GAPI_DEVICE             = 500;
  GAPI_MAX_TEXTURE_SIZE   = 501;
  GAPI_MAX_TEXTURE_UNITS  = 502;
  GAPI_MAX_ANISOTROPY     = 503;
  GAPI_CAN_BLEND_SEPARATE = 504;
  GAPI_CAN_AUTOGEN_MIPMAP = 505;

  VIEWPORT_WIDTH          = 600;
  VIEWPORT_HEIGHT         = 601;
  VIEWPORT_OFFSET_X       = 602;
  VIEWPORT_OFFSET_Y       = 603;

  RENDER_FPS              = 700;
  RENDER_BATCHES_2D       = 701;
  RENDER_CURRENT_MODE     = 702;
  RENDER_CURRENT_TARGET   = 703;

  MANAGER_TIMER           = 800;
  MANAGER_TEXTURE         = 801;
  MANAGER_ATLAS           = 802;
  MANAGER_FONT            = 803;
  MANAGER_RTARGET         = 804;
  MANAGER_SOUND           = 805;
  MANAGER_EMITTER2D       = 806;

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
  APP_USE_DT_CORRECTION = $010000;

procedure zgl_Init( FSAA : Byte = 0; StencilBits : Byte = 0 );
procedure zgl_InitToHandle( Handle : LongWord; FSAA : Byte = 0; StencilBits : Byte = 0 );
procedure zgl_Destroy;
procedure zgl_Exit;
procedure zgl_Reg( What : LongWord; UserData : Pointer );
function  zgl_Get( What : LongWord ) : Ptr;
procedure zgl_GetSysDir;
procedure zgl_GetMem( var Mem : Pointer; Size : LongWord );
procedure zgl_FreeMem( var Mem : Pointer );
procedure zgl_FreeStrList( var List : zglTStringList );
procedure zgl_Enable( What : LongWord );
procedure zgl_Disable( What : LongWord );

implementation
uses
  zgl_application,
  zgl_screen,
  zgl_window,
  zgl_direct3d,
  zgl_direct3d_all,
  zgl_timers,
  zgl_log,
  zgl_render_2d,
  zgl_textures,
  {$IFDEF USE_TEXTURE_ATLAS}
  zgl_texture_atlas,
  {$ENDIF}
  zgl_render_target,
  zgl_font,
  {$IFDEF USE_SENGINE}
  zgl_sengine_2d,
  {$ENDIF}
  {$IFDEF USE_PARTICLES}
  zgl_particles_2d,
  {$ENDIF}
  {$IFDEF USE_SOUND}
  zgl_sound,
  {$ENDIF}
  zgl_utils;

procedure zgl_Init( FSAA : Byte = 0; StencilBits : Byte = 0 );
begin
  zgl_GetSysDir();
  log_Init();

  oglFSAA    := FSAA;
  oglStencil := StencilBits;
  if not scr_Create() Then exit;
  appInitialized := TRUE;
  if wndHeight >= zgl_Get( DESKTOP_HEIGHT ) Then
    wndFullScreen := TRUE;

  if not wnd_Create( wndWidth, wndHeight ) Then exit;
  if not d3d_Create() Then exit;
  wnd_SetCaption( wndCaption );
  appWork := TRUE;

  Set2DMode();
  wnd_ShowCursor( appShowCursor );

  d3d_BeginScene();
  app_PInit();
  app_PLoop();
  d3d_EndScene();
  zgl_Destroy();
end;

procedure zgl_InitToHandle( Handle : LongWord; FSAA : Byte = 0; StencilBits : Byte = 0 );
begin
  zgl_GetSysDir();
  log_Init();

  oglFSAA    := FSAA;
  oglStencil := StencilBits;
  if not scr_Create() Then exit;
  appInitedToHandle := TRUE;
  wndHandle := Handle;
  //wndDC := GetDC( wnd_Handle );
  if not d3d_Create() Then exit;
  wnd_SetCaption( wndCaption );
  appWork := TRUE;

  Set2DMode();
  wnd_ShowCursor( appShowCursor );

  d3d_BeginScene();
  app_PInit();
  app_PLoop();
  d3d_EndScene();
  zgl_Destroy();
end;

procedure zgl_Destroy;
  var
    i : Integer;
    p : Pointer;
begin
  if appWorkTime <> 0 Then
    log_Add( 'Average FPS: ' + u_IntToStr( Round( appFPSAll / appWorkTime ) ) );

  app_PExit();

  if managerTimer.Count <> 0 Then
    log_Add( 'Timers to free: ' + u_IntToStr( managerTimer.Count ) );
  while managerTimer.Count > 0 do
    begin
      p := managerTimer.First.next;
      timer_Del( zglPTimer( p ) );
    end;

  if managerRTarget.Count <> 0 Then
    log_Add( 'Render Targets to free: ' + u_IntToStr( managerRTarget.Count ) );
  while managerRTarget.Count > 0 do
    begin
      p := managerRTarget.First.next;
      rtarget_Del( zglPRenderTarget( p ) );
    end;

  {$IFDEF USE_TEXTURE_ATLAS}
  if managerAtlas.Count <> 0 Then
    log_Add( 'Atlases to free: ' + u_IntToStr( managerAtlas.Count ) );
  while managerAtlas.Count > 0 do
    begin
      p := managerAtlas.First.next;
      atlas_Del( zglPAtlas( p ) );
    end;
  {$ENDIF}

  managerZeroTexture := nil;
  if managerTexture.Count.Items <> 0 Then
    log_Add( 'Textures to free: ' + u_IntToStr( managerTexture.Count.Items ) );
  while managerTexture.Count.Items > 0 do
    begin
      p := managerTexture.First.next;
      tex_Del( zglPTexture( p ) );
    end;

  if managerFont.Count <> 0 Then
    log_Add( 'Fonts to free: ' + u_IntToStr( managerFont.Count ) );
  while managerFont.Count > 0 do
    begin
      p := managerFont.First.next;
      font_Del( zglPFont( p ) );
    end;

  {$IFDEF USE_SENGINE}
  sengine2d_Set( nil );
  sengine2d_ClearAll();
  {$ENDIF}

  {$IFDEF USE_PARTICLES}
  if managerEmitter2D.Count <> 0 Then
    log_Add( 'Emitters2D to free: ' + u_IntToStr( managerEmitter2D.Count ) );
  for i := 0 to managerEmitter2D.Count - 1 do
    emitter2d_Free( managerEmitter2D.List[ i ] );
  SetLength( managerEmitter2D.List, 0 );
  pengine2d_Set( nil );
  pengine2d_ClearAll();
  {$ENDIF}

  {$IFDEF USE_SOUND}
  snd_Free();
  {$ENDIF}

  scr_Destroy();
  if not appInitedToHandle Then wnd_Destroy();
  d3d_Destroy();

  appInitialized := FALSE;

  log_Add( 'End' );
  log_Close();
end;

procedure zgl_Exit;
begin
  appWork := FALSE;
end;

procedure zgl_Reg( What : LongWord; UserData : Pointer );
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

function zgl_Get( What : LongWord ) : Ptr;
begin
  if ( not appGotSysDirs ) and ( ( What = DIRECTORY_APPLICATION ) or ( What = DIRECTORY_HOME ) ) Then
    zgl_GetSysDir();

  if ( not scrInitialized ) and ( ( What = DESKTOP_WIDTH ) or ( What = DESKTOP_HEIGHT ) or ( What = RESOLUTION_LIST ) ) Then
    scr_Init();

  case What of
    ZENGL_VERSION: Result := cv_major shl 16 + cv_minor shl 8 + cv_revision;
    ZENGL_VERSION_STRING: Result := Ptr( PChar( cs_ZenGL ) );
    ZENGL_VERSION_DATE: Result := Ptr( PChar( cs_Date ) );

    DIRECTORY_APPLICATION: Result := Ptr( PChar( appWorkDir ) );
    DIRECTORY_HOME: Result := Ptr( PChar( appHomeDir ) );

    LOG_FILENAME:
      if not appWork Then
        Result := Ptr( @logfile );

    DESKTOP_WIDTH:
      Result := scrDesktop.dmPelsWidth;
    DESKTOP_HEIGHT:
      Result := scrDesktop.dmPelsHeight;
    RESOLUTION_LIST: Result := Ptr( @scrResList );

    WINDOW_HANDLE: Result := Ptr( wndHandle );
    WINDOW_X: Result := Ptr( wndX );
    WINDOW_Y: Result := Ptr( wndY );
    WINDOW_WIDTH: Result := Ptr( wndWidth );
    WINDOW_HEIGHT: Result := Ptr( wndHeight );

    GAPI_DEVICE: Result := Ptr( d3dDevice );
    GAPI_MAX_TEXTURE_SIZE: Result := oglMaxTexSize;
    GAPI_MAX_TEXTURE_UNITS: Result := oglMaxTexUnits;
    GAPI_MAX_ANISOTROPY: Result := oglMaxAnisotropy;
    GAPI_CAN_BLEND_SEPARATE: Result := Ptr( oglSeparate );
    GAPI_CAN_AUTOGEN_MIPMAP: Result := Ptr( oglCanAutoMipMap );

    RENDER_FPS: Result := appFPS;
    RENDER_BATCHES_2D: Result := b2dBatches + 1;
    RENDER_CURRENT_MODE: Result := oglMode;
    RENDER_CURRENT_TARGET: Result := oglTarget;

    VIEWPORT_WIDTH: Result := oglWidth - scrSubCX;
    VIEWPORT_HEIGHT: Result := oglHeight - scrSubCY;
    VIEWPORT_OFFSET_X: Result := scrAddCX;
    VIEWPORT_OFFSET_Y: Result := scrAddCY;

    // Managers
    MANAGER_TIMER:     Result := Ptr( @managerTimer );
    MANAGER_TEXTURE:   Result := Ptr( @managerTexture );
    {$IFDEF USE_TEXTURE_ATLAS}
    MANAGER_ATLAS:     Result := Ptr( @managerAtlas );
    {$ENDIF}
    MANAGER_FONT:      Result := Ptr( @managerFont );
    MANAGER_RTARGET:   Result := Ptr( @managerRTarget );
    {$IFDEF USE_SOUND}
    MANAGER_SOUND:     Result := Ptr( @managerSound );
    {$ENDIF}
    {$IFDEF USE_PARTICLES}
    MANAGER_EMITTER2D: Result := Ptr( @managerEmitter2D );
    {$ENDIF}
  else
    Result := 0;
  end;
end;

procedure zgl_GetSysDir;
  var
    buffer : PChar;
    fn, fp : PChar;
    t      : array[ 0..MAX_PATH - 1 ] of Char;
begin
  wndINST := GetModuleHandle( nil );
  GetMem( buffer, 65535 );
  GetMem( fn, 65535 );
  GetModuleFileName( wndINST, fn, 65535 );
  GetFullPathName( fn, 65535, buffer, fp );
  appWorkDir := copy( String( buffer ), 1, length( buffer ) - length( fp ) );

  GetEnvironmentVariable( 'APPDATA', t, MAX_PATH );
  appHomeDir := t;
  appHomeDir := appHomeDir + '\';

  FreeMem( buffer );
  FreeMem( fn );
end;

procedure zgl_GetMem( var Mem : Pointer; Size : LongWord );
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

procedure zgl_FreeStrList( var List : zglTStringList );
  var
    i : Integer;
begin
  for i := 0 to List.Count - 1 do
    List.Items[ i ] := '';
  List.Count := 0;
  SetLength( List.Items, 0 );
end;

procedure zgl_Enable( What : LongWord );
begin
  appFlags := appFlags or What;

  if What and DEPTH_BUFFER > 0 Then
    glEnable( GL_DEPTH_TEST );

  if What and DEPTH_MASK > 0 Then
    glDepthMask( GL_TRUE );

  if What and CORRECT_RESOLUTION > 0 Then
    appFlags := appFlags or CORRECT_WIDTH or CORRECT_HEIGHT;

  if What and APP_USE_AUTOPAUSE > 0 Then
    appAutoPause := TRUE;

  if What and APP_USE_LOG > 0 Then
    appLog := TRUE;

  if What and APP_USE_UTF8 > 0 Then
    begin
      {$IFNDEF FPC}
      if SizeOf( Char ) = 2 Then
        font_GetCID := font_GetUTF16ID
      else
      {$ENDIF}
        font_GetCID := font_GetUTF8ID;
    end;

  {$IFDEF USE_SOUND}
  if What and SND_CAN_PLAY > 0 Then
    sndCanPlay := TRUE;

  if What and SND_CAN_PLAY_FILE > 0 Then
    sndCanPlayFile := TRUE;
  {$ENDIF}

  if What and CLIP_INVISIBLE > 0 Then
    render2dClip := TRUE;
end;

procedure zgl_Disable( What : LongWord );
begin
  if appFlags and What > 0 Then
    appFlags := appFlags xor What;

  if What and DEPTH_BUFFER > 0 Then
    glDisable( GL_DEPTH_TEST );

  if What and DEPTH_MASK > 0 Then
    glDepthMask( GL_FALSE );

  if What and CORRECT_RESOLUTION > 0 Then
    begin
      scrResCX := 1;
      scrResCY := 1;
      scrAddCX := 0;
      scrAddCY := 0;
      scrSubCX := 0;
      scrSubCY := 0;
    end;

  if What and APP_USE_AUTOPAUSE > 0 Then
    appAutoPause := FALSE;

  if What and APP_USE_LOG > 0 Then
    appLog := FALSE;

  if What and APP_USE_UTF8 > 0 Then
    font_GetCID := font_GetCP1251ID;

  {$IFDEF USE_SOUND}
  if What and SND_CAN_PLAY > 0 Then
    sndCanPlay := FALSE;

  if What and SND_CAN_PLAY_FILE > 0 Then
    sndCanPlayFile := FALSE;
  {$ENDIF}

  if What and CLIP_INVISIBLE > 0 Then
    render2dClip := FALSE;
end;

end.
