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
unit zgl_main;

{$I define.inc}

interface
uses
  GL, GLExt,
  {$IFDEF LINUX}
  GLX, X, XLib,
  {$ENDIF}
  {$IFDEF WIN32}
  Windows, Messages,
  {$ENDIF}
  {$IFDEF DARWIN}
  AGL, MacOSAll,
  {$ENDIF}
  zgl_const,
  zgl_types,
  zgl_global_var,
  
  zgl_keyboard,
  zgl_mouse,

  zgl_gui_main,
  
  zgl_camera_2d,
  zgl_render_target,
  zgl_timers,
  zgl_textures,
  zgl_sound,
  zgl_shader,

  zgl_opengl,
  zgl_opengl_simple,
  zgl_window,
  zgl_screen,
  
  zgl_log,
  zgl_math,
  
  Utils;

procedure zgl_Init( const FSAA, StencilBits : Byte );
{$IFDEF WIN32}
procedure zgl_InitToHandle( const Handle : DWORD; const FSAA, StencilBits : Byte );
{$ENDIF}
procedure zgl_Destroy;
procedure zgl_Exit;
procedure zgl_Reg( const What : WORD; const UserData : Pointer );
function  zgl_Get( const What : DWORD ) : Ptr;
procedure zgl_GetMem( var Mem : Pointer; const Size : DWORD );
procedure zgl_Enable( const What : DWORD );
procedure zgl_Disable( const What : DWORD );

procedure zgl_draw;
procedure zgl_loop;
function  zgl_mess{$IFDEF LINUX} : DWORD{$ENDIF}
                  {$IFDEF WIN32}( hWnd : HWND; Msg : UINT; wParam : WPARAM; lParam : LPARAM ) : LRESULT; stdcall{$ENDIF}
                  {$IFDEF DARWIN}( inHandlerCallRef: EventHandlerCallRef; inEvent: EventRef; inUserData: UnivPtr ): OSStatus; cdecl{$ENDIF};

{$IFDEF LINUX}
function sleep(__useconds:longword):longint;cdecl;external 'libc' name 'usleep';

function Xutf8LookupString( ic : PXIC; event : PXKeyPressedEvent; buffer_return : PChar; bytes_buffer : Integer; keysym_return : PKeySym; status_return : PStatus ) : integer; cdecl; external;
{$ENDIF}

implementation
uses
  zgl_textures_jpg,
  zgl_textures_png,
  zgl_textures_tga,
  zgl_sound_wav;

procedure zgl_Init;
begin
  log_Init;
  if not InitGL Then
    begin
      log_Add( 'Cannot load GL library' );
      exit;
    end;
  {$IFDEF DARWIN}
  if not InitAGL Then
    begin
      log_Add( 'Cannot load AGL library' );
      exit;
    end;
  {$ENDIF}

  ogl_FSAA    := FSAA;
  ogl_Stencil := StencilBits;

  if not scr_Create Then exit;
  if not wnd_Create( wnd_Width, wnd_Height ) Then exit;
  if not gl_Create Then exit;
  app_Work := TRUE;

  {$IFDEF LINUX}
  if ( wnd_Width >= scr_Desktop.hDisplay ) and ( wnd_Height >= scr_Desktop.vDisplay ) Then
    app_FullScreen := TRUE;
  {$ENDIF}
  {$IFDEF WIN32}
  if ( wnd_Width >= scr_Desktop.dmPelsWidth ) and ( wnd_Height >= scr_Desktop.dmPelsHeight ) Then
    app_FullScreen := TRUE;
  {$ENDIF}
  if app_FullScreen Then
    scr_SetOptions( wnd_Width, wnd_Height, scr_BPP, scr_Refresh, app_FullScreen, scr_VSync );

  Set2DMode;
  wnd_ShowCursor( FALSE );

  zgl_loop;
  zgl_Destroy;
end;

{$IFDEF WIN32}
procedure zgl_InitToHandle;
begin
  log_Init;
  if not InitGL Then
    begin
      log_Add( 'Cannot load GL library' );
      exit;
    end;

  ogl_FSAA    := FSAA;
  ogl_Stencil := StencilBits;

  if not scr_Create Then exit;
  app_InitToHandle := TRUE;
  wnd_Handle := Handle;
  wnd_DC := GetDC( wnd_Handle );
  if not gl_Create Then exit;
  app_Work := TRUE;

  Set2DMode;
  wnd_ShowCursor( FALSE );

  zgl_loop;
  zgl_Destroy;
end;
{$ENDIF}

procedure zgl_Destroy;
begin
  scr_Destroy;

  log_Add( 'Timers to free: ' + u_IntToStr( managerTimer.Count ) );
  while managerTimer.Count > 0 do
    timer_Del( managerTimer.First.Next );

  log_Add( 'Render Targets to free: ' + u_IntToStr( managerRTarget.Count ) );
  while managerRTarget.Count > 0 do
    rtarget_Del( managerRTarget.First.Next );

  log_Add( 'Textures to free: ' + u_IntToStr( managerTexture.Count ) );
  while managerTexture.Count > 0 do
    tex_Del( managerTexture.First.Next );

  log_Add( 'Sounds to free: ' + u_IntToStr( managerSound.Count ) );
  while managerSound.Count > 0 do
    snd_Del( managerSound.First.Next );

  if app_WorkTime <> 0 Then
    log_Add( 'Average FPS: ' + u_IntToStr( Round( app_FPSAll / app_WorkTime ) ) );

  gl_Destroy;
  snd_StopFile;
  snd_Free;
  if not app_InitToHandle Then wnd_Destroy;

  app_PExit;

{$IFDEF WIN32}
  wnd_ShowCursor( TRUE );
{$ENDIF}
  log_add( 'End' );
  log_Close;
end;

procedure zgl_Exit;
begin
  app_Work := FALSE;
end;

procedure zgl_Reg;
begin
  case What of
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
    SYS_EXIT:
      begin
        app_PExit := UserData;
        if not Assigned( UserData ) Then app_PExit := zero;
      end;
    TEX_FORMAT_EXTENSION:
      begin
        SetLength( texNewFormats, texNFCount + 1 );
        texNewFormats[ texNFCount ].Extension := StrUp( String( PChar( UserData ) ) );
      end;
    TEX_FORMAT_LOADER:
      begin
        texNewFormats[ texNFCount ].Loader := UserData;
        INC( texNFCount );
      end;
    SND_FORMAT_EXTENSION:
      begin
        SetLength( sndNewFormats, sndNFCount + 1 );
        sndNewFormats[ sndNFCount ].Extension := StrUp( String( PChar( UserData ) ) );
      end;
    SND_FORMAT_LOADER:
      begin
        sndNewFormats[ sndNFCount ].Loader := UserData;
        INC( sndNFCount );
      end;
    WIDGET_TYPE_ID:
      begin
        if Integer( UserData ) > widgetTCount Then
          begin
            SetLength( widgetTypes, widgetTCount + 1 );
            widgetTypes[ widgetTCount ]._type := Integer( UserData );
            widgetTLast := widgetTCount;
            INC( widgetTCount );
          end else
            widgetTLast := Integer( UserData );
      end;
    WIDGET_ONDRAW:
      begin
        widgetTypes[ widgetTLast ].OnDraw := UserData;
      end;
    WIDGET_ONPROC:
      begin
        widgetTypes[ widgetTLast ].OnProc := UserData;
      end;
  end;
end;

function zgl_Get;
begin
  case What of
    SYS_FPS: Result := app_FPS;
    LOG_FILENAME: Result := Ptr( @logfile );
    ZGL_VERSION: Result := cv_version;
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
      Result := 0;
    {$ENDIF}
    DESKTOP_HEIGHT:
    {$IFDEF LINUX}
      Result := scr_Desktop.vdisplay;
    {$ENDIF}
    {$IFDEF WIN32}
      Result := scr_Desktop.dmPelsHeight;
    {$ENDIF}
    {$IFDEF DARWIN}
      Result := 0;
    {$ENDIF}
    RESOLUTION_LIST: Result := Ptr( @scr_ResList );
    MANAGER_TIMER: Result := Ptr( @managerTimer );
    MANAGER_TEXTURE: Result := Ptr( @managerTexture );
    MANAGER_FONT: Result := Ptr( @managerFont );
    MANAGER_RTARGET: Result := Ptr( @managerRTarget );
    MANAGER_SOUND: Result := Ptr( @managerSound );
    MANAGER_GUI: Result := Ptr( @managerGUI );
  end;
end;

procedure zgl_GetMem;
  var
    R : Boolean;
begin
  if Size = 0 Then
    FreeMem( Mem )
  else
    begin
      if Assigned( Mem ) Then R := TRUE;
      if not R Then
        begin
          Mem := ReAllocMem( Mem, Size );
          FillChar( Mem^, Size, 0 );
        end else Mem := AllocMem( Size );
    end;
end;

procedure zgl_Enable;
begin
  app_Flags := app_Flags or What;

  if What and DEPTH_BUFFER > 0 Then
    glEnable( GL_DEPTH_TEST );

  if What and DEPTH_MASK > 0 Then
    glDepthMask( GL_TRUE );

  if What and APP_USE_AUTOPAUSE > 0 Then
    app_AutoPause := TRUE;

  if What and APP_USE_AUTOMINIMIZE > 0 Then
    app_AutoMinimize := TRUE;

  if What and APP_USE_LOG > 0 Then
    begin
      app_Log := TRUE;
      log_Init;
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

  if What and APP_USE_AUTOPAUSE > 0 Then
    app_AutoPause := FALSE;

  if What and APP_USE_AUTOMINIMIZE > 0 Then
    app_AutoMinimize := FALSE;

  if What and APP_USE_LOG > 0 Then
    app_Log := FALSE;

  if What and SND_CAN_PLAY > 0 Then
    sndCanPlay := FALSE;

  if What and SND_CAN_PLAY_FILE > 0 Then
    sndCanPlayFile := FALSE;
end;

procedure zgl_draw;
begin
  scr_Clear;

  app_PDraw;
  //gui_Draw;

  scr_flush;

  if not app_Pause Then
    INC( app_FPSCount );
end;

procedure zgl_FPS;
begin
  app_FPS      := app_FPSCount;
  app_FPSAll   := app_FPSAll + app_FPSCount;
  app_FPSCount := 0;
  INC( app_WorkTime );
end;

procedure zgl_loop;
  var
    i, z : Integer;
    j, t, dt : Double;
    currTimer : zglPTimer;
    {$IFDEF WIN32}
    Mess : tagMsg;
    {$ENDIF}
    {$IFDEF DARWIN}
    Event    : EventRecord;
    Window   : WindowRef;
    PartCode : WindowPartCode;
    {$ENDIF}
    procedure OSProcess;
    begin
      {$IFDEF LINUX}
      zgl_mess;
      {$ENDIF}
      {$IFDEF WIN32}
      while PeekMessage( Mess, wnd_Handle, 0, 0, PM_REMOVE ) do
        begin
          TranslateMessage( Mess );
          DispatchMessage( Mess );
        end;
      {$ENDIF}
      {$IFDEF DARWIN}
      while GetNextEvent( everyEvent, Event ) do
        if Event.what = MacOSAll.mouseDown Then
          begin
            PartCode := FindWindow( Event.where, Window );            if Assigned( Window ) Then
              begin                SelectWindow( Window );
                if Partcode = inDrag Then
                  begin
                    wnd_X := Event.where.h;
                    wnd_Y := Event.where.v;
                    DragWindow( wnd_Handle, Event.where, nil );
                  end;
              end;
          end;
      {$ENDIF}
    end;
begin
  //gui_Init;
  app_PLoad;
  {$IFDEF LINUX}
  glFlush;
  glXWaitGL;
  glXSwapBuffers( scr_Display, wnd_Handle );
  {$ENDIF}
  {$IFDEF WIN32}
  glFlush;
  glFinish;
  SwapBuffers( wnd_DC );
  {$ENDIF}
  {$IFDEF DARWIN}
  aglSwapBuffers( ogl_Context );
  {$ENDIF}
  t := timer_GetTicks;
  timer_Reset;
  timer_Add( @zgl_FPS, 1000 );
  while app_Work do
    begin      
      OSProcess;

      CanKillTimers := FALSE;
      if not app_Pause Then
        begin
          currTimer := @managerTimer.First;
          if currTimer <> nil Then
            for z := 0 to managerTimer.Count do
              begin
                j := timer_GetTicks;
                if currTimer^.Active then
                  begin
                    if j > currTimer^.LastTick + currTimer^.Interval Then
                      begin
                        currTimer^.LastTick := currTimer^.LastTick + currTimer^.Interval;
                        //if z > 0 Then
                        //  gui_Proc;
                        currTimer^.OnTimer;
                        j := timer_GetTicks;
                      end;
                  end else currTimer^.LastTick := timer_GetTicks;

                currTimer := currTimer^.Next;
              end;
        end else
          begin
            currTimer := @managerTimer.First;
            if currTimer <> nil Then
              for z := 0 to managerTimer.Count do
                begin
                  currTimer^.LastTick := timer_GetTicks;
                  currTimer := currTimer^.Next;
                end;
            {$IFNDEF DARWIN}
            Sleep( 10 );
            {$ENDIF}
          end;

      CanKillTimers := TRUE;
      for i := 1 to TimersToKill do
        timer_Del( aTimersToKill[ i ] );
      TimersToKill  := 0;

      dt := timer_GetTicks - t;
      if dt >= 1 Then
        begin
          if dt < 2 Then
            begin
              t := t + 1;
              OSProcess;
              zgl_draw;
            end else
              t := t + dt;
        end;
    end;
end;

function zgl_mess;
  var
  {$IFDEF LINUX}
    Event  : TXEvent;
    Keysym : TKeySym;
    Status : TStatus;
  {$ENDIF}
  {$IFDEF DARWIN}
    eClass  : UInt32;
    eKind   : UInt32;
    mPos    : Point;
    mButton : EventMouseButton;
    mWheel  : Integer;
  {$ENDIF}
    Key : DWORD;
    c   : array[ 0..1 ] of Char;
    i   : Integer;
    len : Integer;
  {$IFDEF LINUX_OR_DARWIN}
    function SCA( KeyCode : DWORD ) : DWORD;
    begin
      Result := KeyCode;
      if ( KeyCode = K_SHIFT_L ) or ( KeyCode = K_SHIFT_R ) Then Result := K_SHIFT;
      if ( KeyCode = K_CTRL_L ) or ( KeyCode = K_CTRL_R ) Then Result := K_CTRL;
      if ( KeyCode = K_ALT_L ) or ( KeyCode = K_ALT_R ) Then Result := K_ALT;
    end;
  {$ENDIF}
begin
{$IFDEF LINUX}
  while XPending( scr_Display ) <> 0 do
    begin
      XNextEvent( scr_Display, @Event );
      
      case Event._type of
        ClientMessage:
          if ( Event.xclient.message_type = wnd_Protocols ) and ( Event.xclient.data.l[ 0 ] = wnd_DestroyAtom ) Then app_Work := FALSE;

        FocusIn:
          begin
            app_Focus := TRUE;
            if app_Pause Then timer_Reset;
            app_Pause := FALSE;
            FillChar( keysDown[ 0 ], 256, 0 );
            FillChar( keysUp[ 0 ], 256, 0 );
            FillChar( mouseDown[ 0 ], 3, 0 );
            FillChar( mouseCanClick[ 0 ], 3, 1 );
            FillChar( mouseClick[ 0 ], 3, 0 );
            FillChar( mouseWheel[ 0 ], 2, 0 );
          end;
        FocusOut:
          begin
            if app_FullScreen Then exit;
            app_Focus := FALSE;
            if app_AutoPause Then app_Pause := TRUE;
          end;

        MotionNotify:
          begin
            if not mouseLock Then
              begin
                mouseX := Event.xmotion.X;
                mouseY := Event.xmotion.Y;
              end else
                begin
                  mouseX := Event.xmotion.X - wnd_Width  div 2;
                  mouseY := Event.xmotion.Y - wnd_Height div 2;
                end;
          end;
        ButtonPress:
          begin
            case Event.xbutton.button of
              1: // Left
                begin
                  mouseDown[ M_BLEFT ]  := TRUE;
                  if mouseCanClick[ M_BLEFT ] Then
                    begin
                      mouseClick[ M_BLEFT ] := TRUE;
                      mouseCanClick[ M_BLEFT ] := FALSE;
                    end;
                end;
              2: // Midle
                begin
                  mouseDown[ M_BMIDLE ] := TRUE;
                  if mouseCanClick[ M_BMIDLE ] Then
                    begin
                      mouseClick[ M_BMIDLE ] := TRUE;
                      mouseCanClick[ M_BMIDLE ] := FALSE;
                    end;
                end;
              3: // Right
                begin
                  mouseDown[ M_BRIGHT ] := TRUE;
                  if mouseCanClick[ M_BRIGHT ] Then
                    begin
                      mouseClick[ M_BRIGHT ] := TRUE;
                      mouseCanClick[ M_BRIGHT ] := FALSE;
                    end;
                end;
            end;
          end;
        ButtonRelease:
          begin
            case Event.xbutton.button of
              1: // Left
                begin
                  mouseDown[ M_BLEFT ]  := FALSE;
                  mouseUp  [ M_BLEFT ]  := TRUE;
                  mouseCanClick[ M_BLEFT ] := TRUE;
                end;
              2: // Midle
                begin
                  mouseDown[ M_BMIDLE ] := FALSE;
                  mouseUp  [ M_BMIDLE ] := TRUE;
                  mouseCanClick[ M_BMIDLE ] := TRUE;
                end;
              3: // Right
                begin
                  mouseDown[ M_BRIGHT ] := FALSE;
                  mouseUp  [ M_BRIGHT ] := TRUE;
                  mouseCanClick[ M_BRIGHT ] := TRUE;
                end;
              4: // Up Wheel
                begin
                  mouseWheel[ M_WUP ] := TRUE;
                end;
              5: // Down Wheel
                begin
                  mouseWheel[ M_WDOWN ] := TRUE;
                end;
            end;
          end;
          
        KeyPress:
          begin
            Key := xkey_to_winkey( XLookupKeysym( @Event.xkey, 0 ) );
            keysDown[ Key ] := TRUE;
            keysUp  [ Key ] := FALSE;
            keysLast[ KA_DOWN ] := Key;

            Key := SCA( Key );
            keysDown[ Key ] := TRUE;
            keysUp  [ Key ] := FALSE;

            case Key of
              K_BACKSPACE: Delete( keysText, Length( keysText ), 1 );
              K_TAB: key_InputText( '  ' );
            else
              len := Xutf8LookupString( app_XIC, @Event, @c, 2, @Keysym, @Status );
              if len > 1 Then
                key_InputText( Char( Getchar( c[ 0 ], c[ 1 ] ) ) )
              else
                if len > 0 Then
                  key_InputText( c );
            end;
          end;
        KeyRelease:
          begin
            Key := xkey_to_winkey( XLookupKeysym( @Event.xkey, 0 ) );
            keysDown[ Key ] := FALSE;
            keysUp  [ Key ] := TRUE;
            keysLast[ KA_UP ] := Key;

            Key := SCA( Key );
            keysDown[ Key ] := FALSE;
            keysUp  [ Key ] := TRUE;
          end;
      end
    end;
{$ENDIF}
{$IFDEF WIN32}
  Result := 0;
  case Msg of
    WM_CLOSE, WM_DESTROY, WM_QUIT:
      app_Work := FALSE;
      
    WM_SETFOCUS:
      begin
        app_Focus := TRUE;
        app_Pause := FALSE;
        FillChar( keysDown[ 0 ], 256, 0 );
        FillChar( keysUp[ 0 ], 256, 0 );
        FillChar( mouseDown[ 0 ], 3, 0 );
        FillChar( mouseCanClick[ 0 ], 3, 1 );
        FillChar( mouseClick[ 0 ], 3, 0 );
        FillChar( mouseWheel[ 0 ], 2, 0 );
        if app_FullScreen Then scr_SetOptions( scr_Width, scr_Height, scr_BPP, scr_Refresh, app_FullScreen, scr_VSync );
      end;
    WM_KILLFOCUS:
      begin
        app_Focus := FALSE;
        if app_AutoPause Then app_Pause := TRUE;
        if app_FullScreen Then scr_Reset;
        if app_AutoMinimize Then ShowWindow( wnd_Handle, SW_MINIMIZE );
      end;
    WM_MOVING:
      begin
        wnd_X := PRect( lParam ).Left;
        wnd_Y := PRect( lParam ).Top;
      end;
      
    WM_LBUTTONDOWN:
      begin
        mouseDown[ M_BLEFT ]  := TRUE;
        if mouseCanClick[ M_BLEFT ] Then
          begin
            mouseClick[ M_BLEFT ] := TRUE;
            mouseCanClick[ M_BLEFT ] := FALSE;
          end;
      end;
    WM_MBUTTONDOWN:
      begin
        mouseDown[ M_BMIDLE ] := TRUE;
        if mouseCanClick[ M_BMIDLE ] Then
          begin
            mouseClick[ M_BMIDLE ] := TRUE;
            mouseCanClick[ M_BMIDLE ] := FALSE;
          end;
      end;
    WM_RBUTTONDOWN:
      begin
        mouseDown[ M_BRIGHT ] := TRUE;
        if mouseCanClick[ M_BRIGHT ] Then
          begin
            mouseClick[ M_BRIGHT ] := TRUE;
            mouseCanClick[ M_BRIGHT ] := FALSE;
          end;
      end;
    WM_LBUTTONUP:
      begin
        mouseDown[ M_BLEFT ]  := FALSE;
        mouseUp  [ M_BLEFT ]  := TRUE;
        mouseCanClick[ M_BLEFT ] := TRUE;
      end;
    WM_MBUTTONUP:
      begin
        mouseDown[ M_BMIDLE ] := FALSE;
        mouseUp  [ M_BMIDLE ] := TRUE;
        mouseCanClick[ M_BMIDLE ] := TRUE;
      end;
    WM_RBUTTONUP:
      begin
        mouseDown[ M_BRIGHT ] := FALSE;
        mouseUp  [ M_BRIGHT ] := TRUE;
        mouseCanClick[ M_BRIGHT ] := TRUE;
      end;
    WM_MOUSEWHEEL:
      begin
        if wParam > 0 Then
          begin
            mouseWheel[ M_WUP   ] := TRUE;
            mouseWheel[ M_WDOWN ] := FALSE;
          end else
            begin
              mouseWheel[ M_WUP   ] := FALSE;
              mouseWheel[ M_WDOWN ] := TRUE;
            end;
      end;
      
    WM_KEYDOWN{, WM_SYSKEYDOWN}:
      begin
        Key := wParam;
        keysDown[ Key ] := TRUE;
        keysUp  [ Key ] := FALSE;
        keysLast[ KA_DOWN ] := Key;
      end;
    WM_KEYUP{, WM_SYSKEYUP}:
      begin
        Key := wParam;
        keysDown[ Key ] := FALSE;
        keysUp  [ Key ] := TRUE;
        keysLast[ KA_UP ] := Key;
      end;
    WM_CHAR:
      begin
        case wParam of
          K_BACKSPACE: Delete( keysText, Length( keysText ), 1 );
          K_TAB: key_InputText( '  ' );
        else
          if wParam >= 32 Then
            key_InputText( Chr( wParam ) );
        end;
      end;
  else
    Result := DefWindowProc( hWnd, Msg, wParam, lParam );
  end;
{$ENDIF}
{$IFDEF DARWIN}
  eClass := GetEventClass( inEvent );
  eKind  := GetEventKind( inEvent );

  Result := CallNextEventHandler( inHandlerCallRef, inEvent );

  case eClass of
    kEventClassWindow:
      case eKind of
        kEventWindowActivated:
          begin
            app_Focus := TRUE;
            if app_Pause Then timer_Reset;
            app_Pause := FALSE;
            FillChar( keysDown[ 0 ], 256, 0 );
            FillChar( keysUp[ 0 ], 256, 0 );
            FillChar( mouseDown[ 0 ], 3, 0 );
            FillChar( mouseCanClick[ 0 ], 3, 1 );
            FillChar( mouseClick[ 0 ], 3, 0 );
            FillChar( mouseWheel[ 0 ], 2, 0 );
          end;
        kEventWindowDeactivated:
          begin
            if app_FullScreen Then exit;
            app_Focus := FALSE;
            if app_AutoPause Then app_Pause := TRUE;
          end;
        kEventWindowClosed:
          begin
            wnd_Handle := nil;
            app_Work   := FALSE;
          end;
      end;

    kEventClassKeyboard:
      begin
        GetEventParameter( inEvent, kEventParamKeyMacCharCodes, typeChar, nil, 1, nil, @c[ 0 ] );
        GetEventParameter( inEvent, kEventParamKeyCode, typeUInt32, nil, 4, nil, @Key );

        case eKind of
          kEventRawKeyDown, kEventRawKeyRepeat:
            begin
              Key := mackeys_to_winkeys( Key );
              keysDown[ Key ] := TRUE;
              keysUp  [ Key ] := FALSE;
              keysLast[ KA_DOWN ] := Key;

              Key := SCA( Key );
              keysDown[ Key ] := TRUE;
              keysUp  [ Key ] := FALSE;

              case Byte( c[ 0 ] ) of
                K_BACKSPACE: Delete( keysText, Length( keysText ), 1 );
                K_TAB: key_InputText( '  ' );
              else
                key_InputText( c[ 0 ] );
              end;
            end;
          kEventRawKeyUp:
            begin
              Key := mackeys_to_winkeys( Key );
              keysDown[ Key ] := FALSE;
              keysUp  [ Key ] := TRUE;
              keysLast[ KA_UP ] := Key;

              Key := SCA( Key );
              keysDown[ Key ] := TRUE;
              keysUp  [ Key ] := FALSE;
            end;
        end;
      end;

    kEventClassMouse:
      case eKind of
        kEventMouseMoved:
          begin
            GetEventParameter( inEvent, kEventParamWindowMouseLocation, typeQDPoint, 0, SizeOf( Point ), nil, @mPos);

            if not mouseLock Then
              begin
                mouseX := mPos.h;
                mouseY := mPos.v;
              end else
                begin
                  mouseX := mPos.h - wnd_Width  div 2;
                  mouseY := mPos.v - wnd_Height div 2;
                end;
          end;
        kEventMouseDown:
          begin
            GetEventParameter( inEvent, kEventParamMouseButton, typeMouseButton, nil, SizeOf( EventMouseButton ), nil, @mButton );

            case mButton of
              kEventMouseButtonPrimary: // Left
                begin
                  mouseDown[ M_BLEFT ]  := TRUE;
                  if mouseCanClick[ M_BLEFT ] Then
                    begin
                      mouseClick[ M_BLEFT ] := TRUE;
                      mouseCanClick[ M_BLEFT ] := FALSE;
                    end;
                end;
              kEventMouseButtonTertiary: // Midle
                begin
                  mouseDown[ M_BMIDLE ] := TRUE;
                  if mouseCanClick[ M_BMIDLE ] Then
                    begin
                      mouseClick[ M_BMIDLE ] := TRUE;
                      mouseCanClick[ M_BMIDLE ] := FALSE;
                    end;
                end;
              kEventMouseButtonSecondary: // Right
                begin
                  mouseDown[ M_BRIGHT ] := TRUE;
                  if mouseCanClick[ M_BRIGHT ] Then
                    begin
                      mouseClick[ M_BRIGHT ] := TRUE;
                      mouseCanClick[ M_BRIGHT ] := FALSE;
                    end;
                end;
            end;
          end;
        kEventMouseUp:
          begin
            GetEventParameter( inEvent, kEventParamMouseButton, typeMouseButton, nil, SizeOf( EventMouseButton ), nil, @mButton );

            case mButton of
              kEventMouseButtonPrimary: // Left
                begin
                  mouseDown[ M_BLEFT ]  := FALSE;
                  mouseUp  [ M_BLEFT ]  := TRUE;
                  mouseCanClick[ M_BLEFT ] := TRUE;
                end;
              kEventMouseButtonTertiary: // Midle
                begin
                  mouseDown[ M_BMIDLE ] := FALSE;
                  mouseUp  [ M_BMIDLE ] := TRUE;
                  mouseCanClick[ M_BMIDLE ] := TRUE;
                end;
              kEventMouseButtonSecondary: // Right
                begin
                  mouseDown[ M_BRIGHT ] := FALSE;
                  mouseUp  [ M_BRIGHT ] := TRUE;
                  mouseCanClick[ M_BRIGHT ] := TRUE;
                end;
            end;
          end;
        kEventMouseWheelMoved:
          begin
            GetEventParameter( inEvent, kEventParamMouseWheelDelta, typeSInt32, nil, 4, nil, @mWheel );

            if mWheel > 0 then
              mouseWheel[ M_WUP ] := TRUE
            else
              mouseWheel[ M_WDOWN ] := TRUE;
          end;
      end;
  end;
{$ENDIF}
end;

initialization
  zgl_Reg( TEX_FORMAT_EXTENSION, PChar( 'jpg' ) );
  zgl_Reg( TEX_FORMAT_LOADER, @jpg_LoadFromFile );
  zgl_Reg( TEX_FORMAT_EXTENSION, PChar( 'png' ) );
  zgl_Reg( TEX_FORMAT_LOADER, @png_LoadFromFile );
  zgl_Reg( TEX_FORMAT_EXTENSION, PChar( 'tga' ) );
  zgl_Reg( TEX_FORMAT_LOADER, @tga_LoadFromFile );

  zgl_Reg( SND_FORMAT_EXTENSION, PChar( 'wav' ) );
  zgl_Reg( SND_FORMAT_LOADER, @wav_LoadFromFile );

end.
