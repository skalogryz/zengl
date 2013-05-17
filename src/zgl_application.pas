{
 *  Copyright (c) 2012 Andrey Kemka
 *
 *  This software is provided 'as-is', without any express or
 *  implied warranty. In no event will the authors be held
 *  liable for any damages arising from the use of this software.
 *
 *  Permission is granted to anyone to use this software for any purpose,
 *  including commercial applications, and to alter it and redistribute
 *  it freely, subject to the following restrictions:
 *
 *  1. The origin of this software must not be misrepresented;
 *     you must not claim that you wrote the original software.
 *     If you use this software in a product, an acknowledgment
 *     in the product documentation would be appreciated but
 *     is not required.
 *
 *  2. Altered source versions must be plainly marked as such,
 *     and must not be misrepresented as being the original software.
 *
 *  3. This notice may not be removed or altered from any
 *     source distribution.
}
unit zgl_application;

{$I zgl_config.cfg}

interface
uses
  Windows,
  Messages,
  zgl_types,
  zgl_direct3d,
  zgl_direct3d_all;

procedure app_Init;
procedure app_MainLoop;
function  app_CloseQuery : Boolean;
procedure app_ProcessOS;
function  app_ProcessMessages( hWnd : HWND; Msg : UINT; wParam : WPARAM; lParam : LPARAM ) : LRESULT; stdcall;

var
  appInitialized    : Boolean;
  appGotSysDirs     : Boolean;
  appWork           : Boolean;
  appWorkTime       : LongWord;
  appPause          : Boolean;
  appAutoPause      : Boolean = TRUE;
  appFocus          : Boolean = TRUE;
  appLog            : Boolean;
  appInitedToHandle : Boolean;
  appWorkDir        : UTF8String;
  appHomeDir        : UTF8String;

  // call-back
  app_PInit       : procedure;
  app_PLoop       : procedure;
  app_PLoad       : procedure;
  app_PDraw       : procedure;
  app_PExit       : procedure;
  app_PUpdate     : procedure( dt : Double );
  app_PActivate   : procedure( activate : Boolean );
  app_PCloseQuery : function : Boolean;

  appTimer      : LongWord;
  appMinimized  : Boolean;
  appShowCursor : Boolean;

  appdt : Double;

  appFPS      : LongWord;
  appFPSCount : LongWord;
  appFPSAll   : LongWord;

  appFlags : LongWord;

implementation
uses
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_render,
  zgl_mouse,
  zgl_keyboard,
  {$IFDEF USE_JOYSTICK}
  zgl_joystick,
  {$ENDIF}
  zgl_timers,
  zgl_resources,
  zgl_textures,
  zgl_font,
  {$IFDEF USE_SOUND}
  zgl_sound,
  {$ENDIF}
  zgl_utils;

procedure app_Draw;
begin
  SetCurrentMode();
  scr_Clear();
  if Assigned( app_PDraw ) Then
    app_PDraw();
  scr_Flush();
  if not appPause Then
    INC( appFPSCount );
end;

procedure app_CalcFPS;
begin
  appFPS      := appFPSCount;
  appFPSAll   := appFPSAll + appFPSCount;
  appFPSCount := 0;
  INC( appWorkTime );
end;

procedure app_Init;
begin
  managerZeroTexture := tex_CreateZero( 4, 4, $FFFFFFFF, TEX_DEFAULT_2D );

  SetCurrentMode();
  scr_Clear();
  if Assigned( app_PLoad ) Then
    app_PLoad();
  scr_Flush();

  res_Init();

  appdt := timer_GetTicks();
  timer_Reset();
  timer_Add( @app_CalcFPS, 1000 );
end;

procedure app_MainLoop;
  var
    t : Double;
begin
  while appWork do
    begin
      app_ProcessOS();
      res_Proc();
      {$IFDEF USE_JOYSTICK}
      joy_Proc();
      {$ENDIF}
      {$IFDEF USE_SOUND}
      snd_MainLoop();
      {$ENDIF}

      if appPause Then
        begin
          timer_Reset();
          appdt := timer_GetTicks();
          u_Sleep( 10 );
          continue;
        end else
          if d3d_BeginScene Then
            timer_MainLoop()
          else
            continue;

      t := timer_GetTicks();
      // Workaround for bug with unstable time between frames...
      if Assigned( app_PUpdate ) and ( scrVSync ) and ( appFPS > 0 ) and ( appFPS = scrRefresh ) and ( appFlags and APP_USE_DT_CORRECTION > 0 ) Then
        app_PUpdate( 1000 / appFPS )
      else
        if Assigned( app_PUpdate ) Then
          app_PUpdate( timer_GetTicks() - appdt );
      appdt := t;

      app_Draw();
    end;
end;

function  app_CloseQuery : Boolean;
begin
  Result := TRUE;
end;

procedure app_ProcessOS;
  var
    m         : tagMsg;
    cursorpos : TPoint;
begin
  GetCursorPos( cursorpos );
  if wndFullScreen Then
    begin
      mouseX := cursorpos.X;
      mouseY := cursorpos.Y;
    end else
      begin
        mouseX := cursorpos.X - wndX - wndBrdSizeX;
        mouseY := cursorpos.Y - wndY - wndBrdSizeY - wndCpnSize;
      end;

  if appFlags and CORRECT_RESOLUTION > 0 Then
    begin
      mouseDX := Round( ( mouseX - wndWidth div 2 - scrAddCX ) / scrResCX );
      mouseDY := Round( ( mouseY - wndHeight div 2 - scrAddCY ) / scrResCY );
      mouseX  := Round( ( mouseX - scrAddCX ) / scrResCX );
      mouseY  := Round( ( mouseY - scrAddCY ) / scrResCY );
    end else
      begin
        mouseDX := mouseX - wndWidth div 2;
        mouseDY := mouseY - wndHeight div 2;
        mouseX  := mouseX;
        mouseY  := mouseY;
      end;
  if ( mouseLX <> mouseX ) or ( mouseLY <> mouseY ) Then
    begin
      mouseLX := mouseX;
      mouseLY := mouseY;

      if Assigned( mouse_PMove ) Then
        mouse_PMove( mouseX, mouseY );
    end;

  while PeekMessageW( m, 0{wnd_Handle}, 0, 0, PM_REMOVE ) do
    begin
      TranslateMessage( m );
      DispatchMessageW( m );
    end;
end;

function app_ProcessMessages( hWnd : HWND; Msg : UINT; wParam : WPARAM; lParam : LPARAM ) : LRESULT; stdcall;
  var
    len : Integer;
    c   : array[ 0..5 ] of AnsiChar;
    str : UTF8String;
    key : LongWord;
begin
  Result := 0;
  if ( not appWork ) and ( Msg <> WM_ACTIVATEAPP ) Then
    begin
      Result := DefWindowProcW( hWnd, Msg, wParam, lParam );
      exit;
    end;
  case Msg of
    WM_CLOSE, WM_DESTROY, WM_QUIT:
      appWork := not app_PCloseQuery();

    WM_PAINT:
      begin
        app_Draw();
        ValidateRect( wndHandle, nil );
      end;
    WM_DISPLAYCHANGE:
      begin
        scr_Init();
      end;
    WM_ACTIVATEAPP:
      begin
        if appMinimized Then exit;
        if ( wParam > 0 ) and ( not appFocus ) Then
          begin
            appPause := FALSE;
            if appWork and Assigned( app_PActivate ) Then
              app_PActivate( TRUE );
            FillChar( keysDown[ 0 ], 256, 0 );
            key_ClearState();
            FillChar( mouseDown[ 0 ], 3, 0 );
            mouse_ClearState();
          end else
            if ( wParam = 0 ) and ( appFocus ) Then
              begin
                if appAutoPause Then appPause := TRUE;
                if appWork and Assigned( app_PActivate ) Then
                  app_PActivate( FALSE );
              end;
        appFocus := wParam > 0;
      end;
    WM_SIZE:
      begin
        if wParam = SIZE_MINIMIZED Then
          begin
            SendMessage( wndHandle, WM_ACTIVATEAPP, 0, 0 );
            appMinimized := TRUE;
          end;
        if ( wParam = SIZE_MAXIMIZED ) or ( wParam = SIZE_RESTORED ) Then
          begin
            appMinimized := FALSE;
            SendMessage( wndHandle, WM_ACTIVATEAPP, 1, 0 );
          end;
      end;
    WM_SHOWWINDOW:
      begin
        if ( wParam = 0 ) and ( appFocus ) Then
          SendMessage( wndHandle, WM_ACTIVATE, WA_INACTIVE, 0 )
        else
          if ( wParam = 1 ) and ( not appFocus ) Then
            SendMessage( wndHandle, WM_ACTIVATE, WA_ACTIVE, 0 );
      end;
    WM_NCHITTEST:
      begin
        Result := DefWindowProcW( hWnd, Msg, wParam, lParam );
        if ( not appFocus ) and ( Result = HTCAPTION ) Then
          Result := HTCLIENT;
      end;
    WM_ENTERSIZEMOVE:
      begin
        if not appAutoPause Then
          appTimer := SetTimer( wndHandle, 1, 1, nil );
      end;
    WM_EXITSIZEMOVE:
      begin
        if appTimer > 0 Then
          begin
            KillTimer( wndHandle, appTimer );
            appTimer := 0;
          end;
      end;
    WM_MOVING:
      begin
        wndX := PRect( lParam ).Left;
        wndY := PRect( lParam ).Top;
        if appAutoPause Then
          timer_Reset();
      end;
    WM_TIMER:
      begin
        timer_MainLoop();
        app_Draw();
      end;
    WM_SETCURSOR:
      begin
        if ( appFocus ) and ( LOWORD ( lparam ) = HTCLIENT ) and ( not appShowCursor ) Then
          SetCursor( 0 )
        else
          SetCursor( LoadCursor( 0, IDC_ARROW ) );
      end;

    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
      begin
        mouseDown[ M_BLEFT ] := TRUE;
        if mouseCanClick[ M_BLEFT ] Then
          begin
            mouseClick[ M_BLEFT ]    := TRUE;
            mouseCanClick[ M_BLEFT ] := FALSE;
          end;
        if Msg = WM_LBUTTONDBLCLK Then
          mouseDblClick[ M_BLEFT ] := TRUE;

        if Assigned( mouse_PPress ) Then
          mouse_PPress( M_BLEFT );
      end;
    WM_MBUTTONDOWN, WM_MBUTTONDBLCLK:
      begin
        mouseDown[ M_BMIDDLE ] := TRUE;
        if mouseCanClick[ M_BMIDDLE ] Then
          begin
            mouseClick[ M_BMIDDLE ]    := TRUE;
            mouseCanClick[ M_BMIDDLE ] := FALSE;
          end;
        if Msg = WM_MBUTTONDBLCLK Then
          mouseDblClick[ M_BMIDDLE ] := TRUE;

        if Assigned( mouse_PPress ) Then
          mouse_PPress( M_BMIDDLE );
      end;
    WM_RBUTTONDOWN, WM_RBUTTONDBLCLK:
      begin
        mouseDown[ M_BRIGHT ] := TRUE;
        if mouseCanClick[ M_BRIGHT ] Then
          begin
            mouseClick[ M_BRIGHT ]    := TRUE;
            mouseCanClick[ M_BRIGHT ] := FALSE;
          end;
        if Msg = WM_RBUTTONDBLCLK Then
          mouseDblClick[ M_BRIGHT ] := TRUE;

        if Assigned( mouse_PPress ) Then
          mouse_PPress( M_BRIGHT );
      end;
    WM_LBUTTONUP:
      begin
        mouseDown[ M_BLEFT ]     := FALSE;
        mouseUp  [ M_BLEFT ]     := TRUE;
        mouseCanClick[ M_BLEFT ] := TRUE;

        if Assigned( mouse_PRelease ) Then
          mouse_PRelease( M_BLEFT );
      end;
    WM_MBUTTONUP:
      begin
        mouseDown[ M_BMIDDLE ]     := FALSE;
        mouseUp  [ M_BMIDDLE ]     := TRUE;
        mouseCanClick[ M_BMIDDLE ] := TRUE;

        if Assigned( mouse_PRelease ) Then
          mouse_PRelease( M_BMIDDLE );
      end;
    WM_RBUTTONUP:
      begin
        mouseDown[ M_BRIGHT ]     := FALSE;
        mouseUp  [ M_BRIGHT ]     := TRUE;
        mouseCanClick[ M_BRIGHT ] := TRUE;

        if Assigned( mouse_PRelease ) Then
          mouse_PRelease( M_BRIGHT );
      end;
    WM_MOUSEWHEEL:
      begin
        if SmallInt( wParam shr 16 ) > 0 Then
          begin
            mouseWheel[ M_WUP   ] := TRUE;
            mouseWheel[ M_WDOWN ] := FALSE;

            if Assigned( mouse_PWheel ) Then
              mouse_PWheel( M_WUP );
          end else
            begin
              mouseWheel[ M_WUP   ] := FALSE;
              mouseWheel[ M_WDOWN ] := TRUE;

              if Assigned( mouse_PWheel ) Then
                mouse_PWheel( M_WDOWN );
            end;
      end;

    WM_KEYDOWN, WM_SYSKEYDOWN:
      begin
        key := winkey_to_scancode( wParam );
        keysDown[ key ]     := TRUE;
        keysUp  [ key ]     := FALSE;
        keysLast[ KA_DOWN ] := key;
        doKeyPress( key );

        key := SCA( key );
        keysDown[ key ] := TRUE;
        keysUp  [ key ] := FALSE;
        doKeyPress( key );

        if Assigned( key_PPress ) Then
          key_PPress( key );

        if ( Msg = WM_SYSKEYDOWN ) and ( key = K_F4 ) Then
          appWork := not app_PCloseQuery();
      end;
    WM_KEYUP, WM_SYSKEYUP:
      begin
        key := winkey_to_scancode( wParam );
        keysDown[ key ]   := FALSE;
        keysUp  [ key ]   := TRUE;
        keysLast[ KA_UP ] := key;

        key := SCA( key );
        keysDown[ key ] := FALSE;
        keysUp  [ key ] := TRUE;

        if Assigned( key_PRelease ) Then
          key_PRelease( key );
      end;
    WM_CHAR:
      begin
        if keysCanText Then
        case winkey_to_scancode( wParam ) of
          K_ENTER:;
          K_BACKSPACE: utf8_Backspace( keysText );
          K_TAB:       key_InputText( '  ' );
        else
          len := WideCharToMultiByte( CP_UTF8, 0, @wParam, 1, nil, 0, nil, nil );
          if len > 0 Then
            begin
              WideCharToMultiByte( CP_UTF8, 0, @wParam, 1, @c[ 0 ], 5, nil, nil );
              SetLength( str, len );
              Move( c[ 0 ], str[ 1 ], len );
              key_InputText( str );
            end;
        end;
      end;
  else
    Result := DefWindowProcW( hWnd, Msg, wParam, lParam );
  end;
end;

initialization
  app_PInit       := @app_Init;
  app_PLoop       := @app_MainLoop;
  app_PCloseQuery := @app_CloseQuery;

  appFlags := WND_USE_AUTOCENTER or APP_USE_LOG or COLOR_BUFFER_CLEAR or CLIP_INVISIBLE or APP_USE_AUTOPAUSE or APP_USE_DT_CORRECTION;

end.
