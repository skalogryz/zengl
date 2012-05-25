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
unit zgl_application;

{$I zgl_config.cfg}

interface
uses
  Windows,
  Messages,
  zgl_types,
  zgl_direct3d,
  zgl_direct3d_all;

procedure zero;
procedure zerou( dt : Double );
procedure zeroa( activate : Boolean );

procedure app_Init;
procedure app_MainLoop;
procedure app_ProcessOS;
function  app_ProcessMessages( hWnd : HWND; Msg : UINT; wParam : WPARAM; lParam : LPARAM ) : LRESULT; stdcall;
procedure app_CalcFPS;

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
  appWorkDir        : String;
  appHomeDir        : String;

  // call-back
  app_PInit     : procedure = app_Init;
  app_PLoop     : procedure = app_MainLoop;
  app_PLoad     : procedure = zero;
  app_PDraw     : procedure = zero;
  app_PExit     : procedure = zero;
  app_PUpdate   : procedure( dt : Double ) = zerou;
  app_PActivate : procedure( activate : Boolean ) = zeroa;

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
  zgl_log,
  zgl_mouse,
  zgl_keyboard,
  {$IFDEF USE_JOYSTICK}
  zgl_joystick,
  {$ENDIF}
  zgl_timers,
  zgl_font,
  {$IFDEF USE_SOUND}
  zgl_sound,
  {$ENDIF}
  zgl_utils;

var
  // workaround for resolution changng
  d3dwrc : Boolean;

procedure zero;  begin end;
procedure zerou; begin end;
procedure zeroa; begin end;

procedure app_Draw;
begin
  SetCurrentMode();
  scr_Clear();
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
  SetCurrentMode();
  scr_Clear();
  app_PLoad();
  scr_Flush();

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
      {$IFDEF WINDOWS}
      // Workaround for bug with unstable time between frames...
      if ( scrVSync ) and ( appFPS > 0 ) and ( appFPS = scrRefresh ) and ( appFlags and APP_USE_DT_CORRECTION > 0 ) Then
        app_PUpdate( 1000 / appFPS )
      else
      {$ENDIF}
      app_PUpdate( timer_GetTicks() - appdt );
      appdt := t;

      app_Draw();
    end;
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

  while PeekMessageW( m, 0{wnd_Handle}, 0, 0, PM_REMOVE ) do
    begin
      TranslateMessage( m );
      DispatchMessageW( m );
    end;

  d3dwrc := FALSE;
end;

function app_ProcessMessages( hWnd : HWND; Msg : UINT; wParam : WPARAM; lParam : LPARAM ) : LRESULT; stdcall;
  var
    focus : Boolean;
    i     : Integer;
    len   : Integer;
    c     : array[ 0..5 ] of AnsiChar;
    str   : AnsiString;
    key   : LongWord;
begin
  Result := 0;
  if ( not appWork ) and ( Msg <> WM_ACTIVATEAPP ) Then
    begin
      Result := DefWindowProcW( hWnd, Msg, wParam, lParam );
      exit;
    end;
  case Msg of
    WM_CLOSE, WM_DESTROY, WM_QUIT:
      appWork := FALSE;

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
            d3dwrc   := TRUE;
            appPause := FALSE;
            if appWork Then app_PActivate( TRUE );
            FillChar( keysDown[ 0 ], 256, 0 );
            key_ClearState();
            FillChar( mouseDown[ 0 ], 3, 0 );
            mouse_ClearState();
          end else
            if ( wParam = 0 ) and ( appFocus ) Then
              begin
                if appAutoPause Then appPause := TRUE;
                if appWork Then app_PActivate( FALSE );
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
      end;
    WM_LBUTTONUP:
      begin
        mouseDown[ M_BLEFT ]     := FALSE;
        mouseUp  [ M_BLEFT ]     := TRUE;
        mouseCanClick[ M_BLEFT ] := TRUE;
      end;
    WM_MBUTTONUP:
      begin
        mouseDown[ M_BMIDDLE ]     := FALSE;
        mouseUp  [ M_BMIDDLE ]     := TRUE;
        mouseCanClick[ M_BMIDDLE ] := TRUE;
      end;
    WM_RBUTTONUP:
      begin
        mouseDown[ M_BRIGHT ]     := FALSE;
        mouseUp  [ M_BRIGHT ]     := TRUE;
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

        if ( Msg = WM_SYSKEYDOWN ) and ( key = K_F4 ) Then
          appWork := FALSE;
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
      end;
    WM_CHAR:
      begin
        if keysCanText Then
        case winkey_to_scancode( wParam ) of
          K_ENTER:;
          K_BACKSPACE: u_Backspace( keysText );
          K_TAB:       key_InputText( '  ' );
        else
          if appFlags and APP_USE_UTF8 > 0 Then
            begin
              {$IFNDEF FPC}
              if SizeOf( Char ) = 1 Then
                begin
              {$ENDIF}
                  len := WideCharToMultiByte( CP_UTF8, 0, @wParam, 1, nil, 0, nil, nil );
                  WideCharToMultiByte( CP_UTF8, 0, @wParam, 1, @c[ 0 ], 5, nil, nil );
                  str := '';
                  for i := 0 to len - 1 do
                    str := str + c[ i ];
                  if str <> '' Then
                    key_InputText( str );
              {$IFNDEF FPC}
                end else
                  key_InputText( Char( wParam ) );
              {$ENDIF}
            end else
            {$IFNDEF FPC}
              if SizeOf( Char ) = 2 Then
                key_InputText( Char( wParam ) )
              else
            {$ENDIF}
              if wParam < 128 Then
                key_InputText( Char( CP1251_TO_UTF8[ wParam ] ) )
              else
                for i := 128 to 255 do
                  if wParam = CP1251_TO_UTF8[ i ] Then
                    begin
                      key_InputText( Char( i ) );
                      break;
                    end;
        end;
      end;
  else
    Result := DefWindowProcW( hWnd, Msg, wParam, lParam );
  end;
end;

initialization
  appFlags := WND_USE_AUTOCENTER or APP_USE_LOG or COLOR_BUFFER_CLEAR or CLIP_INVISIBLE or APP_USE_DT_CORRECTION;

end.
