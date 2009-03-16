{
 * Copyright © Kemka Andrey aka Andru
 * mail: dr.andru@gmail.com
 * site: http://andru-kun.ru
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
unit zgl_application;

{$I zgl_config.cfg}

interface
uses
  {$IFDEF LINUX}
  X, XLib,
  {$ENDIF}
  {$IFDEF WIN32}
  Windows,
  Messages,
  {$ENDIF}
  {$IFDEF DARWIN}
  MacOSAll,
  {$ENDIF}
  zgl_const,
  zgl_opengl_all
  ;

procedure zero;
procedure zerou( dt : Double );

procedure app_MainLoop;
{$IFDEF LINUX}
function app_ProcessMessages : DWORD;
{$ENDIF}
{$IFDEF WIN32}
function app_ProcessMessages( hWnd : HWND; Msg : UINT; wParam : WPARAM; lParam : LPARAM ) : LRESULT; stdcall;
{$ENDIF}
{$IFDEF DARWIN}
function app_ProcessMessages( inHandlerCallRef: EventHandlerCallRef; inEvent: EventRef; inUserData: UnivPtr ): OSStatus; cdecl;
{$ENDIF}
procedure app_CalcFPS;

var
  app_Work         : Boolean;
  app_WorkTime     : DWORD;
  app_Pause        : Boolean;
  app_AutoPause    : Boolean = TRUE;
  app_Focus        : Boolean;
  app_AutoMinimize : Boolean = TRUE;
  app_Log          : Boolean;
  app_InitToHandle : Boolean;

  // call-back
  app_PLoad   : procedure = zero;
  app_PDraw   : procedure = zero;
  app_PExit   : procedure = zero;
  app_PUpdate : procedure( dt : Double ) = zerou;

  {$IFDEF LINUX}
  app_Cursor : TCursor = None;
  app_XIM    : PXIM;
  app_XIC    : PXIC;
  {$ENDIF}
  app_ShowCursor : Boolean = TRUE;

  app_FPS      : DWORD = 1000;
  app_FPSCount : DWORD;
  app_FPSAll   : DWORD;

  app_Flags : DWORD = APP_USE_LOG or COLOR_BUFFER_CLEAR or DEPTH_BUFFER or DEPTH_BUFFER_CLEAR or CROP_INVISIBLE;

implementation
uses
  zgl_screen,
  zgl_window,
  zgl_opengl,
  zgl_log,
  zgl_keyboard,
  zgl_mouse,
  zgl_timers,
  zgl_utils;

procedure zero;
begin
end;
procedure zerou;
begin
end;

procedure OSProcess;
  {$IFDEF WIN32}
  var
    Mess : tagMsg;
  {$ENDIF}
  {$IFDEF DARWIN}
  var
    Event    : EventRecord;
    Window   : WindowRef;
    PartCode : WindowPartCode;
  {$ENDIF}
begin
{$IFDEF LINUX}
  app_ProcessMessages;
{$ENDIF}
{$IFDEF WIN32}
  while PeekMessage( Mess, wnd_Handle, 0, 0, PM_REMOVE ) do
    begin
      TranslateMessage( Mess );
      DispatchMessage( Mess );
    end;
{$ENDIF}
{$IFDEF DARWIN}
  while GetNextEvent( everyEvent, Event ) do;
{$ENDIF}
end;

procedure app_MainLoop;
  var
    i, z : Integer;
    j, t, dt, odt : Double;
    currTimer : zglPTimer;
    {$IFDEF WIN32}
    SysInfo : _SYSTEM_INFO;
    Mess : tagMsg;
    {$ENDIF}
begin
  {$IFDEF WIN32}
  // Багнутое MS-поделко требует патча :)
  // Вешаем все на одно ядро
  GetSystemInfo( SysInfo );
  SetProcessAffinityMask( GetCurrentProcess, SysInfo.dwActiveProcessorMask );
  {$ENDIF}

  scr_Clear;
  app_PLoad;
  scr_Flush;

  t   := timer_GetTicks;
  odt := timer_GetTicks;
  timer_Reset;
  timer_Add( @app_CalcFPS, 1000 );
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
            u_Sleep( 10 );
          end;

      CanKillTimers := TRUE;
      for i := 1 to TimersToKill do
        timer_Del( aTimersToKill[ i ] );
      TimersToKill  := 0;

      app_PUpdate( timer_GetTicks - odt );
      odt := timer_GetTicks;

      dt := timer_GetTicks - t;
      if dt >= 1 Then
        begin
          if dt < 2 Then
            begin
              t := t + 1;

              scr_Clear;
              app_PDraw;
              scr_Flush;
              if not app_Pause Then
                INC( app_FPSCount );
            end else
              t := t + dt;
        end;
    end;
end;

function app_ProcessMessages;
  var
  {$IFDEF LINUX}
    Event  : TXEvent;
    Keysym : TKeySym;
    Status : TStatus;
  {$ENDIF}
  {$IFDEF DARWIN}
    eClass  : UInt32;
    eKind   : UInt32;
    mPos    : HIPoint;
    mButton : EventMouseButton;
    mWheel  : Integer;
  {$ENDIF}
    i   : Integer;
    len : Integer;
    c   : array[ 0..5 ] of Char;
    Key : DWORD;
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
            if wnd_FullScreen Then exit;
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
              K_BACKSPACE: u_Backspace( keysText );
              K_TAB: key_InputText( '  ' );
            else
              len := Xutf8LookupString( app_XIC, @Event, @c[ 0 ], 6, @Keysym, @Status );
              for i := 0 to len - 1 do
                key_InputText( c[ i ] );
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
        if wnd_FullScreen Then scr_SetOptions( scr_Width, scr_Height, scr_BPP, scr_Refresh, wnd_FullScreen, scr_VSync );
      end;
    WM_KILLFOCUS:
      begin
        app_Focus := FALSE;
        if app_AutoPause Then app_Pause := TRUE;
        if wnd_FullScreen Then scr_Reset;
        if app_AutoMinimize Then ShowWindow( wnd_Handle, SW_MINIMIZE );
      end;
    WM_MOVING:
      begin
        wnd_X := PRect( lParam ).Left;
        wnd_Y := PRect( lParam ).Top;
      end;

    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
      begin
        mouseDown[ M_BLEFT ]  := TRUE;
        if mouseCanClick[ M_BLEFT ] Then
          begin
            mouseClick[ M_BLEFT ] := TRUE;
            mouseCanClick[ M_BLEFT ] := FALSE;
          end;
      end;
    WM_MBUTTONDOWN, WM_MBUTTONDBLCLK:
      begin
        mouseDown[ M_BMIDLE ] := TRUE;
        if mouseCanClick[ M_BMIDLE ] Then
          begin
            mouseClick[ M_BMIDLE ] := TRUE;
            mouseCanClick[ M_BMIDLE ] := FALSE;
          end;
      end;
    WM_RBUTTONDOWN, WM_RBUTTONDBLCLK:
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

    WM_KEYDOWN://, WM_SYSKEYDOWN:
      begin
        Key := wParam;
        keysDown[ Key ] := TRUE;
        keysUp  [ Key ] := FALSE;
        keysLast[ KA_DOWN ] := Key;
      end;
    WM_KEYUP://, WM_SYSKEYUP:
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
          if ( wParam >= 32 ) and ( wParam <= 127 ) Then
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
            if wnd_FullScreen Then exit;
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

              case Key of
                K_BACKSPACE: u_Backspace( keysText );
                K_TAB: key_InputText( '  ' );
              else
                //GetEventParameter( inEvent, kEventParamKeyMacCharCodes, typeChar, nil, 1, nil, @c[ 0 ] );
                GetEventParameter( inEvent, kEventParamKeyUnicodes, typeUnicodeText, nil, 6, @len, @c[ 0 ] );
                for i := 0 to len - 1 do
                  key_InputText( c[ i ] );
              end;
            end;
          kEventRawKeyUp:
            begin
              Key := mackeys_to_winkeys( Key );
              keysDown[ Key ] := FALSE;
              keysUp  [ Key ] := TRUE;
              keysLast[ KA_UP ] := Key;

              Key := SCA( Key );
              keysDown[ Key ] := FALSE;
              keysUp  [ Key ] := TRUE;
            end;
        end;
      end;

    kEventClassMouse:
      case eKind of
        kEventMouseMoved, kEventMouseDragged:
          begin
            if wnd_FullScreen Then
              GetEventParameter( inEvent, kEventParamMouseLocation, typeHIPoint, 0, SizeOf( HIPoint ), nil, @mPos )
            else
              GetEventParameter( inEvent, kEventParamWindowMouseLocation, typeHIPoint, 0, SizeOf( HIPoint ), nil, @mPos );

            if not mouseLock Then
              begin
                mouseX := Round( mPos.X );
                mouseY := Round( mPos.Y );
              end else
                begin
                  mouseX := Round( mPos.X - wnd_Width  / 2 );
                  mouseY := Round( mPos.Y - wnd_Height / 2 );
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

procedure app_CalcFPS;
begin
  app_FPS      := app_FPSCount;
  app_FPSAll   := app_FPSAll + app_FPSCount;
  app_FPSCount := 0;
  INC( app_WorkTime );
end;

end.
