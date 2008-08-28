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
unit zgl_keyboard;

{$I define.inc}

interface
uses
{$IFDEF LINUX}
  keysym,
{$ENDIF}
  zgl_timers,
  Utils;

const
  K_BACKSPACE  = 8;
  K_TAB        = 9;
  K_ENTER      = 13;
  K_SHIFT      = 16;
  K_SHIFT_L    = 160;
  K_SHIFT_R    = 161;
  K_CTRL       = 17;
  K_CTRL_L     = 162;
  K_CTRL_R     = 163;
  K_ALT        = 18;
  K_ALT_L      = 164;
  K_ALT_R      = 165;
  K_PAUSE      = 19;
  K_ESCAPE     = 27;
  K_SPACE      = 32;

  K_PAGEUP     = 33;
  K_PAGEDOWN   = 34;
  K_END        = 35;
  K_HOME       = 36;
  K_SNAPSHOT   = 44;
  K_INSERT     = 45;
  K_DELETE     = 46;

  K_LEFT       = 37;
  K_UP         = 38;
  K_RIGHT      = 39;
  K_DOWN       = 40;

  K_0          = 48;
  K_1          = 49;
  K_2          = 50;
  K_3          = 51;
  K_4          = 52;
  K_5          = 53;
  K_6          = 54;
  K_7          = 55;
  K_8          = 56;
  K_9          = 57;

  K_NUMPAD0    = 96;
  K_NUMPAD1    = 97;
  K_NUMPAD2    = 98;
  K_NUMPAD3    = 99;
  K_NUMPAD4    = 100;
  K_NUMPAD5    = 101;
  K_NUMPAD6    = 102;
  K_NUMPAD7    = 103;
  K_NUMPAD8    = 104;
  K_NUMPAD9    = 105;

  K_MULTIPLY   = 106;
  K_ADD        = 107;
  K_SEPARATOR  = 108;
  K_SUBTRACT   = 109;
  K_DECIMAL    = 110;
  K_DIVIDE     = 111;

  K_A          = 65;
  K_B          = 66;
  K_C          = 67;
  K_D          = 68;
  K_E          = 69;
  K_F          = 70;
  K_G          = 71;
  K_H          = 72;
  K_I          = 73;
  K_J          = 74;
  K_K          = 75;
  K_L          = 76;
  K_M          = 77;
  K_N          = 78;
  K_O          = 79;
  K_P          = 80;
  K_Q          = 81;
  K_R          = 82;
  K_S          = 83;
  K_T          = 84;
  K_U          = 85;
  K_V          = 86;
  K_W          = 87;
  K_X          = 88;
  K_Y          = 89;
  K_Z          = 90;

  K_F1         = 112;
  K_F2         = 113;
  K_F3         = 114;
  K_F4         = 115;
  K_F5         = 116;
  K_F6         = 117;
  K_F7         = 118;
  K_F8         = 119;
  K_F9         = 120;
  K_F10        = 121;
  K_F11        = 122;
  K_F12        = 123;

  KA_DOWN      = 0;
  KA_UP        = 1;

function  key_Down( KeyCode : Byte ) : Boolean; extdecl;
function  key_Up( KeyCode : Byte ) : Boolean; extdecl;
function  key_Last( KeyAction : Byte ) : Byte; extdecl;
procedure key_BeginReadText( Text : String; MaxSymbols : WORD ); extdecl;
function  key_EndReadText : PChar; extdecl;
procedure key_ClearState; extdecl;

procedure key_TextInput( KeyCode : WORD );
{$IFDEF LINUX}
function xkey_to_winkey( KeyCode : WORD ) : Byte;
{$ENDIF}

var
  keysDown : array[ 0..255 ] of Boolean;
  keysUp   : array[ 0..255 ] of Boolean;
  keysText : String;
  keysMax  : WORD;
  keysLast : array[ 0..1 ] of Byte;
  keysTick : Double;

implementation

function key_Down;
begin
  Result := keysDown[ KeyCode ];
end;

function key_Up;
begin
  Result := keysUp[ KeyCode ];
//  keysUp[ KeyCode ] := FALSE;
end;

function key_Last;
begin
  Result := keysLast[ KeyAction ];
end;

procedure key_BeginReadText;
begin
  keysText := Text;
  keysMax  := MaxSymbols;
end;

function key_EndReadText;
begin
  Result := PChar( keysText );
end;

procedure key_ClearState;
  var
    i : Byte;
begin
  for i := 0 to 255 do
    keysUp[ i ] := FALSE;
end;

// TODO: доработать
procedure key_TextInput;
begin
  if KeyCode <> keysLast[ KA_DOWN ] Then
    begin
      keysLast[ KA_DOWN ] := KeyCode;
    end else
      if keysTick < timer_GetTicks - 100 Then
        begin
          keysTick := keysTick + 100;
        end else
          exit;

  if KeyCode = K_BACKSPACE Then
    Delete( keysText, Length( keysText ), 1 );
    
  if length( keysText ) > keysMax Then exit;

  if KeyCode = K_SPACE Then
    keysText := keysText + ' ';

  if ( KeyCode >= K_A ) and ( KeyCode <= K_Z ) Then
    if key_Down( K_SHIFT ) Then
      keysText := keysText + StrUp( Chr( KeyCode ) )
    else
      keysText := keysText + StrDown( Chr( KeyCode ) );
end;

{$IFDEF LINUX}
function xkey_to_winkey;
begin
  case KeyCode of
    XK_BackSpace    : Result := K_BACKSPACE;
    XK_Tab          : Result := K_TAB;
    XK_Return       : Result := K_ENTER;
    XK_Shift_L      : Result := K_SHIFT_L;
    XK_Shift_R      : Result := K_SHIFT_R;
    XK_Control_L    : Result := K_CTRL_L;
    XK_Control_R    : Result := K_CTRL_R;
    XK_Alt_L        : Result := K_ALT_L;
		XK_Alt_R        : Result := K_ALT_R;
    XK_Pause        : Result := K_PAUSE;
    XK_Escape       : Result := K_ESCAPE;
    XK_Space        : Result := K_SPACE;
    
    XK_Page_Up      : Result := K_PAGEUP;
    XK_Page_Down    : Result := K_PAGEDOWN;
    XK_End          : Result := K_END;
    XK_Home         : Result := K_HOME;
    XK_Sys_Req      : Result := K_SNAPSHOT;
    XK_Insert       : Result := K_INSERT;
    XK_Delete       : Result := K_DELETE;
  
    XK_Left         : Result := K_LEFT;
    XK_Up           : Result := K_UP;
    XK_Right        : Result := K_RIGHT;
    XK_Down         : Result := K_DOWN;
    
    XK_KP_Multiply  : Result := K_MULTIPLY;
    XK_KP_Add       : Result := K_ADD;
    XK_KP_Separator : Result := K_SEPARATOR;
    XK_KP_Subtract  : Result := K_SUBTRACT;
    XK_KP_Decimal   : Result := K_DECIMAL;
    XK_KP_Divide    : Result := K_DIVIDE;
  else
    if ( KeyCode >= XK_0 ) and ( KeyCode <= XK_9 ) Then Result := KeyCode - ( XK_0 - K_0 );
    if ( KeyCode >= XK_KP_0 ) and ( KeyCode <= XK_KP_9 ) Then Result := KeyCode - ( XK_KP_0 - K_NUMPAD0 );
    if ( KeyCode >= XK_A ) and ( KeyCode <= XK_Z ) Then Result := KeyCode - ( XK_A - K_A );
    if ( KeyCode >= XK_F1 ) and ( KeyCode <= XK_F12 ) Then Result := KeyCode - ( XK_F1 - K_F1 );
  end;
end;
{$ENDIF}

end.
