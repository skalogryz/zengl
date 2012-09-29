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
unit zgl_keyboard;

{$I zgl_config.cfg}

interface
uses
  Windows;

const
  K_SYSRQ      = $B7;
  K_PAUSE      = $C5;
  K_ESCAPE     = $01;
  K_ENTER      = $1C;
  K_KP_ENTER   = $9C;

  K_UP         = $C8;
  K_DOWN       = $D0;
  K_LEFT       = $CB;
  K_RIGHT      = $CD;

  K_BACKSPACE  = $0E;
  K_SPACE      = $39;
  K_TAB        = $0F;
  K_TILDE      = $29;

  K_INSERT     = $D2;
  K_DELETE     = $D3;
  K_HOME       = $C7;
  K_END        = $CF;
  K_PAGEUP     = $C9;
  K_PAGEDOWN   = $D1;

  K_CTRL       = $FF - $01;
  K_CTRL_L     = $1D;
  K_CTRL_R     = $9D;
  K_ALT        = $FF - $02;
  K_ALT_L      = $38;
  K_ALT_R      = $B8;
  K_SHIFT      = $FF - $03;
  K_SHIFT_L    = $2A;
  K_SHIFT_R    = $36;
  K_SUPER      = $FF - $04;
  K_SUPER_L    = $DB;
  K_SUPER_R    = $DC;
  K_APP_MENU   = $DD;

  K_CAPSLOCK   = $3A;
  K_NUMLOCK    = $45;
  K_SCROLL     = $46;

  K_BRACKET_L  = $1A; // [ {
  K_BRACKET_R  = $1B; // ] }
  K_BACKSLASH  = $2B; // \
  K_SLASH      = $35; // /
  K_COMMA      = $33; // ,
  K_DECIMAL    = $34; // .
  K_SEMICOLON  = $27; // : ;
  K_APOSTROPHE = $28; // ' "

  K_0          = $0B;
  K_1          = $02;
  K_2          = $03;
  K_3          = $04;
  K_4          = $05;
  K_5          = $06;
  K_6          = $07;
  K_7          = $08;
  K_8          = $09;
  K_9          = $0A;

  K_MINUS      = $0C;
  K_EQUALS     = $0D;

  K_A          = $1E;
  K_B          = $30;
  K_C          = $2E;
  K_D          = $20;
  K_E          = $12;
  K_F          = $21;
  K_G          = $22;
  K_H          = $23;
  K_I          = $17;
  K_J          = $24;
  K_K          = $25;
  K_L          = $26;
  K_M          = $32;
  K_N          = $31;
  K_O          = $18;
  K_P          = $19;
  K_Q          = $10;
  K_R          = $13;
  K_S          = $1F;
  K_T          = $14;
  K_U          = $16;
  K_V          = $2F;
  K_W          = $11;
  K_X          = $2D;
  K_Y          = $15;
  K_Z          = $2C;

  K_KP_0       = $52;
  K_KP_1       = $4F;
  K_KP_2       = $50;
  K_KP_3       = $51;
  K_KP_4       = $4B;
  K_KP_5       = $4C;
  K_KP_6       = $4D;
  K_KP_7       = $47;
  K_KP_8       = $48;
  K_KP_9       = $49;

  K_KP_SUB     = $4A;
  K_KP_ADD     = $4E;
  K_KP_MUL     = $37;
  K_KP_DIV     = $B5;
  K_KP_DECIMAL = $53;

  K_F1         = $3B;
  K_F2         = $3C;
  K_F3         = $3D;
  K_F4         = $3E;
  K_F5         = $3F;
  K_F6         = $40;
  K_F7         = $41;
  K_F8         = $42;
  K_F9         = $43;
  K_F10        = $44;
  K_F11        = $57;
  K_F12        = $58;

  KA_DOWN      = 0;
  KA_UP        = 1;

function  key_Down( KeyCode : Byte ) : Boolean;
function  key_Up( KeyCode : Byte ) : Boolean;
function  key_Press( KeyCode : Byte ) : Boolean;
function  key_Last( KeyAction : Byte ) : Byte;
procedure key_BeginReadText( const Text : UTF8String; MaxSymbols : Integer = -1 );
procedure key_UpdateReadText( const Text : UTF8String; MaxSymbols : Integer = -1 );
function  key_GetText : UTF8String;
procedure key_EndReadText;
procedure key_ClearState;

procedure key_InputText( const Text : UTF8String );
function  scancode_to_utf8( ScanCode : Byte ) : Byte;
function  winkey_to_scancode( WinKey : Integer ) : Byte;
function  SCA( KeyCode : LongWord ) : LongWord;
procedure doKeyPress( KeyCode : LongWord );

function _key_GetText : PAnsiChar;

var
  keysDown     : array[ 0..255 ] of Boolean;
  keysUp       : array[ 0..255 ] of Boolean;
  keysPress    : array[ 0..255 ] of Boolean;
  keysCanPress : array[ 0..255 ] of Boolean;
  keysText     : UTF8String = '';
  keysCanText  : Boolean;
  keysMax      : Integer;
  keysLast     : array[ 0..1 ] of Byte;

  // callback
  key_PPress     : procedure( KeyCode : Byte );
  key_PRelease   : procedure( KeyCode : Byte );
  key_PInputChar : procedure( Symbol : UTF8String );

implementation
uses
  zgl_application,
  zgl_main,
  zgl_utils;

function key_Down( KeyCode : Byte ) : Boolean;
begin
  Result := keysDown[ KeyCode ];
end;

function key_Up( KeyCode : Byte ) : Boolean;
begin
  Result := keysUp[ KeyCode ];
end;

function key_Press( KeyCode : Byte ) : Boolean;
begin
  Result := keysPress[ KeyCode ];
end;

function key_Last( KeyAction : Byte ) : Byte;
begin
  Result := keysLast[ KeyAction ];
end;

procedure key_BeginReadText( const Text : UTF8String; MaxSymbols : Integer = -1 );
begin
  keysText    := utf8_Copy( Text );
  keysMax     := MaxSymbols;
  keysCanText := TRUE;
end;

procedure key_UpdateReadText( const Text : UTF8String; MaxSymbols : Integer = -1 );
begin
  if keysCanText Then
    begin
      keysText := utf8_Copy( Text );
      keysMax  := MaxSymbols;
    end;
end;

function key_GetText : UTF8String;
begin
  Result := keysText;
end;

procedure key_EndReadText;
begin
  keysText    := '';
  keysCanText := FALSE;
end;

procedure key_ClearState;
  var
    i : Integer;
begin
  for i := 0 to 255 do
    begin
      keysUp      [ i ] := FALSE;
      keysPress   [ i ] := FALSE;
      keysCanPress[ i ] := TRUE;
    end;
  keysLast[ KA_DOWN ] := 0;
  keysLast[ KA_UP   ] := 0;
end;

function scancode_to_utf8( ScanCode : Byte ) : Byte;
begin
  Result := 0;

  case ScanCode of
    K_TILDE:  Result := 96;
    K_MINUS,
    K_KP_SUB: Result := 45;
    K_EQUALS: Result := 61;

    K_0, K_KP_0: Result := 48;
    K_1, K_KP_1: Result := 49;
    K_2, K_KP_2: Result := 50;
    K_3, K_KP_3: Result := 51;
    K_4, K_KP_4: Result := 52;
    K_5, K_KP_5: Result := 53;
    K_6, K_KP_6: Result := 54;
    K_7, K_KP_7: Result := 55;
    K_8, K_KP_8: Result := 56;
    K_9, K_KP_9: Result := 57;

    K_KP_MUL: Result := 42;
    K_KP_ADD: Result := 43;

    K_A: Result := 97;
    K_B: Result := 98;
    K_C: Result := 99;
    K_D: Result := 100;
    K_E: Result := 101;
    K_F: Result := 102;
    K_G: Result := 103;
    K_H: Result := 104;
    K_I: Result := 105;
    K_J: Result := 106;
    K_K: Result := 107;
    K_L: Result := 108;
    K_M: Result := 109;
    K_N: Result := 110;
    K_O: Result := 111;
    K_P: Result := 112;
    K_Q: Result := 113;
    K_R: Result := 114;
    K_S: Result := 115;
    K_T: Result := 116;
    K_U: Result := 117;
    K_V: Result := 118;
    K_W: Result := 119;
    K_X: Result := 120;
    K_Y: Result := 121;
    K_Z: Result := 122;

    K_BRACKET_L:  Result := 91;
    K_BRACKET_R:  Result := 93;
    K_BACKSLASH:  Result := 92;
    K_SLASH,
    K_KP_DIV:     Result := 47;
    K_COMMA:      Result := 44;
    K_DECIMAL,
    K_KP_DECIMAL: Result := 46;
    K_SEMICOLON:  Result := 59;
    K_APOSTROPHE: Result := 39;
  end;

  if keysDown[ K_SHIFT ] and
     ( ScanCode <> K_KP_0 ) and ( ScanCode <> K_KP_1 ) and ( ScanCode <> K_KP_2 ) and ( ScanCode <> K_KP_3 ) and ( ScanCode <> K_KP_4 ) and
     ( ScanCode <> K_KP_5 ) and ( ScanCode <> K_KP_6 ) and ( ScanCode <> K_KP_7 ) and ( ScanCode <> K_KP_8 ) and ( ScanCode <> K_KP_9 ) and
     ( ScanCode <> K_KP_DIV ) and ( ScanCode <> K_KP_MUL) and ( ScanCode <> K_KP_SUB ) and ( ScanCode <> K_KP_ADD ) Then
    case Result of
      96: Result := 126; // ~
      45: Result := 95;  // _
      61: Result := 43;  // +

      48: Result := 41; // (
      49: Result := 33; // !
      50: Result := 64; // @
      51: Result := 35; // #
      52: Result := 36; // $
      53: Result := 37; // %
      54: Result := 94; // ^
      55: Result := 38; // &
      56: Result := 42; // *
      57: Result := 40; // (

      97..122: Result := Result - 32;

      91: Result := 123; // {
      93: Result := 125; // }
      92: Result := 124; // |
      47: Result := 63;  // ?
      44: Result := 60;  // <
      46: Result := 62;  // >
      59: Result := 58;  // :
      39: Result := 34;  // "
    end;
end;

procedure key_InputText( const Text : UTF8String );
  var
    c : AnsiChar;
begin
  if ( utf8_Length( keysText ) < keysMax ) or ( keysMax = -1 ) Then
    begin
      if ( appFlags and APP_USE_ENGLISH_INPUT > 0 ) and ( Text[ 1 ] <> ' ' )  Then
        begin
          c := AnsiChar( scancode_to_utf8( keysLast[ 0 ] ) );
          if c <> #0 Then
            keysText := keysText + UTF8String( c );
        end else
          keysText := keysText + Text;
    end;

  if Assigned( key_PInputChar ) Then
    begin
      if ( appFlags and APP_USE_ENGLISH_INPUT > 0 ) and ( Text[ 1 ] <> ' ' )  Then
        begin
          c := AnsiChar( scancode_to_utf8( keysLast[ 0 ] ) );
          if c <> #0 Then
            key_PInputChar( c );
        end else
          key_PInputChar( Text );
    end;
end;

function winkey_to_scancode( WinKey : Integer ) : Byte;
begin
  case WinKey of
    $26: Result := K_UP;
    $28: Result := K_DOWN;
    $25: Result := K_LEFT;
    $27: Result := K_RIGHT;

    $2D: Result := K_INSERT;
    $2E: Result := K_DELETE;
    $24: Result := K_HOME;
    $23: Result := K_END;
    $21: Result := K_PAGEUP;
    $22: Result := K_PAGEDOWN;
  else
    Result := MapVirtualKey( WinKey, 0 );
  end;
end;

function SCA( KeyCode : LongWord ) : LongWord;
begin
  Result := KeyCode;
  if ( KeyCode = K_SUPER_L ) or ( KeyCode = K_SUPER_R ) Then Result := K_SUPER;
  if ( KeyCode = K_SHIFT_L ) or ( KeyCode = K_SHIFT_R ) Then Result := K_SHIFT;
  if ( KeyCode = K_CTRL_L ) or ( KeyCode = K_CTRL_R ) Then Result := K_CTRL;
  if ( KeyCode = K_ALT_L ) or ( KeyCode = K_ALT_R ) Then Result := K_ALT;
end;

procedure doKeyPress( KeyCode : LongWord );
begin
  if keysCanPress[ KeyCode ] Then
    begin
      keysPress   [ KeyCode ] := TRUE;
      keysCanPress[ KeyCode ] := FALSE;
    end;
end;

function _key_GetText : PAnsiChar;
begin
  Result := utf8_GetPAnsiChar( key_GetText() );
end;

end.
