{-------------------------------}
{-----------= ZenGL =-----------}
{-------------------------------}
{ version: 0.2 RC4              }
{ date:    2010.09.21           }
{ license: GNU LGPL version 3   }
{-------------------------------}
{ by:   Andru ( Kemka Andrey )  }
{ mail: dr.andru@gmail.com      }
{ JID:  dr.andru@googlemail.com }
{ ICQ:  496-929-849             }
{ site: http://andru-kun.inf.ua }
{-------------------------------}
unit zglHeader;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$MACRO ON}
  {$PACKRECORDS C}
  {$IFDEF LINUX}
    {$DEFINE LINUX_OR_DARWIN}
  {$ENDIF}
  {$IFDEF DARWIN}
    {$DEFINE LINUX_OR_DARWIN}
  {$ENDIF}
{$ENDIF}

{$IFDEF MSWINDOWS}
  {$DEFINE WINDOWS}
{$ENDIF}

interface
{$IFDEF DARWIN}
uses
  MacOSAll;
{$ENDIF}

type
  Ptr     = {$IFDEF CPU64}QWORD{$ELSE}LongWord{$ENDIF};
  PPtr    = ^Ptr;
  {$IFDEF WINDOWS}
  HANDLE  = LongWord;
  HDC     = LongWord;
  HGLRC   = LongWord;
  {$ENDIF}

type
  zglTStringList = record
    Count : Integer;
    Items : array of String;
end;

{$IFNDEF WINDOWS}
type zglTFile = LongInt;
{$ELSE}
type zglTFile = LongWord;
{$ENDIF}
type zglTFileList = zglTStringList;
type
  zglPMemory = ^zglTMemory;
  zglTMemory = record
    Memory   : Pointer;
    Size     : LongWord;
    Position : LongWord;
end;

const
{$IFDEF LINUX}
  libZenGL = 'libZenGL.so';
{$ENDIF}
{$IFDEF WINDOWS}
  libZenGL = 'ZenGL.dll';
{$ENDIF}
{$IFDEF DARWIN}
  libZenGL = 'libZenGL.dylib';
var
  mainPath : AnsiString;
{$ENDIF}

function zglLoad( LibraryName : AnsiString; Error : Boolean = TRUE ) : Boolean;
procedure zglFree;

var
  zgl_Init         : procedure( const FSAA : Byte = 0; const StencilBits : Byte = 0 );
  zgl_InitToHandle : procedure( const Handle : Ptr; const FSAA : Byte = 0; const StencilBits : Byte = 0 );
  zgl_Exit         : procedure;

const
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

var
  zgl_Reg : procedure( const What : LongWord; const UserData : Pointer );

const
  SYS_FPS         = 1;  // LongWord,  := zgl_Get( SYS_FPS )
  APP_PAUSED      = 2;  // Boolean
  APP_DIRECTORY   = 3;  // PAnsiChar
  USR_HOMEDIR     = 4;  // PAnsiChar
  LOG_FILENAME    = 5;  // PPChar, := Pointer( zgl_Get( LOG_FILENAME ) )
  ZGL_VERSION     = 6;  // LongWord
  SCR_ADD_X       = 7;  // LongWord
  SCR_ADD_Y       = 8;  // LongWord
  DESKTOP_WIDTH   = 9;  // LongWord
  DESKTOP_HEIGHT  = 10; // LongWord
  RESOLUTION_LIST = 11; // PResolutionList
  MANAGER_TIMER   = 12; // zglPTimerManager
  MANAGER_TEXTURE = 13; // zglPTextureManager
  MANAGER_ATLAS   = 14; // zglPAtlasManager
  MANAGER_FONT    = 15; // zglPFontManager
  MANAGER_RTARGET = 16; // zglTRenderTargetManager
  MANAGER_SOUND   = 17; // zglPSoundManager

var
  zgl_Get       : function( const What : LongWord ) : Ptr;
  zgl_GetSysDir : procedure;
  zgl_GetMem    : procedure( var Mem : Pointer; const Size : LongWord );
  zgl_FreeMem   : procedure( var Mem : Pointer );
  zgl_FreeStr   : procedure( var Str : String );

const
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

var
  zgl_Enable  : procedure( const What : LongWord );
  zgl_Disable : procedure( const What : LongWord );

// LOG
  log_Add : procedure( const Message : AnsiString; const Timings : Boolean = TRUE );

// WINDOW
  wnd_SetCaption : procedure( const NewCaption : String );
  wnd_SetSize    : procedure( const Width, Height : Integer );
  wnd_SetPos     : procedure( const X, Y : Integer );
  wnd_ShowCursor : procedure( const Show : Boolean );

// SCREEN
type
  zglPResolutionList = ^zglTResolutionList;
  zglTResolutionList = record
    Count  : Integer;
    Width  : array of Integer;
    Height : array of Integer;
end;

const
  REFRESH_MAXIMUM = 0;
  REFRESH_DEFAULT = 1;

var
  scr_Clear             : procedure;
  scr_Flush             : procedure;
  scr_SetVSync          : procedure( const VSync : Boolean );
  // ВНИМАНИЕ: Функция уничтожает контекст OpenGL, что потребует перезагрузку ресурсов
  scr_SetFSAA           : procedure( const FSAA : Byte );
  scr_SetOptions        : procedure( const Width, Height, Refresh : Word; const FullScreen, VSync : Boolean );
  scr_CorrectResolution : procedure( const Width, Height : Word );

// GL
const
  TARGET_SCREEN  = 1;
  TARGET_TEXTURE = 2;

var
  Set2DMode : procedure;
  Set3DMode : procedure( FOVY : Single = 45 );

// Z BUFFER
  zbuffer_SetDepth  : procedure( const zNear, zFar : Single );
  zbuffer_Clear     : procedure;

// SCISSOR
  scissor_Begin : procedure( X, Y, Width, Height : Integer );
  scissor_End   : procedure;

// INI
  ini_LoadFromFile  : procedure( const FileName : AnsiString );
  ini_SaveToFile    : procedure( const FileName : AnsiString );
  ini_Add           : procedure( const Section, Key : AnsiString );
  ini_Del           : procedure( const Section, Key : AnsiString );
  ini_Clear         : procedure( const Section : AnsiString );
  ini_IsSection     : function( const Section : AnsiString ) : Boolean;
  ini_IsKey         : function( const Section, Key : AnsiString ) : Boolean;
  ini_ReadKeyStr    : procedure( const Section, Key : AnsiString; var Result : AnsiString );
  ini_ReadKeyInt    : function( const Section, Key : AnsiString ) : Integer;
  ini_ReadKeyFloat  : function( const Section, Key : AnsiString ) : Single;
  ini_ReadKeyBool   : function( const Section, Key : AnsiString ) : Boolean;
  ini_WriteKeyStr   : function( const Section, Key, Value : AnsiString ) : Boolean;
  ini_WriteKeyInt   : function( const Section, Key : AnsiString; const Value : Integer ) : Boolean;
  ini_WriteKeyFloat : function( const Section, Key : AnsiString; const Value : Single; const Digits : Integer = 2 ) : Boolean;
  ini_WriteKeyBool  : function( const Section, Key : AnsiString; const Value : Boolean ) : Boolean;

// TIMERS
type
  zglPTimer = ^zglTTimer;
  zglTTimer = record
    Active     : Boolean;
    Interval   : LongWord;
    LastTick   : Double;
    OnTimer    : procedure;

    Prev, Next : zglPTimer;
end;

type
  zglPTimerManager = ^zglTTimerManager;
  zglTTimerManager = record
    Count   : LongWord;
    First   : zglTTimer;
end;

var
  timer_Add      : function( const OnTimer : Pointer; const Interval : LongWord ) : zglPTimer;
  timer_Del      : procedure( var Timer : zglPTimer );
  timer_GetTicks : function : Double;
  timer_Reset    : procedure;

// MOUSE
const
  M_BLEFT  = 0;
  M_BMIDLE = 1;
  M_BRIGHT = 2;
  M_WUP    = 0;
  M_WDOWN  = 1;

var
  mouse_X          : function : Integer;
  mouse_Y          : function : Integer;
  mouse_DX         : function : Integer;
  mouse_DY         : function : Integer;
  mouse_Down       : function( const Button : Byte ) : Boolean;
  mouse_Up         : function( const Button : Byte ) : Boolean;
  mouse_Click      : function( const Button : Byte ) : Boolean;
  mouse_DblClick   : function( const Button : Byte ) : Boolean;
  mouse_Wheel      : function( const Axis : Byte ) : Boolean;
  mouse_ClearState : procedure;
  mouse_Lock       : procedure;

// KEYBOARD
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

  KA_DOWN     = 0;
  KA_UP       = 1;
var
  key_Down          : function( const KeyCode : Byte ) : Boolean;
  key_Up            : function( const KeyCode : Byte ) : Boolean;
  key_Press         : function( const KeyCode : Byte ) : Boolean;
  key_Last          : function( const KeyAction : Byte ) : Byte;
  key_BeginReadText : procedure( const Text : String; const MaxSymbols : Integer = -1 );
  key_GetText       : procedure( var Result : String );
  key_EndReadText   : procedure;
  key_ClearState    : procedure;

// JOYSTICK
type
  zglPJoyInfo = ^zglTJoyInfo;
  zglTJoyInfo = record
    Name   : AnsiString;
    Count  : record
      Axes    : Integer;
      Buttons : Integer;
             end;
    Caps   : LongWord;
  end;

const
  JOY_HAS_Z   = $000001;
  JOY_HAS_R   = $000002;
  JOY_HAS_U   = $000004;
  JOY_HAS_V   = $000008;
  JOY_HAS_POV = $000010;

  JOY_AXIS_X = 0;
  JOY_AXIS_Y = 1;
  JOY_AXIS_Z = 2;
  JOY_AXIS_R = 3;
  JOY_AXIS_U = 4;
  JOY_AXIS_V = 5;
  JOY_POVX   = 6;
  JOY_POVY   = 7;

var
  joy_Init       : function : Byte;
  joy_GetInfo    : function ( const JoyID : Byte ) : zglPJoyInfo;
  joy_AxisPos    : function ( const JoyID, Axis : Byte ) : Single;
  joy_Down       : function ( const JoyID, Button : Byte ) : Boolean;
  joy_Up         : function ( const JoyID, Button : Byte ) : Boolean;
  joy_Press      : function ( const JoyID, Button : Byte ) : Boolean;
  joy_ClearState : procedure;

// 2D
type
  zglPPoint2D = ^zglTPoint2D;
  zglTPoint2D = record
    X, Y : Single;
end;

type
  zglPPoints2D = ^zglTPoints2D;
  zglTPoints2D = array[ 0..0 ] of zglTPoint2D;

type
  zglPLine = ^zglTLine;
  zglTLine = record
    x0, y0 : Single;
    x1, y1 : Single;
end;

type
  zglPRect = ^zglTRect;
  zglTRect = record
    X, Y, W, H : Single;
end;

type
  zglPCircle = ^zglTCircle;
  zglTCircle = record
    cX, cY : Single;
    Radius : Single;
end;

// TEXTURES
type
  zglPTextureCoord = ^zglTTextureCoord;
  zglTTextureCoord = array[ 0..3 ] of zglTPoint2D;

type
  zglPTexture = ^zglTTexture;
  zglTTexture = record
    ID            : LongWord;
    Width, Height : Word;
    U, V          : Single;
    FramesX       : Word;
    FramesY       : Word;
    FramesCoord   : array of zglTTextureCoord;
    Flags         : LongWord;

    prev, next    : zglPTexture;
end;

type
  zglPTextureFormat = ^zglTTextureFormat;
  zglTTextureFormat = record
    Extension  : String;
    FileLoader : procedure( const FileName : String; var pData : Pointer; var W, H : Word );
    MemLoader  : procedure( const Memory : zglTMemory; var pData : Pointer; var W, H : Word );
end;

type
  zglPTextureManager = ^zglTTextureManager;
  zglTTextureManager = record
    Count   : record
      Items   : LongWord;
      Formats : LongWord;
              end;
    First   : zglTTexture;
    Formats : array of zglTTextureFormat;
end;

const
  TEX_MIPMAP            = $000001;
  TEX_CLAMP             = $000002;
  TEX_REPEAT            = $000004;
  TEX_COMPRESS          = $000008;
  TEX_CONVERT_TO_POT    = $000010;

  TEX_GRAYSCALE         = $000020;
  TEX_INVERT            = $000040;
  TEX_CUSTOM_EFFECT     = $000080;

  TEX_FILTER_NEAREST    = $000100;
  TEX_FILTER_LINEAR     = $000200;
  TEX_FILTER_BILINEAR   = $000400;
  TEX_FILTER_TRILINEAR  = $000800;
  TEX_FILTER_ANISOTROPY = $001000;

  TEX_RGB               = $002000;
  TEX_CALCULATE_ALPHA   = $004000;

  TEX_QUALITY_LOW       = $400000;
  TEX_QUALITY_MEDIUM    = $800000;

  TEX_DEFAULT_2D        = TEX_CLAMP or TEX_CONVERT_TO_POT or TEX_FILTER_LINEAR or TEX_CALCULATE_ALPHA;

var
  tex_Add            : function : zglPTexture;
  tex_Del            : procedure( var Texture : zglPTexture );
  tex_Create         : procedure( var Texture : zglTTexture; var pData : Pointer );
  tex_CreateZero     : function( const Width, Height : Word; const Color, Flags : LongWord ) : zglPTexture;
  tex_LoadFromFile   : function( const FileName : String; const TransparentColor, Flags : LongWord ) : zglPTexture;
  tex_LoadFromMemory : function( const Memory : zglTMemory; const Extension : String; const TransparentColor, Flags : LongWord ) : zglPTexture;
  tex_SetFrameSize   : procedure( var Texture : zglPTexture; FrameWidth, FrameHeight : Word );
  tex_SetMask        : function( var Texture : zglPTexture; const Mask : zglPTexture ) : zglPTexture;
  tex_GetData        : procedure( const Texture : zglPTexture; var pData : Pointer; var pSize : Integer );
  tex_Filter         : procedure( Texture : zglPTexture; const Flags : LongWord );
  tex_SetAnisotropy  : procedure( const Level : Byte );

// ATLASES
type
  zglPAtlasNode = ^zglTAtlasNode;
  zglTAtlasNode = record
    Leaf     : Boolean;
    Texture  : zglPTexture;
    TexCoord : zglTTextureCoord;
    FramesX  : Word;
    FramesY  : Word;
    Rect     : zglTRect;
    child    : array[ 0..1 ] of zglPAtlasNode;
  end;

type
  zglPAtlas = ^zglTAtlas;
  zglTAtlas = record
    root       : zglTAtlasNode;
    Texture    : zglPTexture;
    Full       : Boolean;
    prev, next : zglPAtlas;
  end;

type
  zglPAtlasManager = ^zglTAtlasManager;
  zglTAtlasManager = record
    Count : LongWord;
    First : zglTAtlas;
end;

var
  atlas_Add               : function( const Width, Height : Word; const Flags : LongWord ) : zglPAtlas;
  atlas_Del               : procedure( var Atlas : zglPAtlas );
  atlas_GetFrameCoord     : procedure( const Node : zglPAtlasNode; const Frame : Word; var TexCoord : array of zglTPoint2D );
  atlas_InsertFromTexture : function( const Atlas : zglPAtlas; const Texture : zglPTexture ) : zglPAtlasNode;
  atlas_InsertFromFile    : function( const Atlas : zglPAtlas; const FileName : String; const TransparentColor, Flags : LongWord ) : zglPAtlasNode;
  atlas_InsertFromMemory  : function( const Atlas : zglPAtlas; const Memory : zglTMemory; const Extension : String; const TransparentColor, Flags : LongWord ) : zglPAtlasNode;

// RENDER TARGETS
type
  zglPRenderTarget = ^zglTRenderTarget;
  zglTRenderTarget = record
    _type      : Byte;
    Handle     : Pointer;
    Surface    : zglPTexture;
    Flags      : Byte;

    prev, next : zglPRenderTarget;
end;

type
  zglPRenderTargetManager = ^zglTRenderTargetManager;
  zglTRenderTargetManager = record
    Count : LongWord;
    First : zglTRenderTarget;
end;

type
  zglTRenderCallback = procedure( Data : Pointer );

const
  RT_DEFAULT      = $00;
  RT_FULL_SCREEN  = $01;
  RT_CLEAR_SCREEN = $02;

var
  rtarget_Add    : function( const Surface : zglPTexture; const Flags : Byte ) : zglPRenderTarget;
  rtarget_Del    : procedure( var Target : zglPRenderTarget );
  rtarget_Set    : procedure( const Target : zglPRenderTarget );
  rtarget_DrawIn : procedure( const Target : zglPRenderTarget; const RenderCallback : zglTRenderCallback; const Data : Pointer );

// FX
const
  FX_BLEND_NORMAL = $00;
  FX_BLEND_ADD    = $01;
  FX_BLEND_MULT   = $02;
  FX_BLEND_BLACK  = $03;
  FX_BLEND_WHITE  = $04;
  FX_BLEND_MASK   = $05;

  FX_COLOR_MIX    = $00;
  FX_COLOR_SET    = $01;

  FX_BLEND        = $100000;
  FX_COLOR        = $200000;

var
  fx_SetBlendMode : procedure( const Mode : Byte );
  fx_SetColorMode : procedure( const Mode : Byte );

// FX 2D
const
  FX2D_FLIPX    = $000001;
  FX2D_FLIPY    = $000002;
  FX2D_VCA      = $000004;
  FX2D_VCHANGE  = $000008;
  FX2D_SCALE    = $000010;

var
  fx2d_SetColor    : procedure( const Color : LongWord );
  fx2d_SetVCA      : procedure( const c1, c2, c3, c4 : LongWord; const a1, a2, a3, a4 : Byte );
  fx2d_SetVertexes : procedure( const x1, y1, x2, y2, x3, y3, x4, y4 : Single );
  fx2d_SetScale    : procedure( const scaleX, scaleY : Single );

// Camera 2D
type
  zglPCamera2D = ^zglTCamera2D;
  zglTCamera2D = record
    X, Y  : Single;
    Angle : Single;
    Zoom  : zglTPoint2D;
end;

var
  cam2d_Set   : procedure( const Camera : zglPCamera2D );
  cam2d_Apply : procedure( const Camera : zglPCamera2D );

// Render 2D
  batch2d_Begin : procedure;
  batch2d_End   : procedure;
  batch2d_Flush : procedure;

// Primitives 2D
const
  PR2D_FILL   = $010000;
  PR2D_SMOOTH = $020000;

var
  pr2d_Pixel   : procedure( const X, Y : Single; const Color : LongWord = $FFFFFF; const Alpha : Byte = 255 );
  pr2d_Line    : procedure( const X1, Y1, X2, Y2 : Single; const Color : LongWord = $FFFFFF; const Alpha : Byte = 255; const FX : LongWord = 0 );
  pr2d_Rect    : procedure( const X, Y, W, H : Single; const Color : LongWord = $FFFFFF; const Alpha : Byte = 255; const FX : LongWord = 0 );
  pr2d_Circle  : procedure( const X, Y, Radius : Single; const Color : LongWord = $FFFFFF; const Alpha : Byte = 255; const Quality : Word = 32; const FX : LongWord = 0 );
  pr2d_Ellipse : procedure( const X, Y, xRadius, yRadius : Single; const Color : LongWord = $FFFFFF; const Alpha : Byte = 255; const Quality : Word = 32; const FX : LongWord = 0 );
  pr2d_TriList : procedure( const Texture : zglPTexture; const TriList, TexCoords : zglPPoints2D; const iLo, iHi : Integer; const Color : LongWord = $FFFFFF; const Alpha : Byte = 255; const FX : LongWord = FX_BLEND );

// Sprites 2D
type
  zglPSprite2D = ^zglTSprite2D;
  zglPSEngine2D = ^zglTSEngine2D;

  zglTSEngine2D = record
    Count : LongWord;
    List  : array of zglPSprite2D;
  end;

  zglTSprite2D = record
    ID      : Integer;
    Manager : zglPSEngine2D;
    Texture : zglPTexture;
    Destroy : Boolean;
    Layer   : Integer;
    X, Y    : Single;
    W, H    : Single;
    Angle   : Single;
    Frame   : Single;
    Alpha   : Integer;
    FxFlags : LongWord;
    Data    : Pointer;

    OnInit  : procedure( const Sprite : zglPSprite2D );
    OnDraw  : procedure( const Sprite : zglPSprite2D );
    OnProc  : procedure( const Sprite : zglPSprite2D );
    OnFree  : procedure( const Sprite : zglPSprite2D );
  end;

type
  zglPTiles2D = ^zglTTiles2D;
  zglTTiles2D = record
    Count : record
      X, Y : Integer;
            end;
    Size  : record
      W, H : Single;
            end;
    Tiles : array of array of Integer;
  end;

type
  zglPGrid2D = ^zglTGrid2D;
  zglTGrid2D = record
    Cols : Integer;
    Rows : Integer;
    Grid : array of array of zglTPoint2D;
  end;

var
  sengine2d_AddSprite : function( const Texture : zglPTexture; const Layer : Integer; const OnInit, OnDraw, OnProc, OnFree : Pointer ) : zglPSprite2D;
  sengine2d_DelSprite : procedure( const ID : Integer );
  sengine2d_ClearAll  : procedure;
  sengine2d_Set       : procedure( const SEngine : zglPSEngine2D );
  sengine2d_Draw      : procedure;
  sengine2d_Proc      : procedure;

  texture2d_Draw : procedure( const Texture : zglPTexture; const TexCoord : array of zglTPoint2D; X, Y, W, H, Angle : Single; const Alpha : Byte = 255; const FX : LongWord = FX_BLEND );
  ssprite2d_Draw : procedure( const Texture : zglPTexture; X, Y, W, H, Angle : Single; const Alpha : Byte = 255; const FX : LongWord = FX_BLEND );
  asprite2d_Draw : procedure( const Texture : zglPTexture; X, Y, W, H, Angle : Single; Frame : Word; const Alpha : Byte = 255; const FX : LongWord = FX_BLEND );
  csprite2d_Draw : procedure( const Texture : zglPTexture; X, Y, W, H, Angle : Single; const CutRect : zglTRect; const Alpha : Byte = 255; const FX : LongWord = FX_BLEND );
  tiles2d_Draw   : procedure( const Texture : zglPTexture; const X, Y : Single; const Tiles : zglTTiles2D; const Alpha : Byte = 255; const FX : LongWord = FX_BLEND );
  sgrid2d_Draw   : procedure( const Texture : zglPTexture; const X, Y : Single; const Grid : zglTGrid2D; const Alpha : Byte = 255; const FX : LongWord = FX_BLEND );
  agrid2d_Draw   : procedure( const Texture : zglPTexture; const X, Y : Single; const Grid : zglTGrid2D; const Frame : Integer; const Alpha : Byte = 255; const FX : LongWord = FX_BLEND );
  cgrid2d_Draw   : procedure( const Texture : zglPTexture; const X, Y : Single; const Grid : zglTGrid2D; const CutRect : zglTRect; const Alpha : Byte = 255; const FX : LongWord = FX_BLEND );

// Particles
const
  EMITTER_MAX_PARTICLES = 1024;

  EMITTER_POINT     = 1;
  EMITTER_LINE      = 2;
  EMITTER_RECTANGLE = 3;
  EMITTER_CIRCLE    = 4;

type
  PDiagramByte = ^TDiagramByte;
  TDiagramByte = record
    Life  : Single;
    Value : Byte;
  end;

type
  PDiagramLW = ^TDiagramLW;
  TDiagramLW = record
    Life  : Single;
    Value : LongWord;
  end;

type
  PDiagramSingle = ^TDiagramSingle;
  TDiagramSingle = record
    Life  : Single;
    Value : Single;
  end;

type
  zglPParticle2D = ^zglTParticle2D;
  zglTParticle2D = record
    _lColorID     : Integer;
    _lAlphaID     : Integer;
    _lSizeXID     : Integer;
    _lSizeYID     : Integer;
    _lVelocityID  : Integer;
    _laVelocityID : Integer;
    _lSpinID      : Integer;
    ID            : Integer;

    Life          : Single;
    LifeTime      : LongWord;
    Time          : Double;

    Frame         : Word;
    Color         : LongWord;
    Alpha         : Byte;

    Position      : zglTPoint2D;
    Size          : zglTPoint2D;
    SizeS         : zglTPoint2D;
    Angle         : Single;
    Direction     : Single;

    Velocity      : Single;
    VelocityS     : Single;
    aVelocity     : Single;
    aVelocityS    : Single;
    Spin          : Single;
  end;

type
  zglPEmitterPoint = ^zglTEmitterPoint;
  zglTEmitterPoint = record
    Direction : Single;
    Spread    : Single;
  end;

type
  zglPEmitterLine = ^zglTEmitterLine;
  zglTEmitterLine = record
    Direction : Single;
    Spread    : Single;
    Size      : Single;
    TwoSide   : Boolean;
  end;

type
  zglPEmitterRect = ^zglTEmitterRect;
  zglTEmitterRect = record
    Rect : zglTRect;
  end;

type
  zglPEmitterCircle = ^zglTEmitterCircle;
  zglTEmitterCircle = record
    cX, cY : Single;
    Radius : Single;
  end;

type
  zglPParticleParams = ^zglTParticleParams;
  zglTParticleParams = record
    Texture    : zglPTexture;
    BlendMode  : Byte;
    ColorMode  : Byte;

    LifeTimeS  : LongWord;
    LifeTimeV  : LongWord;
    Frame      : array[ 0..1 ] of LongWord;
    Color      : array of TDiagramLW;
    Alpha      : array of TDiagramByte;
    SizeXS     : Single;
    SizeYS     : Single;
    SizeXV     : Single;
    SizeYV     : Single;
    SizeXD     : array of TDiagramSingle;
    SizeYD     : array of TDiagramSingle;
    AngleS     : Single;
    AngleV     : Single;
    VelocityS  : Single;
    VelocityV  : Single;
    VelocityD  : array of TDiagramSingle;
    aVelocityS : Single;
    aVelocityV : Single;
    aVelocityD : array of TDiagramSingle;
    SpinS      : Single;
    SpinV      : Single;
    SpinD      : array of TDiagramSingle;
  end;

type
  zglPEmitter2D = ^zglTEmitter2D;
  zglTEmitter2D = record
    _type       : Byte;
    _particle   : array[ 0..EMITTER_MAX_PARTICLES - 1 ] of zglTParticle2D;
    _list       : array[ 0..EMITTER_MAX_PARTICLES - 1 ] of zglPParticle2D;
    _parCreated : LongWord;

    Params      : record
      LifeTime : LongWord;
      Loop     : Boolean;
      Emission : LongWord;
      Position : zglTPoint2D;
                  end;
    ParParams   : zglTParticleParams;

    Life        : Single;
    Time        : Double;
    LastSecond  : Double;
    Particles   : LongWord;
    BBox        : record
      MinX, MaxX : Single;
      MinY, MaxY : Single;
                  end;

    case Byte of
      EMITTER_POINT: ( AsPoint : zglTEmitterPoint );
      EMITTER_LINE: ( AsLine : zglTEmitterLine );
      EMITTER_RECTANGLE: ( AsRect : zglTEmitterRect );
      EMITTER_CIRCLE: ( AsCircle : zglTEmitterCircle );
  end;

type
  zglPPEngine2D = ^zglTPEngine2D;
  zglTPEngine2D = record
    Count : record
      Emitters  : LongWord;
      Particles : LongWord;
            end;
    List  : array of zglTEmitter2D;
  end;

var
  pengine2d_AddEmitter : function( const Emitter : zglTEmitter2D ) : Integer;
  pengine2d_DelEmitter : procedure( const ID : Integer );
  pengine2d_ClearAll   : procedure;
  pengine2d_Set        : procedure( const PEngine : zglPPEngine2D );
  pengine2d_Draw       : procedure;
  pengine2d_Proc       : procedure( const dt : Double );
  emitter2d_Init       : procedure( var Emitter : zglTEmitter2D );
  emitter2d_Draw       : procedure( const Emitter : zglTEmitter2D );
  emitter2d_Proc       : procedure( var Emitter : zglTEmitter2D; const dt : Double );

// Text
type
  zglPCharDesc = ^zglTCharDesc;
  zglTCharDesc = record
    Page      : Word;
    Width     : Byte;
    Height    : Byte;
    ShiftX    : Integer;
    ShiftY    : Integer;
    ShiftP    : Integer;
    TexCoords : array[ 0..3 ] of zglTPoint2D;
end;

type
  zglPFont = ^zglTFont;
  zglTFont = record
    Count      : record
      Pages : Word;
      Chars : Word;
                 end;

    Pages      : array of zglPTexture;
    CharDesc   : array[ 0..65535 ] of zglPCharDesc;
    MaxHeight  : Integer;
    MaxShiftY  : Integer;
    Padding    : array[ 0..3 ] of Byte;

    prev, next : zglPFont;
end;

type
  zglPFontManager = ^zglTFontManager;
  zglTFontManager = record
    Count : LongWord;
    First : zglTFont;
end;

const
  TEXT_HALIGN_LEFT    = $000001;
  TEXT_HALIGN_CENTER  = $000002;
  TEXT_HALIGN_RIGHT   = $000004;
  TEXT_HALIGN_JUSTIFY = $000008;
  TEXT_VALIGN_TOP     = $000010;
  TEXT_VALIGN_CENTER  = $000020;
  TEXT_VALIGN_BOTTOM  = $000040;
  TEXT_FX_VCA         = $000080;

var
  font_Add            : function : zglPFont;
  font_Del            : procedure( var Font : zglPFont );
  font_LoadFromFile   : function( const FileName : String ) : zglPFont;
  font_LoadFromMemory : function( const Memory : zglTMemory ) : zglPFont;
  text_Draw           : procedure( const Font : zglPFont; X, Y : Single; const Text : String; const Flags : LongWord = 0 );
  text_DrawEx         : procedure( const Font : zglPFont; X, Y, Scale, Step : Single; const Text : String; const Alpha : Byte = 255; const Color : LongWord = $FFFFFF; const Flags : LongWord = 0 );
  text_DrawInRect     : procedure( const Font : zglPFont; const Rect : zglTRect; const Text : String; const Flags : LongWord = 0 );
  text_DrawInRectEx   : procedure( const Font : zglPFont; const Rect : zglTRect; const Scale, Step : Single; const Text : String; const Alpha : Byte = 0; const Color : LongWord = $FFFFFF; const Flags : LongWord = 0 );
  text_GetWidth       : function( const Font : zglPFont; const Text : String; const Step : Single = 0.0 ) : Single;
  textFx_SetLength    : procedure( const Length : Integer; const LastCoord : zglPPoint2D = nil; const LastCharDesc : zglPCharDesc = nil );

// Sound
const
  SND_ALL           = -2;
  SND_STREAM        = -3;

  SND_STATE_PLAYING = 1;
  SND_STATE_PERCENT = 2;
  SND_STATE_TIME    = 3;
  SND_INFO_LENGTH   = 4;

type
  zglPSound        = ^zglTSound;
  zglPSoundStream  = ^zglTSoundStream;
  zglPSoundDecoder = ^zglTSoundDecoder;
  zglPSoundFormat  = ^zglTSoundFormat;
  zglPSoundManager = ^zglTSoundManager;

  zglTSoundChannel = record
    Source     : LongWord;
    Speed      : Single;
    Volume     : Single;
    Position   : record
      X, Y, Z : Single;
                 end;
  end;

  zglTSound = record
    Buffer      : LongWord;
    SourceCount : LongWord;
    Channel     : array of zglTSoundChannel;

    Data        : Pointer;
    Size        : LongWord;
    Length      : Double;
    Frequency   : LongWord;

    prev, next  : zglPSound;
  end;

  zglTSoundStream = record
    _data      : Pointer;
    _file      : zglTFile;
    _decoder   : zglPSoundDecoder;
    _playing   : Boolean;
    _paused    : Boolean;
    _waiting   : Boolean;
    _complete  : Double;
    _lastTime  : Double;

    Buffer     : Pointer;
    BufferSize : LongWord;

    Frequency  : LongWord;
    Channels   : LongWord;
    Length     : Double;

    Loop       : Boolean;
  end;

  zglTSoundDecoder = record
    Ext   : String;
    Open  : function( var Stream : zglTSoundStream; const FileName : String ) : Boolean;
    Read  : function( var Stream : zglTSoundStream; const Buffer : Pointer; const Bytes : LongWord; var _End : Boolean ) : LongWord;
    Loop  : procedure( var Stream : zglTSoundStream );
    Close : procedure( var Stream : zglTSoundStream );
  end;

  zglTSoundFormat = record
    Extension  : String;
    Decoder    : zglPSoundDecoder;
    FileLoader : procedure( const FileName : String; var Data : Pointer; var Size, Format, Frequency : LongWord );
    MemLoader  : procedure( const Memory : zglTMemory; var Data : Pointer; var Size, Format, Frequency : LongWord );
  end;

  zglTSoundManager = record
    Count   : record
      Items   : LongWord;
      Formats : LongWord;
              end;
    First   : zglTSound;
    Formats : array of zglTSoundFormat;
  end;

var
  snd_Init              : function : Boolean;
  snd_Free              : procedure;
  snd_Add               : function( const SourceCount : Integer ) : zglPSound;
  snd_Del               : procedure( var Sound : zglPSound );
  snd_LoadFromFile      : function( const FileName : String; const SourceCount : Integer = 8 ) : zglPSound;
  snd_LoadFromMemory    : function( const Memory : zglTMemory; const Extension : String; const SourceCount : Integer = 8 ) : zglPSound;
  snd_Play              : function( const Sound : zglPSound; const Loop : Boolean = FALSE; const X : Single = 0; const Y : Single = 0; const Z : Single = 0 ) : Integer;
  snd_Stop              : procedure( const Sound : zglPSound; const ID : Integer );
  snd_SetPos            : procedure( const Sound : zglPSound; const ID : Integer; const X, Y, Z : Single );
  snd_SetVolume         : procedure( const Sound : zglPSound; const ID : Integer; const Volume : Single );
  snd_SetSpeed          : procedure( const Sound : zglPSound; const ID : Integer; const Speed : Single );
  snd_Get               : function( const Sound : zglPSound; const ID, What : Integer ) : Integer;
  snd_PlayFile          : function( const FileName : String; const Loop : Boolean = FALSE ) : Integer;
  snd_PauseFile         : procedure( const ID : Integer );
  snd_StopFile          : procedure( const ID : Integer );
  snd_ResumeFile        : procedure( const ID : Integer );

// MATH
const
  ORIENTATION_LEFT  = -1;
  ORIENTATION_RIGHT = 1;
  ORIENTATION_ZERO  = 0;

var
  m_Cos         : function( Angle : Integer ) : Single;
  m_Sin         : function( Angle : Integer ) : Single;
  m_Distance    : function( const x1, y1, x2, y2 : Single ) : Single;
  m_FDistance   : function( const x1, y1, x2, y2 : Single ) : Single;
  m_Angle       : function( const x1, y1, x2, y2 : Single ) : Single;
  m_Orientation : function( const x, y, x1, y1, x2, y2 : Single ) : Integer;

  tess_Triangulate : procedure( const Contour : zglPPoints2D; const iLo, iHi : Integer; const AddHoles : Boolean = FALSE );
  tess_AddHole     : procedure( const Contour : zglPPoints2D; const iLo, iHi : Integer; const LastHole : Boolean = TRUE );
  tess_GetData     : function( var TriPoints : zglPPoints2D ) : Integer;

// COLLISION 2D
  col2d_PointInRect     : function( const X, Y : Single; const Rect : zglTRect ) : Boolean;
  col2d_PointInTriangle : function( const X, Y : Single; const P1, P2, P3 : zglTPoint2D ) : Boolean;
  col2d_PointInCircle   : function( const X, Y : Single; const Circle : zglTCircle ) : Boolean;
  // line 2d
  col2d_Line           : function( const A, B : zglTLine; ColPoint : zglPPoint2D ) : Boolean;
  col2d_LineVsRect     : function( const Line : zglTLine; const Rect : zglTRect; ColPoint : zglPPoint2D ) : Boolean;
  col2d_LineVsCircle   : function( const Line : zglTLine; const Circle : zglTCircle ) : Boolean;
  col2d_LineVsCircleXY : function( const Line : zglTLine; const Circle : zglTCircle; const Precision : Byte; ColPoint : zglPPoint2D ) : Boolean;
  // rect
  col2d_Rect         : function( const Rect1, Rect2 : zglTRect ) : Boolean;
  col2d_ClipRect     : function( const Rect1, Rect2 : zglTRect ) : zglTRect;
  col2d_RectInRect   : function( const Rect1, Rect2 : zglTRect ) : Boolean;
  col2d_RectInCircle : function( const Rect : zglTRect; const Circle : zglTCircle ) : Boolean;
  col2d_RectVsCircle : function( const Rect : zglTRect; const Circle : zglTCircle ) : Boolean;
  // circle
  col2d_Circle         : function( const Circle1, Circle2 : zglTCircle ) : Boolean;
  col2d_CircleInCircle : function( const Circle1, Circle2 : zglTCircle ) : Boolean;
  col2d_CircleInRect   : function( const Circle : zglTCircle; const Rect : zglTRect ) : Boolean;

const
  // Open Mode
  FOM_CREATE = $01; // Create
  FOM_OPENR  = $02; // Read
  FOM_OPENRW = $03; // Read&Write

  // Seek Mode
  FSM_SET    = $01;
  FSM_CUR    = $02;
  FSM_END    = $03;

  FILE_ERROR = {$IFNDEF WINDOWS} -1 {$ELSE} 0 {$ENDIF};

var
  file_Open         : procedure( var FileHandle : zglTFile; const FileName : String; const Mode : Byte );
  file_Exists       : function( const FileName : String ) : Boolean;
  file_Seek         : function( const FileHandle : zglTFile; const Offset, Mode : LongWord ) : LongWord;
  file_GetPos       : function( const FileHandle : zglTFile ) : LongWord;
  file_Read         : function( const FileHandle : zglTFile; var Buffer; const Bytes : LongWord ) : LongWord;
  file_Write        : function( const FileHandle : zglTFile; const Buffer; const Bytes : LongWord ) : LongWord;
  file_GetSize      : function( const FileHandle : zglTFile ) : LongWord;
  file_Flush        : procedure( const FileHandle : zglTFile );
  file_Close        : procedure( var FileHandle : zglTFile );
  file_Find         : procedure( const Directory : String; var List : zglTFileList; const FindDir : Boolean );
  file_GetName      : procedure( const FileName : String; var Result : String );
  file_GetExtension : procedure( const FileName : String; var Result : String );
  file_GetDirectory : procedure( const FileName : String; var Result : String );
  file_SetPath      : procedure( const Path : String );

var
  mem_LoadFromFile : procedure( var Memory : zglTMemory; const FileName : String );
  mem_SaveToFile   : procedure( var Memory : zglTMemory; const FileName : String );
  mem_Seek         : function( var Memory : zglTMemory; const Offset, Mode : LongWord ) : LongWord;
  mem_Read         : function( var Memory : zglTMemory; var Buffer; const Bytes : LongWord ) : LongWord;
  mem_Write        : function( var Memory : zglTMemory; const Buffer; const Bytes : LongWord ) : LongWord;
  mem_SetSize      : procedure( var Memory : zglTMemory; const Size : LongWord );
  mem_Free         : procedure( var Memory : zglTMemory );

// Utils
function u_IntToStr( const Value : Integer ) : String;
function u_StrToInt( const Value : String ) : Integer;
function u_FloatToStr( const Value : Single; const Digits : Integer = 2 ) : String;
function u_StrToFloat( const Value : String ) : Single;
function u_BoolToStr( const Value : Boolean ) : String;
function u_StrToBool( const Value : String ) : Boolean;
// Только для английских символов попадающих в диапазон 0..127
function u_StrUp( const str : String ) : String;
function u_StrDown( const str : String ) : String;
var
  u_SortList : procedure( var List : zglTStringList; iLo, iHi : Integer );

{$IFDEF LINUX_OR_DARWIN}
function dlopen ( Name : PChar; Flags : longint) : Pointer; cdecl; external 'dl';
function dlclose( Lib : Pointer) : Longint; cdecl; external 'dl';
function dlsym  ( Lib : Pointer; Name : Pchar) : Pointer; cdecl; external 'dl';
{$ENDIF}

{$IFDEF WINDOWS}
function dlopen ( lpLibFileName : PAnsiChar) : HMODULE; stdcall; external 'kernel32.dll' name 'LoadLibraryA';
function dlclose( hLibModule : HMODULE ) : Boolean; stdcall; external 'kernel32.dll' name 'FreeLibrary';
function dlsym  ( hModule : HMODULE; lpProcName : PAnsiChar) : Pointer; stdcall; external 'kernel32.dll' name 'GetProcAddress';

function MessageBoxA( hWnd : LongWord; lpText, lpCaption : PAnsiChar; uType : LongWord) : Integer; stdcall; external 'user32.dll';
{$ENDIF}

implementation

var
  zglLib : {$IFDEF LINUX_OR_DARWIN} Pointer {$ENDIF} {$IFDEF WINDOWS} HMODULE {$ENDIF};
  {$IFDEF DARWIN}
  mainBundle   : CFBundleRef;
  tmpCFURLRef  : CFURLRef;
  tmpCFString  : CFStringRef;
  tmpPath      : array[ 0..8191 ] of Char;
  outItemHit   : SInt16;
  {$ENDIF}

function u_IntToStr;
begin
  Str( Value, Result );
end;

function u_StrToInt;
  var
    E : Integer;
begin
  Val( String( Value ), Result, E );
end;

function u_FloatToStr;
begin
  Str( Value:0:Digits, Result );
end;

function u_StrToFloat;
  var
    E : Integer;
begin
  Val( String( Value ), Result, E );
  if E <> 0 Then
    Result := 0;
end;

function u_BoolToStr;
begin
  if Value Then
    Result := 'TRUE'
  else
    Result := 'FALSE';
end;

function u_StrToBool;
begin
  if Value = '1' Then
    Result := TRUE
  else
    if u_StrUp( Value ) = 'TRUE' Then
      Result := TRUE
    else
      Result := FALSE;
end;

function u_StrUp;
  var
    i, l : Integer;
begin
  l := length( Str );
  SetLength( Result, l );
  for i := 1 to l do
    if ( Byte( Str[ i ] ) >= 97 ) and ( Byte( Str[ i ] ) <= 122 ) Then
      Result[ i ] := Char( Byte( Str[ i ] ) - 32 )
    else
      Result[ i ] := Str[ i ];
end;

function u_StrDown;
  var
    i, l : Integer;
begin
  l := length( Str );
  SetLength( Result, l );
  for i := 1 to l do
    if ( Byte( Str[ i ] ) >= 65 ) and ( Byte( Str[ i ] ) <= 90 ) Then
      Result[ i ] := Char( Byte( Str[ i ] ) + 32 )
    else
      Result[ i ] := Str[ i ];
end;

procedure zglFree;
begin
  dlClose( zglLib );
end;


function zglLoad;
begin
  {$IFDEF DARWIN}
  mainBundle  := CFBundleGetMainBundle;
  tmpCFURLRef := CFBundleCopyBundleURL( mainBundle );
  tmpCFString := CFURLCopyFileSystemPath( tmpCFURLRef, kCFURLPOSIXPathStyle );
  CFStringGetFileSystemRepresentation( tmpCFString, @tmpPath[ 0 ], 8192 );
  mainPath    := tmpPath + '/Contents/';
  LibraryName := mainPath + 'Frameworks/' + LibraryName;
  {$ENDIF}
  zglLib := dlopen( PAnsiChar( LibraryName ) {$IFDEF LINUX_OR_DARWIN}, $001 {$ENDIF} );

  if zglLib <> {$IFDEF LINUX_OR_DARWIN} nil {$ENDIF} {$IFDEF WINDOWS} 0 {$ENDIF} Then
    begin
      Result := TRUE;
      zgl_Init := dlsym( zglLib, 'zgl_Init' );
      zgl_InitToHandle := dlsym( zglLib, 'zgl_InitToHandle' );
      zgl_Exit := dlsym( zglLib, 'zgl_Exit' );
      zgl_Reg := dlsym( zglLib, 'zgl_Reg' );
      zgl_Get := dlsym( zglLib, 'zgl_Get' );
      zgl_GetSysDir := dlsym( zglLib, 'zgl_GetSysDir' );
      zgl_GetMem := dlsym( zglLib, 'zgl_GetMem' );
      zgl_FreeMem := dlsym( zglLib, 'zgl_FreeMem' );
      zgl_FreeStr := dlsym( zglLib, 'zgl_FreeStr' );
      zgl_Enable := dlsym( zglLib, 'zgl_Enable' );
      zgl_Disable := dlsym( zglLib, 'zgl_Disable' );

      log_Add := dlsym( zglLib, 'log_Add' );

      wnd_SetCaption := dlsym( zglLib, 'wnd_SetCaption' );
      wnd_SetSize := dlsym( zglLib, 'wnd_SetSize' );
      wnd_SetPos := dlsym( zglLib, 'wnd_SetPos' );
      wnd_ShowCursor := dlsym( zglLib, 'wnd_ShowCursor' );

      scr_Clear := dlsym( zglLib, 'scr_Clear' );
      scr_Flush := dlsym( zglLib, 'scr_Flush' );
      scr_SetVSync := dlsym( zglLib, 'scr_SetVSync' );
      scr_SetFSAA := dlsym( zglLib, 'scr_SetFSAA' );
      scr_SetOptions := dlsym( zglLib, 'scr_SetOptions' );
      scr_CorrectResolution := dlsym( zglLib, 'scr_CorrectResolution' );

      ini_LoadFromFile := dlsym( zglLib, 'ini_LoadFromFile' );
      ini_SaveToFile := dlsym( zglLib, 'ini_SaveToFile' );
      ini_Add := dlsym( zglLib, 'ini_Add' );
      ini_Del := dlsym( zglLib, 'ini_Del' );
      ini_Clear := dlsym( zglLib, 'ini_Clear' );
      ini_IsSection := dlsym( zglLib, 'ini_IsSection' );
      ini_IsKey := dlsym( zglLib, 'ini_IsKey' );
      ini_ReadKeyStr := dlsym( zglLib, 'ini_ReadKeyStr' );
      ini_ReadKeyInt := dlsym( zglLib, 'ini_ReadKeyInt' );
      ini_ReadKeyFloat := dlsym( zglLib, 'ini_ReadKeyFloat' );
      ini_ReadKeyBool := dlsym( zglLib, 'ini_ReadKeyBool' );
      ini_WriteKeyStr := dlsym( zglLib, 'ini_WriteKeyStr' );
      ini_WriteKeyInt := dlsym( zglLib, 'ini_WriteKeyInt' );
      ini_WriteKeyFloat := dlsym( zglLib, 'ini_WriteKeyFloat' );
      ini_WriteKeyBool := dlsym( zglLib, 'ini_WriteKeyBool' );

      timer_Add := dlsym( zglLib, 'timer_Add' );
      timer_Del := dlsym( zglLib, 'timer_Del' );
      timer_GetTicks := dlsym( zglLib, 'timer_GetTicks' );
      timer_Reset := dlsym( zglLib, 'timer_Reset' );

      mouse_X := dlsym( zglLib, 'mouse_X' );
      mouse_Y := dlsym( zglLib, 'mouse_Y' );
      mouse_DX := dlsym( zglLib, 'mouse_DX' );
      mouse_DY := dlsym( zglLib, 'mouse_DY' );
      mouse_Down := dlsym( zglLib, 'mouse_Down' );
      mouse_Up := dlsym( zglLib, 'mouse_Up' );
      mouse_Click := dlsym( zglLib, 'mouse_Click' );
      mouse_DblClick := dlsym( zglLib, 'mouse_DblClick' );
      mouse_Wheel := dlsym( zglLib, 'mouse_Wheel' );
      mouse_ClearState := dlsym( zglLib, 'mouse_ClearState' );
      mouse_Lock := dlsym( zglLib, 'mouse_Lock' );

      key_Down := dlsym( zglLib, 'key_Down' );
      key_Up := dlsym( zglLib, 'key_Up' );
      key_Press := dlsym( zglLib, 'key_Press' );
      key_Last := dlsym( zglLib, 'key_Last' );
      key_BeginReadText := dlsym( zglLib, 'key_BeginReadText' );
      key_GetText := dlsym( zglLib, 'key_GetText' );
      key_EndReadText := dlsym( zglLib, 'key_EndReadText' );
      key_ClearState := dlsym( zglLib, 'key_ClearState' );

      joy_Init := dlsym( zglLib, 'joy_Init' );
      joy_GetInfo := dlsym( zglLib, 'joy_GetInfo' );
      joy_AxisPos := dlsym( zglLib, 'joy_AxisPos' );
      joy_Down := dlsym( zglLib, 'joy_Down' );
      joy_Up := dlsym( zglLib, 'joy_Up' );
      joy_Press := dlsym( zglLib, 'joy_Press' );
      joy_ClearState := dlsym( zglLib, 'joy_ClearState' );

      tex_Add := dlsym( zglLib, 'tex_Add' );
      tex_Del := dlsym( zglLib, 'tex_Del' );
      tex_Create := dlsym( zglLib, 'tex_Create' );
      tex_CreateZero := dlsym( zglLib, 'tex_CreateZero' );
      tex_LoadFromFile := dlsym( zglLib, 'tex_LoadFromFile' );
      tex_LoadFromMemory := dlsym( zglLib, 'tex_LoadFromMemory' );
      tex_SetFrameSize := dlsym( zglLib, 'tex_SetFrameSize' );
      tex_SetMask := dlsym( zglLib, 'tex_SetMask' );
      tex_GetData := dlsym( zglLib, 'tex_GetData' );
      tex_Filter := dlsym( zglLib, 'tex_Filter' );
      tex_SetAnisotropy := dlsym( zglLib, 'tex_SetAnisotropy' );

      atlas_Add := dlsym( zglLib, 'atlas_Add' );
      atlas_Del := dlsym( zglLib, 'atlas_Del' );
      atlas_GetFrameCoord := dlsym( zglLib, 'atlas_GetFrameCoord' );
      atlas_InsertFromTexture := dlsym( zglLib, 'atlas_InsertFromTexture' );
      atlas_InsertFromFile := dlsym( zglLib, 'atlas_InsertFromFile' );
      atlas_InsertFromMemory := dlsym( zglLib, 'atlas_InsertFromMemory' );

      Set2DMode := dlsym( zglLib, 'Set2DMode' );
      Set3DMode := dlsym( zglLib, 'Set3DMode' );

      zbuffer_SetDepth := dlsym( zglLib, 'zbuffer_SetDepth' );
      zbuffer_Clear := dlsym( zglLib, 'zbuffer_Clear' );

      scissor_Begin := dlsym( zglLib, 'scissor_Begin' );
      scissor_End := dlsym( zglLib, 'scissor_End' );

      rtarget_Add := dlsym( zglLib, 'rtarget_Add' );
      rtarget_Del := dlsym( zglLib, 'rtarget_Del' );
      rtarget_Set := dlsym( zglLib, 'rtarget_Set' );
      rtarget_DrawIn := dlsym( zglLib, 'rtarget_Set' );

      fx_SetBlendMode := dlsym( zglLib, 'fx_SetBlendMode' );
      fx_SetColorMode := dlsym( zglLib, 'fx_SetColorMode' );
      fx2d_SetColor := dlsym( zglLib, 'fx2d_SetColor' );
      fx2d_SetVCA := dlsym( zglLib, 'fx2d_SetVCA' );
      fx2d_SetVertexes := dlsym( zglLib, 'fx2d_SetVertexes' );
      fx2d_SetScale := dlsym( zglLib, 'fx2d_SetScale' );

      cam2d_Set := dlsym( zglLib, 'cam2d_Set' );
      cam2d_Apply := dlsym( zglLib, 'cam2d_Apply' );

      batch2d_Begin := dlsym( zglLib, 'batch2d_Begin' );
      batch2d_End := dlsym( zglLib, 'batch2d_End' );
      batch2d_Flush := dlsym( zglLib, 'batch2d_Flush' );

      pr2d_Pixel := dlsym( zglLib, 'pr2d_Pixel' );
      pr2d_Line := dlsym( zglLib, 'pr2d_Line' );
      pr2d_Rect := dlsym( zglLib, 'pr2d_Rect' );
      pr2d_Circle := dlsym( zglLib, 'pr2d_Circle' );
      pr2d_Ellipse := dlsym( zglLib, 'pr2d_Ellipse' );
      pr2d_TriList := dlsym( zglLib, 'pr2d_TriList' );

      sengine2d_AddSprite := dlsym( zglLib, 'sengine2d_AddSprite' );
      sengine2d_DelSprite := dlsym( zglLib, 'sengine2d_DelSprite' );
      sengine2d_ClearAll := dlsym( zglLib, 'sengine2d_ClearAll' );
      sengine2d_Set := dlsym( zglLib, 'sengine2d_Set' );
      sengine2d_Draw := dlsym( zglLib, 'sengine2d_Draw' );
      sengine2d_Proc := dlsym( zglLib, 'sengine2d_Proc' );

      texture2d_Draw := dlsym( zglLib, 'texture2d_Draw' );
      ssprite2d_Draw := dlsym( zglLib, 'ssprite2d_Draw' );
      asprite2d_Draw := dlsym( zglLib, 'asprite2d_Draw' );
      csprite2d_Draw := dlsym( zglLib, 'csprite2d_Draw' );
      tiles2d_Draw := dlsym( zglLib, 'tiles2d_Draw' );
      sgrid2d_Draw := dlsym( zglLib, 'sgrid2d_Draw' );
      agrid2d_Draw := dlsym( zglLib, 'agrid2d_Draw' );
      cgrid2d_Draw := dlsym( zglLib, 'cgrid2d_Draw' );

      pengine2d_AddEmitter := dlsym( zglLib, 'pengine2d_AddEmitter' );
      pengine2d_DelEmitter := dlsym( zglLib, 'pengine2d_DelEmitter' );
      pengine2d_ClearAll := dlsym( zglLib, 'pengine2d_ClearAll' );
      pengine2d_Set := dlsym( zglLib, 'pengine2d_Set' );
      pengine2d_Draw := dlsym( zglLib, 'pengine2d_Draw' );
      pengine2d_Proc := dlsym( zglLib, 'pengine2d_Proc' );
      emitter2d_Init := dlsym( zglLib, 'emitter2d_Init' );
      emitter2d_Draw := dlsym( zglLib, 'emitter2d_Draw' );
      emitter2d_Proc := dlsym( zglLib, 'emitter2d_Proc' );

      font_Add := dlsym( zglLib, 'font_Add' );
      font_Del := dlsym( zglLib, 'font_Del' );
      font_LoadFromFile := dlsym( zglLib, 'font_LoadFromFile' );
      font_LoadFromMemory := dlsym( zglLib, 'font_LoadFromMemory' );
      text_Draw := dlsym( zglLib, 'text_Draw' );
      text_DrawEx := dlsym( zglLib, 'text_DrawEx' );
      text_DrawInRect := dlsym( zglLib, 'text_DrawInRect' );
      text_DrawInRectEx := dlsym( zglLib, 'text_DrawInRectEx' );
      text_GetWidth := dlsym( zglLib, 'text_GetWidth' );
      textFx_SetLength := dlsym( zglLib, 'textFx_SetLength' );

      snd_Init := dlsym( zglLib, 'snd_Init' );
      snd_Free := dlsym( zglLib, 'snd_Free' );
      snd_Add  := dlsym( zglLib, 'snd_Add' );
      snd_Del  := dlsym( zglLib, 'snd_Del' );
      snd_LoadFromFile := dlsym( zglLib, 'snd_LoadFromFile' );
      snd_LoadFromMemory := dlsym( zglLib, 'snd_LoadFromMemory' );
      snd_Play := dlsym( zglLib, 'snd_Play' );
      snd_Stop := dlsym( zglLib, 'snd_Stop' );
      snd_SetPos := dlsym( zglLib, 'snd_SetPos' );
      snd_SetVolume := dlsym( zglLib, 'snd_SetVolume' );
      snd_SetSpeed := dlsym( zglLib, 'snd_SetSpeed' );
      snd_Get := dlsym( zglLib, 'snd_Get' );
      snd_PlayFile := dlsym( zglLib, 'snd_PlayFile' );
      snd_PauseFile := dlsym( zglLib, 'snd_PauseFile' );
      snd_StopFile := dlsym( zglLib, 'snd_StopFile' );
      snd_ResumeFile := dlsym( zglLib, 'snd_ResumeFile' );

      m_Cos := dlsym( zglLib, 'm_Cos' );
      m_Sin := dlsym( zglLib, 'm_Sin' );
      m_Distance := dlsym( zglLib, 'm_Distance' );
      m_FDistance := dlsym( zglLib, 'm_FDistance' );
      m_Angle := dlsym( zglLib, 'm_Angle' );
      m_Orientation := dlsym( zglLib, 'm_Orientation' );

      tess_Triangulate := dlsym( zglLib, 'tess_Triangulate' );
      tess_AddHole := dlsym( zglLib, 'tess_AddHole' );
      tess_GetData := dlsym( zglLib, 'tess_GetData' );

      col2d_PointInRect := dlsym( zglLib, 'col2d_PointInRect' );
      col2d_PointInTriangle := dlsym( zglLib, 'col2d_PointInriangle' );
      col2d_PointInCircle := dlsym( zglLib, 'col2d_PointInCircle' );
      col2d_Line := dlsym( zglLib, 'col2d_Line' );
      col2d_LineVsRect := dlsym( zglLib, 'col2d_LineVsRect' );
      col2d_LineVsCircle := dlsym( zglLib, 'col2d_LineVsCircle' );
      col2d_LineVsCircleXY := dlsym( zglLib, 'col2d_LineVsCircleXY' );
      col2d_Rect := dlsym( zglLib, 'col2d_Rect' );
      col2d_ClipRect := dlsym( zglLib, 'col2d_ClipRect' );
      col2d_RectInRect := dlsym( zglLib, 'col2d_RectInRect' );
      col2d_RectInCircle := dlsym( zglLib, 'col2d_RectInCircle' );
      col2d_RectVsCircle := dlsym( zglLib, 'col2d_RectVsCircle' );
      col2d_Circle := dlsym( zglLib, 'col2d_Circle' );
      col2d_CircleInCircle := dlsym( zglLib, 'col2d_CircleInCircle' );
      col2d_CircleInRect := dlsym( zglLib, 'col2d_CircleInRect' );

      file_Open := dlsym( zglLib, 'file_Open' );
      file_Exists := dlsym( zglLib, 'file_Exists' );
      file_Seek := dlsym( zglLib, 'file_Seek' );
      file_GetPos := dlsym( zglLib, 'file_GetPos' );
      file_Read := dlsym( zglLib, 'file_Read' );
      file_Write := dlsym( zglLib, 'file_Write' );
      file_GetSize := dlsym( zglLib, 'file_GetSize' );
      file_Flush := dlsym( zglLib, 'file_Flush' );
      file_Close := dlsym( zglLib, 'file_Close' );
      file_Find := dlsym( zglLib, 'file_Find' );
      file_GetName := dlsym( zglLib, 'file_GetName' );
      file_GetExtension := dlsym( zglLib, 'file_GetExtension' );
      file_GetDirectory := dlsym( zglLib, 'file_GetDirectory' );
      file_SetPath := dlsym( zglLib, 'file_SetPath' );

      mem_LoadFromFile := dlsym( zglLib, 'mem_LoadFromFile' );
      mem_SaveToFile := dlsym( zglLib, 'mem_SaveToFile' );
      mem_Seek := dlsym( zglLib, 'mem_Seek' );
      mem_Read := dlsym( zglLib, 'mem_Read' );
      mem_Write := dlsym( zglLib, 'mem_Write' );
      mem_SetSize := dlsym( zglLib, 'mem_SetSize' );
      mem_Free := dlsym( zglLib, 'mem_Free' );

      u_SortList := dlsym( zglLib, 'u_SortList' );
    end else
      if Error Then
        begin
          Result := FALSE;
          {$IFDEF LINUX}
          WriteLn( 'Error while loading ZenGL' );
          {$ENDIF}
          {$IFDEF WINDOWS}
          MessageBoxA( 0, 'Error while loading ZenGL', 'Error', $00000010 );
          {$ENDIF}
          {$IFDEF DARWIN}
          StandardAlert( kAlertNoteAlert, 'Error', 'Error while loading ZenGL', nil, outItemHit );
          {$ENDIF}
        end;
end;

end.
