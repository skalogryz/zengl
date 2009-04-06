{-------------------------------}
{-----------= ZenGL =-----------}
{-------------------------------}
{ version: 0.1.23               }
{ date:    05.04.09             }
{-------------------------------}
{ by:   Andru ( Kemka Andrey )  }
{ mail: dr.andru@gmail.com      }
{ ICQ:  496-929-849             }
{ JID:  dr.andru@jabber.kiev.ua }
{ site: http://andru-kun.ru     }
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

interface
{$IFDEF DARWIN}
uses
  MacOSAll;
{$ENDIF}

type
  DWORD   = LongWord;
  Ptr     = {$IFDEF CPU64}QWORD{$ELSE}DWORD{$ENDIF};
  PPtr    = ^Ptr;
  {$IFDEF WIN32}
  HANDLE  = DWORD;
  HDC     = DWORD;
  HGLRC   = DWORD;
  {$ENDIF}

type
  zglTStringList = record
    Count : Integer;
    Items : array of String;
end;

type zglTFile = DWORD;
type zglTFileList = zglTStringList;
type
  zglPMemory = ^zglTMemory;
  zglTMemory = record
    Memory   : Pointer;
    Size     : DWORD;
    Position : DWORD;
end;

const
{$IFDEF LINUX}
  libZenGL = 'libZenGL.so';
{$ENDIF}
{$IFDEF WIN32}
  libZenGL = 'ZenGL.dll';
{$ENDIF}
{$IFDEF DARWIN}
  libZenGL = 'libZenGL.dylib';
var
  mainPath : String;
{$ENDIF}

function zglLoad( LibraryName : String; Error : Boolean = TRUE ) : Boolean;
procedure zglFree;

var
  zgl_Init         : procedure( const FSAA : Byte = 0; const StencilBits : Byte = 0 );
  zgl_InitToHandle : procedure( const Handle : DWORD; const FSAA : Byte = 0; const StencilBits : Byte = 0 );
  zgl_Exit         : procedure;

const
  SYS_LOAD               = $000001;
  SYS_DRAW               = $000002;
  SYS_UPDATE             = $000003;
  SYS_EXIT               = $000004;
  TEX_FORMAT_EXTENSION   = $000010;
  TEX_FORMAT_FILE_LOADER = $000011;
  TEX_FORMAT_MEM_LOADER  = $000012;
  SND_FORMAT_EXTENSION   = $000020;
  SND_FORMAT_FILE_LOADER = $000021;
  SND_FORMAT_MEM_LOADER  = $000022;
  SND_FORMAT_STREAM      = $000023;
  WIDGET_TYPE_ID         = $000030;
  WIDGET_DESC_SIZE       = $000031;
  WIDGET_ONDRAW          = $000032;
  WIDGET_ONPROC          = $000033;

var
  zgl_Reg : procedure( const What : DWORD; const UserData : Pointer );

const
  SYS_FPS         = 1;  // DWORD,  := zgl_Get( SYS_FPS )
  APP_PAUSED      = 2;  // Boolean
  APP_DIRECTORY   = 3;  // PChar
  USR_HOMEDIR     = 4;  // PChar
  LOG_FILENAME    = 5;  // PPChar, := Pointer( zgl_Get( LOG_FILENAME ) )
  ZGL_VERSION     = 6;  // DWORD
  SCR_ADD_X       = 7;  // DWORD
  SCR_ADD_Y       = 8;  // DWORD
  DESKTOP_WIDTH   = 9;  // DWORD
  DESKTOP_HEIGHT  = 10; // DWORD
  RESOLUTION_LIST = 11; // PResolutionList
  MANAGER_TIMER   = 12; // zglPTimerManager
  MANAGER_TEXTURE = 13; // zglPTextureManager
  MANAGER_FONT    = 14; // zglPFontManager
  MANAGER_RTARGET = 15; // zglTRenderTargetManager
  MANAGER_SOUND   = 16; // zglPSoundManager
  MANAGER_GUI     = 17; // zglPGUIManager

var
  zgl_Get       : function( const What : DWORD ) : Ptr;
  zgl_GetSysDir : procedure;
  zgl_GetMem    : procedure( var Mem : Pointer; const Size : DWORD );

const
  COLOR_BUFFER_CLEAR    = $000001;
  DEPTH_BUFFER          = $000002;
  DEPTH_BUFFER_CLEAR    = $000004;
  DEPTH_MASK            = $000008;
  STENCIL_BUFFER_CLEAR  = $000010;
  CORRECT_RESOLUTION    = $000020;
  APP_USE_AUTOPAUSE     = $000040;
  APP_USE_LOG           = $000080;
  APP_USE_ENGLISH_INPUT = $000100;
  SND_CAN_PLAY          = $000200;
  SND_CAN_PLAY_FILE     = $000400;
  CROP_INVISIBLE        = $000800;

var
  zgl_Enable  : procedure( const What : DWORD );
  zgl_Disable : procedure( const What : DWORD );

// LOG
  log_Add : procedure( const Message : String; const Timings : Boolean = TRUE );

// WINDOW
  wnd_SetCaption : procedure( const NewCaption : String );
  wnd_SetSize    : procedure( const Width, Height : WORD );
  wnd_SetPos     : procedure( const X, Y : WORD );
  wnd_ShowCursor : procedure( const Show : Boolean );

// SCREEN
type
  zglPResolitionList = ^zglTResolutionList;
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
  scr_SetOptions        : procedure( const Width, Height, BPP, Refresh : WORD; const FullScreen, VSync : Boolean );
  scr_CorrectResolution : procedure( const Width, Height : WORD );

// Z BUFFER
  zbuffer_SetDepth  : procedure( const zNear, zFar : Single );
  zbuffer_Clear     : procedure;

// SCISSOR
  scissor_Begin : procedure( X, Y, Width, Height : WORD );
  scissor_End   : procedure;

// INI
  ini_LoadFromFile : procedure( const FileName : String );
  ini_SaveToFile   : procedure( const FileName : String );
  ini_Add          : procedure( const Section, Key : String );
  ini_IsKey        : function( const Section, Key : String ) : Boolean;
  ini_ReadKeyStr   : function( const Section, Key : String ) : PChar;
  ini_ReadKeyInt   : function( const Section, Key : String ) : Integer;
  ini_ReadKeyBool  : function( const Section, Key : String ) : Boolean;
  ini_WriteKeyStr  : function( const Section, Key, Value : String ) : Boolean;
  ini_WriteKeyInt  : function( const Section, Key : String; const Value : Integer ) : Boolean;
  ini_WriteKeyBool : function( const Section, Key : String; const Value : Boolean ) : Boolean;

// TIMERS
type
  zglPTimer = ^zglTTimer;
  zglTTimer = record
    Active     : Boolean;
    Interval   : DWORD;
    LastTick   : Double;
    OnTimer    : procedure;

    Prev, Next : zglPTimer;
end;

type
  zglPTimerManager = ^zglTTimerManager;
  zglTTimerManager = record
    Count   : DWORD;
    First   : zglTTimer;
end;

var
  timer_Add      : function( const OnTimer : Pointer; const Interval : DWORD ) : zglPTimer;
  timer_Del      : procedure( Timer : zglPTimer );
  timer_GetTicks : function : Double;

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
  K_TILDA      = $29;

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
  key_Last          : function( const KeyAction : Byte ) : Byte;
  key_BeginReadText : procedure( const Text : String; const MaxSymbols : WORD );
  key_EndReadText   : procedure( Result : String );
  key_ClearState    : procedure;

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
  mouse_Wheel      : function( const Axis : Byte ) : Boolean;
  mouse_ClearState : procedure;
  mouse_Lock       : procedure;

// GL
  Set2DMode : procedure;
  Set3DMode : procedure( FOVY : Single );

// TEXTURES
type
  zglPTexture = ^zglTTexture;
  zglTTexture = record
    ID            : DWORD;
    Width, Height : WORD;
    U, V          : Single;
    FramesX       : WORD;
    FramesY       : WORD;
    Flags         : DWORD;

    Prev, Next    : zglPTexture;
end;

type
  zglPTextureFormat = ^zglTTextureFormat;
  zglTTextureFormat = record
    Extension  : String;
    FileLoader : procedure( const FileName : String; var pData : Pointer; var W, H : WORD );
    MemLoader  : procedure( const Memory : zglTMemory; var pData : Pointer; var W, H : WORD );
end;

type
  zglPTextureManager = ^zglTTextureManager;
  zglTTextureManager = record
    Count   : record
                Items   : DWORD;
                Formats : DWORD;
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
  TEX_USEMASK           = $000080;

  TEX_FILTER_NEAREST    = $000100;
  TEX_FILTER_LINEAR     = $000200;
  TEX_FILTER_BILINEAR   = $000400;
  TEX_FILTER_TRILINEAR  = $000800;
  TEX_FILTER_ANISOTROPY = $001000;

  TEX_RGB               = $002000;

  TEX_QUALITY_LOW       = $400000;
  TEX_QUALITY_MEDIUM    = $800000;

  TEX_DEFAULT_2D        = TEX_CLAMP or TEX_CONVERT_TO_POT or TEX_FILTER_LINEAR;

var
  tex_Add            : function : zglPTexture;
  tex_Del            : procedure( Texture : zglPTexture );
  tex_Create         : procedure( var Texture : zglTTexture; var pData : Pointer );
  tex_CreateZero     : function( const Width, Height : WORD; const Color, Flags : DWORD ) : zglPTexture;
  tex_LoadFromFile   : function( const FileName : String; const TransparentColor, Flags : DWORD ) : zglPTexture;
  tex_LoadFromMemory : function( const Memory : zglTMemory; const Extension : String; const TransparentColor, Flags : DWORD ) : zglPTexture;
  tex_SetFrameSize   : procedure( const Texture : zglPTexture; FrameWidth, FrameHeight : WORD );
  tex_SetMask        : function( const Texture, Mask : zglPTexture ) : zglPTexture;
  tex_GetData        : procedure( const Texture : zglPTexture; var pData : Pointer; var pSize : Integer );
  tex_Filter         : procedure( const Texture : zglPTexture; const Flags : DWORD );
  tex_SetAnisotropy  : procedure( const Level : Byte );

// RENDER TARGETS
type
  zglPFBO = ^zglTFBO;
  zglTFBO = record
    FrameBuffer  : DWORD;
    RenderBuffer : DWORD;
end;

{$IFDEF WIN32}
type
  zglPPBuffer = ^zglTPBuffer;
  zglTPBuffer = record
    Handle : HANDLE;
    DC     : HDC;
    RC     : HGLRC;
end;
{$ENDIF}

type
  zglPRenderTarget = ^zglTRenderTarget;
  zglTRenderTarget = record
    rtType     : Byte;
    Handle     : Pointer;
    Surface    : zglPTexture;
    Flags      : Byte;

    Prev, Next : zglPRenderTarget;
end;

type
  zglPRenderTargetManager = ^zglTRenderTargetManager;
  zglTRenderTargetManager = record
    Count : DWORD;
    First : zglTRenderTarget;
end;

const
  RT_TYPE_SIMPLE  = 0;
  RT_TYPE_FBO     = 1;
  RT_TYPE_PBUFFER = 2;
  RT_FULL_SCREEN  = $01;
  RT_CLEAR_SCREEN = $02;

var
  rtarget_Add : function( rtType : Byte; const Surface : zglPTexture; const Flags : Byte ) : zglPRenderTarget;
  rtarget_Del : procedure( Target : zglPRenderTarget );
  rtarget_Set : procedure( const Target : zglPRenderTarget );

// 2D
type
  zglPPoint2D = ^zglTPoint2D;
  zglTPoint2D = record
    X, Y : Single;
end;

type
  zglPLine = ^zglTLine;
  zglTLine = record
    x0, y0 : Single;
    x1, y1 : Single;
end;

type
  zglPRect = ^zglTRect;
  zglTRect = record
    x, y, w, h : Single;
end;

type
  zglPCircle = ^zglTCircle;
  zglTCircle = record
    cX, cY : Single;
    radius : Single;
end;

// FX
const
  FX_BLEND_NORMAL = $00;
  FX_BLEND_ADD    = $01;
  FX_BLEND_MULT   = $02;
  FX_BLEND_BLACK  = $03;
  FX_BLEND_WHITE  = $04;
  FX_BLEND_MASK   = $05;

var
  fx_SetBlendMode : procedure( const Mode : Byte );

// FX 2D
const
  FX2D_FLIPX    = $000001;
  FX2D_FLIPY    = $000002;
  FX2D_COLORMIX = $000004;
  FX2D_VCA      = $000008;
  FX2D_VCHANGE  = $000010;
  FX2D_SCALE    = $000020;

  FX_BLEND      = $000040;

var
  fx2d_SetColorMix : procedure( const Color : DWORD );
  fx2d_SetVCA      : procedure( const c1, c2, c3, c4 : DWORD; const a1, a2, a3, a4 : Byte );
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
  cam2d_Set : procedure( const Camera : zglPCamera2D );

// Primitives 2D
const
  PR2D_FILL   = $000001;
  PR2D_SMOOTH = $000002;

var
  pr2d_Pixel   : procedure( const X, Y : Single; const Color : DWORD = $FFFFFF; const Alpha : Byte = 255 );
  pr2d_Line    : procedure( const X1, Y1, X2, Y2 : Single; const Color : DWORD = $FFFFFF; const Alpha : Byte = 255; const FX : DWORD = 0 );
  pr2d_Rect    : procedure( const X, Y, W, H : Single; const Color : DWORD = $FFFFFF; const Alpha : Byte = 255; const FX : DWORD = 0 );
  pr2d_Circle  : procedure( const X, Y, Radius : Single; const Color : DWORD = $FFFFFF; const Alpha : Byte = 255; const Quality : WORD = 32; const FX : DWORD = 0 );
  pr2d_Ellipse : procedure( const X, Y, xRadius, yRadius : Single; const Color : DWORD = $FFFFFF; const Alpha : Byte = 255; const Quality : WORD = 32; const FX : DWORD = 0 );

// Sprites 2D
  ssprite2d_Draw : procedure( const Texture : zglPTexture; X, Y, W, H, Angle : Single; const Alpha : Byte = 255; const FX : DWORD = FX_BLEND );
  asprite2d_Draw : procedure( const Texture : zglPTexture; X, Y, W, H, Angle : Single; Frame : WORD; const Alpha : Byte = 255; const FX : DWORD = FX_BLEND );
  csprite2d_Draw : procedure( const Texture : zglPTexture; X, Y, W, H, Angle : Single; const CutRect : zglTRect; const Alpha : Byte = 255; const FX : DWORD = FX_BLEND );

// Text
type
  zglPCharDesc = ^zglTCharDesc;
  zglTCharDesc = record
    Page      : WORD;
    Width     : Byte;
    Height    : Byte;
    ShiftX    : Integer;
    ShiftY    : Integer;
    TexCoords : array[ 0..3 ] of zglTPoint2D;
end;

type
  zglPFont = ^zglTFont;
  zglTFont = record
    Count      : record
                   Pages : WORD;
                   Chars : WORD;
                 end;

    Pages      : array of zglPTexture;
    CharDesc   : array[ 0..65535 ] of zglPCharDesc;
    MaxHeight  : Integer;
    MaxShiftY  : Integer;

    Prev, Next : zglPFont;
end;

type
  zglPFontManager = ^zglTFontManager;
  zglTFontManager = record
    Count : DWORD;
    First : zglTFont;
end;

var
  font_Add            : function : zglPFont;
  font_Del            : procedure( Font : zglPFont );
  font_LoadFromFile   : function( const FileName : String ) : zglPFont;
  font_LoadFromMemory : function( const Memory : zglTMemory ) : zglPFont;
  text_Draw           : procedure( const Font : zglPFont; X, Y : Single; const Text : String; const Flags : DWORD = 0 );
  text_DrawEx         : procedure( const Font : zglPFont; X, Y, Scale, Step : Single; const Text : String; const Alpha : Byte = 255; const Color : DWORD = $FFFFFF; const Flags : DWORD = 0 );
  text_DrawInRect     : procedure( const Font : zglPFont; const Rect : zglTRect; const Text : String; const Flags : DWORD = 0 );
  text_GetWidth       : function( const Font : zglPFont; const Text : String ) : Single;

// GUI
const
  WIDGET_UNKNOWN     = 0;
  WIDGET_BUTTON      = 1;
  WIDGET_CHECKBOX    = 2;
  WIDGET_RADIOBUTTON = 3;
  WIDGET_LABEL       = 4;
  WIDGET_EDITBOX     = 5;
  WIDGET_LISTBOX     = 6;
  WIDGET_GROUPBOX    = 7;
  WIDGET_SPIN        = 8;
  WIDGET_SCROLLBAR   = 9;

  // Events
  EVENT_CREATE      = 1;

  EVENT_FOCUS_IN    = 2;
  EVENT_FOCUS_OUT   = 3;

  EVENT_MOUSE_MOVE  = 4;
  EVENT_MOUSE_ENTER = 5;
  EVENT_MOUSE_LEAVE = 6;
  EVENT_MOUSE_DOWN  = 7;
  EVENT_MOUSE_UP    = 8;
  EVENT_MOUSE_CLICK = 9;
  EVENT_MOUSE_WHEEL = 10;

  EVENT_KEY_DOWN    = 11;
  EVENT_KEY_UP      = 12;
  EVENT_KEY_CHAR    = 13;

type
  zglPEvent  = ^zglTEvent;
  zglPWidget = ^zglTWidget;

  //Events
  zglTEvents = record
    OnFocus      : procedure( const Widget : zglPWidget; const Focus : Boolean );
    OnClick      : procedure( const Widget : zglPWidget );
    OnMouseUp    : procedure( const Widget : zglPWidget );
    OnMouseMove  : procedure( const Widget : zglPWidget; const X, Y : Single );
    OnMouseEnter : procedure( const Widget : zglPWidget );
    OnMouseLeave : procedure( const Widget : zglPWidget );
    OnKeyDown    : procedure( const Widget : zglPWidget; const KeyCode : Byte );
    OnKeyUp      : procedure( const Widget : zglPWidget; const KeyCode : Byte );
    OnSelectItem : procedure( const Widget : zglPWidget; const ItemIndex : Integer );
    OnChange     : procedure( const Widget : zglPWidget; const Value, Change : Integer );
end;

  //Widget
  zglTWidget = record
    _type      : Integer;
    desc       : Pointer;
    data       : Pointer;
    rect       : zglTRect;
    focus      : Boolean;
    mousein    : Boolean;

    OnDraw     : procedure( const Widget : zglPWidget );
    OnProc     : procedure( const Event  : zglPEvent );
    Events     : zglTEvents;

    parent     : zglPWidget;
    child      : zglPWidget;
    Next, Prev : zglPWidget;
end;

  zglTWidgetType = record
    _type    : Integer;
    DescSize : DWORD;

    OnDraw : procedure( const Widget : zglPWidget );
    OnProc : procedure( const Event  : zglPEvent );
end;

  //GUI Manager
  zglPGUIManager = ^zglTGUIManager;
  zglTGUIManager = record
    Count : record
              Items : DWORD;
              Types : DWORD;
            end;
    First : zglTWidget;
    Types : array of zglTWidgetType;
end;

  //Event
  zglTEvent = record
    _type      : Integer;
    widget     : zglPWidget;
    Next, Prev : zglPEvent;
    case byte of
      1: ( mouse_pos    : zglTPoint2D );
      2: ( mouse_button : Byte );
      3: ( key_code     : Byte );
      4: ( key_char     : PChar );
end;

  //Event list
  zglTEventList = record
    Count : DWORD;
    First : zglTEvent;
end;

  zglPButtonDesc = ^zglTButtonDesc;
  zglTButtonDesc = record
    Font    : zglPFont;
    Caption : String;

    Pressed : Boolean;
end;

  zglPCheckBoxDesc = ^zglTCheckBoxDesc;
  zglTCheckBoxDesc = record
    Font    : zglPFont;
    Caption : String;

    Checked : Boolean;
end;

  zglPRadioButtonDesc = ^zglTRadioButtonDesc;
  zglTRadioButtonDesc = record
    Font    : zglPFont;
    Caption : String;

    Checked : Boolean;
    Group   : Integer;
end;

  zglPLabelDesc = ^zglTLabelDesc;
  zglTLabelDesc = record
    Font    : zglPFont;
    Caption : String;
end;

  zglPEditBoxDesc = ^zglTEditBoxDesc;
  zglTEditBoxDesc = record
    Font : zglPFont;
    Text : String;

    Max  : Integer;
end;

  zglPListBoxDesc = ^zglTListBoxDesc;
  zglTListBoxDesc = record
    Font      : zglPFont;
    List      : zglTStringList;
    ItemIndex : Integer;
end;

  zglPGroupBoxDesc = ^zglTGroupBoxDesc;
  zglTGroupBoxDesc = record
    Font    : zglPFont;
    Caption : String;
end;

  zglPSpinDesc = ^zglTSpinDesc;
  zglTSpinDesc = record
    Value    : Integer;
    Max      : Integer;
    Min      : Integer;

    UPressed : Boolean;
    DPressed : Boolean;
end;

  zglPScrollBarDesc = ^zglTScrollBarDesc;
  zglTScrollBarDesc = record
    Kind     : Byte;
    Step     : Integer;
    Position : Integer;
    PageSize : Integer;
    Max      : Integer;

    UPressed : Boolean;
    DPressed : Boolean;
end;

var
  gui_Init      : procedure;
  gui_Draw      : procedure;
  gui_Proc      : procedure;
  gui_AddWidget : function( const _type : Integer; const X, Y, W, H : Single; const Desc, Data : Pointer; const Parent : zglPWidget ) : zglPWidget;
  gui_DelWidget : procedure( var Widget : zglPWidget );

// Sound
const
  SND_ALL    = -2;
  SND_STREAM = -3;

type
  zglPSound = ^zglTSound;
  zglTSound = record
    Buffer       : DWORD;
    sCount       : DWORD;
    Source       : array of Integer;

    Data         : Pointer;
    Size         : Integer;
    Frequency    : Integer;

    Prev, Next   : zglPSound;
end;

type
  zglPSoundStream = ^zglTSoundStream;
  zglTSoundStream = record
    _File      : zglTFile;
    Extension  : String;
    CodecOpen  : function( const FileName : String; var Stream : zglPSoundStream ) : Boolean;
    CodecRead  : function( const Buffer : Pointer; const Count : DWORD ) : DWORD;
    CodecLoop  : procedure;
    CodecClose : procedure( var Stream : zglPSoundStream );
    Rate       : DWORD;
    Channels   : DWORD;
    Buffer     : Pointer;
    BufferSize : DWORD;
    Loop       : Boolean;
    Played     : Boolean;
end;

type
  zglPSoundFormat = ^zglTSoundFormat;
  zglTSoundFormat = record
    Extension  : String;
    Stream     : zglPSoundStream;
    FileLoader : procedure( const FileName : String; var Data : Pointer; var Size, Format, Frequency : Integer );
    MemLoader  : procedure( const Memory : zglTMemory; var Data : Pointer; var Size, Format, Frequency : Integer );
end;

type
  zglPSoundManager = ^zglTSoundManager;
  zglTSoundManager = record
    Count   : record
                Items   : DWORD;
                Formats : DWORD;
              end;
    First   : zglTSound;
    Formats : array of zglTSoundFormat;
end;

var
  snd_Init              : function : Boolean;
  snd_Free              : procedure;
  snd_Add               : function( const BufferCount, SourceCount : Integer ) : zglPSound;
  snd_Del               : procedure( Sound : zglPSound );
  snd_LoadFromFile      : function( const FileName : String; const SourceCount : Integer ) : zglPSound;
  snd_LoadFromMemory    : function( const Memory : zglTMemory; Extension : String; const SourceCount : Integer ) : zglPSound;
  snd_Play              : function( const Sound : zglPSound; const X, Y, Z : Single; const Loop : Boolean ) : Integer;
  snd_Stop              : procedure( const Sound : zglPSound; const Source : Integer );
  snd_SetVolume         : procedure( const Sound : zglPSound; const Volume : Single; const ID : Integer );
  snd_SetFrequency      : procedure( const Sound : zglPSound; const Frequency, ID : Integer );
  snd_SetFrequencyCoeff : procedure( const Sound : zglPSound; const Coefficient : Single; const ID : Integer );
  snd_PlayFile          : procedure( const FileName : String; const Loop : Boolean );
  snd_StopFile          : procedure;
  snd_ResumeFile        : procedure;

// 3D
type
  zglPPoint3D = ^zglTPoint3D;
  zglTPoint3D = record
    case Byte of
    1 : ( X, Y, Z : Single );
    2:  ( point : array[ 0..2 ] of Single );
end;

{type
  zglPQuaternion = ^zglTQuaternion;
  zglTQuaternion = record
    X, Y, Z, W : Single;
end;

type
  zglPMatrix3f = ^zglTMatrix3f;
  zglTMatrix3f = record
    case Byte of
    1: ( a11, a12, a13 : Single;
         a21, a22, a23 : Single;
         a31, a32, a33 : Single );
    2: ( row : array[ 0..2 ] of zglTPoint3D );
end;

type
  zglPMatrix4f = ^zglTMatrix4f;
  zglTMatrix4f = record
    a11, a12, a13, a14 : Single;
    a21, a22, a23, a24 : Single;
    a31, a32, a33, a34 : Single;
    a41, a42, a43, a44 : Single;
end;

type
  zglPFace = ^zglTFace;
  zglTFace = array[ 0..2 ] of DWORD;

type
  zglPGroup = ^zglTGroup;
  zglTGroup = record
    FCount  : DWORD;
    IFace   : DWORD;
    Indices : Pointer;
end;

type
  zglPFrame = ^zglTFrame;
  zglTFrame = record
    Vertices : array of zglTPoint3D;
    Normals  : array of zglTPoint3D;
end;

type
  zglPLine3D = ^zglTLine3D;
  zglTLine3D = record
    p1, p2 : zglTPoint3D;
end;

type
  zglPPlane = ^zglTPlane;
  zglTPlane = record
    Points : array[ 0..2 ] of zglTPoint3D;
    D      : Single;
    Normal : zglTPoint3D;
end;

type
  zglPAABB = ^zglTAABB;
  zglTAABB = record
    Position : zglTPoint3D;
    Size     : zglTPoint3D;
end;

type
  zglPOBB = ^zglTOBB;
  zglTOBB = record
    Position : zglTPoint3D;
    Size     : zglTPoint3D;
    Matrix   : zglTMatrix3f;
end;

type
  zglPSphere = ^zglTSphere;
  zglTSphere = record
    Position : zglTPoint3D;
    Radius   : Single;
end;

// OBJECT 3D
const
  OBJ3D_TEXTURING     = $0000001;
  OBJ3D_MTEXTURING    = $0000002;
  OBJ3D_BLEND         = $0000004;
  OBJ3D_ALPHA_TEST    = $0000008;
  OBJ3D_WIRE_FRAME    = $0000010;
  OBJ3D_CULL_FACE     = $0000020;
  OBJ3D_LIGHTING      = $0000040;
  OBJ3D_SPHERE_MAP_S  = $0000080;
  OBJ3D_SPHERE_MAP_T  = $0000100;

  MAT_DIFFUSE         = $01;
  MAT_AMBIENT         = $02;
  MAT_SPECULAR        = $03;
  MAT_SHININESS       = $04;
  MAT_EMISSION        = $05;

  SIDE_FRONT          = $01;
  SIDE_BACK           = $02;
  SIDE_FRONT_AND_BACK = $03;

  // HeigthMap, Simple Mesh, Skinned Mesh, Octree
  USE_NORMALS         = $001;
  USE_TEXTURE         = $002;
  USE_MULTITEX1       = $004;
  USE_MULTITEX2       = $008;
  USE_MULTITEX3       = $010;
  USE_VBO             = $020;
  BUILD_SNORMALS      = $040;
  BUILD_PLANES        = $080;
  BUILD_VBO_STATIC    = $100;
  BUILD_VBO_STREAM    = $200;

var
  obj3d_Begin       : procedure( const Flags : DWORD );
  obj3d_End         : procedure;
  obj3d_Enable      : procedure( const Flags : DWORD );
  obj3d_Disable     : procedure( const Flags : DWORD );
  obj3d_SetColor    : procedure( const Color : DWORD; const Alpha : Byte );
  obj3d_BindTexture : procedure( const Texture : zglPTexture; const Level : Byte );
  obj3d_SetMaterial : procedure( const Material, Side : Byte; const Color : DWORD; const Alpha : Byte );
  obj3d_Scale       : procedure( const ScaleX, ScaleY, ScaleZ : Single );
  obj3d_Move        : procedure( const X, Y, Z : Single );
  obj3d_SetMatrix   : procedure( const Matrix : zglTMatrix4f );
  obj3d_MulMatrix   : procedure( const Matrix : zglTMatrix4f );

const
  AX = $01;
  AY = $02;
  AZ = $04;
var
  obj3d_Rotate : procedure( const Angle : Single; const Axis : Byte );

// PRIMITIVES 3D
  pr3d_Point  : procedure( const X, Y, Z : Single );
  pr3d_Line   : procedure( const X1, Y1, Z1, X2, Y2, Z2 : Single );
  pr3d_Plane  : procedure( const Width, Height : Single );
  pr3d_AABB   : procedure( const Width, Height, ZDepth : Single );
  pr3d_Sphere : procedure( Radius : Single; Quality : Integer );

// SPRITE 3D
  ssprite3d_Draw : procedure( const X, Y, Z, sX, sY, sZ : Single; const Matrix : zglTMatrix4f );
  asprite3d_Draw : procedure( const X, Y, Z, sX, sY, sZ : Single; const Frame : Integer; const Matrix : zglTMatrix4f );

// CAMERA 3D
type
  zglPCamera3D = ^zglTCamera3D;
  zglTCamera3D = record
    Position : zglTPoint3D;
    Rotation : zglTPoint3D;
    Matrix   : zglTMatrix4f;
end;

var
  cam3d_Set    : procedure( var Camera : zglTCamera3D );
  cam3d_Fly    : procedure( var Camera : zglTCamera3D; const Speed : Single );
  cam3d_Strafe : procedure( var Camera : zglTCamera3D; const Speed : Single );

// SIMPLE MESH
type
  zglPSimpleAction = ^zglTSimpleAction;
  zglTSimpleAction = record
    Name   : String;
    FPS    : Single;
    FCount : DWORD;
    FFrame : DWORD;
end;

type
  zglPSimpleState = ^zglTSimpleState;
  zglTSimpleState = record
    VBuffer   : DWORD;
    Action    : Integer;
    Frame     : Integer;
    Delta     : Single;
    prevDelta : Single;
    Time      : Double;
    Vertices  : array of zglTPoint3D;
    Normals   : array of zglTPoint3D;
end;

type
  zglPSMesh = ^zglTSMesh;
  zglTSMesh = record
    Flags          : DWORD;

    IBuffer        : DWORD;
    VBuffer        : DWORD;

    VCount         : DWORD;
    RVCount        : DWORD;
    TCount         : DWORD;
    FCount         : DWORD;
    GCount         : DWORD;
    ACount         : DWORD;
    Frames         : DWORD;

    Vertices       : array of zglTPoint3D;
    Normals        : array of zglTPoint3D;
    TexCoords      : array of zglTPoint2D;
    MultiTexCoords : array of zglTPoint2D;
    Faces          : array of zglTFace;
    Indices        : Pointer;
    RIndices       : array of DWORD;
    Groups         : array of zglTGroup;

    State          : zglTSimpleState;
    Actions        : array of zglTSimpleAction;
end;

var
  smesh_LoadFromFile   : function( const FileName : String; const Flags : DWORD = 0 ) : zglPSMesh;
  smesh_LoadFromMemory : function( var Memory : zglTMemory; const Flags : DWORD = 0 ) : zglPSMesh;
  smesh_Animate        : procedure( const Mesh : zglPSMesh; var State : zglTSimpleState );
  smesh_Draw           : procedure( const Mesh : zglPSMesh; const State : zglPSimpleState );
  smesh_DrawGroup      : procedure( const Mesh : zglPSMesh; const State : zglPSimpleState; const Group : DWORD );
  smesh_Free           : procedure( const Mesh : zglPSMesh );

// SKINNED MESH
type
  zglPBone = ^zglTBone;
  zglTBone = record
    Name   : String;
    Parent : Integer;
end;

type
  zglPBoneWeight = ^zglTBoneWeight;
  zglTBoneWeight = record
    boneID : Integer;
    Weight : Single;
end;

type zglTBonesWeights = array of array of zglTBoneWeight;

type
  zglPBonePos = ^zglTBonePos;
  zglTBonePos = record
    Point       : zglTPoint3D;
    Translation : zglTPoint3D;
    Rotation    : zglTPoint3D;
    Matrix      : zglTMatrix4f;
    Quaternion  : zglTQuaternion;
end;

type
  zglPSkeletonState = ^zglTSkeletonState;
  zglTSkeletonState = record
    VBuffer   : DWORD;
    Action    : Integer;
    Frame     : Integer;
    Delta     : Single;
    prevDelta : Single;
    Time      : Double;
    Vertices  : array of zglTPoint3D;
    Normals   : array of zglTPoint3D;
    BonePos   : array of zglTBonePos;
end;

type
  zglPSkeletonAction = ^zglTSkeletonAction;
  zglTSkeletonAction = record
    Name   : String;
    FPS    : Single;
    FCount : DWORD;
    Frames : array of array of zglTBonePos;
end;

type
  zglPSkMesh = ^zglTSkMesh;
  zglTSkMesh = record
    Flags          : DWORD;

    IBuffer        : DWORD;
    VBuffer        : DWORD;

    VCount         : DWORD;
    RVCount        : DWORD;
    TCount         : DWORD;
    FCount         : DWORD;
    GCount         : DWORD;
    BCount         : DWORD;
    WCount         : array of Byte;
    ACount         : DWORD;

    Vertices       : array of zglTPoint3D;
    Normals        : array of zglTPoint3D;
    TexCoords      : array of zglTPoint2D;
    MultiTexCoords : array of zglTPoint2D;
    Faces          : array of zglTFace;
    Indices        : Pointer;
    RIndices       : array of DWORD;
    Groups         : array of zglTGroup;

    Bones          : array of zglTBone;
    BonePos        : array of zglTBonePos;
    Weights        : zglTBonesWeights;
    State          : zglTSkeletonState;
    Actions        : array of zglTSkeletonAction;
end;

var
  skmesh_LoadFromFile   : function( const FileName : String; const Flags : DWORD = 0 ) : zglPSkMesh;
  skmesh_LoadFromMemory : function( var Memory : zglTMemory; const Flags : DWORD = 0 ) : zglPSkMesh;
  skmesh_Animate        : procedure( const Mesh : zglPSkMesh; var State : zglTSkeletonState );
  skmesh_Draw           : procedure( const Mesh : zglPSkMesh; const State : zglPSkeletonState );
  skmesh_DrawGroup      : procedure( const Mesh : zglPSkMesh; const State : zglPSkeletonState; const Group : DWORD );
  skmesh_DrawSkelet     : procedure( const Mesh : zglPSkMesh; const State : zglPSkeletonState );
  skmesh_Free           : procedure( const Mesh : zglPSkMesh );

// HEIGHTMAP
type
  zglPHeightMap = ^zglTHeightMap;
  zglTHeightMap = record
    Flags          : DWORD;

    Width, Height  : WORD;
    xScale, zScale : Single;

    IBuffer        : DWORD;
    VBuffer        : DWORD;

    VCount         : DWORD;
    TCount         : DWORD;
    FCount         : DWORD;
    ICount         : DWORD;
    PCount         : DWORD;

    Vertices       : array of zglTPoint3D;
    Normals        : array of zglTPoint3D;
    TexCoords      : array of zglTPoint2D;
    MultiTexCoords : array of zglTPoint2D;
    Faces          : array of zglTFace;
    Indices        : Pointer;
    Planes         : array of zglTPlane;
end;

var
  heightmap_Build      : function( const Texture : zglPTexture; const xScale, yScale, zScale : Single; const xDetail, yDetail : Integer; const Flags : DWORD ) : zglPHeightMap;
  heightmap_Draw       : procedure( const Heightmap : zglPHeightMap );
  heightmap_Free       : procedure( const Heightmap : zglPHeightMap );
  heightmap_GetPlane   : function( const Heightmap : zglPHeightMap; Position : zglTPoint3D ) : DWORD;
  heightmap_GetYOffset : function( const Heightmap : zglPHeightMap; Position : zglTPoint3D ) : Single;

// VBO
  vbo_Build : procedure( var IBuffer, VBuffer : DWORD; ICount, VCount : DWORD; Indices, Vertices, Normals, TexCoords, MultiTexCoords : Pointer; var Flags : DWORD );
  vbo_Free  : procedure( var IBuffer, VBuffer : DWORD; ICount, VCount : DWORD );}

// FRUSTUM
type
  zglPFrustum = ^zglTFrustum;
  zglTFrustum = array [ 0..5 ] of array[ 0..3 ] of Single;

var
  frustum_Calc       : procedure( var f : zglTFrustum );
  frustum_PointIn    : function( const f : zglTFrustum; const x, y, z : Single ) : Boolean;
  frustum_PPointIn   : function( const f : zglTFrustum; const Vertex : zglTPoint3D ) : Boolean;
  frustum_TriangleIn : function( const f : zglTFrustum; const v1, v2, v3 : zglTPoint3D ) : Boolean;
  frustum_SphereIn   : function( const f : zglTFrustum; const x, y, z, r : Single ) : Boolean;
  frustum_BoxIn      : function( const f : zglTFrustum; const x, y, z, bx, by, bz : Single ) : Boolean;
  frustum_CubeIn     : function( const f : zglTFrustum; const x, y, z, size : Single ) : Boolean;

{// OCTREE
type
  zglPRenderData = ^zglTRenderData;
  zglTRenderData = record
    Texture : DWORD;
    ICount  : DWORD;
    Indices : Pointer;
    IBType  : DWORD;
end;

type
  zglPNode = ^zglTNode;
  zglTNode = record
    Cube       : zglTAABB;

    RDSize     : DWORD;
    RenderData : array of zglTRenderData;
    DFCount    : DWORD;
    DFaces     : array of DWORD;
    PCount     : DWORD;
    Planes     : array of DWORD;

    NInside    : Boolean;
    SubNodes   : array[ 0..7 ] of zglPNode;
end;

type
  zglPOctree = ^zglTOctree;
  zglTOctree  = record
    Flags           : DWORD;

    IBuffer         : DWORD;
    VBuffer         : DWORD;

    MainNode        : zglPNode;

    VCount          : DWORD;
    TCount          : DWORD;
    FCount          : DWORD;
    ICount          : DWORD;

    Vertices        : array of zglTPoint3D;
    TexCoords       : array of zglTPoint2D;
    MultiTexCoords  : array of zglTPoint2D;
    Normals         : array of zglTPoint3D;
    Faces           : array of zglTFace;
    Indices         : Pointer;
    Planes          : array of zglTPlane;
    Textures        : array of DWORD;

    MaxDFaces       : DWORD;
    DFaces          : array of DWORD;

    r_DFacesAlready : array of DWORD;
    r_DFacesCount   : DWORD;
    r_DFacesACount  : DWORD;

    r_NodeACount    : DWORD;
end;

var
  octree_Build     : procedure( var Octree : zglTOctree; const MaxFacesPerNode, Flags : DWORD );
  octree_Free      : procedure( var Octree : zglTOctree );
  octree_Draw      : procedure( var Octree : zglTOctree; const Frustum : zglTFrustum );
  octree_DrawDebug : procedure( var Octree : zglTOctree; const Frustum : zglTFrustum );
  octree_DrawNode  : procedure( var OCtree : zglTOctree; const Node : zglTNode; const Frustum : zglTFrustum );

// LIGHT
  light_Enable      : procedure( const ID : Byte );
  light_Disable     : procedure( const ID : Byte );
  light_SetPosition : procedure( const ID : Byte; const X, Y, Z : Single; const W : Single = 1 );
  light_SetMaterial : procedure( const ID, Material : Byte; const Color : DWORD; const Alpha : Byte );

// FOG
const
  FOG_MODE_EXP    = 0;
  FOG_MODE_EXP2   = 1;
  FOG_MODE_LINEAR = 2;

var
  fog_Enable      : procedure;
  fog_Disable     : procedure;
  fog_SetMode     : procedure( const Mode : Byte );
  fog_SetColor    : procedure( const Color : DWORD );
  fog_SetDensity  : procedure( const Density : Single );
  fog_SetBeginEnd : procedure( const fBegin, fEnd : Single );

// SKYBOX
  skybox_Init : procedure( const Top, Bottom, Left, Right, Front, Back : zglPTexture );
  skybox_Draw : procedure;

// SHADOW VOLUMES
type
  zglPShadowVolume = ^zglTShadowVolume;
  zglTShadowVolume = record
    VCount : DWORD;
    FCount : DWORD;
    ICount : DWORD;

    Caps       : array of zglTPoint3D;
    Indices    : array of DWORD;
    Planes     : array of zglTPlane;

    eVertices : array of zglTPoint3D;
    eVCount   : DWORD;

    isFacingLight    : array of Boolean;
    neighbourIndices : array of Integer;
    isSilhouetteEdge : array of Boolean;
end;

var
  shadow_InitVolume        : function( const Vertices : zglPPoint3D; const FCount : DWORD; const Faces : zglPFace ) : zglPShadowVolume;
  shadow_CalcVolume        : procedure( var Volume : zglTShadowVolume; const Matrix : zglPMatrix4f; const Vertices : zglPPoint3D; const Light : zglTPoint3D; const RebuildPlanes : Boolean; const Extrude : Single );
  shadow_DrawVolume        : procedure( const Volume : zglPShadowVolume; const zFail : Boolean );
  shadow_DrawShadowVolumes : procedure( const DrawVolumes : Pointer );

// SHADERS
const
  SHADER_ARB          = 0;
  SHADER_GLSL         = 1;
  SHADER_VERTEX_ARB   = $8620;
  SHADER_FRAGMENT_ARB = $8804;
  SHADER_VERTEX       = $8B31;
  SHADER_FRAGMENT     = $8B30;

const
  matrix3f_Identity: zglTMatrix3f = ( a11: 1; a12: 0; a13: 0; a21: 0; a22: 1; a23: 0; a31: 0; a32: 0; a33: 1 );
  matrix4f_Identity: zglTMatrix4f = ( a11: 1; a12: 0; a13: 0; a14: 0; a21: 0; a22: 1; a23: 0; a24: 0; a31: 0; a32: 0; a33: 1; a34: 0; a41: 0; a42: 0; a43: 0; a44: 1 );

var
  // ARBfp/ARBvp
  shader_InitARB         : function : Boolean;
  shader_LoadFromFileARB : function( const FileName : String; const ShaderType : DWORD ) : DWORD;
  shader_BeginARB        : procedure( const Shader, ShaderType : DWORD );
  shader_EndARB          : procedure( const ShaderType : DWORD );
  shader_FreeARB         : procedure( const Shader : DWORD );

  // GLSL
  shader_InitGLSL       : function : Boolean;
  shader_LoadFromFile   : function( const FileName : String; const ShaderType : Integer; const Link : Boolean ) : DWORD;
  shader_Attach         : procedure( const Attach : DWORD );
  shader_BeginLink      : procedure;
  shader_EndLink        : function : DWORD;
  shader_Begin          : procedure( const Shader : DWORD );
  shader_End            : procedure;
  shader_Free           : procedure( const Shader : DWORD );
  shader_GetUniform     : function( const Shader : DWORD; const UniformName : String ) : Integer;
  shader_SetUniform1f   : procedure( const Uniform : Integer; const v1 : Single );
  shader_SetUniform1i   : procedure( const Uniform : Integer; const v1 : Integer );
  shader_SetUniform2f   : procedure( const Uniform : Integer; const v1, v2 : Single );
  shader_SetUniform3f   : procedure( const Uniform : Integer; const v1, v2, v3 : Single );
  shader_SetUniform4f   : procedure( const Uniform : Integer; const v1, v2, v3, v4 : Single );
  shader_GetAttrib      : function( const Shader : DWORD; const AttribName : String ) : Integer;
  shader_SetAttribPf    : procedure( const Attrib : Integer; const v : Pointer; const Normalized : Boolean );
  // glVertexAttrib* GLSL/ARB
  shader_SetAttrib1f    : procedure( const Attrib : Integer; const v1 : Single );
  shader_SetAttrib2f    : procedure( const Attrib : Integer; const v1, v2 : Single );
  shader_SetAttrib3f    : procedure( const Attrib : Integer; const v1, v2, v3 : Single );
  shader_SetAttrib4f    : procedure( const Attrib : Integer; const v1, v2, v3, v4 : Single );
  shader_SetParameter4f : procedure( const ShaderType : DWORD; const Parameter : Integer; const v1, v2, v3, v4 : Single; const Local : Boolean = TRUE );}

// MATH
  m_Cos       : function( Angle : Integer ) : Single;
  m_Sin       : function( Angle : Integer ) : Single;
  m_SinCos    : procedure( const Angle : Single; var S, C : Single );
  m_Distance  : function( const x1, y1, x2, y2 : Single ) : Single;
  m_FDistance : function( const x1, y1, x2, y2 : Single ) : Single;
  m_Angle     : function( const x1, y1, x2, y2 : Single ) : Single;
{  //vectros
  vector_Get       : function( const x, y, z : Single ) : zglTPoint3D;
  vector_Add       : function( const Vector1, Vector2 : zglTPoint3D ) : zglTPoint3D;
  vector_Sub       : function( const Vector1, Vector2 : zglTPoint3D ) : zglTPoint3D;
  vector_Mul       : function( const Vector1, Vector2 : zglTPoint3D ) : zglTPoint3D;
  vector_Div       : function( const Vector1, Vector2 : zglTPoint3D ) : zglTPoint3D;
  vector_AddV      : function( const Vector : zglTPoint3D; const Value : Single ) : zglTPoint3D;
  vector_SubV      : function( const Vector : zglTPoint3D; const Value : Single ) : zglTPoint3D;
  vector_MulV      : function( const Vector : zglTPoint3D; const Value : Single ) : zglTPoint3D;
  vector_DivV      : function( const Vector : zglTPoint3D; Value : Single ) : zglTPoint3D;
  vector_MulM3f    : function( const Vector : zglTPoint3D; const Matrix : zglTMatrix3f ) : zglTPoint3D;
  vector_MulM4f    : function( const Vector : zglTPoint3D; const Matrix : zglTMatrix4f ) : zglTPoint3D;
  vector_MulInvM4f : function( const Vector : zglTPoint3D; const Matrix : zglTMatrix4f ) : zglTPoint3D;
  vector_RotateX   : function( const Vector : zglTPoint3D; const Value : Single ) : zglTPoint3D;
  vector_RotateY   : function( const Vector : zglTPoint3D; const Value : Single ) : zglTPoint3D;
  vector_RotateZ   : function( const Vector : zglTPoint3D; const Value : Single ) : zglTPoint3D;
  vector_RotateQ   : function( const Vector : zglTPoint3D; const Quaternion : zglTQuaternion ) : zglTPoint3D;
  vector_Negate    : function( const Vector : zglTPoint3D ) : zglTPoint3D;
  vector_Normalize : function( const Vector : zglTPoint3D ) : zglTPoint3D;
  vector_Angle     : function( const Vector1, Vector2 : zglTPoint3D ) : Single;
  vector_Cross     : function( const Vector1, Vector2 : zglTPoint3D ) : zglTPoint3D;
  vector_Dot       : function( const Vector1, Vector2 : zglTPoint3D ) : Single;
  vector_Distance  : function( const Vector1, Vector2 : zglTPoint3D ) : Single;
  vector_FDistance : function( const Vector1, Vector2 : zglTPoint3D ) : Single;
  vector_Length    : function( const Vector : zglTPoint3D ) : Single;
  vector_Lerp      : function( const Vector1, Vector2 : zglTPoint3D; const Value : Single ) : zglTPoint3D;
  //matrix
  matrix3f_Get            : function( const v1, v2, v3 : zglTPoint3D ) : zglTMatrix3f;
  matrix3f_OrthoNormalize : procedure( var Matrix : zglTMatrix3f );
  matrix3f_Transpose      : procedure( var Matrix : zglTMatrix3f );
  matrix3f_SetRot         : procedure( var Matrix : zglTMatrix3f; const aX, aY, aZ : Single );
  matrix3f_Add            : function( const Matrix1, Matrix2 : zglTMatrix3f ) : zglTMatrix4f;
  matrix3f_Mul            : function( const Matrix1, Matrix2 : zglTMatrix3f ) : zglTMatrix3f;
  matrix4f_Transpose      : procedure( var Matrix : zglTMatrix4f );
  matrix4f_Determinant    : function( const Matrix : zglTMatrix4f ) : Single;
  matrix4f_Inverse        : function( const Matrix : zglTMatrix4f ) : zglTMatrix4f;
  matrix4f_Translate      : procedure( var Matrix : zglTMatrix4f; const tX, tY, tZ : Single );
  matrix4f_SetPos         : procedure( var Matrix : zglTMatrix4f; const X, Y, Z : Single );
  matrix4f_SetRot         : procedure( var Matrix : zglTMatrix4f; const aX, aY, aZ : Single );
  matrix4f_Scale          : procedure( var Matrix : zglTMatrix4f; const sX, sY, sZ : Single );
  matrix4f_Mul            : function ( const Matrix1, Matrix2 : zglTMatrix4f ) : zglTMatrix4f;
  // quaternions
  quater_Get          : function( const X, Y, Z, W : Single ) : zglTQuaternion;
  quater_Add          : function( const q1, q2 : zglTQuaternion ) : zglTQuaternion;
  quater_Sub          : function( const q1, q2 : zglTQuaternion ) : zglTQuaternion;
  quater_Mul          : function( const q1, q2 : zglTQuaternion ) : zglTQuaternion;
  quater_Negate       : function( const Quaternion : zglTQuaternion ) : zglTQuaternion;
  quater_Normalize    : function( const Quaternion : zglTQuaternion ) : zglTQuaternion;
  quater_Dot          : function( const q1, q2 : zglTQuaternion ) : Single;
  quater_Lerp         : function( const q1, q2 : zglTQuaternion; Value : Single ) : zglTQuaternion;
  quater_FromRotation : function( const Rotation : zglTPoint3D ) : zglTQuaternion;
  quater_GetM4f       : function( const Quaternion : zglTQuaternion ) : zglTMatrix4f;
  // line 3d
  line3d_ClosestPoint : function( const A, B, Point : zglTPoint3D ) : zglTPoint3D;
  // plane
  plane_Get      : function( const A, B, C : zglTPoint3D ) : zglTPlane;
  plane_Distance : function( const Plane : zglTPlane; const Point : zglTPoint3D ) : Single;
  // triangle
  tri_GetNormal  : function( const A, B, C : zglTPoint3D ) : zglTPoint3D;}

// COLLISION 2D
  col2d_PointInRect   : function( const X, Y : Single; const Rect : zglTRect   ) : Boolean;
  col2d_PointInCircle : function( const X, Y : Single; const Circ : zglTCircle ) : Boolean;
  // line 2d
  col2d_Line           : function( const A, B : zglTLine; ColPoint : zglPPoint2D ) : Boolean;
  col2d_LineVsRect     : function( const A : zglTLine; const Rect : zglTRect; ColPoint : zglPPoint2D ) : Boolean;
  col2d_LineVsCircle   : function( const L : zglTLine; const Circ : zglTCircle ) : Boolean;
  col2d_LineVsCircleXY : function( const L : zglTLine; const Circ : zglTCircle; const Precision : Byte; ColPoint : zglPPoint2D ) : Boolean;
  // rect
  col2d_Rect         : function( const Rect1, Rect2 : zglTRect ) : Boolean;
  col2d_ClipRect     : function( const Rect1, Rect2 : zglTRect ) : zglTRect;
  col2d_RectInRect   : function( const Rect1, Rect2 : zglTRect ) : Boolean;
  col2d_RectInCircle : function( const Rect : zglTRect; const Circ : zglTCircle ) : Boolean;
  col2d_RectVsCircle : function( const Rect : zglTRect; const Circ : zglTCircle ) : Boolean;
  // circle
  col2d_Circle         : function( const Circ1, Circ2 : zglTCircle ) : Boolean;
  col2d_CircleInCircle : function( const Circ1, Circ2 : zglTCircle ) : Boolean;
  col2d_CircleInRect   : function( const Circ : zglTCircle; const Rect : zglTRect ) : Boolean;

{// COLLISION 3D
type
  zglPCol3DCallback = ^zglTCol3DCallback;
  zglTCol3DCallback = procedure( const Offset : zglTPoint3D; const Data : Pointer );
type
  zglPCollision3D = ^zglTCollision3D;
  zglTCollision3D = record
    Result : Boolean;
    Offset : zglTPoint3D;
end;

var
  // point 3D
  col3d_PointInTri    : function( const Point, A, B, C : zglTPoint3D  ) : Boolean;
  col3d_PointInAABB   : function( const Point : zglPPoint3D; const AABB : zglTAABB ) : Boolean;
  col3d_PointInOBB    : function( const Point : zglPPoint3D; const OBB : zglTOBB ) : Boolean;
  col3d_PointInSphere : function( const Point : zglPPoint3D; const Sphere : zglTSphere ) : Boolean;
  // line3D
  col3d_LineVsAABB   : function( const Line : zglTLine3D; const AABB : zglTAABB ) : Boolean;
  col3d_LineVsOBB    : function( const Line : zglTLine3D; const OBB : zglTOBB ) : Boolean;
  col3d_LineVsSphere : function( const Line : zglTLine3D; const Sphere : zglTSphere ) : Boolean;
  // plane 3d
  col3d_PlaneVsSphere : function( const Plane : zglTPlane; const Sphere : zglTSphere ) : zglTCollision3D;
  // aabb
  col3d_AABBVsAABB   : function( const AABB1, AABB2 : zglTAABB ) : Boolean;
  col3d_AABBVsOBB    : function( const AABB : zglTAABB; const OBB : zglTOBB ) : Boolean;
  col3d_AABBVsSphere : function( const AABB : zglTAABB; const Sphere : zglTSphere ) : Boolean;
  // obb
  col3d_OBBVsOBB    : function( const OBB1, OBB2 : zglTOBB ) : Boolean;
  col3d_OBBVsSphere : function( const OBB : zglTOBB; const Sphere : zglTSphere ) : Boolean;
  // sphere
  col3d_SphereVsSphere : function( const Sphere1, Sphere : zglTSphere ) : Boolean;
  col3d_SphereVsNode   : function( const Sphere : zglTSphere; const Octree : zglTOctree; const Node : zglTNode; const Callback : zglTCol3DCallback; const CData : Pointer ) : Boolean;}

const
  // Open Mode
  FOM_CREATE = $01; // Create
  FOM_OPENR  = $02; // Read
  FOM_OPENRW = $03; // Read&Write

  // Seek Mode
  FSM_SET    = $01;
  FSM_CUR    = $02;
  FSM_END    = $03;

var
  file_Open         : procedure( var FileHandle : zglTFile; const FileName : String; const Mode : Byte );
  file_Exists       : function( const FileName : String ) : Boolean;
  file_Seek         : function( var FileHandle : zglTFile; const Offset, Mode : DWORD ) : DWORD;
  file_GetPos       : function( var FileHandle : zglTFile ) : DWORD;
  file_Read         : function( var FileHandle : zglTFile; var buffer; const count : DWORD ) : DWORD;
  file_Write        : function( var FileHandle : zglTFile; const buffer; const count : DWORD ) : DWORD;
  file_Trunc        : procedure( var FileHandle : zglTFile; const count : DWORD );
  file_GetSize      : function( var FileHandle : zglTFile ) : DWORD;
  file_Flush        : procedure( var FileHandle : zglTFile );
  file_Close        : procedure( var FileHandle : zglTFile );
  file_Find         : procedure( const Directory : String; var List : zglTFileList; const FindDir : Boolean = FALSE );
  file_GetName      : procedure( const FileName : String; var Result : String );
  file_GetExtension : procedure( const FileName : String; var Result : String );
  file_SetPath      : procedure( const Path : String );

var
  mem_LoadFromFile : procedure( var Memory : zglTMemory; const FileName : String );
  mem_SaveToFile   : procedure( var Memory : zglTMemory; const FileName : String );
  mem_Seek         : function( var Memory : zglTMemory; const Offset, Mode : DWORD ) : DWORD;
  mem_Read         : function( var Memory : zglTMemory; var buffer; const count : DWORD ) : DWORD;
  mem_Write        : function( var Memory : zglTMemory; const buffer; const count : DWORD ) : DWORD;
  mem_SetSize      : procedure( var Memory : zglTMemory; const Size : DWORD );
  mem_Free         : procedure( var Memory : zglTMemory );

// Utils
function u_IntToStr( Value : Integer ) : String;
function u_StrToInt( Value : String ) : Integer;

{$IFDEF LINUX_OR_DARWIN}
function dlopen ( Name : PChar; Flags : longint) : Pointer; cdecl; external 'dl';
function dlclose( Lib : Pointer) : Longint; cdecl; external 'dl';
function dlsym  ( Lib : Pointer; Name : Pchar) : Pointer; cdecl; external 'dl';
{$ENDIF}

{$IFDEF WIN32}
function dlopen ( lpLibFileName : PAnsiChar) : HMODULE; stdcall; external 'kernel32.dll' name 'LoadLibraryA';
function dlclose( hLibModule : HMODULE ) : Boolean; stdcall; external 'kernel32.dll' name 'FreeLibrary';
function dlsym  ( hModule : HMODULE; lpProcName : PAnsiChar) : Pointer; stdcall; external 'kernel32.dll' name 'GetProcAddress';

function MessageBoxA( hWnd : DWORD; lpText, lpCaption : PChar; uType : DWORD) : Integer; stdcall; external 'user32.dll';
{$ENDIF}

implementation

var
  zglLib : {$IFDEF LINUX_OR_DARWIN} Pointer {$ENDIF} {$IFDEF WIN32} HMODULE {$ENDIF};
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
  Val( Value, Result, E );
end;

procedure zglFree;
Begin
  dlClose( zglLib );
End;


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
  zglLib := dlopen( PChar( LibraryName ) {$IFDEF LINUX_OR_DARWIN}, $001 {$ENDIF} );

  if zglLib <> {$IFDEF LINUX_OR_DARWIN} nil {$ENDIF} {$IFDEF WIN32} 0 {$ENDIF} Then
    begin
      Result := TRUE;
      zgl_Init := dlsym( zglLib, 'zgl_Init' );
      zgl_InitToHandle := dlsym( zglLib, 'zgl_InitToHandle' );
      zgl_Exit := dlsym( zglLib, 'zgl_Exit' );
      zgl_Reg := dlsym( zglLib, 'zgl_Reg' );
      zgl_Get := dlsym( zglLib, 'zgl_Get' );
      zgl_GetSysDir := dlsym( zglLib, 'zgl_GetSysDir' );
      zgl_GetMem := dlsym( zglLib, 'zgl_GetMem' );
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
      ini_IsKey := dlsym( zglLib, 'ini_IsKey' );
      ini_ReadKeyStr := dlsym( zglLib, 'ini_ReadKeyStr' );
      ini_ReadKeyInt := dlsym( zglLib, 'ini_ReadKeyInt' );
      ini_ReadKeyBool := dlsym( zglLib, 'ini_ReadKeyBool' );
      ini_WriteKeyStr := dlsym( zglLib, 'ini_WriteKeyStr' );
      ini_WriteKeyInt := dlsym( zglLib, 'ini_WriteKeyInt' );
      ini_WriteKeyBool := dlsym( zglLib, 'ini_WriteKeyBool' );

      timer_Add := dlsym( zglLib, 'timer_Add' );
      timer_Del := dlsym( zglLib, 'timer_Del' );
      timer_GetTicks := dlsym( zglLib, 'timer_GetTicks' );
    //  timer_Reset := dlsym( zglLib, 'timer_Reset' );

      key_Down := dlsym( zglLib, 'key_Down' );
      key_Up := dlsym( zglLib, 'key_Up' );
      key_Last := dlsym( zglLib, 'key_Last' );
      key_BeginReadText := dlsym( zglLib, 'key_BeginReadText' );
      key_EndReadText := dlsym( zglLib, 'key_EndReadText' );
      key_ClearState := dlsym( zglLib, 'key_ClearState' );

      mouse_X := dlsym( zglLib, 'mouse_X' );
      mouse_Y := dlsym( zglLib, 'mouse_Y' );
      mouse_DX := dlsym( zglLib, 'mouse_DX' );
      mouse_DY := dlsym( zglLib, 'mouse_DY' );
      mouse_Down := dlsym( zglLib, 'mouse_Down' );
      mouse_Up := dlsym( zglLib, 'mouse_Up' );
      mouse_Click := dlsym( zglLib, 'mouse_Click' );
      mouse_Wheel := dlsym( zglLib, 'mouse_Wheel' );
      mouse_ClearState := dlsym( zglLib, 'mouse_ClearState' );
      mouse_Lock := dlsym( zglLib, 'mouse_Lock' );

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

      Set2DMode := dlsym( zglLib, 'Set2DMode' );
      Set3DMode := dlsym( zglLib, 'Set3DMode' );

      zbuffer_SetDepth := dlsym( zglLib, 'zbuffer_SetDepth' );
      zbuffer_Clear := dlsym( zglLib, 'zbuffer_Clear' );

      scissor_Begin := dlsym( zglLib, 'scissor_Begin' );
      scissor_End := dlsym( zglLib, 'scissor_End' );

      rtarget_Add := dlsym( zglLib, 'rtarget_Add' );
      rtarget_Del := dlsym( zglLib, 'rtarget_Del' );
      rtarget_Set := dlsym( zglLib, 'rtarget_Set' );

      fx_SetBlendMode := dlsym( zglLib, 'fx_SetBlendMode' );
      fx2d_SetColorMix := dlsym( zglLib, 'fx2d_SetColorMix' );
      fx2d_SetVCA := dlsym( zglLib, 'fx2d_SetVCA' );
      fx2d_SetVertexes := dlsym( zglLib, 'fx2d_SetVertexes' );
      fx2d_SetScale := dlsym( zglLib, 'fx2d_SetScale' );

      cam2d_Set := dlsym( zglLib, 'cam2d_Set' );

      pr2d_Pixel := dlsym( zglLib, 'pr2d_Pixel' );
      pr2d_Line := dlsym( zglLib, 'pr2d_Line' );
      pr2d_Rect := dlsym( zglLib, 'pr2d_Rect' );
      pr2d_Circle := dlsym( zglLib, 'pr2d_Circle' );
      pr2d_Ellipse := dlsym( zglLib, 'pr2d_Ellipse' );

      ssprite2d_Draw := dlsym( zglLib, 'ssprite2d_Draw' );
      asprite2d_Draw := dlsym( zglLib, 'asprite2d_Draw' );
      csprite2d_Draw := dlsym( zglLib, 'csprite2d_Draw' );

      font_Add := dlsym( zglLib, 'font_Add' );
      font_Del := dlsym( zglLib, 'font_Del' );
      font_LoadFromFile := dlsym( zglLib, 'font_LoadFromFile' );
      font_LoadFromMemory := dlsym( zglLib, 'font_LoadFromMemory' );
      text_Draw := dlsym( zglLib, 'text_Draw' );
      text_DrawEx := dlsym( zglLib, 'text_Draw' );
      text_DrawInRect := dlsym( zglLib, 'text_DrawInRect' );
      text_GetWidth := dlsym( zglLib, 'text_GetWidth' );

      gui_Init := dlsym( zglLib, 'gui_Init' );
      gui_Draw := dlsym( zglLib, 'gui_Draw' );
      gui_Proc := dlsym( zglLib, 'gui_Proc' );
      gui_AddWidget := dlsym( zglLib, 'gui_AddWidget' );
      gui_DelWidget := dlsym( zglLib, 'gui_DelWidget' );

      snd_Init := dlsym( zglLib, 'snd_Init' );
      snd_Free := dlsym( zglLib, 'snd_Free' );
      snd_Add  := dlsym( zglLib, 'snd_Add' );
      snd_Del  := dlsym( zglLib, 'snd_Del' );
      snd_LoadFromFile := dlsym( zglLib, 'snd_LoadFromFile' );
      snd_LoadFromMemory := dlsym( zglLib, 'snd_LoadFromMemory' );
      snd_Play := dlsym( zglLib, 'snd_Play' );
      snd_Stop := dlsym( zglLib, 'snd_Stop' );
      snd_SetVolume := dlsym( zglLib, 'snd_SetVolume' );
      snd_SetFrequency := dlsym( zglLib, 'snd_SetFrequency' );
      snd_SetFrequencyCoeff := dlsym( zglLib, 'snd_SetFrequencyCoeff' );
      snd_PlayFile := dlsym( zglLib, 'snd_PlayFile' );
      snd_StopFile := dlsym( zglLib, 'snd_StopFile' );
      snd_ResumeFile := dlsym( zglLib, 'snd_ResumeFile' );

      {obj3d_Begin := dlsym( zglLib, 'obj3d_Begin' );
      obj3d_End := dlsym( zglLib, 'obj3d_End' );
      obj3d_Enable := dlsym( zglLib, 'obj3d_Enable' );
      obj3d_Disable := dlsym( zglLib, 'obj3d_Disable' );
      obj3d_SetColor := dlsym( zglLib, 'obj3d_SetColor' );
      obj3d_BindTexture := dlsym( zglLib, 'obj3d_BindTexture' );
      obj3d_SetMaterial := dlsym( zglLib, 'obj3d_SetMaterial' );
      obj3d_Rotate := dlsym( zglLib, 'obj3d_Rotate' );
      obj3d_Scale := dlsym( zglLib, 'obj3d_Scale' );
      obj3d_Move := dlsym( zglLib, 'obj3d_Move' );
      obj3d_SetMatrix := dlsym( zglLib, 'obj3d_SetMatrix' );
      obj3d_MulMatrix := dlsym( zglLib, 'obj3d_MulMatrix' );

      pr3d_Point := dlsym( zglLib, 'pr3d_Point' );
      pr3d_Line := dlsym( zglLib, 'pr3d_Line' );
      pr3d_Plane := dlsym( zglLib, 'pr3d_Plane' );
      pr3d_AABB := dlsym( zglLib, 'pr3d_AABB' );
      pr3d_Sphere := dlsym( zglLib, 'pr3d_Sphere' );

      ssprite3d_Draw := dlsym( zglLib, 'ssprite3d_Draw' );
      asprite3d_Draw := dlsym( zglLib, 'asprite3d_Draw' );

      cam3d_Set := dlsym( zglLib, 'cam3d_Set' );
      cam3d_Fly := dlsym( zglLib, 'cam3d_Fly' );
      cam3d_Strafe := dlsym( zglLib, 'cam3d_Strafe' );

      smesh_LoadFromFile := dlsym( zglLib, 'smesh_LoadFromFile' );
      smesh_LoadFromMemory := dlsym( zglLib, 'smesh_LoadFromMemory' );
      smesh_Animate := dlsym( zglLib, 'smesh_Animate' );
      smesh_Draw := dlsym( zglLib, 'smesh_Draw' );
      smesh_DrawGroup := dlsym( zglLib, 'smesh_DrawGroup' );
      smesh_Free := dlsym( zglLib, 'smesh_Free' );

      skmesh_LoadFromFile := dlsym( zglLib, 'skmesh_LoadFromFile' );
      skmesh_LoadFromMemory := dlsym( zglLib, 'skmesh_LoadFromMemory' );
      skmesh_Animate := dlsym( zglLib, 'skmesh_Animate' );
      skmesh_Draw := dlsym( zglLib, 'skmesh_Draw' );
      skmesh_DrawGroup := dlsym( zglLib, 'skmesh_DrawGroup' );
      skmesh_DrawSkelet := dlsym( zglLib, 'skmesh_DrawSkelet' );
      skmesh_Free := dlsym( zglLib, 'skmesh_Free' );

      heightmap_Build := dlsym( zglLib, 'heightmap_Build' );
      heightmap_Draw := dlsym( zglLib, 'heightmap_Draw' );
      heightmap_Free := dlsym( zglLib, 'heightmap_Free' );
      heightmap_GetPlane := dlsym( zglLib, 'heightmap_GetPlane' );
      heightmap_GetYOffset := dlsym( zglLib, 'heightmap_GetYOffset' );

      vbo_Build := dlsym( zglLib, 'vbo_Build' );
      vbo_Free := dlsym( zglLib, 'vbo_Free' );}

      frustum_Calc := dlsym( zglLib, 'frustum_Calc' );
      frustum_PointIn := dlsym( zglLib, 'frustum_PointIn' );
      frustum_PPointIn := dlsym( zglLib, 'frustum_PPointIn' );
      frustum_TriangleIn := dlsym( zglLib, 'frustum_TriangleIn' );
      frustum_SphereIn := dlsym( zglLib, 'frustum_SphereIn' );
      frustum_BoxIn := dlsym( zglLib, 'frustum_BoxIn' );
      frustum_CubeIn := dlsym( zglLib, 'frustum_CubeIn' );

      {octree_Build := dlsym( zglLib, 'octree_Build' );
      octree_Free := dlsym( zglLib, 'octree_Free' );
      octree_Draw := dlsym( zglLib, 'octree_Draw' );
      octree_DrawDebug := dlsym( zglLib, 'octree_DrawDebug' );
      octree_DrawNode := dlsym( zglLib, 'octree_DrawNode' );
    //  octree_DrawDFaces := dlsym( zglLib, 'octree_DrawDFaces' );

      light_Enable := dlsym( zglLib, 'light_Enable' );
      light_Disable := dlsym( zglLib, 'light_Disable' );
      light_SetPosition := dlsym( zglLib, 'light_SetPosition' );
      light_SetMaterial := dlsym( zglLib, 'light_SetMaterial' );

      fog_Enable := dlsym( zglLib, 'fog_Enable' );
      fog_Disable := dlsym( zglLib, 'fog_Disable' );
      fog_SetMode := dlsym( zglLib, 'fog_SetMode' );
      fog_SetColor := dlsym( zglLib, 'fog_SetColor' );
      fog_SetDensity := dlsym( zglLib, 'fog_SetDensity' );
      fog_SetBeginEnd := dlsym( zglLib, 'fog_SetBeginEnd' );

      skybox_Init := dlsym( zglLib, 'skybox_Init' );
      skybox_Draw := dlsym( zglLib, 'skybox_Draw' );

      shadow_InitVolume := dlsym( zglLib, 'shadow_InitVolume' );
      shadow_CalcVolume := dlsym( zglLib, 'shadow_CalcVolume' );
      shadow_DrawVolume := dlsym( zglLib, 'shadow_DrawVolume' );
      shadow_DrawShadowVolumes := dlsym( zglLib, 'shadow_DrawShadowVolumes' );

      shader_InitARB := dlsym( zglLib, 'shader_InitARB' );
      shader_LoadFromFileARB := dlsym( zglLib, 'shader_LoadFromFileARB' );
      shader_BeginARB := dlsym( zglLib, 'shader_BeginARB' );
      shader_EndARB := dlsym( zglLib, 'shader_EndARB' );
      shader_FreeARB := dlsym( zglLib, 'shader_FreeARB' );
      shader_InitGLSL := dlsym( zglLib, 'shader_InitGLSL' );
      shader_LoadFromFile := dlsym( zglLib, 'shader_LoadFromFile' );
      shader_Attach := dlsym( zglLib, 'shader_Attach' );
      shader_BeginLink := dlsym( zglLib, 'shader_BeginLink' );
      shader_EndLink := dlsym( zglLib, 'shader_EndLink' );
      shader_Begin := dlsym( zglLib, 'shader_Begin' );
      shader_End := dlsym( zglLib, 'shader_End' );
      shader_Free := dlsym( zglLib, 'shader_Free' );
      shader_GetUniform := dlsym( zglLib, 'shader_GetUniform' );
      shader_SetUniform1f := dlsym( zglLib, 'shader_SetUniform1f' );
      shader_SetUniform1i := dlsym( zglLib, 'shader_SetUniform1i' );
      shader_SetUniform2f := dlsym( zglLib, 'shader_SetUniform2f' );
      shader_SetUniform3f := dlsym( zglLib, 'shader_SetUniform3f' );
      shader_SetUniform4f := dlsym( zglLib, 'shader_SetUniform4f' );
      shader_GetAttrib := dlsym( zglLib, 'shader_GetAttrib' );
      shader_SetAttrib1f := dlsym( zglLib, 'shader_SetAttrib1f' );
      shader_SetAttrib2f := dlsym( zglLib, 'shader_SetAttrib2f' );
      shader_SetAttrib3f := dlsym( zglLib, 'shader_SetAttrib3f' );
      shader_SetAttrib4f := dlsym( zglLib, 'shader_SetAttrib4f' );
      shader_SetAttribPf := dlsym( zglLib, 'shader_SetAttribPf' );
      shader_SetParameter4f := dlsym( zglLib, 'shader_SetParameter4f' );}

      m_Cos := dlsym( zglLib, 'm_Cos' );
      m_Sin := dlsym( zglLib, 'm_Sin' );
      m_Distance := dlsym( zglLib, 'm_Distance' );
      m_FDistance := dlsym( zglLib, 'm_FDistance' );
      m_Angle := dlsym( zglLib, 'm_Angle' );

      {vector_Get := dlsym( zglLib, 'vector_Get' );
      vector_Add := dlsym( zglLib, 'vector_Add' );
      vector_Sub := dlsym( zglLib, 'vector_Sub' );
      vector_Mul := dlsym( zglLib, 'vector_Mul' );
      vector_Div := dlsym( zglLib, 'vector_Div' );
      vector_AddV := dlsym( zglLib, 'vector_AddV' );
      vector_SubV := dlsym( zglLib, 'vector_SubV' );
      vector_MulV := dlsym( zglLib, 'vector_MulV' );
      vector_DivV := dlsym( zglLib, 'vector_DivV' );
      vector_MulM3f := dlsym( zglLib, 'vector_MulM3f' );
      vector_MulM4f := dlsym( zglLib, 'vector_MulM4f' );
      vector_MulInvM4f := dlsym( zglLib, 'vector_MulInvM4f' );
      vector_RotateX := dlsym( zglLib, 'vector_RotateX' );
      vector_RotateY := dlsym( zglLib, 'vector_RotateY' );
      vector_RotateZ := dlsym( zglLib, 'vector_RotateZ' );
      vector_RotateQ := dlsym( zglLib, 'vector_RotateQ' );
      vector_Negate := dlsym( zglLib, 'vector_Negate' );
      vector_Normalize := dlsym( zglLib, 'vector_Normalize' );
      vector_Angle := dlsym( zglLib, 'vector_Angle' );
      vector_Cross := dlsym( zglLib, 'vector_Cross' );
      vector_Dot := dlsym( zglLib, 'vector_Dot' );
      vector_Distance := dlsym( zglLib, 'vector_Distance' );
      vector_FDistance := dlsym( zglLib, 'vector_FDistance' );
      vector_Length := dlsym( zglLib, 'vector_Length' );
      vector_Lerp := dlsym( zglLib, 'vector_Lerp' );

      matrix3f_Get := dlsym( zglLib, 'matrix3f_Get' );
      matrix3f_OrthoNormalize := dlsym( zglLib, 'matrix3f_OrthoNormalize' );
      matrix3f_Transpose := dlsym( zglLib, 'matrix3f_Transpose' );
      matrix3f_SetRot := dlsym( zglLib, 'matrix3f_SetRot' );
      matrix3f_Add := dlsym( zglLib, 'matrix3f_Add' );
      matrix3f_Mul := dlsym( zglLib, 'matrix3f_Mul' );

      matrix4f_Transpose := dlsym( zglLib, 'matrix4f_Transpose' );
      matrix4f_Determinant := dlsym( zglLib, 'matrix4f_Determinant' );
      matrix4f_Inverse := dlsym( zglLib, 'matrix4f_Inverse' );
      matrix4f_Translate := dlsym( zglLib, 'matrix4f_Translate' );
      matrix4f_SetPos := dlsym( zglLib, 'matrix4f_SetPos' );
      matrix4f_SetRot := dlsym( zglLib, 'matrix4f_SetRot' );
      matrix4f_Scale := dlsym( zglLib, 'matrix4f_Scale' );
      matrix4f_Mul := dlsym( zglLib, 'matrix4f_Mul' );

      quater_Get := dlsym( zglLib, 'quater_Get' );
      quater_Add := dlsym( zglLib, 'quater_Add' );
      quater_Sub := dlsym( zglLib, 'quater_Sub' );
      quater_Mul := dlsym( zglLib, 'quater_Mul' );
      quater_Negate := dlsym( zglLib, 'quater_Negate' );
      quater_Normalize := dlsym( zglLib, 'quater_Normalize' );
      quater_Dot := dlsym( zglLib, 'quater_Dot' );
      quater_Lerp := dlsym( zglLib, 'quater_Lerp' );
      quater_FromRotation := dlsym( zglLib, 'quater_FromRotation' );
      quater_GetM4f := dlsym( zglLib, 'quater_GetM4f' );

      line3d_ClosestPoint := dlsym( zglLib, 'line3d_ClosestPoint' );

      plane_Get := dlsym( zglLib, 'plane_Get' );
      plane_Distance := dlsym( zglLib, 'plane_Distance' );

      tri_GetNormal := dlsym( zglLib, 'tri_GetNormal' );}

      col2d_PointInRect := dlsym( zglLib, 'col2d_PointInRect' );
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

      {col3d_PointInTri := dlsym( zglLib, 'col3d_PointInTri' );
      col3d_PointInAABB := dlsym( zglLib, 'col3d_PointInAABB' );
      col3d_PointInOBB := dlsym( zglLib, 'col3d_PointInOBB' );
      col3d_PointInSphere := dlsym( zglLib, 'col3d_PointInSphere' );
      col3d_LineVsAABB := dlsym( zglLib, 'col3d_LineVsAABB' );
      col3d_LineVsOBB := dlsym( zglLib, 'col3d_LineVsOBB' );
      col3d_LineVsSphere := dlsym( zglLib, 'col3d_LineVsSphere' );
      col3d_PlaneVsSphere := dlsym( zglLib, 'col3d_PlaneVsSphere' );
      col3d_AABBVsAABB := dlsym( zglLib, 'col3d_AABBVsAABB' );
      col3d_AABBVsOBB := dlsym( zglLib, 'col3d_AABBVsOBB' );
      col3d_AABBVsSphere := dlsym( zglLib, 'col3d_AABBVsSphere' );
      col3d_OBBVsOBB := dlsym( zglLib, 'col3d_OBBVsOBB' );
      col3d_OBBVsSphere := dlsym( zglLib, 'col3d_OBBVsSphere' );
      col3d_SphereVsSphere := dlsym( zglLib, 'col3d_SphereVsSphere' );
      col3d_SphereVsNode := dlsym( zglLib, 'col3d_SphereVsNode' );}

      file_Open := dlsym( zglLib, 'file_Open' );
      file_Exists := dlsym( zglLib, 'file_Exists' );
      file_Seek := dlsym( zglLib, 'file_Seek' );
      file_GetPos := dlsym( zglLib, 'file_GetPos' );
      file_Read := dlsym( zglLib, 'file_Read' );
      file_Write := dlsym( zglLib, 'file_Write' );
      file_Trunc := dlsym( zglLib, 'file_Trunc' );
      file_GetSize := dlsym( zglLib, 'file_GetSize' );
      file_Flush := dlsym( zglLib, 'file_Flush' );
      file_Close := dlsym( zglLib, 'file_Close' );
      file_Find := dlsym( zglLib, 'file_Find' );
      file_GetName := dlsym( zglLib, 'file_GetName' );
      file_GetExtension := dlsym( zglLib, 'file_GetExtension' );
      file_SetPath := dlsym( zglLib, 'file_SetPath' );

      mem_LoadFromFile := dlsym( zglLib, 'mem_LoadFromFile' );
      mem_SaveToFile := dlsym( zglLib, 'mem_SaveToFile' );
      mem_Seek := dlsym( zglLib, 'mem_Seek' );
      mem_Read := dlsym( zglLib, 'mem_Read' );
      mem_Write := dlsym( zglLib, 'mem_Write' );
      mem_SetSize := dlsym( zglLib, 'mem_SetSize' );
      mem_Free := dlsym( zglLib, 'mem_Free' );
    end else
      if Error Then
        begin
          Result := FALSE;
          {$IFDEF LINUX}
          WriteLn( 'Error while loading ZenGL' );
          {$ENDIF}
          {$IFDEF WIN32}
          MessageBoxA( 0, 'Error while loading ZenGL', 'Error', $00000010 );
          {$ENDIF}
          {$IFDEF DARWIN}
          StandardAlert( kAlertNoteAlert, 'Error', 'Error while loading ZenGL', nil, outItemHit );
          {$ENDIF}
        end;
end;

end.
