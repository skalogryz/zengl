{-------------------------------}
{-----------= ZenGL =-----------}
{-------------------------------}
{ version: 0.1.31               }
{ date:    03.07.09             }
{-------------------------------}
{ by:   Andru ( Kemka Andrey )  }
{ mail: dr.andru@gmail.com      }
{ ICQ:  496-929-849             }
{ JID:  dr.andru@jabber.kiev.ua }
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
  WIDGET_DESC_FILL       = $000031;
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
  APP_USE_UTF8          = $000200;
  WND_USE_AUTOCENTER    = $000400;
  SND_CAN_PLAY          = $000800;
  SND_CAN_PLAY_FILE     = $001000;
  CROP_INVISIBLE        = $002000;

var
  zgl_Enable  : procedure( const What : DWORD );
  zgl_Disable : procedure( const What : DWORD );

// LOG
  log_Add : procedure( const Message : String; const Timings : Boolean = TRUE );

// WINDOW
  wnd_SetCaption : procedure( const NewCaption : String );
  wnd_SetSize    : procedure( const Width, Height : Integer );
  wnd_SetPos     : procedure( const X, Y : Integer );
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
  scissor_Begin : procedure( X, Y, Width, Height : Integer );
  scissor_End   : procedure;

// INI
  ini_LoadFromFile : procedure( const FileName : String );
  ini_SaveToFile   : procedure( const FileName : String );
  ini_Add          : procedure( const Section, Key : String );
  ini_Del          : procedure( const Section, Key : String );
  ini_Clear        : procedure( const Section : String );
  ini_IsSection    : function( const Section : String ) : Boolean;
  ini_IsKey        : function( const Section, Key : String ) : Boolean;
  ini_ReadKeyStr   : procedure( const Section, Key : String; var Result : String );
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
  timer_Del      : procedure( var Timer : zglPTimer );
  timer_GetTicks : function : Double;
  timer_Reset    : procedure;

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
  key_Press         : function( const KeyCode : Byte ) : Boolean;
  key_Last          : function( const KeyAction : Byte ) : Byte;
  key_BeginReadText : procedure( const Text : String; const MaxSymbols : Integer = -1 );
  key_EndReadText   : procedure( var Result : String );
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
  Set3DMode : procedure( FOVY : Single = 45 );

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

  TEX_FILTER_NEAREST    = $000080;
  TEX_FILTER_LINEAR     = $000100;
  TEX_FILTER_BILINEAR   = $000200;
  TEX_FILTER_TRILINEAR  = $000400;
  TEX_FILTER_ANISOTROPY = $000800;

  TEX_RGB               = $001000;

  TEX_QUALITY_LOW       = $400000;
  TEX_QUALITY_MEDIUM    = $800000;

  TEX_DEFAULT_2D        = TEX_CLAMP or TEX_CONVERT_TO_POT or TEX_FILTER_LINEAR;

var
  tex_Add            : function : zglPTexture;
  tex_Del            : procedure( var Texture : zglPTexture );
  tex_Create         : procedure( var Texture : zglTTexture; var pData : Pointer );
  tex_CreateZero     : function( const Width, Height : WORD; const Color, Flags : DWORD ) : zglPTexture;
  tex_LoadFromFile   : function( const FileName : String; const TransparentColor, Flags : DWORD ) : zglPTexture;
  tex_LoadFromMemory : function( const Memory : zglTMemory; const Extension : String; const TransparentColor, Flags : DWORD ) : zglPTexture;
  tex_SetFrameSize   : procedure( var Texture : zglPTexture; FrameWidth, FrameHeight : WORD );
  tex_SetMask        : function( var Texture : zglPTexture; const Mask : zglPTexture ) : zglPTexture;
  tex_GetData        : procedure( const Texture : zglPTexture; var pData : Pointer; var pSize : Integer );
  tex_Filter         : procedure( Texture : zglPTexture; const Flags : DWORD );
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
  rtarget_Del : procedure( var Target : zglPRenderTarget );
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

// Render 2D
  batch2d_Begin : procedure;
  batch2d_End   : procedure;
  batch2d_Flush : procedure;

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

const
  TEXT_HALIGN_LEFT    = $000001;
  TEXT_HALIGN_CENTER  = $000002;
  TEXT_HALIGN_RIGHT   = $000004;
  TEXT_HALIGN_JUSTIFY = $000008;
  TEXT_VALIGN_TOP     = $000010;
  TEXT_VALIGN_CENTER  = $000020;
  TEXT_VALIGN_BOTTOM  = $000040;

var
  font_Add            : function : zglPFont;
  font_Del            : procedure( var Font : zglPFont );
  font_LoadFromFile   : function( const FileName : String ) : zglPFont;
  font_LoadFromMemory : function( const Memory : zglTMemory ) : zglPFont;
  text_Draw           : procedure( const Font : zglPFont; X, Y : Single; const Text : String; const Flags : DWORD = 0 );
  text_DrawEx         : procedure( const Font : zglPFont; X, Y, Scale, Step : Single; const Text : String; const Alpha : Byte = 255; const Color : DWORD = $FFFFFF; const Flags : DWORD = 0 );
  text_DrawInRect     : procedure( const Font : zglPFont; const Rect : zglTRect; const Text : String; const Flags : DWORD = 0 );
  text_DrawInRectEx   : procedure( const Font : zglPFont; const Rect : zglTRect; const Scale, Step : Single; const Text : String; const Alpha : Byte = 0; const Color : DWORD = $FFFFFF; const Flags : DWORD = 0 );
  text_GetWidth       : function( const Font : zglPFont; const Text : String; const Step : Single = 0.0 ) : Single;

// GUI
const
  WIDGET_UNKNOWN     = 0;
  WIDGET_BUTTON      = 1;
  WIDGET_CHECKBOX    = 2;
  WIDGET_RADIOBUTTON = 3;
  WIDGET_LABEL       = 4;
  WIDGET_EDITBOX     = 5;
  WIDGET_LISTBOX     = 6;
  WIDGET_COMBOBOX    = 7;
  WIDGET_GROUPBOX    = 8;
  WIDGET_SPIN        = 9;
  WIDGET_SCROLLBAR   = 10;

  // ScrollBar
  SCROLLBAR_VERTICAL   = 0;
  SCROLLBAR_HORIZONTAL = 1;

  // Select Mode
  SELECT_BY_CLICK = 0;
  SELECT_BY_DOWN  = 1;

  // Align
  ALIGN_NONE    = 0;
  ALIGN_CLIENT  = 1;
  ALIGN_LEFT    = 2;
  ALIGN_RIGHT   = 3;
  ALIGN_TOP     = 4;
  ALIGN_BOTTOM  = 5;

  // Events
  EVENT_CREATE      = 1;

  EVENT_FOCUS_IN    = 2;
  EVENT_FOCUS_OUT   = 3;

  EVENT_DRAG_START  = 4;
  EVENT_DRAG_MOVE   = 5;
  EVENT_DRAG_END    = 6;

  EVENT_MOUSE_MOVE  = 7;
  EVENT_MOUSE_ENTER = 8;
  EVENT_MOUSE_LEAVE = 9;
  EVENT_MOUSE_DOWN  = 10;
  EVENT_MOUSE_UP    = 11;
  EVENT_MOUSE_CLICK = 12;
  EVENT_MOUSE_WHEEL = 13;

  EVENT_KEY_DOWN    = 14;
  EVENT_KEY_UP      = 15;
  EVENT_KEY_CHAR    = 16;

type
  zglPEvent  = ^zglTEvent;
  zglPWidget = ^zglTWidget;

  //Events
  zglTEvents = record
    OnFocus      : procedure( Widget : zglPWidget; const Focus : Boolean );
    OnStartDrag  : procedure( Widget : zglPWidget );
    OnDrag       : procedure( Widget : zglPWidget; const X, Y : Single );
    OnEndDrag    : procedure( Widget : zglPWidget; const X, Y : Single );
    OnClick      : procedure( Widget : zglPWidget );
    OnMouseUp    : procedure( Widget : zglPWidget );
    OnMouseMove  : procedure( Widget : zglPWidget; const X, Y : Single );
    OnMouseEnter : procedure( Widget : zglPWidget );
    OnMouseLeave : procedure( Widget : zglPWidget );
    OnMouseWheel : procedure( Widget : zglPWidget; const Axis : Byte );
    OnKeyDown    : procedure( Widget : zglPWidget; const KeyCode : Byte );
    OnKeyUp      : procedure( Widget : zglPWidget; const KeyCode : Byte );
    OnChange     : procedure( Widget : zglPWidget; const Value, Change : Integer );
end;

  //Widget
  zglTWidget = record
    _type      : Integer;
    desc       : Pointer;
    data       : Pointer;
    rect       : zglTRect;
    client     : zglTRect;
    align      : DWORD;
    focus      : Boolean;
    visible    : Boolean;
    mousein    : Boolean;
    draged     : Boolean;

    OnDraw     : procedure( const Widget : zglPWidget );
    OnProc     : procedure( const Event  : zglPEvent );
    Events     : zglTEvents;

    parent     : zglPWidget;
    child      : zglPWidget;
    Next, Prev : zglPWidget;
end;

  zglTWidgetType = record
    _type    : Integer;
    FillDesc : procedure( Src : Pointer; var Desc : Pointer );

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
      3: ( mouse_wheel  : Byte );
      4: ( key_code     : Byte );
      5: ( key_char     : PChar );
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

    Max      : Integer;
    ReadOnly : Boolean;
end;

  zglPListBoxDesc = ^zglTListBoxDesc;
  zglTListBoxDesc = record
    Font       : zglPFont;
    List       : zglTStringList;
    ItemIndex  : Integer;
    ItemHeight : Integer;
    SelectMode : Integer;
end;

  zglPComboBoxDesc = ^zglTComboBoxDesc;
  zglTComboBoxDesc = record
    Font          : zglPFont;
    List          : zglTStringList;
    ItemIndex     : Integer;
    ItemHeight    : Integer;
    DropDownCount : Integer;

    DropedDown    : Boolean;
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
    SDraged  : Boolean;
end;

var
  gui_Init      : procedure;
  gui_Draw      : procedure;
  gui_Proc      : procedure;
  gui_AddWidget : function( const _type : Integer; const X, Y, W, H : Single; const Focus, Visible : Boolean; const Desc, Data : Pointer; const Parent : zglPWidget ) : zglPWidget;
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
    CodecClose : procedure;
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
  snd_Del               : procedure( var Sound : zglPSound );
  snd_LoadFromFile      : function( const FileName : String; const SourceCount : Integer = 16 ) : zglPSound;
  snd_LoadFromMemory    : function( const Memory : zglTMemory; Extension : String; const SourceCount : Integer ) : zglPSound;
  snd_Play              : function( const Sound : zglPSound; const Loop : Boolean = FALSE; const X : Single = 0; const Y : Single = 0; const Z : Single = 0) : Integer;
  snd_Stop              : procedure( const Sound : zglPSound; const Source : Integer );
  snd_SetVolume         : procedure( const Sound : zglPSound; const Volume : Single; const ID : Integer );
  snd_SetFrequency      : procedure( const Sound : zglPSound; const Frequency, ID : Integer );
  snd_SetFrequencyCoeff : procedure( const Sound : zglPSound; const Coefficient : Single; const ID : Integer );
  snd_PlayFile          : procedure( const FileName : String; const Loop : Boolean );
  snd_StopFile          : procedure;
  snd_ResumeFile        : procedure;

// MATH
  m_Cos       : function( Angle : Integer ) : Single;
  m_Sin       : function( Angle : Integer ) : Single;
  m_Distance  : function( const x1, y1, x2, y2 : Single ) : Single;
  m_FDistance : function( const x1, y1, x2, y2 : Single ) : Single;
  m_Angle     : function( const x1, y1, x2, y2 : Single ) : Single;

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
  file_Open         : procedure( const FileHandle : zglTFile; const FileName : String; const Mode : Byte );
  file_Exists       : function( const FileName : String ) : Boolean;
  file_Seek         : function( const FileHandle : zglTFile; const Offset, Mode : DWORD ) : DWORD;
  file_GetPos       : function( const FileHandle : zglTFile ) : DWORD;
  file_Read         : function( const FileHandle : zglTFile; var buffer; const count : DWORD ) : DWORD;
  file_Write        : function( const FileHandle : zglTFile; const buffer; const count : DWORD ) : DWORD;
  file_Trunc        : procedure( const FileHandle : zglTFile; const count : DWORD );
  file_GetSize      : function( const FileHandle : zglTFile ) : DWORD;
  file_Flush        : procedure( const FileHandle : zglTFile );
  file_Close        : procedure( const FileHandle : zglTFile );
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
var
  u_SortList : procedure( var List : zglTStringList; iLo, iHi : Integer );

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
      ini_Del := dlsym( zglLib, 'ini_Del' );
      ini_Clear := dlsym( zglLib, 'ini_Clear' );
      ini_IsSection := dlsym( zglLib, 'ini_IsSection' );
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
      timer_Reset := dlsym( zglLib, 'timer_Reset' );

      key_Down := dlsym( zglLib, 'key_Down' );
      key_Up := dlsym( zglLib, 'key_Up' );
      key_Press := dlsym( zglLib, 'key_Press' );
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

      batch2d_Begin := dlsym( zglLib, 'batch2d_Begin' );
      batch2d_End := dlsym( zglLib, 'batch2d_End' );
      batch2d_Flush := dlsym( zglLib, 'batch2d_Flush' );

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
      text_DrawEx := dlsym( zglLib, 'text_DrawEx' );
      text_DrawInRect := dlsym( zglLib, 'text_DrawInRect' );
      text_DrawInRectEx := dlsym( zglLib, 'text_DrawInRectEx' );
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

      m_Cos := dlsym( zglLib, 'm_Cos' );
      m_Sin := dlsym( zglLib, 'm_Sin' );
      m_Distance := dlsym( zglLib, 'm_Distance' );
      m_FDistance := dlsym( zglLib, 'm_FDistance' );
      m_Angle := dlsym( zglLib, 'm_Angle' );

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

      u_SortList := dlsym( zglLib, 'u_SortList' );
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
