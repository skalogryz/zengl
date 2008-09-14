{-------------------------------}
{-----------= ZenGL =-----------}
{-------------------------------}
{ build: 26                     }
{ date:  12.09.08               }
{-------------------------------}
{ by:   Andru ( Kemka Andrey )  }
{ mail: dr.andru@gmail.com      }
{ ICQ:  496-929-849             }
{ site: http://andru.2x4.ru     }
{-------------------------------}
{                      (C) 2008 }
{-------------------------------}
unit zglHeader;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$MACRO ON}
  {$IFDEF LINUX}
    {$DEFINE stdcall := cdecl}
  {$ENDIF}
{$ENDIF}

interface

type
  DWORD   = LongWord;
  Ptr     = {$IFDEF CPU64}QWORD{$ELSE}DWORD{$ENDIF};
  PPtr    = ^Ptr;
  {$IFDEF WIN32}
  HANDLE  = DWORD;
  HDC     = DWORD;
  HGLRC   = DWORD;
  {$ENDIF}

const
{$IFDEF LINUX}
  libZenGL = 'libZenGL.so';
{$ENDIF}
{$IFDEF WIN32}
  libZenGL = 'ZenGL.dll';
{$ENDIF}

function zglLoad( LibraryName : String; Error : Boolean = TRUE ) : Boolean;
procedure zglFree;

var
  zgl_Init : procedure( FSAA : Byte = 0; StencilBits : Byte = 0 ); stdcall;
  zgl_Exit : procedure; stdcall;

const
  SYS_LOAD             = $000001;
  SYS_DRAW             = $000002;
  SYS_EXIT             = $000003;
  TEX_FORMAT_EXTENSION = $000010;
  TEX_FORMAT_LOADER    = $000011;
  SND_FORMAT_EXTENSION = $000020;
  SND_FORMAT_LOADER    = $000021;

var
  zgl_Reg : procedure( What : WORD; UserData : Pointer ); stdcall;
  
const
  SYS_FPS         = 1;  // DWORD,  := zgl_Get( SYS_FPS )
  LOG_FILENAME    = 2;  // PPChar, := Pointer( zgl_Get( LOG_FILENAME ) )
  ZGL_VERSION     = 3;  // DWORD
  SCR_ADD_X       = 4;  // DWORD
  SCR_ADD_Y       = 5;  // DWORD
  DESKTOP_WIDTH   = 6;  // DWORD
  DESKTOP_HEIGHT  = 7;  // DWORD
  RESOLUTION_LIST = 8;  // PResolutionList
  MANAGER_TIMER   = 9;  // zglPTimerManager
  MANAGER_TEXTURE = 10; // zglPTextureManager
  MANAGER_FONT    = 11; // zglPFontManager
  MANAGER_RTARGET = 12; // zglTRenderTargetManager
  MANAGER_SOUND   = 13; // zglPSoundManager

var
  zgl_Get    : function( What : DWORD ) : Ptr; stdcall;
  zgl_GetMem : procedure( var Mem : Pointer; Size : DWORD ); stdcall;

const
  COLOR_BUFFER_CLEAR   = $000001;
  DEPTH_BUFFER         = $000002;
  DEPTH_BUFFER_CLEAR   = $000004;
  DEPTH_MASK           = $000008;
  CORRECT_RESOLUTION   = $000010;
  APP_USE_AUTOPAUSE    = $000020;
  APP_USE_AUTOMINIMIZE = $000040;
  SND_CAN_PLAY         = $000080;
  SND_CAN_PLAY_FILE    = $000100;

var
  zgl_Enable  : procedure( What : DWORD ); stdcall;
  zgl_Disable : procedure( What : DWORD ); stdcall;

// LOG
  log_Add : procedure( Message : String; Timings : Boolean = TRUE ); stdcall;
  
// WINDOW
  wnd_SetCaption : procedure( NewCaption : String ); stdcall;
  wnd_SetSize    : procedure( Width, Height : WORD ); stdcall;
  wnd_SetPos     : procedure( X, Y : WORD ); stdcall;
  wnd_SetOnTop   : procedure( OnTop : Boolean ); stdcall;
  wnd_ShowCursor : procedure( Show : Boolean ); stdcall;
  
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
  scr_Clear             : procedure; stdcall;
  scr_Flush             : procedure; stdcall;
  scr_SetVSync          : procedure( VSync : Boolean ); stdcall;
  // ВНИМАНИЕ: Функция уничтожает контекст OpenGL, что потребует перезагрузку ресурсов
  scr_SetFSAA           : procedure( FSAA : Byte ); stdcall;
  scr_SetOptions        : procedure( Width, Height, BPP, Refresh : WORD; FullScreen, VSync : Boolean ); stdcall;
  scr_CorrectResolution : procedure( Width, Height : WORD ); stdcall;

// INI
  ini_LoadFromFile : procedure( FileName : String ); stdcall;
  ini_SaveToFile   : procedure( FileName : String ); stdcall;
  ini_Add          : procedure( Section, Key : String ); stdcall;
  ini_ReadKeyStr   : function( Section, Key : String ) : String; stdcall;
  ini_ReadKeyInt   : function( Section, Key : String ) : Integer; stdcall;
  ini_ReadKeyBool  : function( Section, Key : String ) : Boolean; stdcall;
  ini_WriteKeyStr  : function( Section, Key, Value : String ) : Boolean; stdcall;
  ini_WriteKeyInt  : function( Section, Key : String; Value : Integer ) : Boolean; stdcall;
  ini_WriteKeyBool : function( Section, Key : String; Value : Boolean ) : Boolean; stdcall;
  
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
    Count : DWORD;
    First : zglTTimer;
end;

var
  timer_Add      : function( OnTimer : Pointer; Interval : DWORD ) : zglPTimer; stdcall;
  timer_Del      : procedure( Timer : zglPTimer ); stdcall;
  timer_GetTicks : function : Double; stdcall;
  
// KEYBOARD
const
  K_BACKSPACE = 8;
  K_TAB       = 9;
  K_ENTER     = 13;
  K_SHIFT     = 16;
  K_SHIFT_L   = 160;
  K_SHIFT_R   = 161;
  K_CTRL      = 17;
  K_CTRL_L    = 162;
  K_CTRL_R    = 163;
  K_ALT       = 18;
  K_ALT_L     = 164;
  K_ALT_R     = 165;
  K_PAUSE     = 19;
  K_ESCAPE    = 27;
  K_SPACE     = 32;

  K_PAGEUP    = 33;
  K_PAGEDOWN  = 34;
  K_END       = 35;
  K_HOME      = 36;
  K_SNAPSHOT  = 44;
  K_INSERT    = 45;
  K_DELETE    = 46;

  K_LEFT      = 37;
  K_UP        = 38;
  K_RIGHT     = 39;
  K_DOWN      = 40;

  K_0         = 48;
  K_1         = 49;
  K_2         = 50;
  K_3         = 51;
  K_4         = 52;
  K_5         = 53;
  K_6         = 54;
  K_7         = 55;
  K_8         = 56;
  K_9         = 57;

  K_NUMPAD0   = 96;
  K_NUMPAD1   = 97;
  K_NUMPAD2   = 98;
  K_NUMPAD3   = 99;
  K_NUMPAD4   = 100;
  K_NUMPAD5   = 101;
  K_NUMPAD6   = 102;
  K_NUMPAD7   = 103;
  K_NUMPAD8   = 104;
  K_NUMPAD9   = 105;

  K_MULTIPLY  = 106;
  K_ADD       = 107;
  K_SEPARATOR = 108;
  K_SUBTRACT  = 109;
  K_DECIMAL   = 110;
  K_DIVIDE    = 111;

  K_A         = 65;
  K_B         = 66;
  K_C         = 67;
  K_D         = 68;
  K_E         = 69;
  K_F         = 70;
  K_G         = 71;
  K_H         = 72;
  K_I         = 73;
  K_J         = 74;
  K_K         = 75;
  K_L         = 76;
  K_M         = 77;
  K_N         = 78;
  K_O         = 79;
  K_P         = 80;
  K_Q         = 81;
  K_R         = 82;
  K_S         = 83;
  K_T         = 84;
  K_U         = 85;
  K_V         = 86;
  K_W         = 87;
  K_X         = 88;
  K_Y         = 89;
  K_Z         = 90;

  K_F1        = 112;
  K_F2        = 113;
  K_F3        = 114;
  K_F4        = 115;
  K_F5        = 116;
  K_F6        = 117;
  K_F7        = 118;
  K_F8        = 119;
  K_F9        = 120;
  K_F10       = 121;
  K_F11       = 122;
  K_F12       = 123;

  KA_DOWN     = 0;
  KA_UP       = 1;
var
  key_Down          : function( KeyCode : Byte ) : Boolean; stdcall;
  key_Up            : function( KeyCode : Byte ) : Boolean; stdcall;
  key_Last          : function( KeyAction : Byte ) : Byte; stdcall;
  key_BeginReadText : procedure( Text : String; MaxSymbols : WORD ); stdcall;
  key_EndReadText   : function : PChar; stdcall;
  key_ClearState    : procedure; stdcall;
  
// MOUSE
const
  M_BLEFT  = 0;
  M_BMIDLE = 1;
  M_BRIGHT = 2;
  M_WUP    = 0;
  M_WDOWN  = 1;

var
  mouse_X          : function : Integer; stdcall;
  mouse_Y          : function : Integer; stdcall;
  mouse_DX         : function : Integer; stdcall;
  mouse_DY         : function : Integer; stdcall;
  mouse_Down       : function( Button : Byte ) : Boolean; stdcall;
  mouse_Up         : function( Button : Byte ) : Boolean; stdcall;
  mouse_Click      : function( Button : Byte ) : Boolean; stdcall;
  mouse_Wheel      : function( Axis : Byte ) : Boolean; stdcall;
  mouse_ClearState : procedure; stdcall;
  mouse_Lock       : procedure; stdcall;
  
// GL
  Set2DMode : procedure; stdcall;
  Set3DMode : procedure( FOVY : Single ); stdcall;
  
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
  zglPTextureManager = ^zglTTextureManager;
  zglTTextureManager = record
    Count : DWORD;
    First : zglTTexture;
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
  tex_Add           : function : zglPTexture; stdcall;
  tex_Del           : procedure( Texture : zglPTexture ); stdcall;
  tex_Create        : procedure( var Texture : zglTTexture; pData : Pointer ); stdcall;
  tex_CreateZero    : function( Width, Height : WORD; Color, Flags : DWORD ) : zglPTexture; stdcall;
  tex_LoadFromFile  : function( FileName : String; TransparentColor, Flags : DWORD ) : zglPTexture; stdcall;
  tex_SetFrameSize  : procedure( Texture : zglPTexture; FrameWidth, FrameHeight : WORD ); stdcall;
  tex_SetMask       : function( Texture, Mask : zglPTexture ) : zglPTexture; stdcall;
  tex_GetData       : procedure( Texture : zglPTexture; var pData : Pointer; var pSize : Integer ); stdcall;
  tex_Filter        : procedure( Texture : zglPTexture; Flags : DWORD ); stdcall;
  tex_SetAnisotropy : procedure( Level : Byte ); stdcall;

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
  rtarget_Add : function( rtType : Byte; Surface : zglPTexture; Flags : Byte ) : zglPRenderTarget; stdcall;
  rtarget_Del : procedure( Target : zglPRenderTarget ); stdcall;
  rtarget_Set : procedure( Target : zglPRenderTarget ); stdcall;
  
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

type
  zglPPolyLine = ^zglTPolyLine;
  zglTPolyLine = record
    Count  : DWORD;
    cX, cY : Single;
    Points : array of zglTPoint2D;
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
  fx_SetBlendMode : procedure( Mode : Byte ); stdcall;
  
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
  fx2d_SetColorMix : procedure( Color : DWORD ); stdcall;
  fx2d_SetVCA      : procedure( c1, c2, c3, c4 : DWORD; a1, a2, a3, a4 : Byte ); stdcall;
  fx2d_SetVertexes : procedure( x1, y1, x2, y2, x3, y3, x4, y4 : Single ); stdcall;
  fx2d_SetScale    : procedure( scaleX, scaleY : Single ); stdcall;
  
// Camera 2D
type
  zglPCamera2D = ^zglTCamera2D;
  zglTCamera2D = record
    X, Y  : Single;
    Angle : Single;
end;

var
  cam2d_Set : procedure( Camera : zglPCamera2D ); stdcall;
  
// Primitives 2D
const
  PR2D_FILL   = $000001;
  PR2D_SMOOTH = $000002;

var
  pr2d_Pixel   : procedure( X, Y : Single; Color : DWORD = $FFFFFF; Alpha : Byte = 255 ); stdcall;
  pr2d_Line    : procedure( X1, Y1, X2, Y2 : Single; Color : DWORD = $FFFFFF; Alpha : Byte = 255; FX : DWORD = 0 ); stdcall;
  pr2d_Rect    : procedure( X, Y, W, H : Single; Color : DWORD = $FFFFFF; Alpha : Byte = 255; FX : DWORD = 0 ); stdcall;
  pr2d_Circle  : procedure( X, Y, Radius : Single; Color : DWORD = $FFFFFF; Alpha : Byte = 255; Quality : WORD = 32; FX : DWORD = 0 ); stdcall;
  pr2d_Ellipse : procedure( X, Y, xRadius, yRadius : Single; Color : DWORD = $FFFFFF; Alpha : Byte = 255; Quality : WORD = 32; FX : DWORD = 0 ); stdcall;
  
// Sprites 2D
  ssprite2d_Draw : procedure( Texture : zglPTexture; X, Y, W, H, Angle : Single; Alpha : Byte = 255; FX : DWORD = FX_BLEND ); stdcall;
  asprite2d_Draw : procedure( Texture : zglPTexture; X, Y, W, H, Angle : Single; Frame : WORD; Alpha : Byte = 255; FX : DWORD = FX_BLEND ); stdcall;
  csprite2d_Draw : procedure( Texture : zglPTexture; X, Y, W, H, Angle : Single; CutRect : zglTRect; Alpha : Byte = 255; FX : DWORD = FX_BLEND ); stdcall;
  
// Text
type
  zglPFont = ^zglTFont;
  zglTFont = record
    Texture    : zglPTexture;
    Height     : Byte;
    Width      : array[ 0..255 ] of Byte;
    TexCoords  : array[ 0..255 ] of array[ 0..3 ] of zglTPoint2D;

    Prev, Next : zglPFont;
end;

type
  zglPFontManager = ^zglTFontManager;
  zglTFontManager = record
    Count : DWORD;
    First : zglTFont;
end;

var
  font_Add          : function : zglPFont; stdcall;
  font_Del          : procedure( Font : zglPFont ); stdcall;
  font_LoadFromFile : function( Texture, FontInfo : String ) : zglPFont; stdcall;
  text_Draw         : procedure( Font : zglPFont; X, Y : Single; Text : String; Alpha : Byte = 255; Color : DWORD = $FFFFFF; Step : Single = 0; Scale : Single = 1 ); stdcall;
  text_GetWidth     : function( Font : zglPFont; Text : String; Step : Single = 0.0; Scale : Single = 1.0 ) : Single; stdcall;

// Sound
type
  zglPSound = ^zglTSound;
  zglTSound = record
    Buffer       : DWORD;
    sCount       : DWORD;
    Source       : array of Integer;

    Data         : Pointer;
    Size         : DWORD;
    Frequency    : DWORD;

    Prev, Next   : zglPSound;
end;

type
  zglPSoundFile = ^zglTSoundFile;
  zglTSoundFile = record
    _File      : DWORD;
    CodecRead  : Pointer;//function( Buffer : Pointer; Count : DWORD ) : DWORD; stdcall;
    CodecLoop  : Pointer;//procedure; stdcall;
    Rate       : DWORD;
    Channels   : DWORD;
    Buffer     : Pointer;
    BufferSize : DWORD;
    Loop       : Boolean;
    Played     : Boolean;
end;

type
  zglPSoundManager = ^zglTSoundManager;
  zglTSoundManager = record
    Count : DWORD;
    First : zglTSound;
end;

var
  snd_Init              : function : Boolean; stdcall;
  snd_Free              : procedure; stdcall;
  snd_Add               : function( BufferCount, SourceCount : Integer ) : zglPSound; stdcall;
  snd_Del               : procedure( Sound : zglPSound ); stdcall;
  snd_LoadFromFile      : function( FileName : String; SourceCount : Integer ) : zglPSound; stdcall;
  snd_Play              : function( Sound : zglPSound; X, Y, Z : Single; Loop : Boolean = FALSE ) : Integer; stdcall;
  snd_Stop              : procedure( Sound : zglPSound; Source : Integer = -1 ); stdcall;
  snd_SetVolume         : procedure( Volume : Byte; ID : Integer ); stdcall;
  snd_SetFrequency      : procedure( Frequency : Integer; ID : Integer ); stdcall;
  snd_SetFrequencyCoeff : procedure( Coefficient : Single; ID : Integer ); stdcall;
  snd_PlayFile          : procedure( SoundFile : zglPSoundFile ); stdcall;
  snd_StopFile          : procedure; stdcall;
  snd_RestoreFile       : procedure; stdcall;

// 3D
type
  zglPPoint3D = ^zglTPoint3D;
  zglTPoint3D = record
    case Byte of
    1 : ( X, Y, Z : Single );
    2:  ( point : array[ 0..2 ] of Single );
end;

type
  zglPQuaternion = ^zglTQuaternion;
  zglTQuaternion = record
    X, Y, Z, W : Single;
end;

type
  zglPMatrix3f = ^zglTMatrix3f;
  zglTMatrix3f = array[ 0..2 ] of zglTPoint3D;
  
  zglPMatrix4f = ^zglTMatrix4f;
  zglTMatrix4f = array[ 0..3, 0..3 ] of Single;
  
type
  zglPFace = ^zglTFace;
  zglTFace = record
    vIndex : array[ 0..2 ] of DWORD;
    tIndex : array[ 0..2 ] of DWORD;
end;

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
  
var
// Z BUFFER
  zbuffer_SetDepth  : procedure( zNear, zFar : Single ); stdcall;
  zbuffer_Clear     : procedure; stdcall;
  
// SCISSOR
  scissor_Begin : procedure( X, Y, Width, Height : WORD ); stdcall;
  scissor_End   : procedure; stdcall;
  
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

  // HeigthMap, Static Mesh, VBO, Octree
  USE_NORMALS         = $001;
  USE_TEXTURE         = $002;
  USE_MULTITEX1       = $004;
  USE_MULTITEX2       = $008;
  USE_MULTITEX3       = $010;
  BUILD_FNORMALS      = $020;
  BUILD_SNORMALS      = $040;
  BUILD_PLANES        = $080;
  BUILD_VBO           = $100;

var
  obj3d_Begin       : procedure( Flags : DWORD ); stdcall;
  obj3d_End         : procedure; stdcall;
  obj3d_Enable      : procedure( Flags : DWORD ); stdcall;
  obj3d_Disable     : procedure( Flags : DWORD ); stdcall;
  obj3d_SetColor    : procedure( Color : DWORD; Alpha : Byte ); stdcall;
  obj3d_BindTexture : procedure( Texture : zglPTexture; Level : Byte ); stdcall;
  obj3d_SetMaterial : procedure( Material, Side : Byte; Color : DWORD; Alpha : Byte ); stdcall;
  obj3d_Scale       : procedure( ScaleX, ScaleY, ScaleZ : Single ); stdcall;
  obj3d_Move        : procedure( X, Y, Z : Single ); stdcall;
  obj3d_SetMatrix   : procedure( Matrix : zglPMatrix4f ); stdcall;
  obj3d_MulMatrix   : procedure( Matrix : zglPMatrix4f ); stdcall;
  
const
  AX = $01;
  AY = $02;
  AZ = $04;
var
  obj3d_Rotate : procedure( Angle : Single; Axis : Byte ); stdcall;

// PRIMITIVES 3D
  pr3d_Point  : procedure( X, Y, Z : Single ); stdcall;
  pr3d_Line   : procedure( X1, Y1, Z1, X2, Y2, Z2 : Single ); stdcall;
  pr3d_Plane  : procedure( Width, Height : Single ); stdcall;
  pr3d_AABB   : procedure( Width, Height, ZDepth : Single ); stdcall;
  pr3d_Sphere : procedure( Radius : Single; Quality : Integer ); stdcall;

// SPRITE 3D
  ssprite3d_Draw : procedure( X, Y, Z, sX, sY, sZ : Single; Matrix : zglPMatrix4f ); stdcall;
  asprite3d_Draw : procedure( X, Y, Z, sX, sY, sZ : Single; Frame : Integer; Matrix : zglPMatrix4f ); stdcall;

// CAMERA 3D
type
  zglPCamera3D = ^zglTCamera3D;
  zglTCamera3D = record
    Position : zglTPoint3D;
    Rotation : zglTPoint3D;
    Matrix   : zglTMatrix4f;
end;

var
  cam3d_Set    : procedure( Camera : zglPCamera3D ); stdcall;
  cam3d_Fly    : procedure( Camera : zglPCamera3D; Speed : Single ); stdcall;
  cam3d_Strafe : procedure( Camera : zglPCamera3D; Speed : Single ); stdcall;
  
// STATIC MESH
type
  zglPSMesh = ^zglTSMesh;
  zglTSMesh = record
    Flags          : DWORD;

    VCount         : DWORD;
    TCount         : DWORD;
    FCount         : DWORD;
    GCount         : DWORD;

    Vertices       : array of zglTPoint3D;
    Normals        : array of zglTPoint3D;
    TexCoords      : array of zglTPoint2D;
    MultiTexCoords : array of zglTPoint2D;
    Faces          : array of zglTFace;
    Indices        : Pointer;
    Groups         : array of zglTGroup;
end;

var
  smesh_LoadFromFile : function( var Mesh : zglPSMesh; FileName : String; Flags : DWORD = 0 ) : Boolean; stdcall;
  smesh_Draw         : procedure( Mesh : zglPSMesh ); stdcall;
  smesh_DrawGroup    : procedure( Mesh : zglPSMesh; Group : DWORD ); stdcall;
  smesh_Free         : procedure( var Mesh : zglPSMesh ); stdcall;

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
  zglPSkeletonFrame = ^zglTSkeletonFrame;
  zglTSkeletonFrame = record
    BonePos : array of zglTBonePos;
end;

type
  zglPSkeletonState = ^zglTSkeletonState;
  zglTSkeletonState = record
    prevFrame   : DWORD;
    nextFrame   : DWORD;
    prevAction  : DWORD;
    nextAction  : DWORD;
    _time       : Single;
    _timeOld    : Single;
    _timeNow    : Single;
    Frame       : zglTSkeletonFrame;
    Vertices    : array of zglTPoint3D;
end;

type
  zglPSkeletonAction = ^zglTSkeletonAction;
  zglTSkeletonAction = record    Name   : String;
    FPS    : Single;
    FCount : DWORD;
    Frames : array of zglTSkeletonFrame;
end;

type
  zglPSkMesh = ^zglTSkMesh;
  zglTSkMesh = record
    Flags          : DWORD;

    VCount         : DWORD;
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
    Groups         : array of zglTGroup;

    Bones          : array of zglTBone;
    Weights        : zglTBonesWeights;
    State          : zglTSkeletonState;
    Actions        : array of zglTSkeletonAction;
    Skeleton       : zglTSkeletonFrame;
end;

var
  skmesh_LoadFromFile : function( var Mesh : zglPSkMesh; FileName : String; Flags : DWORD = 0 ) : Boolean; stdcall;
  skmesh_Draw         : procedure( Mesh : zglPSkMesh; State : zglPSkeletonState ); stdcall;
  skmesh_Animate      : procedure( Mesh : zglPSkMesh; State : zglPSkeletonState; Action : Integer; FPS : Integer = 60 ); stdcall;
  skmesh_Free         : procedure( var Mesh : zglPSkMesh ); stdcall;

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
  heightmap_Build      : procedure( var Heightmap : zglPHeightMap; Texture : zglPTexture; xScale, yScale, zScale : Single; xDetail, yDetail : Integer; Flags : DWORD ); stdcall;
  heightmap_Draw       : procedure( Heightmap : zglPHeightMap ); stdcall;
  heightmap_Free       : procedure( var Heightmap : zglPHeightMap ); stdcall;
  heightmap_GetPlane   : function( Heightmap : zglPHeightMap; Position : zglTPoint3D ) : DWORD; stdcall;
  heightmap_GetYOffset : function( Heightmap : zglPHeightMap; Position : zglTPoint3D ) : Single; stdcall;
  
// VBO
  vbo_Build : procedure( var IBuffer, VBuffer : DWORD; ICount, VCount : DWORD; Indices, Vertices, Normals, TexCoords, MultiTexCoords : Pointer; var Flags : DWORD ); stdcall;
  vbo_Free  : procedure( var IBuffer, VBuffer : DWORD; ICount, VCount : DWORD ); stdcall;

// FRUSTUM
type
  zglPFrustum = ^zglTFrustum;
  zglTFrustum = array [ 0..5 ] of array[ 0..3 ] of Single;

var
  frustum_Calc       : procedure( f : zglPFrustum ); stdcall;
  frustum_PointIn    : function( f : zglPFrustum; x, y, z : Single ) : Boolean; stdcall;
  frustum_PPointIn   : function( f : zglPFrustum; Vertex : zglPPoint3D ) : Boolean; stdcall;
  frustum_TriangleIn : function( f : zglPFrustum; v1, v2, v3 : zglTPoint3D ) : Boolean; stdcall;
  frustum_SphereIn   : function( f : zglPFrustum; x, y, z, r : Single ) : Boolean; stdcall;
  frustum_BoxIn      : function( f : zglPFrustum; x, y, z, bx, by, bz : Single ) : Boolean; stdcall;
  frustum_CubeIn     : function( f : zglPFrustum; x, y, z, size : Single ) : Boolean; stdcall;
  
// OCTREE
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
  octree_Build     : procedure( Octree : zglPOctree; MaxFacesPerNode, Flags : DWORD ); stdcall;
  octree_Free      : procedure( Octree : zglPOctree ); stdcall;
  octree_Draw      : procedure( Octree : zglPOctree; Frustum : zglPFrustum ); stdcall;
  octree_DrawDebug : procedure( Octree : zglPOctree; Frustum : zglPFrustum ); stdcall;
  octree_DrawNode  : procedure( OCtree : zglPOctree; Node : zglPNode; Frustum : zglPFrustum ); stdcall;
  
// LIGHT
  light_Enable      : procedure( ID : Byte ); stdcall;
  light_Disable     : procedure( ID : Byte ); stdcall;
  light_SetPosition : procedure( ID : Byte; X, Y, Z : Single; W : Single = 1 ); stdcall;
  light_SetMaterial : procedure( ID, Material : Byte; Color : DWORD; Alpha : Byte ); stdcall;

  
// FOG
const
  FOG_MODE_EXP    = 0;
  FOG_MODE_EXP2   = 1;
  FOG_MODE_LINEAR = 2;

var
  fog_Enable      : procedure; stdcall;
  fog_Disable     : procedure; stdcall;
  fog_SetMode     : procedure( Mode : Byte ); stdcall;
  fog_SetColor    : procedure( Color : DWORD ); stdcall;
  fog_SetDensity  : procedure( Density : Single ); stdcall;
  fog_SetBeginEnd : procedure( fBegin, fEnd : Single ); stdcall;
  
// SKYBOX
  skybox_Init : procedure( Top, Bottom, Left, Right, Front, Back : zglPTexture ); stdcall;
  skybox_Draw : procedure; stdcall;
  
// SHADERS
const
  SHADER_ARB          = 0;
  SHADER_GLSL         = 1;
  SHADER_VERTEX_ARB   = $8620;
  SHADER_FRAGMENT_ARB = $8804;
  SHADER_VERTEX       = $8B31;
  SHADER_FRAGMENT     = $8B30;

var
  // ARBfp/ARBvp
  shader_InitARB         : function : Boolean; stdcall;
  shader_LoadFromFileARB : function( FileName : String; ShaderType : DWORD ) : DWORD; stdcall;
  shader_BeginARB        : procedure( Shader, ShaderType : DWORD ); stdcall;
  shader_EndARB          : procedure( ShaderType : DWORD ); stdcall;
  shader_FreeARB         : procedure( Shader : DWORD ); stdcall;

  // GLSL
  shader_InitGLSL       : function : Boolean; stdcall;
  shader_LoadFromFile   : function( FileName : String; ShaderType : Integer; Link : Boolean ) : DWORD; stdcall;
  shader_Attach         : procedure( Attach : DWORD ); stdcall;
  shader_BeginLink      : procedure; stdcall;
  shader_EndLink        : function : DWORD; stdcall;
  shader_Begin          : procedure( Shader : DWORD ); stdcall;
  shader_End            : procedure; stdcall;
  shader_Free           : procedure( Shader : DWORD ); stdcall;
  shader_GetUniform     : function( Shader : DWORD; UniformName : String ) : Integer; stdcall;
  shader_SetUniform1f   : procedure( Uniform : Integer; v1 : Single ); stdcall;
  shader_SetUniform1i   : procedure( Uniform : Integer; v1 : Integer ); stdcall;
  shader_SetUniform2f   : procedure( Uniform : Integer; v1, v2 : Single ); stdcall;
  shader_SetUniform3f   : procedure( Uniform : Integer; v1, v2, v3 : Single ); stdcall;
  shader_SetUniform4f   : procedure( Uniform : Integer; v1, v2, v3, v4 : Single ); stdcall;
  shader_GetAttrib      : function( Shader : DWORD; AttribName : String ) : Integer; stdcall;
  // glVertexAttrib* GLSL/ARB
  shader_SetAttrib1f    : procedure( Attrib : Integer; v1 : Single ); stdcall;
  shader_SetAttrib2f    : procedure( Attrib : Integer; v1, v2 : Single ); stdcall;
  shader_SetAttrib3f    : procedure( Attrib : Integer; v1, v2, v3 : Single ); stdcall;
  shader_SetAttrib4f    : procedure( Attrib : Integer; v1, v2, v3, v4 : Single ); stdcall;
  shader_SetAttribPf    : procedure( Attrib : Integer; v : Pointer; Normalized : Boolean ); stdcall;
  shader_SetParameter4f : procedure( ShaderType : DWORD; Parameter : Integer; v1, v2, v3, v4 : Single ); stdcall;
  
// MATH
  m_Round     : function( value : Single ) : Integer; stdcall;
  m_Cos       : function( Angle : Integer ) : Single; stdcall;
  m_Sin       : function( Angle : Integer ) : Single; stdcall;
  m_SinCos    : procedure( Angle : Single; var S, C : Single );
  m_Distance  : function( x1, y1, x2, y2 : Single ) : Single; stdcall;
  m_FDistance : function( x1, y1, x2, y2 : Single ) : Single; stdcall;
  m_Angle     : function( x1, y1, x2, y2 : Single ) : Single; stdcall;
  //vectros
  vector_Get       : function( x, y, z : Single ) : zglTPoint3D;
  vector_Add       : function( Vector1, Vector2 : zglTPoint3D ) : zglTPoint3D;
  vector_Sub       : function( Vector1, Vector2 : zglTPoint3D ) : zglTPoint3D;
  vector_Mul       : function( Vector1, Vector2 : zglTPoint3D ) : zglTPoint3D;
  vector_Div       : function( Vector1, Vector2 : zglTPoint3D ) : zglTPoint3D;
  vector_AddV      : function( Vector : zglTPoint3D; Value : Single ) : zglTPoint3D;
  vector_SubV      : function( Vector : zglTPoint3D; Value : Single ) : zglTPoint3D;
  vector_MulV      : function( Vector : zglTPoint3D; Value : Single ) : zglTPoint3D;
  vector_DivV      : function( Vector : zglTPoint3D; Value : Single ) : zglTPoint3D;
  vector_MulM3f    : function( Vector : zglTPoint3D; Matrix : zglPMatrix3f ) : zglTPoint3D;
  vector_MulM4f    : function( Vector : zglTPoint3D; Matrix : zglPMatrix4f ) : zglTPoint3D;
  vector_MulInvM4f : function( Vector : zglTPoint3D; Matrix : zglPMatrix4f ) : zglTPoint3D;
  vector_RotateQ   : function( Vector : zglTPoint3D; Quaternion : zglTQuaternion ) : zglTPoint3D;
  vector_Negate    : function( Vector : zglTPoint3D ) : zglTPoint3D;
  vector_Normalize : function( Vector : zglTPoint3D ) : zglTPoint3D;
  vector_Angle     : function( Vector1, Vector2 : zglTPoint3D ) : Single;
  vector_Cross     : function( Vector1, Vector2 : zglTPoint3D ) : zglTPoint3D;
  vector_Dot       : function( Vector1, Vector2 : zglTPoint3D ) : Single;
  vector_Distance  : function( Vector1, Vector2 : zglTPoint3D ) : Single;
  vector_FDistance : function( Vector1, Vector2 : zglTPoint3D ) : Single;
  vector_Length    : function( Vector : zglTPoint3D ) : Single;
  vector_Lerp      : function( Vector1, Vector2 : zglTPoint3D; Value : Single ) : zglTPoint3D;
  //matrix
  matrix3f_Get            : function( v1, v2, v3 : zglTPoint3D ) : zglTMatrix3f;
  matrix3f_Identity       : procedure( Matrix : zglPMatrix3f );
  matrix3f_OrthoNormalize : procedure( Matrix : zglPMatrix3f );
  matrix3f_Transpose      : procedure( Matrix : zglPMatrix3f );
  matrix3f_Rotate         : procedure( Matrix : zglPMatrix3f; aX, aY, aZ : Single );
  matrix3f_Add            : procedure( Matrix1, Matrix2 : zglPMatrix3f );
  matrix3f_Mul            : function ( Matrix1, Matrix2 : zglPMatrix3f ) : zglTMatrix3f;
  matrix4f_Identity       : procedure( Matrix : zglPMatrix4f );
  matrix4f_Transpose      : procedure( Matrix : zglPMatrix4f );
  matrix4f_Translate      : procedure( Matrix : zglPMatrix4f; tX, tY, tZ : Single );
  matrix4f_Rotate         : procedure( Matrix : zglPMatrix4f; aX, aY, aZ : Single );
  matrix4f_Scale          : procedure( Matrix : zglPMatrix4f; sX, sY, sZ : Single );
  matrix4f_Add            : procedure( Matrix1, Matrix2 : zglPMatrix4f );
  matrix4f_Mul            : function ( Matrix1, Matrix2 : zglPMatrix4f ) : zglTMatrix4f;
  matrix4f_Concat         : function ( Matrix1, Matrix2 : zglPMatrix4f ) : zglTMatrix4f;
  // quaternions
  quater_Add          : function( q1, q2 : zglTQuaternion ) : zglTQuaternion;
  quater_Sub          : function( q1, q2 : zglTQuaternion ) : zglTQuaternion;
  quater_Mul          : function( q1, q2 : zglTQuaternion ) : zglTQuaternion;
  quater_Negate       : function( Quaternion : zglTQuaternion ) : zglTQuaternion;
  quater_Normalize    : function( Quaternion : zglTQuaternion ) : zglTQuaternion;
  quater_Dot          : function( q1, q2 : zglTQuaternion ) : Single;
  quater_Lerp         : function( q1, q2 : zglTQuaternion; Value : Single ) : zglTQuaternion;
  quater_FromRotation : function( Rotation : zglTPoint3D ) : zglTQuaternion;
  quater_GetM4f       : function( Quaternion : zglTQuaternion ) : zglTMatrix4f;
  // line 3d
  line3d_ClosestPoint : function( A, B, Point : zglTPoint3D ) : zglTPoint3D;
  // plane
  plane_Get      : function( A, B, C : zglTPoint3D ) : zglTPlane;
  plane_Distance : function( Plane : zglPPlane; Point : zglTPoint3D ) : Single;
  // triangle
  tri_GetNormal  : function( A, B, C : zglPPoint3D ) : zglTPoint3D;

// COLLISION 2D
  col2d_PointInRect     : function( X, Y : Single; Rect : zglPRect   ) : Boolean; stdcall;
  col2d_PointInCircle   : function( X, Y : Single; Circ : zglPCircle ) : Boolean; stdcall;
  col2d_PointInPolyLine : function( X, Y : Single; PL : zglPPolyLine ) : Boolean; stdcall;
  // line 2d
  col2d_Line           : function( A, B : zglPLine ) : Boolean; stdcall;
  col2d_LineVsRect     : function( A : zglPLine; Rect : zglPRect ) : Boolean; stdcall;
  col2d_LineVsCircle   : function( L : zglPLine; Circ : zglPCircle ) : Boolean; stdcall;
  col2d_LineVsCircleXY : function( L : zglPLine; Circ : zglPCircle; Precision : Byte ) : Boolean; stdcall;
  col2d_LineVsPolyLine : function( A : zglPLine; B : zglPPolyLine ) : Boolean; stdcall;
  // polyline
  col2d_PolyLine           : function( A, B : zglPPolyLine ) : Boolean; stdcall;
  col2d_PolyLineVsRect     : function( A : zglPPolyLine; Rect : zglPRect ) : Boolean; stdcall;
  col2d_PolyLineVsCircle   : function( A : zglPPolyLine; Circ : zglPCircle ) : Boolean; stdcall;
  col2d_PolyLineVsCircleXY : function( A : zglPPolyLine; Circ : zglPCircle; Precision : Integer ) : Boolean; stdcall;
  // rect
  col2d_Rect         : function( Rect1, Rect2 : zglPRect ) : Boolean; stdcall;
  col2d_RectInRect   : function( Rect1, Rect2 : zglPRect ) : Boolean; stdcall;
  col2d_RectInCircle : function( Rect : zglPRect; Circ : zglPCircle ) : Boolean; stdcall;
  col2d_RectVsCircle : function( Rect : zglPRect; Circ : zglPCircle ) : Boolean; stdcall;
  // circle
  col2d_Circle         : function( Circ1, Circ2 : zglPCircle ) : Boolean; stdcall;
  col2d_CircleInCircle : function( Circ1, Circ2 : zglPCircle ) : Boolean; stdcall;
  col2d_CircleInRect   : function( Circ : zglPCircle; Rect : zglPRect ) : Boolean; stdcall;
  // extended
  col2dEx_LastX     : function : Single; stdcall;
  col2dEx_LastY     : function : Single; stdcall;
  col2dEx_LastLineA : function : Integer; stdcall;
  col2dEx_LastLineB : function : Integer; stdcall;
  // polyline transformations
  col2dEx_PolyRotate : procedure( A, B : zglPPolyLine; Angle : Single ); stdcall;
  col2dEx_PolyScale  : procedure( A : zglPPolyLine; ScaleX, ScaleY : Single ); stdcall;
  col2dEx_PolyMove   : procedure( A, B : zglPPolyLine; X, Y : Single ); stdcall;
  col2dEx_PolyCenter : procedure( A : zglPPolyLine ); stdcall;
  col2dEx_PolyRect   : procedure( A : zglPPolyLine; Rect : zglPRect ); stdcall;
  // line
  col2dEx_CalcLineCross : procedure( A, B : zglPLine ); stdcall;
  
// COLLISION 3D
type
  zglPCol3DCallback = ^zglTCol3DCallback;
  zglTCol3DCallback = procedure( Offset : zglPPoint3D; Data : Pointer );
type
  zglPCollision3D = ^zglTCollision3D;
  zglTCollision3D = record
    Result : Boolean;
    Offset : zglTPoint3D;
end;

var
  // point 3D
  col3d_PointInTri    : function( Point, A, B, C : zglPPoint3D  ) : Boolean; stdcall;
  col3d_PointInAABB   : function( Point : zglPPoint3D; AABB : zglPAABB ) : Boolean; stdcall;
  col3d_PointInOBB    : function( Point : zglPPoint3D; OBB : zglPOBB ) : Boolean; stdcall;
  col3d_PointInSphere : function( Point : zglPPoint3D; Sphere : zglPSphere ) : Boolean; stdcall;
  // line3D
  col3d_LineVsAABB   : function( Line : zglPLine3D; AABB : zglPAABB ) : Boolean; stdcall;
  col3d_LineVsOBB    : function( Line : zglPLine3D; OBB : zglPOBB ) : Boolean; stdcall;
  col3d_LineVsSphere : function( Line : zglPLine3D; Sphere : zglPSphere ) : Boolean; stdcall;
  // plane 3d
  col3d_PlaneVsSphere : function( Plane : zglPPlane; Sphere : zglPSphere ) : zglTCollision3D; stdcall;
  // aabb
  col3d_AABBVsAABB   : function( AABB1, AABB2 : zglPAABB ) : Boolean; stdcall;
  col3d_AABBVsOBB    : function( AABB : zglPAABB; OBB : zglPOBB ) : Boolean; stdcall;
  col3d_AABBVsSphere : function( AABB : zglPAABB; Sphere : zglPSphere ) : Boolean; stdcall;
  // obb
  col3d_OBBVsOBB    : function( OBB1, OBB2 : zglPOBB ) : Boolean; stdcall;
  col3d_OBBVsSphere : function( OBB : zglPOBB; Sphere : zglPSphere ) : Boolean; stdcall;
  // sphere
  col3d_SphereVsSphere : function( Sphere1, Sphere : zglPSphere ) : Boolean; stdcall;
  col3d_SphereVsNode   : function( Sphere : zglPSphere; Octree : zglPOctree; Node : zglPNode; Callback : zglTCol3DCallback; CData : Pointer ) : Boolean; stdcall;

type zglTFile = DWORD;
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
  file_Open    : procedure( var FileHandle : zglTFile; FileName : String; Mode : Byte ); stdcall;
  file_Exists  : function( FileName : String ) : Boolean; stdcall;
  file_Seek    : function( FileHandle : zglTFile; Offset : DWORD; Mode : Byte ) : DWORD; stdcall;
  file_GetPos  : function( FileHandle : zglTFile ) : DWORD; stdcall;
  file_Read    : function( FileHandle : zglTFile; var buffer; count : DWORD ) : DWORD; stdcall;
  file_Write   : function( FileHandle : zglTFile; const buffer; count : DWORD ) : DWORD; stdcall;
  file_Trunc   : procedure( FileHandle : zglTFile; count : DWORD ); stdcall;
  file_GetSize : function( FileHandle : zglTFile ) : DWORD; stdcall;
  file_Flush   : procedure( FileHandle : zglTFile ); stdcall;
  file_Close   : procedure( FileHandle : zglTFile ); stdcall;

type
  zglPMemory = ^zglTMemory;
  zglTMemory = record
    Memory   : Pointer;
    Size     : DWORD;
    Position : DWORD;
end;

var
  mem_LoadFromFile : procedure( var Memory : zglTMemory; FileName : String ); stdcall;
  mem_SaveToFile   : procedure( var Memory : zglTMemory; FileName : String ); stdcall;
  mem_Seek         : function( var Memory : zglTMemory; Offset : DWORD; Mode : Byte ) : DWORD; stdcall;
  mem_Read         : function( var Memory : zglTMemory; var buffer; count : DWORD ) : DWORD; stdcall;
  mem_Write        : function( var Memory : zglTMemory; const buffer; count : DWORD ) : DWORD; stdcall;
  mem_SetSize      : procedure( var Memory : zglTMemory; Size : DWORD ); stdcall;
  mem_Free         : procedure( var Memory : zglTMemory ); stdcall;

// Utils
function u_IntToStr( Value : Integer ) : String;
function u_StrToInt( Value : String ) : Integer;

{$IFDEF LINUX}
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
  zglLib : {$IFDEF LINUX} Pointer {$ENDIF} {$IFDEF WIN32} HMODULE {$ENDIF};
  
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
  zglLib := dlopen( PChar( LibraryName ) {$IFDEF LINUX}, $001 {$ENDIF} );

  if zglLib <> {$IFDEF LINUX} nil {$ENDIF} {$IFDEF WIN32} 0 {$ENDIF} Then
    begin
      Result := TRUE;
      zgl_Init := dlsym( zglLib, 'zgl_Init' );
      zgl_Exit := dlsym( zglLib, 'zgl_Exit' );
      zgl_Reg := dlsym( zglLib, 'zgl_Reg' );
      zgl_Get := dlsym( zglLib, 'zgl_Get' );
      zgl_GetMem := dlsym( zglLib, 'zgl_GetMem' );
      zgl_Enable := dlsym( zglLib, 'zgl_Enable' );
      zgl_Disable := dlsym( zglLib, 'zgl_Disable' );

      log_Add := dlsym( zglLib, 'log_Add' );

      wnd_SetCaption := dlsym( zglLib, 'wnd_SetCaption' );
      wnd_SetSize := dlsym( zglLib, 'wnd_SetSize' );
      wnd_SetPos := dlsym( zglLib, 'wnd_SetPos' );
      wnd_SetOnTop := dlsym( zglLib, 'wnd_SetOnTop' );
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
    //  pr2d_Triangle := dlsym( zglLib, 'pr2d_Triangle' );
      pr2d_Rect := dlsym( zglLib, 'pr2d_Rect' );
      pr2d_Circle := dlsym( zglLib, 'pr2d_Circle' );
      pr2d_Ellipse := dlsym( zglLib, 'pr2d_Ellipse' );

      ssprite2d_Draw := dlsym( zglLib, 'ssprite2d_Draw' );
      asprite2d_Draw := dlsym( zglLib, 'asprite2d_Draw' );
      csprite2d_Draw := dlsym( zglLib, 'csprite2d_Draw' );

      font_Add := dlsym( zglLib, 'font_Add' );
      font_Del := dlsym( zglLib, 'font_Del' );
      font_LoadFromFile := dlsym( zglLib, 'font_LoadFromFile' );
      text_Draw := dlsym( zglLib, 'text_Draw' );
      text_GetWidth := dlsym( zglLib, 'text_GetWidth' );

      snd_Init := dlsym( zglLib, 'snd_Init' );
      snd_Free := dlsym( zglLib, 'snd_Free' );
      snd_Add  := dlsym( zglLib, 'snd_Add' );
      snd_Del  := dlsym( zglLib, 'snd_Del' );
      snd_LoadFromFile := dlsym( zglLib, 'snd_LoadFromFile' );
      snd_Play := dlsym( zglLib, 'snd_Play' );
      snd_Stop := dlsym( zglLib, 'snd_Stop' );
      snd_SetVolume := dlsym( zglLib, 'snd_SetVolume' );
      snd_SetFrequency := dlsym( zglLib, 'snd_SetFrequency' );
      snd_SetFrequencyCoeff := dlsym( zglLib, 'snd_SetFrequencyCoeff' );
      snd_PlayFile := dlsym( zglLib, 'snd_PlayFile' );
      snd_StopFile := dlsym( zglLib, 'snd_StopFile' );
      snd_RestoreFile := dlsym( zglLib, 'snd_RestoreFile' );

      obj3d_Begin := dlsym( zglLib, 'obj3d_Begin' );
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
      smesh_Draw := dlsym( zglLib, 'smesh_Draw' );
      smesh_DrawGroup := dlsym( zglLib, 'smesh_DrawGroup' );
      smesh_Free := dlsym( zglLib, 'smesh_Free' );

      skmesh_LoadFromFile := dlsym( zglLib, 'skmesh_LoadFromFile' );
      skmesh_Draw := dlsym( zglLib, 'skmesh_Draw' );
      skmesh_Animate := dlsym( zglLib, 'skmesh_Animate' );
      skmesh_Free := dlsym( zglLib, 'skmesh_Free' );

      heightmap_Build := dlsym( zglLib, 'heightmap_Build' );
      heightmap_Draw := dlsym( zglLib, 'heightmap_Draw' );
      heightmap_Free := dlsym( zglLib, 'heightmap_Free' );
      heightmap_GetPlane := dlsym( zglLib, 'heightmap_GetPlane' );
      heightmap_GetYOffset := dlsym( zglLib, 'heightmap_GetYOffset' );

      vbo_Build := dlsym( zglLib, 'vbo_Build' );
      vbo_Free := dlsym( zglLib, 'vbo_Free' );

      frustum_Calc := dlsym( zglLib, 'frustum_Calc' );
      frustum_PointIn := dlsym( zglLib, 'frustum_PointIn' );
      frustum_PPointIn := dlsym( zglLib, 'frustum_PPointIn' );
      frustum_TriangleIn := dlsym( zglLib, 'frustum_TriangleIn' );
      frustum_SphereIn := dlsym( zglLib, 'frustum_SphereIn' );
      frustum_BoxIn := dlsym( zglLib, 'frustum_BoxIn' );
      frustum_CubeIn := dlsym( zglLib, 'frustum_CubeIn' );

      octree_Build := dlsym( zglLib, 'octree_Build' );
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
      shader_SetParameter4f := dlsym( zglLib, 'shader_SetParameter4f' );
  
      m_Round := dlsym( zglLib, 'm_Round' );
      m_Cos := dlsym( zglLib, 'm_Cos' );
      m_Sin := dlsym( zglLib, 'm_Sin' );
      m_SinCos := dlsym( zglLib, 'm_SinCos' );
      m_Distance := dlsym( zglLib, 'm_Distance' );
      m_FDistance := dlsym( zglLib, 'm_FDistance' );
      m_Angle := dlsym( zglLib, 'm_Angle' );

      vector_Get := dlsym( zglLib, 'vector_Get' );
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
      matrix3f_Identity := dlsym( zglLib, 'matrix3f_Identity' );
      matrix3f_OrthoNormalize := dlsym( zglLib, 'matrix3f_OrthoNormalize' );
      matrix3f_Transpose := dlsym( zglLib, 'matrix3f_Transpose' );
      matrix3f_Rotate := dlsym( zglLib, 'matrix3f_Rotate' );
      matrix3f_Add := dlsym( zglLib, 'matrix3f_Add' );
      matrix3f_Mul := dlsym( zglLib, 'matrix3f_Mul' );

      matrix4f_Identity := dlsym( zglLib, 'matrix4f_Identity' );
      matrix4f_Transpose := dlsym( zglLib, 'matrix4f_Transpose' );
      matrix4f_Translate := dlsym( zglLib, 'matrix4f_Translate' );
      matrix4f_Rotate := dlsym( zglLib, 'matrix4f_Rotate' );
      matrix4f_Scale := dlsym( zglLib, 'matrix4f_Scale' );
      matrix4f_Add := dlsym( zglLib, 'matrix4f_Add' );
      matrix4f_Mul := dlsym( zglLib, 'matrix4f_Mul' );
      matrix4f_Concat := dlsym( zglLib, 'matrix4f_Concat' );

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

      tri_GetNormal := dlsym( zglLib, 'tri_GetNormal' );

      col2d_PointInRect := dlsym( zglLib, 'col2d_PointInRect' );
      col2d_PointInCircle := dlsym( zglLib, 'col2d_PointInCircle' );
      col2d_PointInPolyLine := dlsym( zglLib, 'col2d_PointInPolyLine' );
      col2d_Line := dlsym( zglLib, 'col2d_Line' );
      col2d_LineVsRect := dlsym( zglLib, 'col2d_LineVsRect' );
      col2d_LineVsCircle := dlsym( zglLib, 'col2d_LineVsCircle' );
      col2d_LineVsCircleXY := dlsym( zglLib, 'col2d_LineVsCircleXY' );
      col2d_LineVsPolyLine := dlsym( zglLib, 'col2d_LineVsPolyLine' );
      col2d_PolyLine := dlsym( zglLib, 'col2d_PolyLine' );
      col2d_PolyLineVsRect := dlsym( zglLib, 'col2d_PolyLineVsRect' );
      col2d_PolyLineVsCircle := dlsym( zglLib, 'col2d_PolyLineVsCircle' );
      col2d_PolyLineVsCircleXY := dlsym( zglLib, 'col2d_PolyLineVsCircleXY' );
      col2d_Rect := dlsym( zglLib, 'col2d_Rect' );
      col2d_RectInRect := dlsym( zglLib, 'col2d_RectInRect' );
      col2d_RectInCircle := dlsym( zglLib, 'col2d_RectInCircle' );
      col2d_RectVsCircle := dlsym( zglLib, 'col2d_RectVsCircle' );
      col2d_Circle := dlsym( zglLib, 'col2d_Circle' );
      col2d_CircleInCircle := dlsym( zglLib, 'col2d_CircleInCircle' );
      col2d_CircleInRect := dlsym( zglLib, 'col2d_CircleInRect' );
      col2dEx_LastX := dlsym( zglLib, 'col2dEx_LastX' );
      col2dEx_LastY := dlsym( zglLib, 'col2dEx_LastY' );
      col2dEx_LastLineA := dlsym( zglLib, 'col2dEx_LastLineA' );
      col2dEx_LastLineB := dlsym( zglLib, 'col2dEx_LastLineB' );
      col2dEx_PolyRotate := dlsym( zglLib, 'col2dEx_PolyRotate' );
      col2dEx_PolyScale := dlsym( zglLib, 'col2dEx_PolyScale' );
      col2dEx_PolyMove := dlsym( zglLib, 'col2dEx_PolyMove' );
      col2dEx_PolyCenter := dlsym( zglLib, 'col2dEx_PolyCenter' );
      col2dEx_PolyRect := dlsym( zglLib, 'col2dEx_PolyRect' );
      col2dEx_CalcLineCross := dlsym( zglLib, 'col2dEx_CalcLineCross' );

      col3d_PointInTri := dlsym( zglLib, 'col3d_PointInTri' );
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
      col3d_SphereVsNode := dlsym( zglLib, 'col3d_SphereVsNode' );

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
          WriteLn( 'Error while loading ZenGL Engine' );
          {$ENDIF}
          {$IFDEF WIN32}
          MessageBoxA( 0, 'Error while loading ZenGL Engine', 'Error', $00000010 );
          {$ENDIF}
        end;
end;

end.
