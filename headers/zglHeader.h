/*-------------------------------*/
/*-----------= ZenGL =-----------*/
/*-------------------------------*/
/* build: 30                     */
/* date:  17.11.08               */
/* beta version of header !      */
/*-------------------------------*/
/* by:   Andru ( Kemka Andrey )  */
/* mail: dr.andru@gmail.com      */
/* ICQ:  496-929-849             */
/* site: http://andru.2x4.ru     */
/*-------------------------------*/
/*                      (C) 2008 */
/*-------------------------------*/
#ifndef _ZGLHEADER_
#define _ZGLHEADER_

#pragma pack(push,8)

#ifdef linux
  #define __LINUX__
  #ifdef __cplusplus
  #define __CPP__
  #endif
#else
  #ifndef __WIN32__
  #define __WIN32__
  #endif
#endif

#ifdef __LINUX__
  #include <stdio.h>
  #ifdef __CPP__
  #include <dlfcn.h>
  #endif
#endif
#ifdef __WIN32__
  #include <windows.h>
#endif

#ifdef __CPP__
extern "C" {
#endif

typedef unsigned short WORD;
#ifndef __WIN32__
typedef unsigned int DWORD;
#endif
typedef unsigned char byte;
#ifndef __MINGW32__
#ifndef __CPP__
typedef unsigned char bool;
#endif
#endif

#ifndef APIENTRY
#define APIENTRY
#endif
extern void ( APIENTRY *zgl_Init )( byte FSAA, byte StencilBits );
extern void ( APIENTRY *zgl_Exit )(void);
  
#define SYS_LOAD             0x000001
#define SYS_DRAW             0x000002
#define SYS_EXIT             0x000003
#define TEX_FORMAT_EXTENSION 0x000010
#define TEX_FORMAT_LOADER    0x000011
#define SND_FORMAT_EXTENSION 0x000020
#define SND_FORMAT_LOADER    0x000021

extern void ( APIENTRY *zgl_Reg )( WORD What, void* UserData );
  
#define SYS_FPS         1  /* DWORD                   */
#define LOG_FILENAME    2  /* char*                   */
#define ZGL_VERSION     3  /* DWORD                   */
#define ZGL_ADD_X       4  /* DWORD                   */
#define ZGL_ADD_Y       5  /* DWORD                   */
#define DESKTOP_WIDTH   6  /* DWORD                   */
#define DESKTOP_HEIGHT  7  /* DWORD                   */
#define RESOLUTION_LIST 8  /* zglPResolutionList      */
#define MANAGER_TIMER   9  /* zglPTimerManager        */
#define MANAGER_TEXTURE 10 /* zglPTextureManager      */
#define MANAGER_FONT    11 /* zglPFontManager         */
#define MANAGER_RTARGET 12 /* zglPRenderTargetManager */
#define MANAGER_SOUND   13 /* zglPSoundManager        */

extern DWORD ( APIENTRY *zgl_Get )( DWORD What );
extern void ( APIENTRY *zgl_GetMem )( void** Ptr, DWORD Size );

#define COLOR_BUFFER_CLEAR   0x000001
#define DEPTH_BUFFER         0x000002
#define DEPTH_BUFFER_CLEAR   0x000004
#define DEPTH_MASK           0x000008
#define STENCIL_BUFFER_CLEAR 0x000010
#define CORRECT_RESOLUTION   0x000020
#define APP_USE_AUTOPAUSE    0x000040
#define APP_USE_AUTOMINIMIZE 0x000080
#define APP_USE_LOG          0x000100
#define SND_CAN_PLAY         0x000200
#define SND_CAN_PLAY_FILE    0x000400
#define CROP_INVISIBLE       0x000800

extern void ( APIENTRY *zgl_Enable )( DWORD What );
extern void ( APIENTRY *zgl_Disable )( DWORD What );

/* LOG */
extern void ( APIENTRY *log_Add )( const char* Message, bool Timings );
  
/* WINDOW */
extern void ( APIENTRY *wnd_SetCaption )( const char* NewCaption );
extern void ( APIENTRY *wnd_SetSize )( WORD Width, WORD Height );
extern void ( APIENTRY *wnd_SetPos )( WORD X, WORD Y );
extern void ( APIENTRY *wnd_SetOnTop )( bool OnTop );
extern void ( APIENTRY *wnd_ShowCursor )( bool Show );
  
/* SCREEN */
typedef struct
{
  int  Count;
  int *Width;
  int *Height;
} zglTResolutionList, *zglPResolutionList;

#define REFRESH_MAXIMUM 0
#define REFRESH_DEFAULT 1

extern void ( APIENTRY *scr_Clear )(void);
extern void ( APIENTRY *scr_Flush )(void);
extern void ( APIENTRY *scr_SetVSync )( bool VSync );
/* ВНИМАНИЕ: Функция уничтожает контекст OpenGL, что потребует перезагрузку ресурсов */
extern void ( APIENTRY *scr_SetFSAA )( byte FSAA );
extern void ( APIENTRY *scr_SetOptions )( WORD Width, WORD Height, WORD BPP, WORD Refresh, bool FullScreen, bool VSync );
extern void ( APIENTRY *scr_CorrectResolution )( WORD Width, WORD Height );

/* INI */
extern void ( APIENTRY *ini_LoadFromFile )( const char* FileName );
extern void ( APIENTRY *ini_SaveToFile )( const char* FileName );
extern void ( APIENTRY *ini_Add )( const char* Section, const char* Key );
extern char* ( APIENTRY *ini_ReadKeyStr )( const char* Section, const char* Key );
extern int ( APIENTRY *ini_ReadKeyInt )( const char* Section, const char* Key );
extern bool ( APIENTRY *ini_ReadKeyBool )( const char* Section, const char* Key );
extern bool ( APIENTRY *ini_WriteKeyStr )( const char* Section, const char* Key, const char* Value );
extern bool ( APIENTRY *ini_WriteKeyInt )( const char* Section, const char* Key, int Value );
extern bool ( APIENTRY *ini_WriteKeyBool )( const char* Section, const char* Key, bool Value );
  
/* TIMERS */
typedef struct
{
  bool   Active;
  DWORD  Interval;
  double LastTick;
  void*  OnTimer;

  void*  Prev;
  void*  Next;
} zglTTimer, *zglPTimer;

typedef struct
{
  DWORD     Count;
  zglTTimer First;
} zglTTimerManager, *zglPTimerManager;


extern zglPTimer ( APIENTRY *timer_Add )( void* OnTimer, DWORD Interval );
extern void      ( APIENTRY *timer_Del )( zglPTimer Timer );
extern double    ( APIENTRY *timer_GetTicks )(void);
  
/* KEYBOARD */
#define K_BACKSPACE 8
#define K_TAB       9
#define K_ENTER     13
#define K_SHIFT     16
#define K_SHIFT_L   160
#define K_SHIFT_R   161
#define K_CTRL      17
#define K_CTRL_L    162
#define K_CTRL_R    163
#define K_ALT       18
#define K_ALT_L     164
#define K_ALT_R     165
#define K_PAUSE     19
#define K_ESCAPE    27
#define K_SPACE     32

#define K_PAGEUP    33
#define K_PAGEDOWN  34
#define K_END       35
#define K_HOME      36
#define K_SNAPSHOT  44
#define K_INSERT    45
#define K_DELETE    46

#define K_LEFT      37
#define K_UP        38
#define K_RIGHT     39
#define K_DOWN      40

#define K_0         48
#define K_1         49
#define K_2         50
#define K_3         51
#define K_4         52
#define K_5         53
#define K_6         54
#define K_7         55
#define K_8         56
#define K_9         57

#define K_NUMPAD0   96
#define K_NUMPAD1   97
#define K_NUMPAD2   98
#define K_NUMPAD3   99
#define K_NUMPAD4   100
#define K_NUMPAD5   101
#define K_NUMPAD6   102
#define K_NUMPAD7   103
#define K_NUMPAD8   104
#define K_NUMPAD9   105

#define K_MULTIPLY  106
#define K_ADD       107
#define K_SEPARATOR 108
#define K_SUBTRACT  109
#define K_DECIMAL   110
#define K_DIVIDE    111

#define K_A         65
#define K_B         66
#define K_C         67
#define K_D         68
#define K_E         69
#define K_F         70
#define K_G         71
#define K_H         72
#define K_I         73
#define K_J         74
#define K_K         75
#define K_L         76
#define K_M         77
#define K_N         78
#define K_O         79
#define K_P         80
#define K_Q         81
#define K_R         82
#define K_S         83
#define K_T         84
#define K_U         85
#define K_V         86
#define K_W         87
#define K_X         88
#define K_Y         89
#define K_Z         90

#define K_F1        112
#define K_F2        113
#define K_F3        114
#define K_F4        115
#define K_F5        116
#define K_F6        117
#define K_F7        118
#define K_F8        119
#define K_F9        120
#define K_F10       121
#define K_F11       122
#define K_F12       123

#define KA_DOWN     0
#define KA_UP       1
extern bool ( APIENTRY * key_Down )( byte KeyCode );
extern bool ( APIENTRY * key_Up )( byte KeyCode );
extern byte  ( APIENTRY * key_Last )( byte KeyAction );
extern void ( APIENTRY * key_BeginReadText )( const char* Text, WORD MaxSymbols );
extern char* ( APIENTRY * key_EndReadText )(void);
extern void ( APIENTRY * key_ClearState )(void);
  
/* MOUSE */
#define M_BLEFT  0
#define M_BMIDLE 1
#define M_BRIGHT 2
#define M_WUP    0
#define M_WDOWN  1

extern int  ( APIENTRY *mouse_X )(void);
extern int  ( APIENTRY *mouse_Y )(void);
extern int  ( APIENTRY *mouse_DX )(void);
extern int  ( APIENTRY *mouse_DY )(void);
extern bool ( APIENTRY *mouse_Down )( byte Button );
extern bool ( APIENTRY *mouse_Up )( byte Button );
extern bool ( APIENTRY *mouse_Click )( byte Button );
extern bool ( APIENTRY *mouse_Wheel )( byte Axis );
extern void ( APIENTRY *mouse_ClearState )(void);
extern void ( APIENTRY *mouse_Lock       )(void);
  
/* GL */
extern void ( APIENTRY *Set2DMode )(void);
extern void ( APIENTRY *Set3DMode )( float FOVY );
  
/* TEXTURES */
typedef struct
{
  DWORD  ID;
  WORD   Width;
  WORD   Height;
  float  U;
  float  V;
  WORD   FramesX;
  WORD   FramesY;
  DWORD  Flags;

  void*  Prev;
  void*  Next;
} zglTTexture, *zglPTexture;

typedef struct
{
  DWORD       Count;
  zglTTexture First;
} zglTTextureManager, *zglPTextureManager;

#define TEX_MIPMAP            0x000001
#define TEX_CLAMP             0x000002
#define TEX_REPEAT            0x000004
#define TEX_COMPRESS          0x000008
#define TEX_CONVERT_TO_POT    0x000010

#define TEX_GRAYSCALE         0x000020
#define TEX_INVERT            0x000040
#define TEX_USEMASK           0x000080

#define TEX_FILTER_NEAREST    0x000100
#define TEX_FILTER_LINEAR     0x000200
#define TEX_FILTER_BILINEAR   0x000400
#define TEX_FILTER_TRILINEAR  0x000800
#define TEX_FILTER_ANISOTROPY 0x001000

#define TEX_RGB               0x002000

#define TEX_QUALITY_LOW       0x400000
#define TEX_QUALITY_MEDIUM    0x800000

#define TEX_DEFAULT_2D        TEX_CLAMP | TEX_CONVERT_TO_POT | TEX_FILTER_LINEAR

extern zglPTexture ( APIENTRY *tex_Add )(void);
extern void        ( APIENTRY *tex_Del )( zglPTexture Texture );
extern void        ( APIENTRY *tex_Create )( zglTTexture *Texture, void* pData );
extern zglPTexture ( APIENTRY *tex_CreateZero )( WORD Width, WORD Height, DWORD Color, DWORD Flags );
extern zglPTexture ( APIENTRY *tex_LoadFromFile )( const char* FileName, DWORD TransparentColor, DWORD Flags );
extern void        ( APIENTRY *tex_SetFrameSize )( zglPTexture Texture, WORD FrameWidth, WORD FrameHeight );
extern zglPTexture ( APIENTRY *tex_SetMask )( zglPTexture Texture, zglPTexture Mask );
extern void        ( APIENTRY *tex_GetData )( zglPTexture Texture, void** pData, int *pSize );
extern void        ( APIENTRY *tex_Filter )( zglPTexture Texture, DWORD Flags );
extern void        ( APIENTRY *tex_SetAnisotropy )( byte Level );

/* RENDER TARGETS */
typedef struct
{
  DWORD FrameBuffer;
  DWORD RenderBuffer;
} zglTFBO, *zglPFBO;

#ifdef win32
typedef struct
{
  HANDLE Handle;
  HDC    DC;
  HGLRC  RC;
} zglTPBuffer, *zglPPBuffer;
#endif

typedef struct
{
  byte        rtType;
  void*       Handle;
  zglPTexture Surface;
  byte        Flags;

  void*       Prev;
  void*       Next;
} zglTRenderTarget, *zglPRenderTarget;

typedef struct
{
  DWORD            Count;
  zglTRenderTarget First;
} zglTRenderTargetManager, *zglPRenderTargetManager;

#define RT_TYPE_SIMPLE  0
#define RT_TYPE_FBO     1
#define RT_TYPE_PBUFFER 2
#define RT_FULL_SCREEN  0x01
#define RT_CLEAR_SCREEN 0x02

extern zglPRenderTarget ( APIENTRY *rtarget_Add )( byte rtType, zglPTexture Surface, byte Flags );
extern void             ( APIENTRY *rtarget_Del )( zglPRenderTarget Target );
extern void             ( APIENTRY *rtarget_Set )( zglPRenderTarget Target );
  
/* 2D */
typedef struct zglTPoint2D
{
  float X;
  float Y;
} zglTPoint2D, *zglPPoint2D;

typedef struct
{
  float x0;
  float y0;
  float x1;
  float y1;
} zglTLine, *zglPLine;

typedef struct
{
  float x;
  float y;
  float w;
  float h;
} zglTRect, *zglPRect;

typedef struct
{
  float cX;
  float cY;
  float radius;
} zglTCircle, *zglPCircle;

typedef struct
{
  DWORD Count;
  float cX;
  float cY;
  zglTPoint2D *Points;
} zglTPolyLine, *zglPPolyLine;

/* FX */
#define FX_BLEND_NORMAL 0x00
#define FX_BLEND_ADD    0x01
#define FX_BLEND_MULT   0x02
#define FX_BLEND_BLACK  0x03
#define FX_BLEND_WHITE  0x04
#define FX_BLEND_MASK   0x05

extern void ( APIENTRY *fx_SetBlendMode )( byte Mode );
  
/* FX 2D */
#define FX2D_FLIPX    0x000001
#define FX2D_FLIPY    0x000002
#define FX2D_COLORMIX 0x000004
#define FX2D_VCA      0x000008
#define FX2D_VCHANGE  0x000010
#define FX2D_SCALE    0x000020

#define FX_BLEND      0x000040

extern void ( APIENTRY *fx2d_SetColorMix )( DWORD Color );
extern void ( APIENTRY *fx2d_SetVCA )( DWORD c1, DWORD c2, DWORD c3, DWORD c4, byte a1, byte a2, byte a3, byte a4 );
extern void ( APIENTRY *fx2d_SetVertexes )( float x1, float y1, float x2, float y2, float x3, float y3, float x4, float y4 );
extern void ( APIENTRY *fx2d_SetScale )( float scaleX, float scaleY );
  
/* Camera 2D */
typedef struct
{
  float X;
  float Y;
  float Angle;
} zglTCamera2D, *zglPCamera2D;

extern void ( APIENTRY *cam2d_Set )( zglPCamera2D Camera );
  
/* Primitives 2D */
#define PR2D_FILL   0x000001
#define PR2D_SMOOTH 0x000002

extern void ( APIENTRY *pr2d_Pixel )( float X, float Y, DWORD Color, byte Alpha );
extern void ( APIENTRY *pr2d_Line )( float X1, float Y1, float X2, float Y2, DWORD Color, byte Alpha, DWORD FX );
extern void ( APIENTRY *pr2d_Rect )( float X, float Y, float W, float H, DWORD Color, byte Alpha, DWORD FX );
extern void ( APIENTRY *pr2d_Circle )( float X, float Y, float Radius, DWORD Color, byte Alpha, WORD Quality, DWORD FX );
extern void ( APIENTRY *pr2d_Ellipse )( float X, float Y, float xRadius, float yRadius, DWORD Color, byte Alpha, WORD Quality, DWORD FX );
  
/* Sprites 2D */
extern void ( APIENTRY *ssprite2d_Draw )( zglPTexture Texture, float X, float Y, float W, float H, float Angle, byte Alpha, DWORD FX );
extern void ( APIENTRY *asprite2d_Draw )( zglPTexture Texture, float X, float Y, float W, float H, float Angle, WORD Frame, byte Alpha, DWORD FX );
extern void ( APIENTRY *csprite2d_Draw )( zglPTexture Texture, float X, float Y, float W, float H, float Angle, zglTRect CutRect, byte Alpha, DWORD FX );
  
/* Text */
typedef struct
{
  zglPTexture Texture;
  byte        Height;
  byte        Width[256];
  zglTPoint2D TexCoords[256][4];

  void*       Prev;
  void*       Next;
} zglTFont, *zglPFont;

typedef struct
{
  DWORD    Count;
  zglTFont First;
} zglTFontManager, *zglPFontManager;

extern zglPFont ( APIENTRY *font_Add )(void);
extern void     ( APIENTRY *font_Del )( zglPFont Font );
extern zglPFont ( APIENTRY *font_LoadFromFile )( const char* Texture, const char* FontInfo );
extern void     ( APIENTRY *text_Draw )( zglPFont Font, float X, float Y, const char* Text, byte Alpha, DWORD Color, float Step, float Scale );
extern float    ( APIENTRY *text_GetWidth )( zglPFont Font, const char* Text, float Step, float Scale );

/* Sound */
typedef struct
{
  DWORD Buffer;
  DWORD sCount;
  int*  Source;

  void* Data;
  DWORD Size;
  DWORD Frequency;

  void* Prev;
  void* Next;
} zglTSound, *zglPSound;

typedef struct
{
  DWORD _File;
  void* CodecRead; /* DWORD func( void* Buffer, DWORD Count ) */
  void* CodecLoop; /* void func(void) */
  DWORD Rate;
  DWORD Channels;
  void* Buffer;
  DWORD BufferSize;
  bool  Loop;
  bool  Played;
} zglTSoundFile, *zglPSoundFile;

typedef struct
{
  DWORD     Count;
  zglTSound Sound;
} zglTSoundManager, *zglPSoundManager;

extern bool      ( APIENTRY *snd_Init )(void);
extern void      ( APIENTRY *snd_Free )(void);
extern zglPSound ( APIENTRY *snd_Add )( int BufferCount, int SourceCount );
extern void      ( APIENTRY *snd_Del )( zglPSound Sound );
extern zglPSound ( APIENTRY *snd_LoadFromFile )( char* FileName, int SourceCount );
extern int       ( APIENTRY *snd_Play )( zglPSound Sound, float X, float Y, float Z, bool Loop );
extern void      ( APIENTRY *snd_Stop )( zglPSound Sound, int Source );
extern void      ( APIENTRY *snd_SetVolume )( byte Volume, int ID );
extern void      ( APIENTRY *snd_SetFrequency )( int Frequency, int ID );
extern void      ( APIENTRY *snd_SetFrequencyCoeff )( float Coefficient, int ID );
extern void      ( APIENTRY *snd_PlayFile )( zglPSoundFile SoundFile );
extern void      ( APIENTRY *snd_StopFile )(void);
extern void      ( APIENTRY *snd_RestoreFile )(void);

/* 3D */
typedef struct
{
  float X;
  float Y;
  float Z;
} zglTPoint3D, *zglPPoint3D;

typedef struct
{
  float X;
  float Y;
  float Z;
  float W;
} zglTQuaternion, *zglPQuaternion;

/*typedef zglTPoint3D *zglTMatrix3f[3];*/
/*typedef zglTMatrix3f *zglPMatrix3f;*/
typedef struct
{
  float a11, a12, a13;
  float a21, a22, a23;
  float a31, a32, a33;
} zglTMatrix3f, *zglPMatrix3f;
  
/*typedef float zglTMatrix4f[3][3];*/
/*typedef zglTMatrix4f *zglPMatrix4f;*/
typedef struct
{
  float a11, a12, a13, a14;
  float a21, a22, a23, a24;
  float a31, a32, a33, a34;
  float a41, a42, a43, a44;
} zglTMatrix4f, *zglPMatrix4f;

typedef struct
{
  DWORD a;
  DWORD b;
  DWORD c;
} zglTFace, *zglPFace;

typedef struct
{
  DWORD FCount;
  DWORD IFace;
  void* Indices;
} zglTGroup, *zglPGroup;

typedef struct
{
  zglTPoint3D *Vertices;
  zglTPoint3D *Normals;
} zglTFrame, *zglPFrame;

typedef struct
{
  zglTPoint3D p1;
  zglTPoint3D p2;
} zglTLine3D, *zglPLine3D;

typedef struct
{
  zglTPoint3D Points[3];
  float       D;
  zglTPoint3D Normal;
} zglTPlane, *zglPPlane;

typedef struct
{
  zglTPoint3D Position;
  zglTPoint3D Size;
} zglTAABB, *zglPAABB;

typedef struct
{
  zglTPoint3D  Position;
  zglTPoint3D  Size;
  zglTMatrix3f Matrix;
} zglTOBB, *zglPOBB;

typedef struct
{
  zglTPoint3D Position;
  float       Radius;
} zglTSphere, *zglPSphere;
  
/* Z BUFFER */
extern void ( APIENTRY *zbuffer_SetDepth )( float zNear, float zFar );
extern void ( APIENTRY *zbuffer_Clear )(void);
  
/* SCISSOR */
extern void ( APIENTRY *scissor_Begin )( WORD X, WORD Y, WORD Width, WORD Height );
extern void ( APIENTRY *scissor_End )(void);
  
/* OBJECT 3D */
#define OBJ3D_TEXTURING     0x0000001
#define OBJ3D_MTEXTURING    0x0000002
#define OBJ3D_BLEND         0x0000004
#define OBJ3D_ALPHA_TEST    0x0000008
#define OBJ3D_WIRE_FRAME    0x0000010
#define OBJ3D_CULL_FACE     0x0000020
#define OBJ3D_LIGHTING      0x0000040
#define OBJ3D_SPHERE_MAP_S  0x0000080
#define OBJ3D_SPHERE_MAP_T  0x0000100

#define MAT_DIFFUSE         0x01
#define MAT_AMBIENT         0x02
#define MAT_SPECULAR        0x03
#define MAT_SHININESS       0x04
#define MAT_EMISSION        0x05

#define SIDE_FRONT          0x01
#define SIDE_BACK           0x02
#define SIDE_FRONT_AND_BACK 0x03

/* HeigthMap, Static Mesh, VBO, Octree */
#define USE_NORMALS        0x001
#define USE_TEXTURE        0x002
#define USE_MULTITEX1      0x004
#define USE_MULTITEX2      0x008
#define USE_MULTITEX3      0x010
#define USE_VBO            0x020
#define BUILD_SNORMALS     0x040
#define BUILD_PLANES       0x080
#define BUILD_VBO_STATIC   0x100
#define BUILD_VBO_STREAM   0x200
  
extern void ( APIENTRY *obj3d_Begin )( DWORD Flags );
extern void ( APIENTRY *obj3d_End )(void);
extern void ( APIENTRY *obj3d_Enable )( DWORD Flags );
extern void ( APIENTRY *obj3d_Disable )( DWORD Flags );
extern void ( APIENTRY *obj3d_SetColor )( DWORD Color, byte Alpha );
extern void ( APIENTRY *obj3d_BindTexture )( zglPTexture Texture, byte Level );
extern void ( APIENTRY *obj3d_SetMaterial )( byte Material, byte Side, DWORD Color, byte Alpha );
extern void ( APIENTRY *obj3d_Scale )( float ScaleX, float ScaleY, float ScaleZ );
extern void ( APIENTRY *obj3d_Move )( float X, float Y, float Z );
extern void ( APIENTRY *obj3d_SetMatrix )( zglTMatrix4f Matrix );
extern void ( APIENTRY *obj3d_MulMatrix )( zglTMatrix4f Matrix );

#define AX 0x01
#define AY 0x02
#define AZ 0x04

extern void ( APIENTRY *obj3d_Rotate )( float Angle, byte Axis );

/* PRIMITIVES 3D */
extern void ( APIENTRY *pr3d_Point )( float X, float Y, float Z );
extern void ( APIENTRY *pr3d_Line )( float X1, float Y1, float Z1, float X2, float Y2, float Z2 );
extern void ( APIENTRY *pr3d_Plane )( float Width, float Height );
extern void ( APIENTRY *pr3d_AABB )( float Width, float Height, float ZDepth );
extern void ( APIENTRY *pr3d_Sphere )( float Radius, int Quality );

/* SPRITE 3D */
extern void ( APIENTRY *ssprite3d_Draw )( float X, float Y, float Z, float sX, float sY, float sZ, zglPMatrix4f Matrix );
extern void ( APIENTRY *asprite3d_Draw )( float X, float Y, float Z, float sX, float sY, float sZ, int Frame, zglPMatrix4f Matrix );

/* CAMERA 3D */
typedef struct
{
  zglTPoint3D  Position;
  zglTPoint3D  Rotation;
  zglTMatrix4f Matrix;
} zglTCamera3D, *zglPCamera3D;

extern void ( APIENTRY *cam3d_Set )( zglPCamera3D Camera );
extern void ( APIENTRY *cam3d_Fly )( zglPCamera3D Camera, float Speed );
extern void ( APIENTRY *cam3d_Strafe )( zglPCamera3D Camera, float Speed );
  
/* SIMPLE MESH */
typedef struct
{
  char* Name;
  float FPS;
  DWORD FCount;
  DWORD FFrame;
} zglTSimpleAction, *zglPSimpleAction;

typedef struct
{
  DWORD       VBuffer;
  int         Action;
  int         Frame;
  float       Delta;
  float       prevDelta;
  double      Time;
  zglTPoint3D *Vertices;
  zglTPoint3D *Normals;
} zglTSimpleState, *zglPSimpleState;

typedef struct
{
  DWORD Flags;

  DWORD IBuffer;
  DWORD VBuffer;

  DWORD VCount;
  DWORD RVCount;
  DWORD TCount;
  DWORD FCount;
  DWORD GCount;
  DWORD ACount;
  DWORD Frames;

  zglTPoint3D *Vertices;
  zglTPoint3D *Normals;
  zglTPoint2D *TexCoords;
  zglTPoint2D *MultiTexCoords;
  zglTFace    *Faces;
  void*       Indices;
  DWORD       *RIndices;
  zglTGroup   *Groups;

  zglTSimpleState  State;
  zglTSimpleAction *Actions;
} zglTSMesh, *zglPSMesh;

extern bool ( APIENTRY *smesh_LoadFromFile )( zglPSMesh *Mesh, char* FileName, DWORD Flags );
extern void ( APIENTRY *smesh_Animate )( zglPSMesh Mesh, zglPSimpleState State );
extern void ( APIENTRY *smesh_Draw )( zglPSMesh Mesh, zglPSimpleState State );
extern void ( APIENTRY *smesh_DrawGroup )( zglPSMesh Mesh, zglPSimpleState State, DWORD Group );
extern void ( APIENTRY *smesh_Free )( zglPSMesh *Mesh );

/* SKINNED MESH */
typedef struct
{
  char* Name;
  int   Parent;
} zglTBone, *zglPBone;

typedef struct
{
  int   boneID;
  float Weight;
} zglTBoneWeight, *zglPBoneWeight;

typedef zglTBoneWeight **zglTBonesWeights;

typedef struct
{
  zglTPoint3D    Point;
  zglTPoint3D    Translation;
  zglTPoint3D    Rotation;
  zglTMatrix4f   Matrix;
  zglTQuaternion Quaternion;
} zglTBonePos, *zglPBonePos;

typedef struct
{
  DWORD             VBuffer;
  int               Action;
  int               Frame;
  float             Delta;
  float             prevDelta;
  double            Time;
  zglTPoint3D       *Vertices;
  zglTPoint3D       *Normals;
  zglTBonePos       *BonePos;
} zglTSkeletonState, *zglPSkeletonState;

typedef struct
{
  char*       Name;
  float       FPS;
  DWORD       FCount;
  zglTBonePos **Frames;
} zglTSkeletonAction, *zglPSkeletonAction;

typedef struct
{
  DWORD              Flags;

  DWORD              IBuffer;
  DWORD              VBuffer;

  DWORD              VCount;
  DWORD              RVCount;
  DWORD              TCount;
  DWORD              FCount;
  DWORD              GCount;
  DWORD              BCount;
  byte               *WCount;
  DWORD              ACount;

  zglTPoint3D        *Vertices;
  zglTPoint3D        *Normals;
  zglTPoint2D        *TexCoords;
  zglTPoint2D        *MultiTexCoords;
  zglTFace           *Faces;
  void*              Indices;
  DWORD              *RIndices;
  zglTGroup          *Groups;

  zglTBone           *Bones;
  zglTBonePos        *BonePos;
  zglTBonesWeights   Weights;
  zglTSkeletonState  State;
  zglTSkeletonAction *Actions;
} zglTSkMesh, *zglPSkMesh;

extern bool ( APIENTRY *skmesh_LoadFromFile )( zglPSkMesh *Mesh, char *FileName, DWORD Flags );
extern void ( APIENTRY *skmesh_Animate )( zglPSkMesh Mesh, zglPSkeletonState State );
extern void ( APIENTRY *skmesh_Draw )( zglPSkMesh Mesh, zglPSkeletonState State );
extern void ( APIENTRY *skmesh_DrawGroup )( zglPSkMesh Mesh, zglPSkeletonState State, DWORD Group );
extern void ( APIENTRY *skmesh_DrawSkelet )( zglPSkMesh Mesh, zglPSkeletonState State );
extern void ( APIENTRY *skmesh_Free )( zglPSkMesh *Mesh );

/* HEIGHTMAP */
typedef struct
{
  DWORD       Flags;

  WORD       Width;
  WORD       Height;
  float      xScale;
  float      zScale;

  DWORD       IBuffer;
  DWORD       VBuffer;

  DWORD       VCount;
  DWORD       TCount;
  DWORD       FCount;
  DWORD       ICount;
  DWORD       PCount;
    
  zglTPoint3D *Vertices;
  zglTPoint3D *Normals;
  zglTPoint2D *TexCoords;
  zglTPoint2D *MultiTexCoords;
  zglTFace    *Faces;
  void*       Indices;
  zglTPlane   *Planes;
} zglTHeightMap, *zglPHeightMap;

extern void ( APIENTRY *heightmap_Build )( zglPHeightMap *Heightmap, zglPTexture Texture, float xScale, float yScale, float zScale, int xDetail, int yDetail, DWORD Flags );
extern void ( APIENTRY *heightmap_Draw )( zglPHeightMap HeightMap );
extern void ( APIENTRY *heightmap_Free )( zglPHeightMap *HeightMap );
extern int ( APIENTRY *heightmap_GetPlane )( zglPHeightMap HeightMap, zglTPoint3D Pos );
extern float ( APIENTRY *heightmap_GetYOffset )( zglPHeightMap HeightMap, zglTPoint3D Pos );

/* VBO */
extern void ( APIENTRY *vbo_Build )( DWORD *IBuffer, DWORD *VBuffer, DWORD ICount, DWORD VCount, void* Indices, void* Vertices, void* Normals, void* TexCoords, void* MultiTexCoords, DWORD *Flags );
extern void ( APIENTRY *vbo_Free )( DWORD *IBuffer, DWORD *VBuffer, DWORD ICount, DWORD VCount, void* Indices, void* Vertices );

/* FRUSTUM */
typedef float zglTFrustum[6][4];
typedef zglTFrustum *zglPFrustum;
  
extern void ( APIENTRY *frustum_Calc )( zglPFrustum f );
extern bool ( APIENTRY *frustum_PointIn )( zglPFrustum f, float x, float y, float z );
extern bool ( APIENTRY *frustum_PPointIn )( zglPFrustum f, zglPPoint3D Vertex );
extern bool ( APIENTRY *frustum_TriangleIn )( zglPFrustum f, zglTPoint3D v1, zglTPoint3D v2, zglTPoint3D v3 );
extern bool ( APIENTRY *frustum_SphereIn )( zglPFrustum f, float x, float y, float z, float r );
extern bool ( APIENTRY *frustum_BoxIn )( zglPFrustum f, float x, float y, float z, float bx, float by, float bz );
extern bool ( APIENTRY *frustum_CubeIn )( zglPFrustum f, float x, float y, float z, float size );
  
/* OCTREE */
typedef struct
{
  DWORD Texture;
  DWORD ICount;
  void* Indices;
  DWORD IBuffer;
  DWORD IBType;
} zglTRenderData, *zglPRenderData;

typedef struct
{
  zglTAABB Cube;

  DWORD          RDSize;
  zglTRenderData *RenderData;
  DWORD          DFCount;
  DWORD          *DFaces;
  DWORD          PCount;
  DWORD          *Planes;

  bool           NInside;
  void*          SubNodes[8];
} zglTNode, *zglPNode;

typedef struct
{
  DWORD Flags;

  DWORD IBuffer;
  DWORD VBuffer;

  zglPNode MainNode;

  DWORD VCount;
  DWORD TCount;
  DWORD FCount;
  DWORD ICount;

  zglTPoint3D *Vertices;
  zglTPoint3D *TexCoords;
  zglTPoint2D *MultiTexCoords;
  zglTPoint3D *Normals;
  zglTFace    *Faces;
  void*       Indices;
  zglTPlane   *Planes;
  DWORD       *Textures;

  DWORD MaxDFaces;
  DWORD *DFaces;

  DWORD *r_DFacesAlready;
  DWORD r_DFacesCount;
  DWORD r_DFacesACount ;

  DWORD r_NodeACount;
} zglTOctree, *zglPOctree;

extern void ( APIENTRY *octree_Build )( zglPOctree Octree, DWORD MaxFacesPerNode, DWORD Flags );
extern void ( APIENTRY *octree_Free )( zglPOctree Octree );
extern void ( APIENTRY *octree_Draw )( zglPOctree Octree, zglPFrustum Frustum );
extern void ( APIENTRY *octree_DrawDebug )( zglPOctree Octree, zglPFrustum Frustum );
extern void ( APIENTRY *octree_DrawNode )( zglPOctree Octree, zglPNode Node, zglPFrustum Frustum );
  
/*/ LIGHT */
extern void ( APIENTRY *light_Enable )( byte ID );
extern void ( APIENTRY *light_Disable )( byte ID );
extern void ( APIENTRY *light_SetPosition )( byte ID, float X, float Y, float Z, float W );
extern void ( APIENTRY *light_SetMaterial )( byte ID, byte Material, DWORD Color, byte Alpha );

  
/* FOG */
#define FOG_MODE_EXP    0
#define FOG_MODE_EXP2   1
#define FOG_MODE_LINEAR 2

extern void ( APIENTRY *fog_Enable )(void);
extern void ( APIENTRY *fog_Disable )(void);
extern void ( APIENTRY *fog_SetMode )( byte Mode );
extern void ( APIENTRY *fog_SetColor )( DWORD Color );
extern void ( APIENTRY *fog_SetDensity )( float Density );
extern void ( APIENTRY *fog_SetBeginEnd )( float fBegin, float fEnd );
  
/* SKYBOX */
extern void ( APIENTRY *skybox_Init )( zglPTexture Top, zglPTexture Bottom, zglPTexture Left, zglPTexture Right, zglPTexture Front, zglPTexture Back );
extern void ( APIENTRY *skybox_Draw )(void);
  
/* SHADERS */
#define SHADER_ARB          0
#define SHADER_GLSL         1
#define SHADER_VERTEX_ARB   0x8620
#define SHADER_FRAGMENT_ARB 0x8804
#define SHADER_VERTEX       0x8B31
#define SHADER_FRAGMENT     0x8B30

/* ARBfp/ARBvp */
extern bool ( APIENTRY *shader_InitARB )(void);
extern DWORD ( APIENTRY *shader_LoadFromFileARB )( const char* FileName, DWORD ShaderType );
extern void ( APIENTRY *shader_BeginARB )( DWORD Shader, DWORD ShaderType );
extern void ( APIENTRY *shader_EndARB )( DWORD ShaderType );
extern void ( APIENTRY *shader_FreeARB )( DWORD Shader );

/* GLSL */
extern bool ( APIENTRY *shader_InitGLSL )(void);
extern DWORD ( APIENTRY *shader_LoadFromFile )( const char* FileName, int ShaderType, bool Link );
extern void ( APIENTRY *shader_Attach )( DWORD Attach );
extern void ( APIENTRY *shader_BeginLink )(void);
extern DWORD ( APIENTRY *shader_EndLink )(void);
extern void ( APIENTRY *shader_Begin )( DWORD Shader );
extern void ( APIENTRY *shader_End )(void);
extern void ( APIENTRY *shader_Free )( DWORD Shader );
extern int ( APIENTRY *shader_GetUniform )( DWORD Shader, const char* UniformName );
extern void ( APIENTRY *shader_SetUniform1f )( int Uniform, float v1 );
extern void ( APIENTRY *shader_SetUniform1i )( int Uniform, int v1 );
extern void ( APIENTRY *shader_SetUniform2f )( int Uniform, float v1, float v2 );
extern void ( APIENTRY *shader_SetUniform3f )( int Uniform, float v1, float v2, float v3 );
extern void ( APIENTRY *shader_SetUniform4f )( int Uniform, float v1, float v2, float v3, float v4 );
extern int ( APIENTRY *shader_GetAttrib )( DWORD Shader, const char* AttribName );
/* glVertexAttrib* GLSL/ARB */
extern void ( APIENTRY *shader_SetAttrib1f )( int Attrib, float v1 );
extern void ( APIENTRY *shader_SetAttrib2f )( int Attrib, float v1, float v2 );
extern void ( APIENTRY *shader_SetAttrib3f )( int Attrib, float v1, float v2, float v3 );
extern void ( APIENTRY *shader_SetAttrib4f )( int Attrib, float v1, float v2, float v3, float v4 );
extern void ( APIENTRY *shader_SetAttribPf )( int Attrib, void* v, bool Normalized );
extern void ( APIENTRY *shader_SetParameter4f )( DWORD ShaderType, int Parameterm, float v1, float v2, float v3, float v4 );
  
/* MATH */
extern int ( APIENTRY *m_Round )( float value );
extern float ( APIENTRY *m_Cos )( int Angle );
extern float ( APIENTRY *m_Sin )( int Angle );
extern float ( APIENTRY *m_Distance )( float x1, float y1, float x2, float y2 );
extern float ( APIENTRY *m_FDistance )( float x1, float y1, float x2, float y2 );
extern float ( APIENTRY *m_Angle )( float x1, float y1, float x2, float y2 );
  /* vectros */
extern zglTPoint3D ( APIENTRY *vector_Get )( float x, float y, float z );
extern zglTPoint3D ( APIENTRY *vector_Add )( zglTPoint3D Vector1, zglTPoint3D Vector2 );
extern zglTPoint3D ( APIENTRY *vector_Sub )( zglTPoint3D Vector1, zglTPoint3D Vector2 );
extern zglTPoint3D ( APIENTRY *vector_Mul )( zglTPoint3D Vector1, zglTPoint3D Vector2 );
extern zglTPoint3D ( APIENTRY *vector_Div )( zglTPoint3D Vector1, zglTPoint3D Vector2 );
extern zglTPoint3D ( APIENTRY *vector_AddV )( zglTPoint3D Vector, float Value );
extern zglTPoint3D ( APIENTRY *vector_SubV )( zglTPoint3D Vector, float Value );
extern zglTPoint3D ( APIENTRY *vector_MulV )( zglTPoint3D Vector, float Value );
extern zglTPoint3D ( APIENTRY *vector_DivV )( zglTPoint3D Vector, float Value );
extern zglTPoint3D ( APIENTRY *vector_MulM3f )( zglTPoint3D Vector, zglTMatrix3f Matrix );
extern zglTPoint3D ( APIENTRY *vector_MulM4f )( zglTPoint3D Vector, zglTMatrix4f Matrix );
extern zglTPoint3D ( APIENTRY *vector_MulInvM4f )( zglTPoint3D Vector, zglTMatrix4f Matrix );
extern zglTPoint3D ( APIENTRY *vector_Negate )( zglTPoint3D Vector );
extern zglTPoint3D ( APIENTRY *vector_Normalize )( zglTPoint3D Vector );
extern float       ( APIENTRY *vector_Angle )( zglTPoint3D Vector1, zglTPoint3D Vector2 );
extern zglTPoint3D ( APIENTRY *vector_Cross )( zglTPoint3D Vector1, zglTPoint3D Vector2 );
extern float       ( APIENTRY *vector_Dot )( zglTPoint3D Vector1, zglTPoint3D Vector2 );
extern float       ( APIENTRY *vector_Distance )( zglTPoint3D Vector1, zglTPoint3D Vector2 );
extern float       ( APIENTRY *vector_FDistance )( zglTPoint3D Vector1, zglTPoint3D Vector2 );
extern float       ( APIENTRY *vector_Length )( zglTPoint3D Vector );
extern zglTPoint3D ( APIENTRY *vector_Lerp )( zglTPoint3D Vector1, zglTPoint3D Vector2, float Value );
/* matrix */
extern zglTMatrix3f ( APIENTRY *matrix3f_Get )( zglTPoint3D v1, zglTPoint3D v2, zglTPoint3D v3 );
extern void         ( APIENTRY *matrix3f_OrthoNormalize )( zglPMatrix3f Matrix );
extern void         ( APIENTRY *matrix3f_Transpose )( zglPMatrix3f Matrix );
extern void         ( APIENTRY *matrix3f_Rotate )( zglPMatrix3f Matrix, float aX, float aY, float aZ );
extern zglTMatrix3f ( APIENTRY *matrix3f_Add )( zglTMatrix3f Matrix1, zglTMatrix3f Matrix2 );
extern zglTMatrix3f ( APIENTRY *matrix3f_Mul )( zglTMatrix3f Matrix1, zglTMatrix3f Matrix2 );
extern void         ( APIENTRY *matrix4f_Transpose )( zglPMatrix4f Matrix );
extern float        ( APIENTRY *matrix4f_Determinant )( zglTMatrix4f Matrix );
extern zglTMatrix4f ( APIENTRY *matrix4f_Inverse )( zglTMatrix4f Matrix );
extern void         ( APIENTRY *matrix4f_Translate )( zglPMatrix4f Matrix, float tX, float tY, float tZ );
extern void         ( APIENTRY *matrix4f_Rotate )( zglPMatrix4f Matrix, float aX, float aY, float aZ );
extern void         ( APIENTRY *matrix4f_Scale )( zglPMatrix4f Matrix, float sX, float sY, float sZ );
extern zglTMatrix4f ( APIENTRY *matrix4f_Mul )( zglTMatrix4f Matrix1, zglTMatrix4f Matrix2 );
/* quaternions */
extern zglTQuaternion ( APIENTRY *quater_Get )( float X, float Y, float Z, float W );
extern zglTQuaternion ( APIENTRY *quater_Add )( zglTQuaternion q1, zglTQuaternion q2 );
extern zglTQuaternion ( APIENTRY *quater_Sub )( zglTQuaternion q1, zglTQuaternion q2 );
extern zglTQuaternion ( APIENTRY *quater_Mul )( zglTQuaternion q1, zglTQuaternion q2 );
extern zglTQuaternion ( APIENTRY *quater_Negate )( zglTQuaternion Quaternion );
extern zglTQuaternion ( APIENTRY *quater_Normalize )( zglTQuaternion Quaternion );
extern float          ( APIENTRY *quater_Dot )( zglTQuaternion q1, zglTQuaternion q2 );
extern zglTQuaternion ( APIENTRY *quater_Lerp )( zglTQuaternion q1, zglTQuaternion q2, float Value );
extern zglTQuaternion ( APIENTRY *quater_FromRotation )( zglTPoint3D Rotation );
extern zglTMatrix4f   ( APIENTRY *quater_GetM4f )( zglTQuaternion Quaternion );
/* line 3d */
extern zglTPoint3D ( APIENTRY *line3d_ClosestPoint )( zglTPoint3D A, zglTPoint3D B, zglTPoint3D Point );
  /* plane */
extern zglTPlane ( APIENTRY *plane_Get )( zglTPoint3D A, zglTPoint3D B, zglTPoint3D C );
extern float ( APIENTRY *plane_Distance )( zglTPlane Plane, zglTPoint3D Point );
  /* triangle */
extern zglTPoint3D ( APIENTRY *tri_GetNormal )( zglTPoint3D A, zglTPoint3D B, zglTPoint3D C );

/* COLLISION 2D */
  /* point */
extern bool ( APIENTRY *col2d_PointInRect )( float X, float Y, zglPRect Rect );
extern bool ( APIENTRY *col2d_PointInCircle )( float X, float Y, zglPCircle Circ );
extern bool ( APIENTRY *col2d_PointInPolyLine )( float X, float Y, zglPPolyLine PL );
  /* line 2d */
extern bool ( APIENTRY *col2d_Line )( zglPLine A, zglPLine B );
extern bool ( APIENTRY *col2d_LineVsRect )( zglPLine A, zglPRect Rect );
extern bool ( APIENTRY *col2d_LineVsCircle )( zglPLine L, zglPCircle Circ );
extern bool ( APIENTRY *col2d_LineVsCircleXY )( zglPLine L, zglPCircle Circ, byte Precision );
extern bool ( APIENTRY *col2d_LineVsPolyLine )( zglPLine A, zglPPolyLine PL );
  /* polyline */
extern bool ( APIENTRY *col2d_PolyLine )( zglPPolyLine A, zglPPolyLine B );
extern bool ( APIENTRY *col2d_PolyLineVsRect )( zglPPolyLine A, zglPRect Rect );
extern bool ( APIENTRY *col2d_PolyLineVsCircle )( zglPPolyLine A, zglPCircle Circ );
extern bool ( APIENTRY *col2d_PolyLineVsCircleXY )( zglPPolyLine A, zglPCircle Circ, int Precision );
  /* rect */
extern bool ( APIENTRY *col2d_Rect )( zglPRect Rect1, zglPRect Rect2 );
extern bool ( APIENTRY *col2d_RectInRect )( zglPRect Rect1, zglPRect Rect2 );
extern bool ( APIENTRY *col2d_RectInCircle )( zglPRect Rect, zglPCircle Circ );
extern bool ( APIENTRY *col2d_RectVsCircle )( zglPRect Rect, zglPCircle Circ );
  /* circle */
extern bool ( APIENTRY *col2d_Circle )( zglPCircle Circ1, zglPCircle Circ2 );
extern bool ( APIENTRY *col2d_CircleInCircle )( zglPCircle Circ1, zglPCircle Circ2 );
extern bool ( APIENTRY *col2d_CircleInRect )( zglPCircle Circ, zglPRect Rect );
  /* extended */
extern float ( APIENTRY *col2dEx_LastX )(void);
extern float ( APIENTRY *col2dEx_LastY )(void);
extern int ( APIENTRY *col2dEx_LastLineA )(void);
extern int ( APIENTRY *col2dEx_LastLineB )(void);
  /* polyline transformations */
extern void ( APIENTRY *col2dEx_PolyRotate )( zglPPolyLine A, zglPPolyLine B, float Angle );
extern void ( APIENTRY *col2dEx_PolyScale )( zglPPolyLine A, float ScaleX, float ScaleY );
extern void ( APIENTRY *col2dEx_PolyMove )( zglPPolyLine A, zglPPolyLine B, float X, float Y );
extern void ( APIENTRY *col2dEx_PolyCenter )( zglPPolyLine A );
extern void ( APIENTRY *col2dEx_PolyRect )( zglPPolyLine A, zglPRect Rect );
  /* line */
extern void ( APIENTRY *col2dEx_CalcLineCross )( zglPLine A, zglPLine B );
  
/* COLLISION 3D */
/*typedef void func( zglPPoint3D Offset, void* Data ) zglTCol3DCallback;*/

typedef struct
{
  bool        Result;
  zglTPoint3D Offset;
} zglTCollision3D, *zglPCollision3D;
  
  /* point 3D */
extern bool ( APIENTRY *col3d_PointInTri )( zglPPoint3D Point, zglPPoint3D A, zglPPoint3D B, zglPPoint3D C );
extern bool ( APIENTRY *col3d_PointInAABB )( zglPPoint3D Point, zglPAABB AABB );
extern bool ( APIENTRY *col3d_PointInOBB )( zglPPoint3D Point, zglPOBB OBB );
extern bool ( APIENTRY *col3d_PointInSphere )( zglPPoint3D Point, zglPSphere Sphere );
  /* line3D */
extern bool ( APIENTRY *col3d_LineVsAABB )( zglPLine3D Line, zglPAABB AABB );
extern bool ( APIENTRY *col3d_LineVsOBB )( zglPLine3D Line, zglPOBB OBB );
extern bool ( APIENTRY *col3d_LineVsSphere )( zglPLine3D Line, zglPSphere Sphere );
  /* plane 3d */
extern zglTCollision3D ( APIENTRY *col3d_PlaneVsSphere )( zglPPlane Plane, zglPSphere Sphere );
  /* aabb */
extern bool ( APIENTRY *col3d_AABBVsAABB )( zglPAABB AABB1, zglPAABB AABB2 );
extern bool ( APIENTRY *col3d_AABBVsOBB )( zglPAABB AABB, zglPOBB OBB );
extern bool ( APIENTRY *col3d_AABBVsSphere )( zglPAABB AABB, zglPSphere Sphere );
  /* obb */
extern bool ( APIENTRY *col3d_OBBVsOBB )( zglPOBB OBB1, zglPOBB OBB2 );
extern bool ( APIENTRY *col3d_OBBVsSphere )( zglPOBB OBB, zglPSphere Sphere );
  /* sphere */
extern bool ( APIENTRY *col3d_SphereVsSphere )( zglPSphere Sphere1, zglPSphere Sphere );
extern bool ( APIENTRY *col3d_SphereVsNode )( zglPSphere Sphere, zglPOctree Octree, zglPNode Node, void* Callback, void* CData );

typedef DWORD zglTFile;
/* Open Mode */
#define FOM_CREATE 0x01 /* Create */
#define FOM_OPENR  0x02 /* Read */
#define FOM_OPENRW 0x03 /* Read&Write */
  
/* Seek Mode */
#define FSM_SET 0x01
#define FSM_CUR 0x02
#define FSM_END 0x03

extern void ( APIENTRY *file_Open )( zglTFile *FileHandle, const char* FileName, byte Mode );
extern bool ( APIENTRY *file_Exists )( const char* FileName );
extern DWORD ( APIENTRY *file_Seek )( zglTFile FileHandle, DWORD Offset, byte Mode );
extern DWORD ( APIENTRY *file_GetPos )( zglTFile FileHandle );
extern DWORD ( APIENTRY *file_Read )( zglTFile FileHandle, int *buffer, DWORD count );
extern DWORD ( APIENTRY *file_Write )( zglTFile FileHandle, int *buffer, DWORD count );
extern void ( APIENTRY *file_Trunc )( zglTFile FileHandle, DWORD count );
extern DWORD ( APIENTRY *file_GetSize )( zglTFile FileHandle );
extern void ( APIENTRY *file_Flush )( zglTFile FileHandle );
extern void ( APIENTRY *file_Close )( zglTFile FileHandle );

typedef struct
{
  void* Memory;
  DWORD Size;
  DWORD Position;
} zglTMemory, *zglPMemory;

extern void ( APIENTRY *mem_LoadFromFile )( zglTMemory *Memory, const char* FileName );
extern void ( APIENTRY *mem_SaveToFile )( zglTMemory *Memory, const char* FileName );
extern DWORD ( APIENTRY *mem_Seek )( zglTMemory *Memory, DWORD Offset, byte Mode );
extern DWORD ( APIENTRY *mem_Read )( zglTMemory *Memory, int *buffer, DWORD count );
extern DWORD ( APIENTRY *mem_Write )( zglTMemory *Memory, int *buffer, DWORD count );
extern void ( APIENTRY *mem_SetSize )( zglTMemory *Memory, DWORD Size );
extern void ( APIENTRY *mem_Free )( zglTMemory *Memory );

#ifdef __LINUX__
  extern void* zglLib;
  #define libZenGL "libZenGL.so"
  #define zglLoadLibrary dlopen
  #define zglGetAddress( a, b, c ) a = (typeof(a))dlsym(b, c)
#endif
#ifdef __WIN32__
  extern HMODULE zglLib;
  #define libZenGL "ZenGL.dll"
  #define zglLoadLibrary LoadLibraryA
  #ifdef __MINGW32__
  #define zglGetAddress( a, b, c ) a = (typeof(a))GetProcAddress( b, c )
  #else
  #define zglGetAddress( a, b, c ) a = (void*)GetProcAddress( b, c )
  #endif
#endif

extern void zglLoad( char* LibraryName );

#ifdef __CPP__
}
#endif

#endif
