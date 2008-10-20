/*-------------------------------*/
/*-----------= ZenGL =-----------*/
/*-------------------------------*/
/* build: 30                     */
/* date:  10.10.08               */
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
#ifndef __CPP__
typedef unsigned char bool;
#endif

#ifndef APIENTRY
#define APIENTRY
#endif
void ( APIENTRY *zgl_Init )( byte FSAA, byte StencilBits );
void ( APIENTRY *zgl_Exit )(void);
  
#define SYS_LOAD             0x000001
#define SYS_DRAW             0x000002
#define SYS_EXIT             0x000003
#define TEX_FORMAT_EXTENSION 0x000010
#define TEX_FORMAT_LOADER    0x000011
#define SND_FORMAT_EXTENSION 0x000020
#define SND_FORMAT_LOADER    0x000021

void ( APIENTRY *zgl_Reg )( WORD What, void* UserData );
  
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

DWORD ( APIENTRY *zgl_Get )( DWORD What );
void ( APIENTRY *zgl_GetMem )( void** Ptr, DWORD Size );

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

void ( APIENTRY *zgl_Enable )( DWORD What );
void ( APIENTRY *zgl_Disable )( DWORD What );

/* LOG */
void ( APIENTRY *log_Add )( const char* Message, bool Timings );
  
/* WINDOW */
void ( APIENTRY *wnd_SetCaption )( const char* NewCaption );
void ( APIENTRY *wnd_SetSize )( WORD Width, WORD Height );
void ( APIENTRY *wnd_SetPos )( WORD X, WORD Y );
void ( APIENTRY *wnd_SetOnTop )( bool OnTop );
void ( APIENTRY *wnd_ShowCursor )( bool Show );
  
/* SCREEN */
typedef struct
{
  int  Count;
  int *Width;
  int *Height;
} zglTResolutionList, *zglPResolutionList;

#define REFRESH_MAXIMUM 0
#define REFRESH_DEFAULT 1

void ( APIENTRY *scr_Clear )(void);
void ( APIENTRY *scr_Flush )(void);
void ( APIENTRY *scr_SetVSync )( bool VSync );
/* ВНИМАНИЕ: Функция уничтожает контекст OpenGL, что потребует перезагрузку ресурсов */
void ( APIENTRY *scr_SetFSAA )( byte FSAA );
void ( APIENTRY *scr_SetOptions )( WORD Width, WORD Height, WORD BPP, WORD Refresh, bool FullScreen, bool VSync );
void ( APIENTRY *scr_CorrectResolution )( WORD Width, WORD Height );

/* INI */
void  ( APIENTRY *ini_LoadFromFile )( const char* FileName );
void  ( APIENTRY *ini_SaveToFile )( const char* FileName );
void  ( APIENTRY *ini_Add )( const char* Section, const char* Key );
char* ( APIENTRY *ini_ReadKeyStr )( const char* Section, const char* Key );
int   ( APIENTRY *ini_ReadKeyInt )( const char* Section, const char* Key );
bool  ( APIENTRY *ini_ReadKeyBool )( const char* Section, const char* Key );
bool  ( APIENTRY *ini_WriteKeyStr )( const char* Section, const char* Key, const char* Value );
bool  ( APIENTRY *ini_WriteKeyInt )( const char* Section, const char* Key, int Value );
bool  ( APIENTRY *ini_WriteKeyBool )( const char* Section, const char* Key, bool Value );
  
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


zglPTimer ( APIENTRY *timer_Add )( void* OnTimer, DWORD Interval );
void      ( APIENTRY *timer_Del )( zglPTimer Timer );
double    ( APIENTRY *timer_GetTicks )(void);
  
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
bool  ( APIENTRY * key_Down )( byte KeyCode );
bool  ( APIENTRY * key_Up )( byte KeyCode );
byte  ( APIENTRY * key_Last )( byte KeyAction );
void  ( APIENTRY * key_BeginReadText )( const char* Text, WORD MaxSymbols );
char* ( APIENTRY * key_EndReadText )(void);
void  ( APIENTRY * key_ClearState )(void);
  
/* MOUSE */
#define M_BLEFT  0
#define M_BMIDLE 1
#define M_BRIGHT 2
#define M_WUP    0
#define M_WDOWN  1

int  ( APIENTRY *mouse_X )(void);
int  ( APIENTRY *mouse_Y )(void);
int  ( APIENTRY *mouse_DX )(void);
int  ( APIENTRY *mouse_DY )(void);
bool ( APIENTRY *mouse_Down )( byte Button );
bool ( APIENTRY *mouse_Up )( byte Button );
bool ( APIENTRY *mouse_Click )( byte Button );
bool ( APIENTRY *mouse_Wheel )( byte Axis );
void ( APIENTRY *mouse_ClearState )(void);
void ( APIENTRY *mouse_Lock       )(void);
  
/* GL */
void ( APIENTRY *Set2DMode )(void);
void ( APIENTRY *Set3DMode )( float FOVY );
  
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

zglPTexture ( APIENTRY *tex_Add )(void);
void        ( APIENTRY *tex_Del )( zglPTexture Texture );
void        ( APIENTRY *tex_Create )( zglTTexture *Texture, void* pData );
zglPTexture ( APIENTRY *tex_CreateZero )( WORD Width, WORD Height, DWORD Color, DWORD Flags );
zglPTexture ( APIENTRY *tex_LoadFromFile )( const char* FileName, DWORD TransparentColor, DWORD Flags );
void        ( APIENTRY *tex_SetFrameSize )( zglPTexture Texture, WORD FrameWidth, WORD FrameHeight );
zglPTexture ( APIENTRY *tex_SetMask )( zglPTexture Texture, zglPTexture Mask );
void        ( APIENTRY *tex_GetData )( zglPTexture Texture, void** pData, int *pSize );
void        ( APIENTRY *tex_Filter )( zglPTexture Texture, DWORD Flags );
void        ( APIENTRY *tex_SetAnisotropy )( byte Level );

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

zglPRenderTarget ( APIENTRY *rtarget_Add )( byte rtType, zglPTexture Surface, byte Flags );
void             ( APIENTRY *rtarget_Del )( zglPRenderTarget Target );
void             ( APIENTRY *rtarget_Set )( zglPRenderTarget Target );
  
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

void ( APIENTRY *fx_SetBlendMode )( byte Mode );
  
/* FX 2D */
#define FX2D_FLIPX    0x000001
#define FX2D_FLIPY    0x000002
#define FX2D_COLORMIX 0x000004
#define FX2D_VCA      0x000008
#define FX2D_VCHANGE  0x000010
#define FX2D_SCALE    0x000020

#define FX_BLEND      0x000040

void ( APIENTRY *fx2d_SetColorMix )( DWORD Color );
void ( APIENTRY *fx2d_SetVCA )( DWORD c1, DWORD c2, DWORD c3, DWORD c4, byte a1, byte a2, byte a3, byte a4 );
void ( APIENTRY *fx2d_SetVertexes )( float x1, float y1, float x2, float y2, float x3, float y3, float x4, float y4 );
void ( APIENTRY *fx2d_SetScale )( float scaleX, float scaleY );
  
/* Camera 2D */
typedef struct
{
  float X;
  float Y;
  float Angle;
} zglTCamera2D, *zglPCamera2D;

void ( APIENTRY *cam2d_Set )( zglPCamera2D Camera );
  
/* Primitives 2D */
#define PR2D_FILL   0x000001
#define PR2D_SMOOTH 0x000002

void ( APIENTRY *pr2d_Pixel )( float X, float Y, DWORD Color, byte Alpha );
void ( APIENTRY *pr2d_Line )( float X1, float Y1, float X2, float Y2, DWORD Color, byte Alpha, DWORD FX );
void ( APIENTRY *pr2d_Rect )( float X, float Y, float W, float H, DWORD Color, byte Alpha, DWORD FX );
void ( APIENTRY *pr2d_Circle )( float X, float Y, float Radius, DWORD Color, byte Alpha, WORD Quality, DWORD FX );
void ( APIENTRY *pr2d_Ellipse )( float X, float Y, float xRadius, float yRadius, DWORD Color, byte Alpha, WORD Quality, DWORD FX );
  
/* Sprites 2D */
void ( APIENTRY *ssprite2d_Draw )( zglPTexture Texture, float X, float Y, float W, float H, float Angle, byte Alpha, DWORD FX );
void ( APIENTRY *asprite2d_Draw )( zglPTexture Texture, float X, float Y, float W, float H, float Angle, WORD Frame, byte Alpha, DWORD FX );
void ( APIENTRY *csprite2d_Draw )( zglPTexture Texture, float X, float Y, float W, float H, float Angle, zglTRect CutRect, byte Alpha, DWORD FX );
  
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

zglPFont ( APIENTRY *font_Add )(void);
void     ( APIENTRY *font_Del )( zglPFont Font );
zglPFont ( APIENTRY *font_LoadFromFile )( const char* Texture, const char* FontInfo );
void     ( APIENTRY *text_Draw )( zglPFont Font, float X, float Y, const char* Text, byte Alpha, DWORD Color, float Step, float Scale );
float    ( APIENTRY *text_GetWidth )( zglPFont Font, const char* Text, float Step, float Scale );

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

bool      ( APIENTRY *snd_Init )(void);
void      ( APIENTRY *snd_Free )(void);
zglPSound ( APIENTRY *snd_Add )( int BufferCount, int SourceCount );
void      ( APIENTRY *snd_Del )( zglPSound Sound );
zglPSound ( APIENTRY *snd_LoadFromFile )( char* FileName, int SourceCount );
int       ( APIENTRY *snd_Play )( zglPSound Sound, float X, float Y, float Z, bool Loop );
void      ( APIENTRY *snd_Stop )( zglPSound Sound, int Source );
void      ( APIENTRY *snd_SetVolume )( byte Volume, int ID );
void      ( APIENTRY *snd_SetFrequency )( int Frequency, int ID );
void      ( APIENTRY *snd_SetFrequencyCoeff )( float Coefficient, int ID );
void      ( APIENTRY *snd_PlayFile )( zglPSoundFile SoundFile );
void      ( APIENTRY *snd_StopFile )(void);
void      ( APIENTRY *snd_RestoreFile )(void);

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
  DWORD vIndex[3];
  DWORD tIndex[3];
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
void ( APIENTRY *zbuffer_SetDepth )( float zNear, float zFar );
void ( APIENTRY *zbuffer_Clear )(void);
  
/* SCISSOR */
void ( APIENTRY *scissor_Begin )( WORD X, WORD Y, WORD Width, WORD Height );
void ( APIENTRY *scissor_End )(void);
  
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
#define BUILD_FNORMALS     0x020
#define BUILD_SNORMALS     0x040
#define BUILD_PLANES       0x080
#define BUILD_VBO          0x100
  
void ( APIENTRY *obj3d_Begin )( DWORD Flags );
void ( APIENTRY *obj3d_End )(void);
void ( APIENTRY *obj3d_Enable )( DWORD Flags );
void ( APIENTRY *obj3d_Disable )( DWORD Flags );
void ( APIENTRY *obj3d_SetColor )( DWORD Color, byte Alpha );
void ( APIENTRY *obj3d_BindTexture )( zglPTexture Texture, byte Level );
void ( APIENTRY *obj3d_SetMaterial )( byte Material, byte Side, DWORD Color, byte Alpha );
void ( APIENTRY *obj3d_Scale )( float ScaleX, float ScaleY, float ScaleZ );
void ( APIENTRY *obj3d_Move )( float X, float Y, float Z );
void ( APIENTRY *obj3d_SetMatrix )( zglTMatrix4f Matrix );
void ( APIENTRY *obj3d_MulMatrix )( zglTMatrix4f Matrix );
  
#define AX 0x01
#define AY 0x02
#define AZ 0x04

void ( APIENTRY *obj3d_Rotate )( float Angle, byte Axis );

/* PRIMITIVES 3D */
void ( APIENTRY *pr3d_Point )( float X, float Y, float Z );
void ( APIENTRY *pr3d_Line )( float X1, float Y1, float Z1, float X2, float Y2, float Z2 );
void ( APIENTRY *pr3d_Plane )( float Width, float Height );
void ( APIENTRY *pr3d_AABB )( float Width, float Height, float ZDepth );
void ( APIENTRY *pr3d_Sphere )( float Radius, int Quality );

/* SPRITE 3D */
void ( APIENTRY *ssprite3d_Draw )( float X, float Y, float Z, float sX, float sY, float sZ, zglPMatrix4f Matrix );
void ( APIENTRY *asprite3d_Draw )( float X, float Y, float Z, float sX, float sY, float sZ, int Frame, zglPMatrix4f Matrix );

/* CAMERA 3D */
typedef struct
{
  zglTPoint3D  Position;
  zglTPoint3D  Rotation;
  zglTMatrix4f Matrix;
} zglTCamera3D, *zglPCamera3D;

void ( APIENTRY *cam3d_Set )( zglPCamera3D Camera );
void ( APIENTRY *cam3d_Fly )( zglPCamera3D Camera, float Speed );
void ( APIENTRY *cam3d_Strafe )( zglPCamera3D Camera, float Speed );
  
/* STATIC MESH */
typedef struct
{
  DWORD Flags;

  DWORD IBuffer;
  DWORD VBuffer;

  DWORD VCount;
  DWORD TCount;
  DWORD FCount;
  DWORD GCount;

  zglTPoint3D *Vertices;
  zglTPoint3D *Normals;
  zglTPoint2D *TexCoords;
  zglTPoint2D *MultiTexCoords;
  zglTFace    *Faces;
  void*       Indices;
  zglTGroup   *Groups;
} zglTSMesh, *zglPSMesh;

bool ( APIENTRY *smesh_LoadFromFile )( zglPSMesh *Mesh, char* FileName, DWORD Flags );
void ( APIENTRY *smesh_Draw )( zglPSMesh Mesh );
void ( APIENTRY *smesh_DrawGroup )( zglPSMesh Mesh, DWORD Group );
void ( APIENTRY *smesh_Free )( zglPSMesh *Mesh );

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
  zglTBonePos *BonePos;
} zglTSkeletonFrame, *zglPSkeletonFrame;

typedef struct
{
  int               nAction;
  int               nFrame;
  float             Delta;
  float             prevDelta;
  zglTSkeletonFrame Frame;
  zglTPoint3D       *Vertices;
  zglTPoint3D       *Normals;
} zglTSkeletonState, *zglPSkeletonState;

typedef struct
{
  char*             Name;
  float             FPS;
  DWORD             FCount;
  zglTSkeletonFrame Frames;
} zglTSkeletonAction, *zglPSkeletonAction;

typedef struct
{
  DWORD              Flags;

  DWORD              IBuffer;
  DWORD              VBuffer;

  DWORD              VCount;
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
  zglTGroup          *Groups;

  zglTBone           *Bones;
  zglTBonesWeights   Weights;
  zglTSkeletonState  State;
  zglTSkeletonAction *Actions;
  zglTSkeletonFrame  Skeleton;
} zglTSkMesh, *zglPSkMesh;

bool  ( APIENTRY *skmesh_LoadFromFile )( zglPSkMesh *Mesh, char *FileName, DWORD Flags );
void  ( APIENTRY *skmesh_Animate )( zglPSkMesh Mesh, zglPSkeletonState State );
void  ( APIENTRY *skmesh_Draw )( zglPSkMesh Mesh, zglPSkeletonState State );
void  ( APIENTRY *skmesh_DrawGroup )( zglPSkMesh Mesh, zglPSkeletonState State, DWORD Group );
void  ( APIENTRY *skmesh_DrawSkelet )( zglPSkMesh Mesh, zglPSkeletonState State );
void  ( APIENTRY *skmesh_Free )( zglPSkMesh *Mesh );

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

void  ( APIENTRY *heightmap_Build )( zglPHeightMap *Heightmap, zglPTexture Texture, float xScale, float yScale, float zScale, int xDetail, int yDetail, DWORD Flags );
void  ( APIENTRY *heightmap_Draw )( zglPHeightMap HeightMap );
void  ( APIENTRY *heightmap_Free )( zglPHeightMap *HeightMap );
int   ( APIENTRY *heightmap_GetPlane )( zglPHeightMap HeightMap, zglTPoint3D Pos );
float ( APIENTRY *heightmap_GetYOffset )( zglPHeightMap HeightMap, zglTPoint3D Pos );

/* VBO */
void ( APIENTRY *vbo_Build )( DWORD *IBuffer, DWORD *VBuffer, DWORD ICount, DWORD VCount, void* Indices, void* Vertices, void* Normals, void* TexCoords, void* MultiTexCoords, DWORD *Flags );
void ( APIENTRY *vbo_Free )( DWORD *IBuffer, DWORD *VBuffer, DWORD ICount, DWORD VCount, void* Indices, void* Vertices );

/* FRUSTUM */
typedef float zglTFrustum[6][4];
typedef zglTFrustum *zglPFrustum;
  
void ( APIENTRY *frustum_Calc )( zglPFrustum f );
bool ( APIENTRY *frustum_PointIn )( zglPFrustum f, float x, float y, float z );
bool ( APIENTRY *frustum_PPointIn )( zglPFrustum f, zglPPoint3D Vertex );
bool ( APIENTRY *frustum_TriangleIn )( zglPFrustum f, zglTPoint3D v1, zglTPoint3D v2, zglTPoint3D v3 );
bool ( APIENTRY *frustum_SphereIn )( zglPFrustum f, float x, float y, float z, float r );
bool ( APIENTRY *frustum_BoxIn )( zglPFrustum f, float x, float y, float z, float bx, float by, float bz );
bool ( APIENTRY *frustum_CubeIn )( zglPFrustum f, float x, float y, float z, float size );
  
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

void ( APIENTRY *octree_Build )( zglPOctree Octree, DWORD MaxFacesPerNode, DWORD Flags );
void ( APIENTRY *octree_Free )( zglPOctree Octree );
void ( APIENTRY *octree_Draw )( zglPOctree Octree, zglPFrustum Frustum );
void ( APIENTRY *octree_DrawDebug )( zglPOctree Octree, zglPFrustum Frustum );
void ( APIENTRY *octree_DrawNode )( zglPOctree Octree, zglPNode Node, zglPFrustum Frustum );
  
/*/ LIGHT */
void ( APIENTRY *light_Enable )( byte ID );
void ( APIENTRY *light_Disable )( byte ID );
void ( APIENTRY *light_SetPosition )( byte ID, float X, float Y, float Z, float W );
void ( APIENTRY *light_SetMaterial )( byte ID, byte Material, DWORD Color, byte Alpha );

  
/* FOG */
#define FOG_MODE_EXP    0
#define FOG_MODE_EXP2   1
#define FOG_MODE_LINEAR 2

void ( APIENTRY *fog_Enable )(void);
void ( APIENTRY *fog_Disable )(void);
void ( APIENTRY *fog_SetMode )( byte Mode );
void ( APIENTRY *fog_SetColor )( DWORD Color );
void ( APIENTRY *fog_SetDensity )( float Density );
void ( APIENTRY *fog_SetBeginEnd )( float fBegin, float fEnd );
  
/* SKYBOX */
void ( APIENTRY *skybox_Init )( zglPTexture Top, zglPTexture Bottom, zglPTexture Left, zglPTexture Right, zglPTexture Front, zglPTexture Back );
void ( APIENTRY *skybox_Draw )(void);
  
/* SHADERS */
#define SHADER_ARB          0
#define SHADER_GLSL         1
#define SHADER_VERTEX_ARB   0x8620
#define SHADER_FRAGMENT_ARB 0x8804
#define SHADER_VERTEX       0x8B31
#define SHADER_FRAGMENT     0x8B30

/* ARBfp/ARBvp */
bool  ( APIENTRY *shader_InitARB )(void);
DWORD ( APIENTRY *shader_LoadFromFileARB )( const char* FileName, DWORD ShaderType );
void  ( APIENTRY *shader_BeginARB )( DWORD Shader, DWORD ShaderType );
void  ( APIENTRY *shader_EndARB )( DWORD ShaderType );
void  ( APIENTRY *shader_FreeARB )( DWORD Shader );

/* GLSL */
bool  ( APIENTRY *shader_InitGLSL )(void);
DWORD ( APIENTRY *shader_LoadFromFile )( const char* FileName, int ShaderType, bool Link );
void  ( APIENTRY *shader_Attach )( DWORD Attach );
void  ( APIENTRY *shader_BeginLink )(void);
DWORD ( APIENTRY *shader_EndLink )(void);
void  ( APIENTRY *shader_Begin )( DWORD Shader );
void  ( APIENTRY *shader_End )(void);
void  ( APIENTRY *shader_Free )( DWORD Shader );
int   ( APIENTRY *shader_GetUniform )( DWORD Shader, const char* UniformName );
void  ( APIENTRY *shader_SetUniform1f )( int Uniform, float v1 );
void  ( APIENTRY *shader_SetUniform1i )( int Uniform, int v1 );
void  ( APIENTRY *shader_SetUniform2f )( int Uniform, float v1, float v2 );
void  ( APIENTRY *shader_SetUniform3f )( int Uniform, float v1, float v2, float v3 );
void  ( APIENTRY *shader_SetUniform4f )( int Uniform, float v1, float v2, float v3, float v4 );
int   ( APIENTRY *shader_GetAttrib )( DWORD Shader, const char* AttribName );
/* glVertexAttrib* GLSL/ARB */
void ( APIENTRY *shader_SetAttrib1f )( int Attrib, float v1 );
void ( APIENTRY *shader_SetAttrib2f )( int Attrib, float v1, float v2 );
void ( APIENTRY *shader_SetAttrib3f )( int Attrib, float v1, float v2, float v3 );
void ( APIENTRY *shader_SetAttrib4f )( int Attrib, float v1, float v2, float v3, float v4 );
void ( APIENTRY *shader_SetAttribPf )( int Attrib, void* v, bool Normalized );
void ( APIENTRY *shader_SetParameter4f )( DWORD ShaderType, int Parameterm, float v1, float v2, float v3, float v4 );
  
/* MATH */
int   ( APIENTRY *m_Round )( float value );
float ( APIENTRY *m_Cos )( int Angle );
float ( APIENTRY *m_Sin )( int Angle );
float ( APIENTRY *m_Distance )( float x1, float y1, float x2, float y2 );
float ( APIENTRY *m_FDistance )( float x1, float y1, float x2, float y2 );
float ( APIENTRY *m_Angle )( float x1, float y1, float x2, float y2 );
  /* vectros */
zglTPoint3D ( APIENTRY *vector_Get )( float x, float y, float z );
zglTPoint3D ( APIENTRY *vector_Add )( zglTPoint3D Vector1, zglTPoint3D Vector2 );
zglTPoint3D ( APIENTRY *vector_Sub )( zglTPoint3D Vector1, zglTPoint3D Vector2 );
zglTPoint3D ( APIENTRY *vector_Mul )( zglTPoint3D Vector1, zglTPoint3D Vector2 );
zglTPoint3D ( APIENTRY *vector_Div )( zglTPoint3D Vector1, zglTPoint3D Vector2 );
zglTPoint3D ( APIENTRY *vector_AddV )( zglTPoint3D Vector, float Value );
zglTPoint3D ( APIENTRY *vector_SubV )( zglTPoint3D Vector, float Value );
zglTPoint3D ( APIENTRY *vector_MulV )( zglTPoint3D Vector, float Value );
zglTPoint3D ( APIENTRY *vector_DivV )( zglTPoint3D Vector, float Value );
zglTPoint3D ( APIENTRY *vector_MulM3f )( zglTPoint3D Vector, zglTMatrix3f Matrix );
zglTPoint3D ( APIENTRY *vector_MulM4f )( zglTPoint3D Vector, zglTMatrix4f Matrix );
zglTPoint3D ( APIENTRY *vector_MulInvM4f )( zglTPoint3D Vector, zglTMatrix4f Matrix );
zglTPoint3D ( APIENTRY *vector_Negate )( zglTPoint3D Vector );
zglTPoint3D ( APIENTRY *vector_Normalize )( zglTPoint3D Vector );
float       ( APIENTRY *vector_Angle )( zglTPoint3D Vector1, zglTPoint3D Vector2 );
zglTPoint3D ( APIENTRY *vector_Cross )( zglTPoint3D Vector1, zglTPoint3D Vector2 );
float       ( APIENTRY *vector_Dot )( zglTPoint3D Vector1, zglTPoint3D Vector2 );
float       ( APIENTRY *vector_Distance )( zglTPoint3D Vector1, zglTPoint3D Vector2 );
float       ( APIENTRY *vector_FDistance )( zglTPoint3D Vector1, zglTPoint3D Vector2 );
float       ( APIENTRY *vector_Length )( zglTPoint3D Vector );
zglTPoint3D ( APIENTRY *vector_Lerp )( zglTPoint3D Vector1, zglTPoint3D Vector2, float Value );
/* matrix */
zglTMatrix3f ( APIENTRY *matrix3f_Get )( zglTPoint3D v1, zglTPoint3D v2, zglTPoint3D v3 );
void         ( APIENTRY *matrix3f_OrthoNormalize )( zglPMatrix3f Matrix );
void         ( APIENTRY *matrix3f_Transpose )( zglPMatrix3f Matrix );
void         ( APIENTRY *matrix3f_Rotate )( zglPMatrix3f Matrix, float aX, float aY, float aZ );
zglTMatrix3f ( APIENTRY *matrix3f_Add )( zglTMatrix3f Matrix1, zglTMatrix3f Matrix2 );
zglTMatrix3f ( APIENTRY *matrix3f_Mul )( zglTMatrix3f Matrix1, zglTMatrix3f Matrix2 );
void         ( APIENTRY *matrix4f_Transpose )( zglPMatrix4f Matrix );
float        ( APIENTRY *matrix4f_Determinant )( zglTMatrix4f Matrix );
zglTMatrix4f ( APIENTRY *matrix4f_Inverse )( zglTMatrix4f Matrix );
void         ( APIENTRY *matrix4f_Translate )( zglPMatrix4f Matrix, float tX, float tY, float tZ );
void         ( APIENTRY *matrix4f_Rotate )( zglPMatrix4f Matrix, float aX, float aY, float aZ );
void         ( APIENTRY *matrix4f_Scale )( zglPMatrix4f Matrix, float sX, float sY, float sZ );
zglTMatrix4f ( APIENTRY *matrix4f_Mul )( zglTMatrix4f Matrix1, zglTMatrix4f Matrix2 );
/* quaternions */
zglTQuaternion ( APIENTRY *quater_Get )( float X, float Y, float Z, float W );
zglTQuaternion ( APIENTRY *quater_Add )( zglTQuaternion q1, zglTQuaternion q2 );
zglTQuaternion ( APIENTRY *quater_Sub )( zglTQuaternion q1, zglTQuaternion q2 );
zglTQuaternion ( APIENTRY *quater_Mul )( zglTQuaternion q1, zglTQuaternion q2 );
zglTQuaternion ( APIENTRY *quater_Negate )( zglTQuaternion Quaternion );
zglTQuaternion ( APIENTRY *quater_Normalize )( zglTQuaternion Quaternion );
float          ( APIENTRY *quater_Dot )( zglTQuaternion q1, zglTQuaternion q2 );
zglTQuaternion ( APIENTRY *quater_Lerp )( zglTQuaternion q1, zglTQuaternion q2, float Value );
zglTQuaternion ( APIENTRY *quater_FromRotation )( zglTPoint3D Rotation );
zglTMatrix4f   ( APIENTRY *quater_GetM4f )( zglTQuaternion Quaternion );
/* line 3d */
zglTPoint3D ( APIENTRY *line3d_ClosestPoint )( zglTPoint3D A, zglTPoint3D B, zglTPoint3D Point );
  /* plane */
zglTPlane ( APIENTRY *plane_Get )( zglTPoint3D A, zglTPoint3D B, zglTPoint3D C );
float     ( APIENTRY *plane_Distance )( zglTPlane Plane, zglTPoint3D Point );
  /* triangle */
zglTPoint3D ( APIENTRY *tri_GetNormal )( zglTPoint3D A, zglTPoint3D B, zglTPoint3D C );

/* COLLISION 2D */
  /* point */
bool ( APIENTRY *col2d_PointInRect )( float X, float Y, zglPRect Rect );
bool ( APIENTRY *col2d_PointInCircle )( float X, float Y, zglPCircle Circ );
bool ( APIENTRY *col2d_PointInPolyLine )( float X, float Y, zglPPolyLine PL );
  /* line 2d */
bool ( APIENTRY *col2d_Line )( zglPLine A, zglPLine B );
bool ( APIENTRY *col2d_LineVsRect )( zglPLine A, zglPRect Rect );
bool ( APIENTRY *col2d_LineVsCircle )( zglPLine L, zglPCircle Circ );
bool ( APIENTRY *col2d_LineVsCircleXY )( zglPLine L, zglPCircle Circ, byte Precision );
bool ( APIENTRY *col2d_LineVsPolyLine )( zglPLine A, zglPPolyLine PL );
  /* polyline */
bool ( APIENTRY *col2d_PolyLine )( zglPPolyLine A, zglPPolyLine B );
bool ( APIENTRY *col2d_PolyLineVsRect )( zglPPolyLine A, zglPRect Rect );
bool ( APIENTRY *col2d_PolyLineVsCircle )( zglPPolyLine A, zglPCircle Circ );
bool ( APIENTRY *col2d_PolyLineVsCircleXY )( zglPPolyLine A, zglPCircle Circ, int Precision );
  /* rect */
bool ( APIENTRY *col2d_Rect )( zglPRect Rect1, zglPRect Rect2 );
bool ( APIENTRY *col2d_RectInRect )( zglPRect Rect1, zglPRect Rect2 );
bool ( APIENTRY *col2d_RectInCircle )( zglPRect Rect, zglPCircle Circ );
bool ( APIENTRY *col2d_RectVsCircle )( zglPRect Rect, zglPCircle Circ );
  /* circle */
bool ( APIENTRY *col2d_Circle )( zglPCircle Circ1, zglPCircle Circ2 );
bool ( APIENTRY *col2d_CircleInCircle )( zglPCircle Circ1, zglPCircle Circ2 );
bool ( APIENTRY *col2d_CircleInRect )( zglPCircle Circ, zglPRect Rect );
  /* extended */
float ( APIENTRY *col2dEx_LastX )(void);
float ( APIENTRY *col2dEx_LastY )(void);
int   ( APIENTRY *col2dEx_LastLineA )(void);
int   ( APIENTRY *col2dEx_LastLineB )(void);
  /* polyline transformations */
void ( APIENTRY *col2dEx_PolyRotate )( zglPPolyLine A, zglPPolyLine B, float Angle );
void ( APIENTRY *col2dEx_PolyScale )( zglPPolyLine A, float ScaleX, float ScaleY );
void ( APIENTRY *col2dEx_PolyMove )( zglPPolyLine A, zglPPolyLine B, float X, float Y );
void ( APIENTRY *col2dEx_PolyCenter )( zglPPolyLine A );
void ( APIENTRY *col2dEx_PolyRect )( zglPPolyLine A, zglPRect Rect );
  /* line */
void ( APIENTRY *col2dEx_CalcLineCross )( zglPLine A, zglPLine B );
  
/* COLLISION 3D */
/*typedef void func( zglPPoint3D Offset, void* Data ) zglTCol3DCallback;*/

typedef struct
{
  bool        Result;
  zglTPoint3D Offset;
} zglTCollision3D, *zglPCollision3D;
  
  /* point 3D */
bool ( APIENTRY *col3d_PointInTri )( zglPPoint3D Point, zglPPoint3D A, zglPPoint3D B, zglPPoint3D C );
bool ( APIENTRY *col3d_PointInAABB )( zglPPoint3D Point, zglPAABB AABB );
bool ( APIENTRY *col3d_PointInOBB )( zglPPoint3D Point, zglPOBB OBB );
bool ( APIENTRY *col3d_PointInSphere )( zglPPoint3D Point, zglPSphere Sphere );
  /* line3D */
bool ( APIENTRY *col3d_LineVsAABB )( zglPLine3D Line, zglPAABB AABB );
bool ( APIENTRY *col3d_LineVsOBB )( zglPLine3D Line, zglPOBB OBB );
bool ( APIENTRY *col3d_LineVsSphere )( zglPLine3D Line, zglPSphere Sphere );
  /* plane 3d */
zglTCollision3D ( APIENTRY *col3d_PlaneVsSphere )( zglPPlane Plane, zglPSphere Sphere );
  /* aabb */
bool ( APIENTRY *col3d_AABBVsAABB )( zglPAABB AABB1, zglPAABB AABB2 );
bool ( APIENTRY *col3d_AABBVsOBB )( zglPAABB AABB, zglPOBB OBB );
bool ( APIENTRY *col3d_AABBVsSphere )( zglPAABB AABB, zglPSphere Sphere );
  /* obb */
bool ( APIENTRY *col3d_OBBVsOBB )( zglPOBB OBB1, zglPOBB OBB2 );
bool ( APIENTRY *col3d_OBBVsSphere )( zglPOBB OBB, zglPSphere Sphere );
  /* sphere */
bool ( APIENTRY *col3d_SphereVsSphere )( zglPSphere Sphere1, zglPSphere Sphere );
bool ( APIENTRY *col3d_SphereVsNode )( zglPSphere Sphere, zglPOctree Octree, zglPNode Node, void* Callback, void* CData );

typedef DWORD zglTFile;
/* Open Mode */
#define FOM_CREATE 0x01 /* Create */
#define FOM_OPENR  0x02 /* Read */
#define FOM_OPENRW 0x03 /* Read&Write */
  
/* Seek Mode */
#define FSM_SET 0x01
#define FSM_CUR 0x02
#define FSM_END 0x03

void  ( APIENTRY *file_Open )( zglTFile *FileHandle, const char* FileName, byte Mode );
bool  ( APIENTRY *file_Exists )( const char* FileName );
DWORD ( APIENTRY *file_Seek )( zglTFile FileHandle, DWORD Offset, byte Mode );
DWORD ( APIENTRY *file_GetPos )( zglTFile FileHandle );
DWORD ( APIENTRY *file_Read )( zglTFile FileHandle, int *buffer, DWORD count );
DWORD ( APIENTRY *file_Write )( zglTFile FileHandle, int *buffer, DWORD count );
void  ( APIENTRY *file_Trunc )( zglTFile FileHandle, DWORD count );
DWORD ( APIENTRY *file_GetSize )( zglTFile FileHandle );
void  ( APIENTRY *file_Flush )( zglTFile FileHandle );
void  ( APIENTRY *file_Close )( zglTFile FileHandle );

typedef struct
{
  void* Memory;
  DWORD Size;
  DWORD Position;
} zglTMemory, *zglPMemory;

void  ( APIENTRY *mem_LoadFromFile )( zglTMemory *Memory, const char* FileName );
void  ( APIENTRY *mem_SaveToFile )( zglTMemory *Memory, const char* FileName );
DWORD ( APIENTRY *mem_Seek )( zglTMemory *Memory, DWORD Offset, byte Mode );
DWORD ( APIENTRY *mem_Read )( zglTMemory *Memory, int *buffer, DWORD count );
DWORD ( APIENTRY *mem_Write )( zglTMemory *Memory, int *buffer, DWORD count );
void  ( APIENTRY *mem_SetSize )( zglTMemory *Memory, DWORD Size );
void  ( APIENTRY *mem_Free )( zglTMemory *Memory );

#ifdef __LINUX__
  void* zglLib;
  #define libZenGL "libZenGL.so"
  #define zglLoadLibrary dlopen
  #define zglGetAddress( a, b, c ) a = (typeof(a))dlsym(b, c)
#endif
#ifdef __WIN32__
  HMODULE zglLib;
  #define libZenGL "ZenGL.dll"
  #define zglLoadLibrary LoadLibraryA
  #define zglGetAddress( a, b, c ) a = (void*)GetProcAddress( b, c )
#endif

void zglLoad( char* LibraryName )
{
  #ifdef __LINUX__
  zglLib = (void*)zglLoadLibrary( LibraryName, 0x01 );
  #endif
  #ifdef __WIN32__
  zglLib = zglLoadLibrary( LibraryName );
  #endif
  if ( zglLib )
    {
      zglGetAddress( zgl_Init, zglLib, "zgl_Init" );
      zglGetAddress( zgl_Exit, zglLib, "zgl_Exit" );
      zglGetAddress( zgl_Reg, zglLib, "zgl_Reg" );
      zglGetAddress( zgl_Get, zglLib, "zgl_Get" );
      zglGetAddress( zgl_GetMem, zglLib, "zgl_GetMem" );
      zglGetAddress( zgl_Enable, zglLib, "zgl_Enable" );
      zglGetAddress( zgl_Disable, zglLib, "zgl_Disable" );

      zglGetAddress( log_Add, zglLib, "log_Add" );

      zglGetAddress( wnd_SetCaption, zglLib, "wnd_SetCaption" );
      zglGetAddress( wnd_SetSize, zglLib, "wnd_SetSize" );
      zglGetAddress( wnd_SetPos, zglLib, "wnd_SetPos" );
      zglGetAddress( wnd_SetOnTop, zglLib, "wnd_SetOnTop" );
      zglGetAddress( wnd_ShowCursor, zglLib, "wnd_ShowCursor" );

      zglGetAddress( scr_Clear, zglLib, "scr_Clear" );
      zglGetAddress( scr_Flush, zglLib, "scr_Flush" );
      zglGetAddress( scr_SetVSync, zglLib, "scr_SetVSync" );
      zglGetAddress( scr_SetFSAA, zglLib, "scr_SetFSAA" );
      zglGetAddress( scr_SetOptions, zglLib, "scr_SetOptions" );
      zglGetAddress( scr_CorrectResolution, zglLib, "scr_CorrectResolution" );

      zglGetAddress( ini_LoadFromFile, zglLib, "ini_LoadFromFile" );
      zglGetAddress( ini_SaveToFile, zglLib, "ini_SaveToFile" );
      zglGetAddress( ini_Add, zglLib, "ini_Add" );
      zglGetAddress( ini_ReadKeyStr, zglLib, "ini_ReadKeyStr" );
      zglGetAddress( ini_ReadKeyInt, zglLib, "ini_ReadKeyInt" );
      zglGetAddress( ini_ReadKeyBool, zglLib, "ini_ReadKeyBool" );
      zglGetAddress( ini_WriteKeyStr, zglLib, "ini_WriteKeyStr" );
      zglGetAddress( ini_WriteKeyInt, zglLib, "ini_WriteKeyInt" );
      zglGetAddress( ini_WriteKeyBool, zglLib, "ini_WriteKeyBool" );

      zglGetAddress( timer_Add, zglLib, "timer_Add" );
      zglGetAddress( timer_Del, zglLib, "timer_Del" );
      zglGetAddress( timer_GetTicks, zglLib, "timer_GetTicks" );
    /*  timer_Reset = zglGetAddress( zglLib, "timer_Reset" );*/

      zglGetAddress( key_Down, zglLib, "key_Down" );
      zglGetAddress( key_Up, zglLib, "key_Up" );
      zglGetAddress( key_Last, zglLib, "key_Last" );
      zglGetAddress( key_BeginReadText, zglLib, "key_BeginReadText" );
      zglGetAddress( key_EndReadText, zglLib, "key_EndReadText" );
      zglGetAddress( key_ClearState, zglLib, "key_ClearState" );

      zglGetAddress( mouse_X, zglLib, "mouse_X" );
      zglGetAddress( mouse_Y, zglLib, "mouse_Y" );
      zglGetAddress( mouse_DX, zglLib, "mouse_DX" );
      zglGetAddress( mouse_DY, zglLib, "mouse_DY" );
      zglGetAddress( mouse_Down, zglLib, "mouse_Down" );
      zglGetAddress( mouse_Up, zglLib, "mouse_Up" );
      zglGetAddress( mouse_Click, zglLib, "mouse_Click" );
      zglGetAddress( mouse_Wheel, zglLib, "mouse_Wheel" );
      zglGetAddress( mouse_ClearState, zglLib, "mouse_ClearState" );
      zglGetAddress( mouse_Lock, zglLib, "mouse_Lock" );

      zglGetAddress( tex_Add, zglLib, "tex_Add" );
      zglGetAddress( tex_Del, zglLib, "tex_Del" );
      zglGetAddress( tex_Create, zglLib, "tex_Create" );
      zglGetAddress( tex_CreateZero, zglLib, "tex_CreateZero" );
      zglGetAddress( tex_LoadFromFile, zglLib, "tex_LoadFromFile" );
      zglGetAddress( tex_SetFrameSize, zglLib, "tex_SetFrameSize" );
      zglGetAddress( tex_SetMask, zglLib, "tex_SetMask" );
      zglGetAddress( tex_GetData, zglLib, "tex_GetData" );
      zglGetAddress( tex_Filter, zglLib, "tex_Filter" );
      zglGetAddress( tex_SetAnisotropy, zglLib, "tex_SetAnisotropy" );

      zglGetAddress( Set2DMode, zglLib, "Set2DMode" );
      zglGetAddress( Set3DMode, zglLib, "Set3DMode" );
  
      zglGetAddress( zbuffer_SetDepth, zglLib, "zbuffer_SetDepth" );
      zglGetAddress( zbuffer_Clear, zglLib, "zbuffer_Clear" );

      zglGetAddress( scissor_Begin, zglLib, "scissor_Begin" );
      zglGetAddress( scissor_End, zglLib, "scissor_End" );

      zglGetAddress( rtarget_Add, zglLib, "rtarget_Add" );
      zglGetAddress( rtarget_Del, zglLib, "rtarget_Del" );
      zglGetAddress( rtarget_Set, zglLib, "rtarget_Set" );

      zglGetAddress( fx_SetBlendMode, zglLib, "fx_SetBlendMode" );
      zglGetAddress( fx2d_SetColorMix, zglLib, "fx2d_SetColorMix" );
      zglGetAddress( fx2d_SetVCA, zglLib, "fx2d_SetVCA" );
      zglGetAddress( fx2d_SetVertexes, zglLib, "fx2d_SetVertexes" );
      zglGetAddress( fx2d_SetScale, zglLib, "fx2d_SetScale" );

      zglGetAddress( cam2d_Set, zglLib, "cam2d_Set" );

      zglGetAddress( pr2d_Pixel, zglLib, "pr2d_Pixel" );
      zglGetAddress( pr2d_Line, zglLib, "pr2d_Line" );
    /*  pr2d_Triangle = zglGetAddress( zglLib, "pr2d_Triangle" );*/
      zglGetAddress( pr2d_Rect, zglLib, "pr2d_Rect" );
      zglGetAddress( pr2d_Circle, zglLib, "pr2d_Circle" );
      zglGetAddress( pr2d_Ellipse, zglLib, "pr2d_Ellipse" );

      zglGetAddress( ssprite2d_Draw, zglLib, "ssprite2d_Draw" );
      zglGetAddress( asprite2d_Draw, zglLib, "asprite2d_Draw" );
      zglGetAddress( csprite2d_Draw, zglLib, "csprite2d_Draw" );

      zglGetAddress( font_Add, zglLib, "font_Add" );
      zglGetAddress( font_Del, zglLib, "font_Del" );
      zglGetAddress( font_LoadFromFile, zglLib, "font_LoadFromFile" );
      zglGetAddress( text_Draw, zglLib, "text_Draw" );
      zglGetAddress( text_GetWidth, zglLib, "text_GetWidth" );

      zglGetAddress( snd_Init, zglLib, "snd_Init" );
      zglGetAddress( snd_Free, zglLib, "snd_Free" );
      zglGetAddress( snd_Add, zglLib, "snd_Add" );
      zglGetAddress( snd_Del, zglLib, "snd_Del" );
      zglGetAddress( snd_LoadFromFile, zglLib, "snd_LoadFromFile" );
      zglGetAddress( snd_Play, zglLib, "snd_Play" );
      zglGetAddress( snd_Stop, zglLib, "snd_Stop" );
      zglGetAddress( snd_SetVolume, zglLib, "snd_SetVolume" );
      zglGetAddress( snd_SetFrequency, zglLib, "snd_SetFrequency" );
      zglGetAddress( snd_SetFrequencyCoeff, zglLib, "snd_SetFrequencyCoeff" );
      zglGetAddress( snd_PlayFile, zglLib, "snd_PlayFile" );
      zglGetAddress( snd_StopFile, zglLib, "snd_StopFile" );
      zglGetAddress( snd_RestoreFile, zglLib, "snd_RestoreFile" );

      zglGetAddress( obj3d_Begin, zglLib, "obj3d_Begin" );
      zglGetAddress( obj3d_End, zglLib, "obj3d_End" );
      zglGetAddress( obj3d_Enable, zglLib, "obj3d_Enable" );
      zglGetAddress( obj3d_Disable, zglLib, "obj3d_Disable" );
      zglGetAddress( obj3d_SetColor, zglLib, "obj3d_SetColor" );
      zglGetAddress( obj3d_BindTexture, zglLib, "obj3d_BindTexture" );
      zglGetAddress( obj3d_SetMaterial, zglLib, "obj3d_SetMaterial" );
      zglGetAddress( obj3d_Rotate, zglLib, "obj3d_Rotate" );
      zglGetAddress( obj3d_Scale, zglLib, "obj3d_Scale" );
      zglGetAddress( obj3d_Move, zglLib, "obj3d_Move" );
      zglGetAddress( obj3d_SetMatrix, zglLib, "obj3d_SetMatrix" );
      zglGetAddress( obj3d_MulMatrix, zglLib, "obj3d_MulMatrix" );

      zglGetAddress( pr3d_Point, zglLib, "pr3d_Point" );
      zglGetAddress( pr3d_Line, zglLib, "pr3d_Line" );
      zglGetAddress( pr3d_Plane, zglLib, "pr3d_Plane" );
      zglGetAddress( pr3d_AABB, zglLib, "pr3d_AABB" );
      zglGetAddress( pr3d_Sphere, zglLib, "pr3d_Sphere" );

      zglGetAddress( ssprite3d_Draw, zglLib, "ssprite3d_Draw" );
      zglGetAddress( asprite3d_Draw, zglLib, "asprite3d_Draw" );

      zglGetAddress( cam3d_Set, zglLib, "cam3d_Set" );
      zglGetAddress( cam3d_Fly, zglLib, "cam3d_Fly" );
      zglGetAddress( cam3d_Strafe, zglLib, "cam3d_Strafe" );

      zglGetAddress( smesh_LoadFromFile, zglLib, "smesh_LoadFromFile" );
      zglGetAddress( smesh_Draw, zglLib, "smesh_Draw" );
      zglGetAddress( smesh_DrawGroup, zglLib, "smesh_DrawGroup" );
      zglGetAddress( smesh_Free, zglLib, "smesh_Free" );

      zglGetAddress( skmesh_LoadFromFile, zglLib, "skmesh_LoadFromFile" );
      zglGetAddress( skmesh_Animate, zglLib, "skmesh_Animate" );
      zglGetAddress( skmesh_Draw, zglLib, "skmesh_Draw" );
      zglGetAddress( skmesh_DrawGroup, zglLib, "skmesh_DrawGroup" );
      zglGetAddress( skmesh_DrawSkelet, zglLib, "skmesh_DrawSkelet" );
      zglGetAddress( skmesh_Free, zglLib, "skmesh_Free" );

      zglGetAddress( heightmap_Build, zglLib, "heightmap_Build" );
      zglGetAddress( heightmap_Draw, zglLib, "heightmap_Draw" );
      zglGetAddress( heightmap_Free, zglLib, "heightmap_Free" );
      zglGetAddress( heightmap_GetPlane, zglLib, "heightmap_GetPlane" );
      zglGetAddress( heightmap_GetYOffset, zglLib, "heightmap_GetYOffset" );

      zglGetAddress( vbo_Build, zglLib, "vbo_Build" );
      zglGetAddress( vbo_Free, zglLib, "vbo_Free" );

      zglGetAddress( frustum_Calc, zglLib, "frustum_Calc" );
      zglGetAddress( frustum_PointIn, zglLib, "frustum_PointIn" );
      zglGetAddress( frustum_PPointIn, zglLib, "frustum_PPointIn" );
      zglGetAddress( frustum_TriangleIn, zglLib, "frustum_TriangleIn" );
      zglGetAddress( frustum_SphereIn, zglLib, "frustum_SphereIn" );
      zglGetAddress( frustum_BoxIn, zglLib, "frustum_BoxIn" );
      zglGetAddress( frustum_CubeIn, zglLib, "frustum_CubeIn" );

      zglGetAddress( octree_Build, zglLib, "octree_Build" );
      zglGetAddress( octree_Free, zglLib, "octree_Free" );
      zglGetAddress( octree_Draw, zglLib, "octree_Draw" );
      zglGetAddress( octree_DrawDebug, zglLib, "octree_DrawDebug" );
      zglGetAddress( octree_DrawNode, zglLib, "octree_DrawNode" );
    /*  octree_DrawDFaces = zglGetAddress( zglLib, "octree_DrawDFaces" );*/

      zglGetAddress( light_Enable, zglLib, "light_Enable" );
      zglGetAddress( light_Disable, zglLib, "light_Disable" );
      zglGetAddress( light_SetPosition, zglLib, "light_SetPosition" );
      zglGetAddress( light_SetMaterial, zglLib, "light_SetMaterial" );

      zglGetAddress( fog_Enable, zglLib, "fog_Enable" );
      zglGetAddress( fog_Disable, zglLib, "fog_Disable" );
      zglGetAddress( fog_SetMode, zglLib, "fog_SetMode" );
      zglGetAddress( fog_SetColor, zglLib, "fog_SetColor" );
      zglGetAddress( fog_SetDensity, zglLib, "fog_SetDensity" );
      zglGetAddress( fog_SetBeginEnd, zglLib, "fog_SetBeginEnd" );

      zglGetAddress( skybox_Init, zglLib, "skybox_Init" );
      zglGetAddress( skybox_Draw, zglLib, "skybox_Draw" );
  
      zglGetAddress( shader_InitARB, zglLib, "shader_InitARB" );
      zglGetAddress( shader_LoadFromFileARB, zglLib, "shader_LoadFromFileARB" );
      zglGetAddress( shader_BeginARB, zglLib, "shader_BeginARB" );
      zglGetAddress( shader_EndARB, zglLib, "shader_EndARB" );
      zglGetAddress( shader_FreeARB, zglLib, "shader_FreeARB" );
      zglGetAddress( shader_InitGLSL, zglLib, "shader_InitGLSL" );
      zglGetAddress( shader_LoadFromFile, zglLib, "shader_LoadFromFile" );
      zglGetAddress( shader_Attach, zglLib, "shader_Attach" );
      zglGetAddress( shader_BeginLink, zglLib, "shader_BeginLink" );
      zglGetAddress( shader_EndLink, zglLib, "shader_EndLink" );
      zglGetAddress( shader_Begin, zglLib, "shader_Begin" );
      zglGetAddress( shader_End, zglLib, "shader_End" );
      zglGetAddress( shader_Free, zglLib, "shader_Free" );
      zglGetAddress( shader_GetUniform, zglLib, "shader_GetUniform" );
      zglGetAddress( shader_SetUniform1f, zglLib, "shader_SetUniform1f" );
      zglGetAddress( shader_SetUniform1i, zglLib, "shader_SetUniform1i" );
      zglGetAddress( shader_SetUniform2f, zglLib, "shader_SetUniform2f" );
      zglGetAddress( shader_SetUniform3f, zglLib, "shader_SetUniform3f" );
      zglGetAddress( shader_SetUniform4f, zglLib, "shader_SetUniform4f" );
      zglGetAddress( shader_GetAttrib, zglLib, "shader_GetAttrib" );
      zglGetAddress( shader_SetAttrib1f, zglLib, "shader_SetAttrib1f" );
      zglGetAddress( shader_SetAttrib2f, zglLib, "shader_SetAttrib2f" );
      zglGetAddress( shader_SetAttrib3f, zglLib, "shader_SetAttrib3f" );
      zglGetAddress( shader_SetAttrib4f, zglLib, "shader_SetAttrib4f" );
      zglGetAddress( shader_SetAttribPf, zglLib, "shader_SetAttribPf" );
      zglGetAddress( shader_SetParameter4f, zglLib, "shader_SetParameter4f" );
  
      zglGetAddress( m_Round, zglLib, "m_Round" );
      zglGetAddress( m_Cos, zglLib, "m_Cos" );
      zglGetAddress( m_Sin, zglLib, "m_Sin" );
      zglGetAddress( m_Distance, zglLib, "m_Distance" );
      zglGetAddress( m_FDistance, zglLib, "m_FDistance" );
      zglGetAddress( m_Angle, zglLib, "m_Angle" );

      zglGetAddress( vector_Get, zglLib, "vector_Get" );
      zglGetAddress( vector_Add, zglLib, "vector_Add" );
      zglGetAddress( vector_Sub, zglLib, "vector_Sub" );
      zglGetAddress( vector_Mul, zglLib, "vector_Mul" );
      zglGetAddress( vector_Div, zglLib, "vector_Div" );
      zglGetAddress( vector_AddV, zglLib, "vector_AddV" );
      zglGetAddress( vector_SubV, zglLib, "vector_SubV" );
      zglGetAddress( vector_MulV, zglLib, "vector_MulV" );
      zglGetAddress( vector_DivV, zglLib, "vector_DivV" );
      zglGetAddress( vector_MulM3f, zglLib, "vector_MulM3f" );
      zglGetAddress( vector_MulM4f, zglLib, "vector_MulM4f" );
      zglGetAddress( vector_MulInvM4f, zglLib, "vector_MulInvM4f" );
      zglGetAddress( vector_Negate, zglLib, "vector_Negate" );
      zglGetAddress( vector_Normalize, zglLib, "vector_Normalize" );
      zglGetAddress( vector_Angle, zglLib, "vector_Angle" );
      zglGetAddress( vector_Cross, zglLib, "vector_Cross" );
      zglGetAddress( vector_Dot, zglLib, "vector_Dot" );
      zglGetAddress( vector_Distance, zglLib, "vector_Distance" );
      zglGetAddress( vector_FDistance, zglLib, "vector_FDistance" );
      zglGetAddress( vector_Length, zglLib, "vector_Length" );
      zglGetAddress( vector_Lerp, zglLib, "vector_Lerp" );

      zglGetAddress( matrix3f_Get, zglLib, "matrix3f_Get" );
      zglGetAddress( matrix3f_OrthoNormalize, zglLib, "matrix3f_OrthoNormalize" );
      zglGetAddress( matrix3f_Transpose, zglLib, "matrix3f_Transpose" );
      zglGetAddress( matrix3f_Rotate, zglLib, "matrix3f_Rotate" );
      zglGetAddress( matrix3f_Add, zglLib, "matrix3f_Add" );
      zglGetAddress( matrix3f_Mul, zglLib, "matrix3f_Mul" );

      zglGetAddress( matrix4f_Transpose, zglLib, "matrix4f_Transpose" );
      zglGetAddress( matrix4f_Determinant, zglLib, "matrix4f_Determinant" );
      zglGetAddress( matrix4f_Inverse, zglLib, "matrix4f_Inverse" );
      zglGetAddress( matrix4f_Translate, zglLib, "matrix4f_Translate" );
      zglGetAddress( matrix4f_Rotate, zglLib, "matrix4f_Rotate" );
      zglGetAddress( matrix4f_Scale, zglLib, "matrix4f_Scale" );
      zglGetAddress( matrix4f_Mul, zglLib, "matrix4f_Mul" );

      zglGetAddress( line3d_ClosestPoint, zglLib, "line3d_ClosestPoint" );

      zglGetAddress( plane_Get, zglLib, "plane_Get" );
      zglGetAddress( plane_Distance, zglLib, "plane_Distance" );

      zglGetAddress( tri_GetNormal, zglLib, "tri_GetNormal" );

      zglGetAddress( col2d_PointInRect, zglLib, "col2d_PointInRect" );
      zglGetAddress( col2d_PointInCircle, zglLib, "col2d_PointInCircle" );
      zglGetAddress( col2d_PointInPolyLine, zglLib, "col2d_PointInPolyLine" );
      zglGetAddress( col2d_Line, zglLib, "col2d_Line" );
      zglGetAddress( col2d_LineVsRect, zglLib, "col2d_LineVsRect" );
      zglGetAddress( col2d_LineVsCircle, zglLib, "col2d_LineVsCircle" );
      zglGetAddress( col2d_LineVsCircleXY, zglLib, "col2d_LineVsCircleXY" );
      zglGetAddress( col2d_LineVsPolyLine, zglLib, "col2d_LineVsPolyLine" );
      zglGetAddress( col2d_PolyLine, zglLib, "col2d_PolyLine" );
      zglGetAddress( col2d_PolyLineVsRect, zglLib, "col2d_PolyLineVsRect" );
      zglGetAddress( col2d_PolyLineVsCircle, zglLib, "col2d_PolyLineVsCircle" );
      zglGetAddress( col2d_PolyLineVsCircleXY, zglLib, "col2d_PolyLineVsCircleXY" );
      zglGetAddress( col2d_Rect, zglLib, "col2d_Rect" );
      zglGetAddress( col2d_RectInRect, zglLib, "col2d_RectInRect" );
      zglGetAddress( col2d_RectInCircle, zglLib, "col2d_RectInCircle" );
      zglGetAddress( col2d_RectVsCircle, zglLib, "col2d_RectVsCircle" );
      zglGetAddress( col2d_Circle, zglLib, "col2d_Circle" );
      zglGetAddress( col2d_CircleInCircle, zglLib, "col2d_CircleInCircle" );
      zglGetAddress( col2d_CircleInRect, zglLib, "col2d_CircleInRect" );
      zglGetAddress( col2dEx_LastX, zglLib, "col2dEx_LastX" );
      zglGetAddress( col2dEx_LastY, zglLib, "col2dEx_LastY" );
      zglGetAddress( col2dEx_LastLineA, zglLib, "col2dEx_LastLineA" );
      zglGetAddress( col2dEx_LastLineB, zglLib, "col2dEx_LastLineB" );
      zglGetAddress( col2dEx_PolyRotate, zglLib, "col2dEx_PolyRotate" );
      zglGetAddress( col2dEx_PolyScale, zglLib, "col2dEx_PolyScale" );
      zglGetAddress( col2dEx_PolyMove, zglLib, "col2dEx_PolyMove" );
      zglGetAddress( col2dEx_PolyCenter, zglLib, "col2dEx_PolyCenter" );
      zglGetAddress( col2dEx_PolyRect, zglLib, "col2dEx_PolyRect" );
      zglGetAddress( col2dEx_CalcLineCross, zglLib, "col2dEx_CalcLineCross" );

      zglGetAddress( col3d_PointInTri, zglLib, "col3d_PointInTri" );
      zglGetAddress( col3d_PointInAABB, zglLib, "col3d_PointInAABB" );
      zglGetAddress( col3d_PointInOBB, zglLib, "col3d_PointInOBB" );
      zglGetAddress( col3d_PointInSphere, zglLib, "col3d_PointInSphere" );
      zglGetAddress( col3d_LineVsAABB, zglLib, "col3d_LineVsAABB" );
      zglGetAddress( col3d_LineVsOBB, zglLib, "col3d_LineVsOBB" );
      zglGetAddress( col3d_LineVsSphere, zglLib, "col3d_LineVsSphere" );
      zglGetAddress( col3d_PlaneVsSphere, zglLib, "col3d_PlaneVsSphere" );
      zglGetAddress( col3d_AABBVsAABB, zglLib, "col3d_AABBVsAABB" );
      zglGetAddress( col3d_AABBVsOBB, zglLib, "col3d_AABBVsOBB" );
      zglGetAddress( col3d_AABBVsSphere, zglLib, "col3d_AABBVsSphere" );
      zglGetAddress( col3d_OBBVsOBB, zglLib, "col3d_OBBVsOBB" );
      zglGetAddress( col3d_OBBVsSphere, zglLib, "col3d_OBBVsSphere" );
      zglGetAddress( col3d_SphereVsSphere, zglLib, "col3d_SphereVsSphere" );
      zglGetAddress( col3d_SphereVsNode, zglLib, "col3d_SphereVsNode" );

      zglGetAddress( file_Open, zglLib, "file_Open" );
      zglGetAddress( file_Exists, zglLib, "file_Exists" );
      zglGetAddress( file_Seek, zglLib, "file_Seek" );
      zglGetAddress( file_GetPos, zglLib, "file_GetPos" );
      zglGetAddress( file_Read, zglLib, "file_Read" );
      zglGetAddress( file_Write, zglLib, "file_Write" );
      zglGetAddress( file_Trunc, zglLib, "file_Trunc" );
      zglGetAddress( file_GetSize, zglLib, "file_GetSize" );
      zglGetAddress( file_Flush, zglLib, "file_Flush" );
      zglGetAddress( file_Close, zglLib, "file_Close" );

      zglGetAddress( mem_LoadFromFile, zglLib, "mem_LoadFromFile" );
      zglGetAddress( mem_SaveToFile, zglLib, "mem_SaveToFile" );
      zglGetAddress( mem_Seek, zglLib, "mem_Seek" );
      zglGetAddress( mem_Read, zglLib, "mem_Read" );
      zglGetAddress( mem_Write, zglLib, "mem_Write" );
      zglGetAddress( mem_SetSize, zglLib, "mem_SetSize" );
      zglGetAddress( mem_Free, zglLib, "mem_Free" );
    }
    else
    {
      #ifdef __LINUX__
      printf( "Error while loading ZenGL Engine" );
      #endif
      #ifdef __WIN32__
      MessageBoxA( 0, "Error while loading ZenGL Engine", "Error", 0x00000010 );
      #endif
    }
}

#ifdef __CPP__
}
#endif

#endif
