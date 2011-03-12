{
 *  Copyright © Kemka Andrey aka Andru
 *  mail: dr.andru@gmail.com
 *  site: http://andru-kun.inf.ua
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
unit zgl_opengles_all;

{$I zgl_config.cfg}
{$IFDEF LINUX_OR_DARWIN}
  {$DEFINE stdcall := cdecl}
{$ENDIF}

interface
uses
  math,
  {$IFDEF LINUX}
  X, XLib
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows
  {$ENDIF}
  ;

function InitGLES : Boolean;
procedure FreeGLES;

function gl_GetProc( const Proc : AnsiString ) : Pointer;
function gl_IsSupported( const Extension, SearchIn : AnsiString ) : Boolean;

const
  {$IFNDEF USE_GLES_ON_DESKTOP}
    {$IFDEF LINUX}
    libEGL     = 'libEGL.so';
    libGLES_CM = 'libGLES_CM.so';
    libGLESv1  = 'libGLESv1.so';
    libGLESv2  = 'libGLESv2.so';
    {$ENDIF}
    {$IFDEF WINDOWS}
    libEGL     = 'libEGL.dll';
    libGLES_CM = 'libGLES_CM.dll';
    libGLESv1  = 'libGLESv1.dll';
    libGLESv2  = 'libGLESv2.dll';
    {$ENDIF}
    {$IFDEF DARWIN}
    libEGL  = '';
    libGLES = '/System/Library/Frameworks/OpenGLES.framework/OpenGLES';
    {$ENDIF}
  {$ELSE}
    {$IFDEF LINUX}
      {$IFNDEF USE_AMD_DRIVER}
      libEGL     = 'libEGL.so';
      libGLES_CM = 'libGLES_CM.so';
      libGLESv1  = 'libGLESv1.so';
      libGLESv2  = 'libGLESv2.so';
      {$ELSE}
      libEGL     = 'libGL.so.1';
      libGLES_CM = 'libGL.so.1';
      libGLESv1  = 'libGL.so.1';
      libGLESv2  = 'libGL.so.1';
      {$ENDIF}
    {$ENDIF}
    {$IFDEF WINDOWS}
    libEGL     = 'libEGL.dll';
    libGLES_CM = 'libGLES_CM.dll';
    libGLESv1  = 'libGLESv1.dll';
    libGLESv2  = 'libGLESv2.dll';
    {$ENDIF}
  {$ENDIF}

  GL_FALSE                          = 0;
  GL_TRUE                           = 1;
  GL_ZERO                           = 0;
  GL_ONE                            = 1;

  // String Name
  GL_VENDOR                         = $1F00;
  GL_RENDERER                       = $1F01;
  GL_VERSION                        = $1F02;
  GL_EXTENSIONS                     = $1F03;

  // DataType
  GL_UNSIGNED_BYTE                  = $1401;
  GL_UNSIGNED_SHORT                 = $1403;
  GL_FLOAT                          = $1406;

  // PixelFormat
  GL_RGBA                           = $1908;

  // Alpha Function
  GL_NEVER                          = $0200;
  GL_LESS                           = $0201;
  GL_EQUAL                          = $0202;
  GL_LEQUAL                         = $0203;
  GL_GREATER                        = $0204;
  GL_NOTEQUAL                       = $0205;
  GL_GEQUAL                         = $0206;
  GL_ALWAYS                         = $0207;

  // Blend
  GL_BLEND                          = $0BE2;
  // Blending Factor Dest
  GL_SRC_COLOR                      = $0300;
  GL_ONE_MINUS_SRC_COLOR            = $0301;
  GL_SRC_ALPHA                      = $0302;
  GL_ONE_MINUS_SRC_ALPHA            = $0303;
  GL_DST_ALPHA                      = $0304;
  GL_ONE_MINUS_DST_ALPHA            = $0305;
  // Blending Factor Src
  GL_DST_COLOR                      = $0306;
  GL_ONE_MINUS_DST_COLOR            = $0307;
  GL_SRC_ALPHA_SATURATE             = $0308;

  // blendOP
  GL_FUNC_ADD_EXT                   = $8006; // GL_FUNC_ADD_OES
  GL_MIN_EXT                        = $8007;
  GL_MAX_EXT                        = $8008;
  GL_FUNC_SUBTRACT_EXT              = $800A; // GL_FUNC_SUBTRACT_OES
  GL_FUNC_REVERSE_SUBTRACT_EXT      = $800B; // GL_FUNC_REVERSE_SUBTRACT_OES

  GL_BLEND_DST_RGB_EXT              = $80C8; // GL_BLEND_DST_RGB_OES
  GL_BLEND_SRC_RGB_EXT              = $80C9; // GL_BLEND_SRC_RGB_OES
  GL_BLEND_DST_ALPHA_EXT            = $80CA; // GL_BLEND_DST_ALPHA_OES
  GL_BLEND_SRC_ALPHA_EXT            = $80CB; // GL_BLEND_SRC_ALPHA_OES
  GL_BLEND_EQUATION_RGB_EXT         = $8009; // GL_BLEND_EQUATION_RGB_OES
  GL_BLEND_EQUATION_ALPHA_EXT       = $883D; // GL_BLEND_EQUATION_ALPHA_OES

  // Hint Mode
  GL_DONT_CARE                      = $1100;
  GL_FASTEST                        = $1101;
  GL_NICEST                         = $1102;

  // Hints
  GL_PERSPECTIVE_CORRECTION_HINT    = $0C50;
  GL_LINE_SMOOTH_HINT               = $0C52;
  GL_FOG_HINT                       = $0C54;

  // Shading Model
  GL_SHADE_MODEL                    = $0B54;
  GL_FLAT                           = $1D00;
  GL_SMOOTH                         = $1D01;

  // Buffer Bit
  GL_DEPTH_BUFFER_BIT               = $00000100;
  GL_STENCIL_BUFFER_BIT             = $00000400;
  GL_COLOR_BUFFER_BIT               = $00004000;

  // Enable
  GL_LINE_SMOOTH                    = $0B20;
  GL_NORMALIZE                      = $0BA1;

  // glBegin/glEnd
  GL_POINTS                         = $0000;
  GL_LINES                          = $0001;
  GL_TRIANGLES                      = $0004;
  GL_TRIANGLE_STRIP                 = $0005;
  GL_TRIANGLE_FAN                   = $0006;
  GL_QUADS                          = $0007; // Doesn't exists

  // Texture
  GL_TEXTURE_2D                     = $0DE1;
  GL_TEXTURE0_ARB                   = $84C0; // GL_TEXTURE0
  GL_MAX_TEXTURE_SIZE               = $0D33;
  GL_MAX_TEXTURE_UNITS_ARB          = $84E2; // GL_MAX_TEXTURE_UNITS
  GL_TEXTURE_MAX_ANISOTROPY_EXT     = $84FE;
  GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT = $84FF;
  // Texture Wrap Mode
  GL_CLAMP_TO_EDGE                  = $812F;
  GL_REPEAT                         = $2901;
  // Texture Env Mode
  GL_MODULATE                       = $2100;
  GL_DECAL                          = $2101;
  // Texture Env Parameter
  GL_TEXTURE_ENV_MODE               = $2200;
  GL_TEXTURE_ENV_COLOR              = $2201;
  // Texture Env Target
  GL_TEXTURE_ENV                    = $2300;
  // Texture Mag Filter
  GL_NEAREST                        = $2600;
  GL_LINEAR                         = $2601;
  // Mipmaps
  GL_GENERATE_MIPMAP                = $8191;
  GL_GENERATE_MIPMAP_HINT           = $8192;
  // Texture Min Filter
  GL_NEAREST_MIPMAP_NEAREST         = $2700;
  GL_LINEAR_MIPMAP_NEAREST          = $2701;
  GL_NEAREST_MIPMAP_LINEAR          = $2702;
  GL_LINEAR_MIPMAP_LINEAR           = $2703;
  // Texture Parameter Name
  GL_TEXTURE_MAG_FILTER             = $2800;
  GL_TEXTURE_MIN_FILTER             = $2801;
  GL_TEXTURE_WRAP_S                 = $2802;
  GL_TEXTURE_WRAP_T                 = $2803;

  GL_COMBINE_ARB                    = $8570; // GL_COMBINE
  GL_COMBINE_RGB_ARB                = $8571; // GL_COMBINE_RGB
  GL_COMBINE_ALPHA_ARB              = $8572; // GL_COMBINE_ALPHA
  GL_SOURCE0_RGB_ARB                = $8580; // GL_SRC0_RGB
  GL_SOURCE1_RGB_ARB                = $8581; // GL_SRC1_RGB
  GL_SOURCE2_RGB_ARB                = $8582; // GL_SRC2_RGB
  GL_SOURCE0_ALPHA_ARB              = $8588; // GL_SRC0_ALPHA
  GL_SOURCE1_ALPHA_ARB              = $8589; // GL_SRC1_ALPHA
  GL_SOURCE2_ALPHA_ARB              = $858A; // GL_SRC2_ALPHA
  GL_OPERAND0_RGB_ARB               = $8590; // GL_OPERAND0_RGB
  GL_OPERAND1_RGB_ARB               = $8591; // GL_OPERAND1_RGB
  GL_OPERAND2_RGB_ARB               = $8592; // GL_OPERAND2_RGB
  GL_OPERAND0_ALPHA_ARB             = $8598; // GL_OPERAND0_ALPHA
  GL_OPERAND1_ALPHA_ARB             = $8599; // GL_OPERAND1_ALPHA
  GL_OPERAND2_ALPHA_ARB             = $859A; // GL_OPERAND2_ALPHA
  GL_RGB_SCALE_ARB                  = $8573; // GL_RGB_SCALE
  GL_ADD_SIGNED_ARB                 = $8574; // GL_ADD_SIGNED
  GL_INTERPOLATE_ARB                = $8575; // GL_INTERPOLATE
  GL_SUBTRACT_ARB                   = $84E7; // GL_SUBTRACT
  GL_CONSTANT_ARB                   = $8576; // GL_CONSTANT
  GL_PRIMARY_COLOR_ARB              = $8577; // GL_PRIMARY_COLOR
  GL_PREVIOUS_ARB                   = $8578; // GL_PREVIOUS
  GL_DOT3_RGB                       = $86AE; // GL_DOT3_RGB
  GL_DOT3_RGBA                      = $86AF; // GL_DOT3_RGBA

  // Vertex Array
  GL_VERTEX_ARRAY                   = $8074;
  GL_NORMAL_ARRAY                   = $8075;
  GL_COLOR_ARRAY                    = $8076;
  GL_TEXTURE_COORD_ARRAY            = $8078;

  // FBO
  GL_FRAMEBUFFER                    = $8D40; // GL_FRAMEBUFFER_OES
  GL_RENDERBUFFER                   = $8D41; // GL_RENDERBUFFER_OES
  GL_DEPTH_COMPONENT16              = $81A5; // GL_DEPTH_COMPONENT16_OES
  GL_DEPTH_COMPONENT24              = $81A6; // GL_DEPTH_COMPONENT24_OES
  GL_DEPTH_COMPONENT32              = $81A7; // GL_DEPTH_COMPONENT32_OES
  GL_COLOR_ATTACHMENT0              = $8CE0; // GL_COLOR_ATTACHMENT0_OES
  GL_DEPTH_ATTACHMENT               = $8D00; // GL_DEPTH_ATTACHMENT_OES
  GL_MAX_RENDERBUFFER_SIZE          = $84E8; // GL_MAX_RENDERBUFFER_SIZE_OES

  // Matrices
  GL_MODELVIEW_MATRIX               = $0BA6;
  GL_PROJECTION_MATRIX              = $0BA7;

  // Matrix Mode
  GL_MODELVIEW                      = $1700;
  GL_PROJECTION                     = $1701;
  GL_TEXTURE                        = $1702;

  // Test
  GL_DEPTH_TEST                     = $0B71;
  GL_STENCIL_TEST                   = $0B90;
  GL_ALPHA_TEST                     = $0BC0;
  GL_SCISSOR_TEST                   = $0C11;

  // StencilOp
  GL_KEEP                           = $1E00;
  GL_REPLACE                        = $1E01;
  GL_INCR                           = $1E02;
  GL_DECR                           = $1E03;

  // VBO
  GL_BUFFER_SIZE_ARB                = $8764; // GL_BUFFER_SIZE
  GL_ARRAY_BUFFER_ARB               = $8892; // GL_ARRAY_BUFFER
  GL_ELEMENT_ARRAY_BUFFER_ARB       = $8893; // GL_ELEMENT_ARRAY_BUFFER
  GL_WRITE_ONLY_ARB                 = $88B9; // GL_WRITE_ONLY_OES, GL_OES_mapbuffer
  GL_STATIC_DRAW_ARB                = $88E4;
  GL_DYNAMIC_DRAW_ARB               = $88E8;

  // Triangulation
  {GLU_TESS_BEGIN                    = $18704;
  GLU_TESS_VERTEX                   = $18705;
  GLU_TESS_END                      = $18706;
  GLU_TESS_ERROR                    = $18707;
  GLU_TESS_EDGE_FLAG                = $18708;
  GLU_TESS_COMBINE                  = $18709;
  GLU_TESS_BEGIN_DATA               = $1870A;
  GLU_TESS_VERTEX_DATA              = $1870B;
  GLU_TESS_END_DATA                 = $1870C;
  GLU_TESS_ERROR_DATA               = $1870D;
  GLU_TESS_EDGE_FLAG_DATA           = $1870E;
  GLU_TESS_COMBINE_DATA             = $1870F;}

type
  // EGL Types
  {$IFDEF LINUX}
  EGLNativeDisplayType = PDisplay;
  EGLNativeWindowType  = TWindow;
  {$ENDIF}
  {$IFDEF WINDOWS}
  EGLNativeDisplayType = HDC;
  EGLNativeWindowType  = HWND;
  {$ENDIF}
  EGLBoolean      = LongBool;
  EGLint          = LongInt;
  PEGLint         = ^EGLint;
  EGLenum         = LongWord;
  EGLConfig       = Pointer;
  PEGLConfig      = ^EGLConfig;
  EGLContext      = Pointer;
  EGLDisplay      = Pointer;
  EGLSurface      = Pointer;
  EGLClientBuffer = Pointer;

type
  GLenum     = Cardinal;      PGLenum     = ^GLenum;
  GLboolean  = Byte;          PGLboolean  = ^GLboolean;
  GLbitfield = Cardinal;      PGLbitfield = ^GLbitfield;
  GLbyte     = ShortInt;      PGLbyte     = ^GLbyte;
  GLshort    = SmallInt;      PGLshort    = ^GLshort;
  GLint      = Integer;       PGLint      = ^GLint;
  GLsizei    = Integer;       PGLsizei    = ^GLsizei;
  GLubyte    = Byte;          PGLubyte    = ^GLubyte;
  GLushort   = Word;          PGLushort   = ^GLushort;
  GLuint     = Cardinal;      PGLuint     = ^GLuint;
  GLfloat    = Single;        PGLfloat    = ^GLfloat;
  GLclampf   = Single;        PGLclampf   = ^GLclampf;
  GLdouble   = Double;        PGLdouble   = ^GLdouble;
  GLclampd   = Double;        PGLclampd   = ^GLclampd;
{ GLvoid     = void; }        PGLvoid     = Pointer;
                              PPGLvoid    = ^PGLvoid;

var
  glGetString  : function(name: GLenum): PAnsiChar; stdcall;
  glHint       : procedure(target, mode: GLenum); stdcall;
  glShadeModel : procedure(mode: GLenum); stdcall;
  glReadPixels : procedure(x, y: GLint; width, height: GLsizei; format, atype: GLenum; pixels: Pointer); stdcall;
  // Clear
  glClear      : procedure(mask: GLbitfield); stdcall;
  glClearColor : procedure(red, green, blue, alpha: GLclampf); stdcall;
  {$IF DEFINED(USE_GLES_ON_DESKTOP) and DEFINED(USE_AMD_DRIVERS)}
  glClearDepth : procedure(depth: GLclampd); stdcall;
  {$ELSE}
  glClearDepth : procedure(depth: GLclampf); stdcall;
  {$ENDIF}
  // Get
  glGetFloatv   : procedure(pname: GLenum; params: PGLfloat); stdcall;
  glGetIntegerv : procedure(pname: GLenum; params: PGLint); stdcall;
  // State
  glEnable             : procedure(cap: GLenum); stdcall;
  glEnableClientState  : procedure(aarray: GLenum); stdcall;
  glDisable            : procedure(cap: GLenum); stdcall;
  glDisableClientState : procedure(aarray: GLenum); stdcall;
  glIsEnabled          : function(cap: GLenum): GLboolean; stdcall;
  // Viewport
  glViewport : procedure(x, y: GLint; width, height: GLsizei); stdcall;
  {$IF DEFINED(USE_GLES_ON_DESKTOP) and DEFINED(USE_AMD_DRIVERS)}
  glOrtho : procedure(left, right, bottom, top, zNear, zFar: GLdouble); stdcall;
  {$ELSE}
  glOrtho : procedure(left, right, bottom, top, zNear, zFar: GLfloat); stdcall;
  {$ENDIF}
  glScissor : procedure(x, y: GLint; width, height: GLsizei); stdcall;
  // Depth
  glDepthFunc : procedure(func: GLenum); stdcall;
  glDepthMask : procedure(flag: GLboolean); stdcall;
  // Color
  glColorMask    : procedure(red, green, blue, alpha: GLboolean); stdcall;
  glColorPointer : procedure(size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer); stdcall;
  // Alpha
  glAlphaFunc         : procedure(func: GLenum; ref: GLclampf); stdcall;
  glBlendFunc         : procedure(sfactor, dfactor: GLenum); stdcall;
  glBlendEquation     : procedure(mode: GLenum); stdcall;
  glBlendFuncSeparate : procedure(sfactorRGB: GLenum; dfactorRGB: GLenum; sfactorAlpha: GLenum; dfactorAlpha: GLenum); stdcall;
  // Matrix
  glPushMatrix   : procedure; stdcall;
  glPopMatrix    : procedure; stdcall;
  glMatrixMode   : procedure(mode: GLenum); stdcall;
  glLoadIdentity : procedure; stdcall;
  glLoadMatrixf  : procedure(const m: PGLfloat); stdcall;
  glRotatef      : procedure(angle, x, y, z: GLfloat); stdcall;
  glScalef       : procedure(x, y, z: GLfloat); stdcall;
  glTranslatef   : procedure(x, y, z: GLfloat); stdcall;
  // Vertex
  glVertexPointer : procedure(size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer); stdcall;
  // Texture
  glBindTexture       : procedure(target: GLenum; texture: GLuint); stdcall;
  glGenTextures       : procedure(n: GLsizei; textures: PGLuint); stdcall;
  glDeleteTextures    : procedure(n: GLsizei; const textures: PGLuint); stdcall;
  glTexParameterf     : procedure(target: GLenum; pname: GLenum; param: GLfloat); stdcall;
  glTexParameteri     : procedure(target: GLenum; pname: GLenum; param: GLint); stdcall;
  glPixelStorei       : procedure(pname: GLenum; param: GLint); stdcall;
  glTexImage2D        : procedure(target: GLenum; level, internalformat: GLint; width, height: GLsizei; border: GLint; format, atype: GLenum; const pixels: Pointer); stdcall;
  glTexSubImage2D     : procedure(target: GLenum; level, xoffset, yoffset: GLint; width, height: GLsizei; format, atype: GLenum; const pixels: Pointer); stdcall;
  glCopyTexSubImage2D : procedure(target: GLenum; level, xoffset, yoffset, x, y: GLint; width, height: GLsizei); stdcall;
  glTexEnvi           : procedure(target: GLenum; pname: GLenum; param: GLint); stdcall;
  // TexCoords
  glTexCoordPointer : procedure(size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer); stdcall;
  //
  glDrawArrays : procedure(mode: GLenum; first: GLint; count: GLsizei); stdcall;
  // FBO
  glIsRenderbuffer          : function(renderbuffer: GLuint): GLboolean; stdcall;
  glBindRenderbuffer        : procedure(target: GLenum; renderbuffer: GLuint); stdcall;
  glDeleteRenderbuffers     : procedure(n: GLsizei; const renderbuffers: PGLuint); stdcall;
  glGenRenderbuffers        : procedure(n: GLsizei; renderbuffers: PGLuint); stdcall;
  glRenderbufferStorage     : procedure(target: GLenum; internalformat: GLenum; width: GLsizei; height: GLsizei); stdcall;
  glIsFramebuffer           : function(framebuffer: GLuint): GLboolean; stdcall;
  glBindFramebuffer         : procedure(target: GLenum; framebuffer: GLuint); stdcall;
  glDeleteFramebuffers      : procedure(n: GLsizei; const framebuffers: PGLuint); stdcall;
  glGenFramebuffers         : procedure(n: GLsizei; framebuffers: PGLuint); stdcall;
  glCheckFramebufferStatus  : function(target: GLenum): GLenum; stdcall;
  glFramebufferTexture2D    : procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint); stdcall;
  glFramebufferRenderbuffer : procedure(target: GLenum; attachment: GLenum; renderbuffertarget: GLenum; renderbuffer: GLuint); stdcall;

  // State
  procedure glBegin(mode: GLenum);
  procedure glEnd;
  // Color
  procedure glColor4ub(red, green, blue, alpha: GLubyte); {$IFDEF USE_INLINE} inline; {$ENDIF}
  procedure glColor4ubv(v: PGLubyte); {$IFDEF USE_INLINE} inline; {$ENDIF}
  procedure glColor4f(red, green, blue, alpha: GLfloat); {$IFDEF USE_INLINE} inline; {$ENDIF}
  // Matrix
  procedure gluPerspective(fovy, aspect, zNear, zFar: GLdouble);
  // Vertex
  procedure glVertex2f(x, y: GLfloat);
  procedure glVertex2fv(v: PGLfloat);
  procedure glVertex3f(x, y, z: GLfloat);
  // Texture
  procedure glGetTexImage(target: GLenum; level: GLint; format: GLenum; atype: GLenum; pixels: Pointer);
  // TexCoords
  procedure glTexCoord2f(s, t: GLfloat);
  procedure glTexCoord2fv(v: PGLfloat);

// Triangulation
  {$IFDEF USE_TRIANGULATION}
  {procedure gluDeleteTess(tess: Integer); stdcall external libGLU;
  function  gluErrorString(error: Integer): PChar; stdcall external libGLU;
  function  gluNewTess: Integer; stdcall external libGLU;
  procedure gluTessBeginContour(tess: Integer); stdcall external libGLU;
  procedure gluTessBeginPolygon(tess: Integer; data: Pointer); stdcall external libGLU;
  procedure gluTessCallback(tess: Integer; which: Integer; fn: Pointer); stdcall external libGLU;
  procedure gluTessEndContour(tess: Integer); stdcall external libGLU;
  procedure gluTessEndPolygon(tess: Integer); stdcall external libGLU;
  procedure gluTessVertex(tess: Integer; vertex: PDouble; data: Pointer); stdcall external libGLU;}
  {$ENDIF}

// EGL
const
  EGL_SUCCESS         = $3000;

  EGL_DEFAULT_DISPLAY = 0;
  EGL_NO_CONTEXT      = 0;
  EGL_NO_DISPLAY      = 0;
  EGL_NO_SURFACE      = 0;

  EGL_NONE            = $3038;

  EGL_ALPHA_SIZE      = $3021;
  EGL_BLUE_SIZE       = $3022;
  EGL_GREEN_SIZE      = $3023;
  EGL_RED_SIZE        = $3024;
  EGL_DEPTH_SIZE      = $3025;
  EGL_STENCIL_SIZE    = $3026;
  EGL_SAMPLES         = $3031;

  EGL_SURFACE_TYPE    = $3033;
  EGL_PBUFFER_BIT     = $0001;
  EGL_WINDOW_BIT      = $0004;

  EGL_RENDERABLE_TYPE = $3040;
  EGL_OPENGL_ES_BIT   = $0001;
  EGL_OPENGL_ES2_BIT  = $0004;

var
  eglGetProcAddress      : function( name: PAnsiChar ) : Pointer; stdcall;
  eglGetError            : function : GLint; stdcall;
  eglGetDisplay          : function( display_id : EGLNativeDisplayType ) : EGLDisplay; stdcall;
  eglInitialize          : function( dpy : EGLDisplay; major : PEGLint; minor : PEGLint ) : EGLBoolean; stdcall;
  eglTerminate           : function( dpy : EGLDisplay ) : EGLBoolean; stdcall;
  eglChooseConfig        : function( dpy : EGLDisplay; attrib_list : PEGLint; configs : PEGLConfig; config_size : EGLint; num_config : PEGLint ) : EGLBoolean; stdcall;
  eglCreateWindowSurface : function( dpy : EGLDisplay; config : EGLConfig; win : EGLNativeWindowType; attrib_list : PEGLint ) : EGLSurface; stdcall;
  eglDestroySurface      : function( dpy : EGLDisplay; surface : EGLSurface ) : EGLBoolean; stdcall;
  eglSwapInterval        : function( dpy : EGLDisplay; interval : EGLint ) : EGLBoolean; stdcall;
  eglCreateContext       : function( dpy : EGLDisplay; config : EGLConfig; share_context : EGLContext; attrib_list : PEGLint ) : EGLContext; stdcall;
  eglDestroyContext      : function( dpy : EGLDisplay; ctx : EGLContext ) : EGLBoolean; stdcall;
  eglMakeCurrent         : function( dpy : EGLDisplay; draw : EGLSurface; read : EGLSurface; ctx : EGLContext ) : EGLBoolean; stdcall;
  eglSwapBuffers         : function( dpy : EGLDisplay; surface : EGLSurface ) : EGLBoolean; stdcall;

var
  eglLibrary  : {$IFDEF WINDOWS} LongWord {$ELSE} Pointer {$ENDIF};
  glesLibrary : {$IFDEF WINDOWS} LongWord {$ELSE} Pointer {$ENDIF};

implementation
uses
  zgl_math_2d,
  zgl_types,
  zgl_utils;

var
  RenderMode     : LongWord;
  RenderQuad     : Boolean;
  RenderTextured : Boolean;
  // Buffers
  newTriangle  : Boolean;
  newTriangleC : Integer;
  bColor       : LongWord;
  bVertices    : array of array[ 0..2 ] of Single;
  bTexCoords   : array of zglTPoint2D;
  bColors      : array of LongWord;
  bSize        : Integer;

function InitGLES : Boolean;
begin
  {$IFDEF LINUX}
  eglLibrary := dlopen( libEGL, $001 );
  if eglLibrary = LIB_ERROR Then eglLibrary := dlopen( libGLES_CM, $001 );
  if eglLibrary = LIB_ERROR Then eglLibrary := dlopen( libGLESv1, $001 );
  // ./
  if eglLibrary = LIB_ERROR Then eglLibrary := dlopen( PChar( './' + libEGL ), $001 );
  if eglLibrary = LIB_ERROR Then eglLibrary := dlopen( PChar( './' + libGLES_CM ), $001 );
  if eglLibrary = LIB_ERROR Then eglLibrary := dlopen( PChar( './' + libGLESv1 ), $001 );

  glesLibrary := dlopen( libGLES_CM, $001 );
  if glesLibrary = LIB_ERROR Then glesLibrary := dlopen( libGLESv1, $001 );
  // ./
  if glesLibrary = LIB_ERROR Then glesLibrary := dlopen( PChar( './' + libGLES_CM ), $001 );
  if glesLibrary = LIB_ERROR Then glesLibrary := dlopen( PChar( './' + libGLESv1 ), $001 );
  {$ENDIF}
  {$IFDEF WINDOWS}
  eglLibrary := dlopen( libEGL );
  if eglLibrary = LIB_ERROR Then eglLibrary := dlopen( libGLES_CM );
  if eglLibrary = LIB_ERROR Then eglLibrary := dlopen( libGLESv1 );

  {$IFDEF USE_GLES_SOFTWARE}
  glesLibrary := dlopen( 'libGLES_CM_NoE.dll' );
  {$ELSE}
  glesLibrary := dlopen( libGLES_CM );
  if glesLibrary = LIB_ERROR Then glesLibrary := dlopen( libGLESv1 );
  {$ENDIF}
  {$ENDIF}

  if ( eglLibrary = LIB_ERROR ) or ( glesLibrary = LIB_ERROR ) Then
    begin
      Result := FALSE;
      exit;
    end;

  eglGetProcAddress      := dlsym( eglLibrary, 'eglGetProcAddress' );
  {$IFDEF USE_AMD_DRIVERS}
  eglGetError            := eglGetProcAddress( 'eglGetError' );
  eglGetDisplay          := eglGetProcAddress( 'eglGetDisplay' );
  eglInitialize          := eglGetProcAddress( 'eglInitialize' );
  eglTerminate           := eglGetProcAddress( 'eglTerminate' );
  eglChooseConfig        := eglGetProcAddress( 'eglChooseConfig' );
  eglCreateWindowSurface := eglGetProcAddress( 'eglCreateWindowSurface' );
  eglDestroySurface      := eglGetProcAddress( 'eglDestroySurface' );
  eglSwapInterval        := eglGetProcAddress( 'eglSwapInterval' );
  eglCreateContext       := eglGetProcAddress( 'eglCreateContext' );
  eglDestroyContext      := eglGetProcAddress( 'eglDestroyContext' );
  eglMakeCurrent         := eglGetProcAddress( 'eglMakeCurrent' );
  eglSwapBuffers         := eglGetProcAddress( 'eglSwapBuffers' );
  {$ELSE}
  eglGetError            := dlsym( eglLibrary, 'eglGetError' );
  eglGetDisplay          := dlsym( eglLibrary, 'eglGetDisplay' );
  eglInitialize          := dlsym( eglLibrary, 'eglInitialize' );
  eglTerminate           := dlsym( eglLibrary, 'eglTerminate' );
  eglChooseConfig        := dlsym( eglLibrary, 'eglChooseConfig' );
  eglCreateWindowSurface := dlsym( eglLibrary, 'eglCreateWindowSurface' );
  eglDestroySurface      := dlsym( eglLibrary, 'eglDestroySurface' );
  eglSwapInterval        := dlsym( eglLibrary, 'eglSwapInterval' );
  eglCreateContext       := dlsym( eglLibrary, 'eglCreateContext' );
  eglDestroyContext      := dlsym( eglLibrary, 'eglDestroyContext' );
  eglMakeCurrent         := dlsym( eglLibrary, 'eglMakeCurrent' );
  eglSwapBuffers         := dlsym( eglLibrary, 'eglSwapBuffers' );
  {$ENDIF}

  glGetString          := dlsym( glesLibrary, 'glGetString' );
  glHint               := dlsym( glesLibrary, 'glHint' );
  glShadeModel         := dlsym( glesLibrary, 'glShadeModel' );
  glReadPixels         := dlsym( glesLibrary, 'glReadPixels' );
  glClear              := dlsym( glesLibrary, 'glClear' );
  glClearColor         := dlsym( glesLibrary, 'glClearColor' );
  {$IF DEFINED(USE_GLES_ON_DESKTOP) and DEFINED(USE_AMD_DRIVERS)}
  glClearDepth         := dlsym( glesLibrary, 'glClearDepth' );
  {$ELSE}
  glClearDepth         := dlsym( glesLibrary, 'glClearDepthf' );
  {$ENDIF}
  glGetFloatv          := dlsym( glesLibrary, 'glGetFloatv' );
  glGetIntegerv        := dlsym( glesLibrary, 'glGetIntegerv' );
  glEnable             := dlsym( glesLibrary, 'glEnable' );
  glEnableClientState  := dlsym( glesLibrary, 'glEnableClientState' );
  glDisable            := dlsym( glesLibrary, 'glDisable' );
  glDisableClientState := dlsym( glesLibrary, 'glDisableClientState' );
  glIsEnabled          := dlsym( glesLibrary, 'glIsEnabled' );
  glViewport           := dlsym( glesLibrary, 'glViewport' );
  {$IF DEFINED(USE_GLES_ON_DESKTOP) and DEFINED(USE_AMD_DRIVERS)}
  glOrtho              := dlsym( glesLibrary, 'glOrtho' );
  {$ELSE}
  glOrtho              := dlsym( glesLibrary, 'glOrthof' );
  {$ENDIF}
  glScissor            := dlsym( glesLibrary, 'glScissor' );
  glDepthFunc          := dlsym( glesLibrary, 'glDepthFunc' );
  glDepthMask          := dlsym( glesLibrary, 'glDepthMask' );
  glColorMask          := dlsym( glesLibrary, 'glColorMask' );
  glColorPointer       := dlsym( glesLibrary, 'glColorPointer' );
  glAlphaFunc          := dlsym( glesLibrary, 'glAlphaFunc' );
  glBlendFunc          := dlsym( glesLibrary, 'glBlendFunc' );
  glPushMatrix         := dlsym( glesLibrary, 'glPushMatrix' );
  glPopMatrix          := dlsym( glesLibrary, 'glPopMatrix' );
  glMatrixMode         := dlsym( glesLibrary, 'glMatrixMode' );
  glLoadIdentity       := dlsym( glesLibrary, 'glLoadIdentity' );
  glLoadMatrixf        := dlsym( glesLibrary, 'glLoadMatrixf' );
  glRotatef            := dlsym( glesLibrary, 'glRotatef' );
  glScalef             := dlsym( glesLibrary, 'glScalef' );
  glTranslatef         := dlsym( glesLibrary, 'glTranslatef' );
  glVertexPointer      := dlsym( glesLibrary, 'glVertexPointer' );
  glBindTexture        := dlsym( glesLibrary, 'glBindTexture' );
  glGenTextures        := dlsym( glesLibrary, 'glGenTextures' );
  glDeleteTextures     := dlsym( glesLibrary, 'glDeleteTextures' );
  glTexParameterf      := dlsym( glesLibrary, 'glTexParameterf' );
  glTexParameteri      := dlsym( glesLibrary, 'glTexParameteri' );
  glPixelStorei        := dlsym( glesLibrary, 'glPixelStorei' );
  glTexImage2D         := dlsym( glesLibrary, 'glTexImage2D' );
  glTexSubImage2D      := dlsym( glesLibrary, 'glTexSubImage2D' );
  glCopyTexSubImage2D  := dlsym( glesLibrary, 'glCopyTexSubImage2D' );
  glTexEnvi            := dlsym( glesLibrary, 'glTexEnvi' );
  glTexCoordPointer    := dlsym( glesLibrary, 'glTexCoordPointer' );
  glDrawArrays         := dlsym( glesLibrary, 'glDrawArrays' );

  // OpenGL ES 1.0
  if not Assigned( glTexParameteri ) Then
    glTexParameteri    := dlsym( glesLibrary, 'glTexParameterx' );
  if not Assigned( glTexEnvi ) Then
    glTexEnvi          := dlsym( glesLibrary, 'glTexEnvx' );

  Result := Assigned( eglGetDisplay ) and Assigned( eglInitialize ) and Assigned( eglTerminate ) and Assigned( eglChooseConfig ) and
            Assigned( eglCreateWindowSurface ) and Assigned( eglDestroySurface ) and Assigned( eglCreateContext ) and Assigned( eglDestroyContext ) and
            Assigned( eglMakeCurrent ) and Assigned( eglSwapBuffers );
end;

procedure FreeGLES;
begin
end;

function gl_GetProc( const Proc : AnsiString ) : Pointer;
  {$IFDEF WINCE}
  var
    wideStr : PWideChar;
  {$ENDIF}
begin
  Result := eglGetProcAddress( PAnsiChar( Proc ) );
  if Result = nil Then
    Result := eglGetProcAddress( PAnsiChar( Proc + 'OES' ) );

  {$IFNDEF WINCE}
  if Result = nil Then
    Result := dlsym( glesLibrary, PAnsiChar( Proc ) );
  if Result = nil Then
    Result := dlsym( glesLibrary, PAnsiChar( Proc + 'OES' ) );
  {$ELSE}
  if Result = nil Then
    begin
      wideStr := u_GetPWideChar( Proc );
      Result  := dlsym( glesLibrary, wideStr );
      FreeMem( wideStr );
    end;
  if Result = nil Then
    begin
      wideStr := u_GetPWideChar( Proc + 'OES' );
      Result  := dlsym( glesLibrary, wideStr );
      FreeMem( wideStr );
    end;
  {$ENDIF}
end;

function gl_IsSupported( const Extension, SearchIn: AnsiString ) : Boolean;
  var
    extPos: Integer;
begin
  extPos := Pos( Extension, SearchIn );
  Result := extPos > 0;
  if Result Then
    Result := ( ( extPos + Length( Extension ) - 1 ) = Length( SearchIn ) ) or ( SearchIn[ extPos + Length( Extension ) ] = ' ' );
end;

procedure glBegin(mode: GLenum);
begin
  bSize := 0;
  RenderTextured := glIsEnabled( GL_TEXTURE_2D ) > 0;

  if Mode = GL_QUADS Then
    begin
      RenderQuad   := TRUE;
      newTriangle  := FALSE;
      newTriangleC := 0;
      RenderMode   := GL_TRIANGLES;
    end else
      begin
        RenderQuad := FALSE;
        RenderMode := Mode;
      end;
end;

procedure glEnd;
begin
  if RenderQuad Then
    begin
      if RenderTextured Then
        begin
          INC( bSize );
          if bSize + 1 > length( bVertices ) Then
            begin
              SetLength( bVertices, bSize + 1 );
              SetLength( bTexCoords, bSize + 1 );
              SetLength( bColors, bSize + 1 );
            end;
          bVertices[ bSize - 1 ] := bVertices[ 0 ];
          bTexCoords[ bSize - 1 ] := bTexCoords[ 0 ];
          bColors[ bSize - 1 ] := bColors[ 0 ];
        end else
          begin
            INC( bSize );
            if bSize + 1 > length( bVertices ) Then
              begin
                SetLength( bVertices, bSize + 1 );
                SetLength( bTexCoords, bSize + 1 );
                SetLength( bColors, bSize + 1 );
              end;
            bVertices[ bSize - 1 ] := bVertices[ 0 ];
            bTexCoords[ bSize - 1 ] := bTexCoords[ 0 ];
            bColors[ bSize - 1 ] := bColors[ 0 ];
          end;
    end;

  if bSize = 0 Then exit;

  if RenderTextured Then
    begin
      glEnableClientState( GL_TEXTURE_COORD_ARRAY );
      glTexCoordPointer( 2, GL_FLOAT, 0, @bTexCoords[ 0 ] );
    end;

  glEnableClientState( GL_COLOR_ARRAY );
  glColorPointer( 4, GL_UNSIGNED_BYTE, 0, @bColors[ 0 ] );

  glEnableClientState( GL_VERTEX_ARRAY );
  glVertexPointer( 3, GL_FLOAT, 0, @bVertices[ 0, 0 ] );

  glDrawArrays( RenderMode, 0, bSize );

  glDisableClientState( GL_VERTEX_ARRAY );
  glDisableClientState( GL_COLOR_ARRAY );
  if RenderTextured Then
    glDisableClientState( GL_TEXTURE_COORD_ARRAY );
end;

procedure glColor4ub(red, green, blue, alpha: GLubyte);
begin
  PByte( Ptr( @bColor ) + 0 )^ := red;
  PByte( Ptr( @bColor ) + 1 )^ := green;
  PByte( Ptr( @bColor ) + 2 )^ := blue;
  PByte( Ptr( @bColor ) + 3 )^ := alpha;
end;

procedure glColor4ubv(v: PGLubyte);
begin
  bColor := PLongWord( v )^;
end;

procedure glColor4f(red, green, blue, alpha: GLfloat);
begin
  PByte( Ptr( @bColor ) + 0 )^ := Round( red * 255 );
  PByte( Ptr( @bColor ) + 1 )^ := Round( green * 255 );
  PByte( Ptr( @bColor ) + 2 )^ := Round( blue * 255 );
  PByte( Ptr( @bColor ) + 3 )^ := Round( alpha * 255 );
end;

procedure gluPerspective(fovy, aspect, zNear, zFar: GLdouble);
  var
    m : array[ 1..4, 1..4 ] of Single;
    f : Single;
begin
  f := 1 / tan( FOVY * pi / 360 );

  m[ 1, 1 ] := f / aspect;
  m[ 1, 2 ] := 0;
  m[ 1, 3 ] := 0;
  m[ 1, 4 ] := 0;

  m[ 2, 1 ] := 0;
  m[ 2, 2 ] := f;
  m[ 2, 3 ] := 0;
  m[ 2, 4 ] := 0;

  m[ 3, 1 ] := 0;
  m[ 3, 2 ] := 0;
  m[ 3, 3 ] := ( zFar + zNear ) / ( zNear - zFar );
  m[ 3, 4 ] := -1;

  m[ 4, 1 ] := 0;
  m[ 4, 2 ] := 0;
  m[ 4, 3 ] := 2 * zFar * zNear / ( zNear - zFar );
  m[ 4, 4 ] := 0;

  glLoadMatrixf( @m[ 0, 0 ] );
end;

procedure glVertex2f(x, y: GLfloat);
begin
  if RenderTextured Then
    begin
      bVertices[ bSize - 1, 0 ] := x;
      bVertices[ bSize - 1, 1 ] := y;
      bVertices[ bSize - 1, 2 ] := 0;
      bColors[ bSize - 1 ]      := bColor;
      if RenderQuad Then
        begin
          if newTriangle Then
            begin
              INC( bSize );
              if bSize + 1 > length( bVertices ) Then
                begin
                  SetLength( bVertices, bSize + 1 );
                  SetLength( bTexCoords, bSize + 1 );
                  SetLength( bColors, bSize + 1 );
                end;
              bVertices[ bSize - 1 ] := bVertices[ bSize - 2 ];
              bTexCoords[ bSize - 1 ] := bTexCoords[ bSize - 2 ];
              bColors[ bSize - 1 ] := bColors[ bSize - 2 ];
              newTriangle := FALSE;
            end else
          if newTriangleC = 4 Then
            begin
              newTriangleC := 0;
              INC( bSize );
              if bSize + 1 > length( bVertices ) Then
                begin
                  SetLength( bVertices, bSize + 1 );
                  SetLength( bTexCoords, bSize + 1 );
                  SetLength( bColors, bSize + 1 );
                end;
              bVertices[ bSize - 1 ] := bVertices[ bSize - 6 ];
              bTexCoords[ bSize - 1 ] := bTexCoords[ bSize - 6 ];
              bColors[ bSize - 1 ] := bColors[ bSize - 6 ];
            end;
        end;
    end else
      begin
        if bSize + 1 > length( bVertices ) Then
          begin
            SetLength( bVertices, bSize + 1 );
            SetLength( bTexCoords, bSize + 1 );
            SetLength( bColors, bSize + 1 );
          end;
        bVertices[ bSize, 0 ] := x;
        bVertices[ bSize, 1 ] := y;
        bVertices[ bSize, 2 ] := 0;
        bColors[ bSize ]      := bColor;
        INC( bSize );
        if RenderQuad Then
          begin
            INC( newTriangleC );
            if newTriangleC = 3 Then newTriangle := TRUE;
            if newTriangle Then
              begin
                INC( bSize );
                if bSize + 1 > length( bVertices ) Then
                  begin
                    SetLength( bVertices, bSize + 1 );
                    SetLength( bTexCoords, bSize + 1 );
                    SetLength( bColors, bSize + 1 );
                  end;
                bVertices[ bSize - 1 ] := bVertices[ bSize - 2 ];
                bTexCoords[ bSize - 1 ] := bTexCoords[ bSize - 2 ];
                bColors[ bSize - 1 ] := bColors[ bSize - 2 ];
                newTriangle := FALSE;
              end else
            if newTriangleC = 4 Then
              begin
                newTriangleC := 0;
                INC( bSize );
                if bSize + 1 > length( bVertices ) Then
                  begin
                    SetLength( bVertices, bSize + 1 );
                    SetLength( bTexCoords, bSize + 1 );
                    SetLength( bColors, bSize + 1 );
                  end;
                bVertices[ bSize - 1 ] := bVertices[ bSize - 6 ];
                bTexCoords[ bSize - 1 ] := bTexCoords[ bSize - 6 ];
                bColors[ bSize - 1 ] := bColors[ bSize - 6 ];
              end;
          end;
      end;
end;

procedure glVertex2fv(v: PGLfloat);
begin
  if RenderTextured Then
    begin
      bVertices[ bSize - 1, 0 ] := zglPPoint2D( v ).X;
      bVertices[ bSize - 1, 1 ] := zglPPoint2D( v ).Y;
      bVertices[ bSize - 1, 2 ] := 0;
      bColors[ bSize - 1 ]      := bColor;
      if RenderQuad Then
        begin
          if newTriangle Then
            begin
              INC( bSize );
              if bSize + 1 > length( bVertices ) Then
                begin
                  SetLength( bVertices, bSize + 1 );
                  SetLength( bTexCoords, bSize + 1 );
                  SetLength( bColors, bSize + 1 );
                end;
              bVertices[ bSize - 1 ] := bVertices[ bSize - 2 ];
              bTexCoords[ bSize - 1 ] := bTexCoords[ bSize - 2 ];
              bColors[ bSize - 1 ] := bColors[ bSize - 2 ];
              newTriangle := FALSE;
            end else
          if newTriangleC = 4 Then
            begin
              newTriangleC := 0;
              INC( bSize );
              if bSize + 1 > length( bVertices ) Then
                begin
                  SetLength( bVertices, bSize + 1 );
                  SetLength( bTexCoords, bSize + 1 );
                  SetLength( bColors, bSize + 1 );
                end;
              bVertices[ bSize - 1 ] := bVertices[ bSize - 6 ];
              bTexCoords[ bSize - 1 ] := bTexCoords[ bSize - 6 ];
              bColors[ bSize - 1 ] := bColors[ bSize - 6 ];
            end;
        end;
    end else
      begin
        if bSize + 1 > length( bVertices ) Then
          begin
            SetLength( bVertices, bSize + 1 );
            SetLength( bTexCoords, bSize + 1 );
            SetLength( bColors, bSize + 1 );
          end;
        bVertices[ bSize, 0 ] := zglPPoint2D( v ).X;
        bVertices[ bSize, 1 ] := zglPPoint2D( v ).Y;
        bVertices[ bSize, 2 ] := 0;
        bColors[ bSize ]      := bColor;
        INC( bSize );
        if RenderQuad Then
          begin
            INC( newTriangleC );
            if newTriangleC = 3 Then newTriangle := TRUE;
            if newTriangle Then
              begin
                INC( bSize );
                if bSize + 1 > length( bVertices ) Then
                  begin
                    SetLength( bVertices, bSize + 1 );
                    SetLength( bTexCoords, bSize + 1 );
                    SetLength( bColors, bSize + 1 );
                  end;
                bVertices[ bSize - 1 ] := bVertices[ bSize - 2 ];
                bTexCoords[ bSize - 1 ] := bTexCoords[ bSize - 2 ];
                bColors[ bSize - 1 ] := bColors[ bSize - 2 ];
                newTriangle := FALSE;
              end else
            if newTriangleC = 4 Then
              begin
                newTriangleC := 0;
                INC( bSize );
                if bSize + 1 > length( bVertices ) Then
                  begin
                    SetLength( bVertices, bSize + 1 );
                    SetLength( bTexCoords, bSize + 1 );
                    SetLength( bColors, bSize + 1 );
                  end;
                bVertices[ bSize - 1 ] := bVertices[ bSize - 6 ];
                bTexCoords[ bSize - 1 ] := bTexCoords[ bSize - 6 ];
                bColors[ bSize - 1 ] := bColors[ bSize - 6 ];
              end;
          end;
      end;
end;

procedure glVertex3f(x, y, z: GLfloat);
begin
  if RenderTextured Then
    begin
      bVertices[ bSize - 1, 0 ] := x;
      bVertices[ bSize - 1, 1 ] := y;
      bVertices[ bSize - 1, 2 ] := z;
      bColors[ bSize - 1 ]      := bColor;
      if RenderQuad Then
        begin
          if newTriangle Then
            begin
              INC( bSize );
              if bSize + 1 > length( bVertices ) Then
                begin
                  SetLength( bVertices, bSize + 1 );
                  SetLength( bTexCoords, bSize + 1 );
                  SetLength( bColors, bSize + 1 );
                end;
              bVertices[ bSize - 1 ] := bVertices[ bSize - 2 ];
              bTexCoords[ bSize - 1 ] := bTexCoords[ bSize - 2 ];
              bColors[ bSize - 1 ] := bColors[ bSize - 2 ];
              newTriangle := FALSE;
            end else
          if newTriangleC = 4 Then
            begin
              newTriangleC := 0;
              INC( bSize );
              if bSize + 1 > length( bVertices ) Then
                begin
                  SetLength( bVertices, bSize + 1 );
                  SetLength( bTexCoords, bSize + 1 );
                  SetLength( bColors, bSize + 1 );
                end;
              bVertices[ bSize - 1 ] := bVertices[ bSize - 6 ];
              bTexCoords[ bSize - 1 ] := bTexCoords[ bSize - 6 ];
              bColors[ bSize - 1 ] := bColors[ bSize - 6 ];
            end;
        end;
    end else
      begin
        if bSize + 1 > length( bVertices ) Then
          begin
            SetLength( bVertices, bSize + 1 );
            SetLength( bTexCoords, bSize + 1 );
            SetLength( bColors, bSize + 1 );
          end;
        bVertices[ bSize, 0 ] := x;
        bVertices[ bSize, 1 ] := y;
        bVertices[ bSize, 2 ] := z;
        bColors[ bSize ]      := bColor;
        INC( bSize );
        if RenderQuad Then
          begin
            INC( newTriangleC );
            if newTriangleC = 3 Then newTriangle := TRUE;
            if newTriangle Then
              begin
                INC( bSize );
                if bSize + 1 > length( bVertices ) Then
                  begin
                    SetLength( bVertices, bSize + 1 );
                    SetLength( bTexCoords, bSize + 1 );
                    SetLength( bColors, bSize + 1 );
                  end;
                bVertices[ bSize - 1 ] := bVertices[ bSize - 2 ];
                bTexCoords[ bSize - 1 ] := bTexCoords[ bSize - 2 ];
                bColors[ bSize - 1 ] := bColors[ bSize - 2 ];
                newTriangle := FALSE;
              end else
            if newTriangleC = 4 Then
              begin
                newTriangleC := 0;
                INC( bSize );
                if bSize + 1 > length( bVertices ) Then
                  begin
                    SetLength( bVertices, bSize + 1 );
                    SetLength( bTexCoords, bSize + 1 );
                    SetLength( bColors, bSize + 1 );
                  end;
                bVertices[ bSize - 1 ] := bVertices[ bSize - 6 ];
                bTexCoords[ bSize - 1 ] := bTexCoords[ bSize - 6 ];
                bColors[ bSize - 1 ] := bColors[ bSize - 6 ];
              end;
          end;
      end;
end;

procedure glGetTexImage(target: GLenum; level: GLint; format: GLenum; atype: GLenum; pixels: Pointer);
begin
end;

procedure glTexCoord2f(s, t: GLfloat);
begin
  if bSize + 1 > length( bVertices ) Then
    begin
      SetLength( bVertices, bSize + 1 );
      SetLength( bTexCoords, bSize + 1 );
      SetLength( bColors, bSize + 1 );
    end;
  bTexCoords[ bSize ].X := s;
  bTexCoords[ bSize ].Y := t;
  INC( bSize );
  INC( newTriangleC );

  if newTriangleC = 3 Then
    newTriangle := TRUE;
end;

procedure glTexCoord2fv(v: PGLfloat);
begin
  if bSize + 1 > length( bVertices ) Then
    begin
      SetLength( bVertices, bSize + 1 );
      SetLength( bTexCoords, bSize + 1 );
      SetLength( bColors, bSize + 1 );
    end;
  bTexCoords[ bSize ] := zglPPoint2D( v )^;
  INC( bSize );
  INC( newTriangleC );

  if newTriangleC = 3 Then
    newTriangle := TRUE;
end;

end.