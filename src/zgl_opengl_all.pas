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
unit zgl_opengl_all;

{$I zgl_config.cfg}
{$IFDEF LINUX_OR_DARWIN}
  {$DEFINE stdcall := cdecl}
{$ENDIF}
{$IFDEF DARWIN}
  {$LINKFRAMEWORK OpenGL}
{$ENDIF}

interface
uses
  {$IFDEF LINUX}
  X, XLib, XUtil
  {$ENDIF}
  {$IFDEF WIN32}
  Windows
  {$ENDIF}
  {$IFDEF DARWIN}
  MacOSAll
  {$ENDIF}
  ;

function InitGL : Boolean;
procedure FreeGL;
{$IFDEF DARWIN}
function InitAGL : Boolean;
procedure FreeAGL;
{$ENDIF}
function gl_GetProc( const Proc : PChar ) : Pointer;
function gl_IsSupported( const Extension : String; const searchIn: String ) : Boolean;

var
  gl_TexCoord2f  : procedure( U, V : Single ); stdcall;
  gl_TexCoord2fv : procedure( Coord : PSingle ); stdcall;
  gl_Vertex2f    : procedure( X, Y : Single ); stdcall;
  gl_Vertex2fv   : procedure( v : PSingle ); stdcall;

const
  {$IFDEF LINUX}
  libGL  = 'libGL.so.1';
  libGLU = 'libGLU.so.1';
  {$ENDIF}
  {$IFDEF WIN32}
  libGL  = 'opengl32.dll';
  libGLU = 'glu32.dll';
  {$ENDIF}
  {$IFDEF DARWIN}
  libGL  = '/System/Library/Frameworks/OpenGL.framework/Libraries/libGL.dylib';
  libGLU = '/System/Library/Frameworks/OpenGL.framework/Libraries/libGLU.dylib';
  libAGL = '/System/Library/Frameworks/AGL.framework/AGL';
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

  // PixelFormat
  GL_RGB                            = $1907;
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

  // Hint Mode
  GL_DONT_CARE                      = $1100;
  GL_FASTEST                        = $1101;
  GL_NICEST                         = $1102;

  // Hints
  GL_PERSPECTIVE_CORRECTION_HINT    = $0C50;
  GL_LINE_SMOOTH_HINT               = $0C52;
  GL_POLYGON_SMOOTH_HINT            = $0C53;
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
  GL_POLYGON_SMOOTH                 = $0B41;
  GL_NORMALIZE                      = $0BA1;

  // glBegin/glEnd
  GL_POINTS                         = $0000;
  GL_LINES                          = $0001;
  GL_TRIANGLES                      = $0004;
  GL_TRIANGLE_STRIP                 = $0005;
  GL_QUADS                          = $0007;

  // Texture
  GL_TEXTURE_2D                     = $0DE1;
  GL_MAX_TEXTURE_SIZE               = $0D33;
  GL_MAX_TEXTURE_UNITS_ARB          = $84E2;
  GL_TEXTURE_MAX_ANISOTROPY_EXT     = $84FE;
  GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT = $84FF;
  // Texture Wrap Mode
  GL_CLAMP_TO_EDGE                  = $812F;
  GL_REPEAT                         = $2901;
  // Texture Format
  GL_RGB16                          = $8054;
  GL_RGBA16                         = $805B;
  GL_COMPRESSED_RGB_ARB             = $84ED;
  GL_COMPRESSED_RGBA_ARB            = $84EE;
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

  // FBO
  GL_FRAMEBUFFER_EXT                = $8D40;
  GL_RENDERBUFFER_EXT               = $8D41;
  GL_DEPTH_COMPONENT16              = $81A5;
  GL_DEPTH_COMPONENT24              = $81A6;
  GL_DEPTH_COMPONENT32              = $81A7;
  GL_COLOR_ATTACHMENT0_EXT          = $8CE0;
  GL_DEPTH_ATTACHMENT_EXT           = $8D00;

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

  // VBO
  GL_BUFFER_SIZE_ARB                = $8764;
  GL_ARRAY_BUFFER_ARB               = $8892;
  GL_ELEMENT_ARRAY_BUFFER_ARB       = $8893;
  GL_WRITE_ONLY_ARB                 = $88B9;
  GL_STREAM_DRAW_ARB                = $88E0;
  GL_STATIC_DRAW_ARB                = $88E4;

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

  procedure glFinish; stdcall; external libGL;
  procedure glFlush; stdcall; external libGL;

  function  glGetString(name: GLenum): PChar; stdcall; external libGL;
  procedure glHint(target, mode: GLenum); stdcall; external libGL;

  procedure glShadeModel(mode: GLenum); stdcall; external libGL;

  // Clear
  procedure glClear(mask: GLbitfield); stdcall; external libGL;
  procedure glClearColor(red, green, blue, alpha: GLclampf); stdcall; external libGL;
  procedure glClearDepth(depth: GLclampd); stdcall; external libGL;
  // Get
  procedure glGetFloatv(pname: GLenum; params: PGLfloat); stdcall; external libGL;
  procedure glGetIntegerv(pname: GLenum; params: PGLint); stdcall; external libGL;
  // State
  procedure glBegin(mode: GLenum); stdcall; external libGL;
  procedure glEnd; stdcall; external libGL;
  procedure glEnable(cap: GLenum); stdcall; external libGL;
  procedure glEnableClientState(aarray: GLenum); stdcall; external libGL;
  procedure glDisable(cap: GLenum); stdcall; external libGL;
  procedure glDisableClientState(aarray: GLenum); stdcall; external libGL;
  // Viewport
  procedure glViewport(x, y: GLint; width, height: GLsizei); stdcall; external libGL;
  procedure glOrtho(left, right, bottom, top, zNear, zFar: GLdouble); stdcall; external libGL;
  procedure gluPerspective(fovy, aspect, zNear, zFar: GLdouble); stdcall; external libGLU;
  procedure glScissor(x, y: GLint; width, height: GLsizei); stdcall; external libGL;
  // Depth
  procedure glDepthFunc(func: GLenum); stdcall; external libGL;
  procedure glDepthMask(flag: GLboolean); stdcall; external libGL;
  // Color
  procedure glColor4ub(red, green, blue, alpha: GLubyte); stdcall; external libGL;
  procedure glColor4f(red, green, blue, alpha: GLfloat); stdcall; external libGL;
  // Alpha
  procedure glAlphaFunc(func: GLenum; ref: GLclampf); stdcall; external libGL;
  procedure glBlendFunc(sfactor, dfactor: GLenum); stdcall; external libGL;
  // Matrix
  procedure glMatrixMode(mode: GLenum); stdcall; external libGL;
  procedure glLoadIdentity; stdcall; external libGL;
  procedure glLoadMatrixf(const m: PGLfloat); stdcall; external libGL;
  procedure glMultMatrixf(const m: PGLfloat); stdcall; external libGL;
  procedure glTranslatef(x, y, z: GLfloat); stdcall; external libGL;
  // Vertex
  procedure glVertex2f(x, y: GLfloat); stdcall; external libGL;
  procedure glVertex2fv(v: PGLfloat); stdcall; external libGL;
  // Texture
  procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external libGL;
  procedure glGenTextures(n: GLsizei; textures: PGLuint); stdcall; external libGL;
  procedure glDeleteTextures(n: GLsizei; const textures: PGLuint); stdcall; external libGL;
  procedure glTexParameterf(target: GLenum; pname: GLenum; param: GLfloat); stdcall; external libGL;
  procedure glTexParameteri(target: GLenum; pname: GLenum; param: GLint); stdcall; external libGL;
  procedure glTexImage2D(target: GLenum; level, internalformat: GLint; width, height: GLsizei; border: GLint; format, atype: GLenum; const pixels: Pointer); stdcall; external libGL;
  procedure glGetTexImage(target: GLenum; level: GLint; format: GLenum; atype: GLenum; pixels: Pointer); stdcall; external libGL;
  procedure glCopyTexSubImage2D(target: GLenum; level, xoffset, yoffset, x, y: GLint; width, height: GLsizei); stdcall; external libGL;
  procedure glTexEnvi(target: GLenum; pname: GLenum; param: GLint); stdcall; external libGL;
  function  gluBuild2DMipmaps(target: GLenum; components, width, height: GLint; format, atype: GLenum; const data: Pointer): Integer; stdcall; external libGLU;
var
  glActiveTextureARB: procedure(texture: GLenum); stdcall;
  glClientActiveTextureARB: procedure(texture: GLenum); stdcall;
  // TexCoords
  procedure glTexCoord2f(s, t: GLfloat); stdcall; external libGL;
  procedure glTexCoord2fv(v: PGLfloat); stdcall; external libGL;
var
  glMultiTexCoord2fARB: procedure(target: GLenum; s: GLfloat; t: GLfloat); stdcall;
  glMultiTexCoord2fvARB: procedure(target: GLenum; const v: PGLfloat); stdcall;
  // FBO
  glIsRenderbufferEXT: function(renderbuffer: GLuint): GLboolean; stdcall;
  glBindRenderbufferEXT: procedure(target: GLenum; renderbuffer: GLuint); stdcall;
  glDeleteRenderbuffersEXT: procedure(n: GLsizei; const renderbuffers: PGLuint); stdcall;
  glGenRenderbuffersEXT: procedure(n: GLsizei; renderbuffers: PGLuint); stdcall;
  glRenderbufferStorageEXT: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; height: GLsizei); stdcall;
  glIsFramebufferEXT: function(framebuffer: GLuint): GLboolean; stdcall;
  glBindFramebufferEXT: procedure(target: GLenum; framebuffer: GLuint); stdcall;
  glDeleteFramebuffersEXT: procedure(n: GLsizei; const framebuffers: PGLuint); stdcall;
  glGenFramebuffersEXT: procedure(n: GLsizei; framebuffers: PGLuint); stdcall;
  glCheckFramebufferStatusEXT: function(target: GLenum): GLenum; stdcall;
  glFramebufferTexture2DEXT: procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint); stdcall;
  glFramebufferRenderbufferEXT: procedure(target: GLenum; attachment: GLenum; renderbuffertarget: GLenum; renderbuffer: GLuint); stdcall;
  // VBO
  glBindBufferARB : procedure(target : GLenum; buffer: GLuint); stdcall;
  glDeleteBuffersARB : procedure(n : GLsizei; buffers : PGLuint); stdcall;
  glGenBuffersARB : procedure(n : GLsizei; buffers : PGLuint); stdcall;
  glIsBufferARB : function (buffer : GLuint) :GLboolean; stdcall;
  glBufferDataARB : procedure(target : GLenum; size:GLsizei; data:PGLvoid;usage: GLenum); stdcall;
  glBufferSubDataARB : procedure(target : GLenum; offset :GLint; size : GLsizei; data: PGLvoid); stdcall;
  glMapBufferARB : function (target :GLenum; access: GLenum) : PGLvoid; stdcall;
  glUnmapBufferARB : function (target :GLenum) :GLboolean; stdcall;
  glGetBufferParameterivARB:procedure(target:GLenum; pname:GLenum; params:PGLint); stdcall;

{$IFDEF LINUX}
type
  GLXContext = Pointer;
  GLXDrawable = TXID;

const
  GLX_BUFFER_SIZE  = 2;
  GLX_RGBA         = 4;
  GLX_DOUBLEBUFFER = 5;
  GLX_RED_SIZE     = 8;
  GLX_GREEN_SIZE   = 9;
  GLX_BLUE_SIZE    = 10;
  GLX_ALPHA_SIZE   = 11;
  GLX_DEPTH_SIZE   = 12;
  GLX_STENCIL_SIZE = 13;

  GLX_SAMPLES_SGIS = $186A1;

  function  glXChooseVisual(dpy: PDisplay; screen: Integer; attribList: PInteger): PXVisualInfo; cdecl; external libGL;
  function  glXCreateContext(dpy: PDisplay; vis: PXVisualInfo; shareList: GLXContext; direct: Boolean): GLXContext; cdecl; external libGL;
  procedure glXDestroyContext(dpy: PDisplay; ctx: GLXContext); cdecl; external libGL;
  function  glXMakeCurrent(dpy: PDisplay; drawable: GLXDrawable; ctx: GLXContext): Boolean; cdecl; external libGL;
  procedure glXSwapBuffers(dpy: PDisplay; drawable: GLXDrawable); cdecl; external libGL;
  function  glXQueryExtension(dpy: PDisplay; var errorb, event: Integer): Boolean; cdecl; external libGL;
  function  glXIsDirect(dpy: PDisplay; ctx: GLXContext): Boolean; cdecl; external libGL;
  procedure glXWaitGL; cdecl; external libGL;
  procedure glXWaitX; cdecl; external libGL;

var
  glXGetVideoSyncSGI: function(var counter: LongWord): Integer; cdecl;
  glXWaitVideoSyncSGI: function(divisor, remainder: Integer; var count: LongWord): Integer; cdecl;
{$ENDIF}
{$IFDEF WIN32}
const
  // Pixel Format
  WGL_DRAW_TO_WINDOW_ARB    = $2001;
  WGL_ACCELERATION_ARB      = $2003;
  WGL_FULL_ACCELERATION_ARB = $2027;
  WGL_SUPPORT_OPENGL_ARB    = $2010;
  WGL_DOUBLE_BUFFER_ARB     = $2011;
  WGL_COLOR_BITS_ARB        = $2014;
  WGL_ALPHA_BITS_ARB        = $201B;
  WGL_DEPTH_BITS_ARB        = $2022;
  WGL_STENCIL_BITS_ARB      = $2023;

  // AA
  WGL_SAMPLE_BUFFERS_ARB    = $2041;
  WGL_SAMPLES_ARB           = $2042;

  // PBuffer
  WGL_DRAW_TO_PBUFFER_ARB   = $202D;


  function wglGetProcAddress(proc: PChar): Pointer; stdcall; external libGL;
var
  wglChoosePixelFormatARB: function(hdc: HDC; const piAttribIList: PGLint; const pfAttribFList: PGLfloat; nMaxFormats: GLuint; piFormats: PGLint; nNumFormats: PGLuint): BOOL; stdcall;
  wglSwapIntervalEXT: function(interval: GLint): BOOL; stdcall;
  wglGetSwapIntervalEXT: function(): GLint; stdcall;
  // PBuffer
  wglCreatePbufferARB: function(hDC: HDC; iPixelFormat: GLint; iWidth: GLint; iHeight: GLint; const piAttribList: PGLint): THandle; stdcall;
  wglGetPbufferDCARB: function(hPbuffer: THandle): HDC; stdcall;
  wglReleasePbufferDCARB: function(hPbuffer: THandle; hDC: HDC): GLint; stdcall;
  wglDestroyPbufferARB: function(hPbuffer: THandle): BOOL; stdcall;
{$ENDIF}
{$IFDEF DARWIN}
const
  AGL_NONE         = 0;
  AGL_BUFFER_SIZE  = 2;
  AGL_RGBA         = 4;
  AGL_DOUBLEBUFFER = 5;
  AGL_RED_SIZE     = 8;
  AGL_GREEN_SIZE   = 9;
  AGL_BLUE_SIZE    = 10;
  AGL_ALPHA_SIZE   = 11;
  AGL_DEPTH_SIZE   = 12;
  AGL_STENCIL_SIZE = 13;
  AGL_FULLSCREEN   = 54;

  AGL_SAMPLE_BUFFERS_ARB = 55;
  AGL_SAMPLES_ARB        = 56;
  AGL_MULTISAMPLE        = 59;
  AGL_SUPERSAMPLE        = 60;

  AGL_SWAP_INTERVAL = 222;

type
  TGDHandle = ptrint;
  TCGrafPtr = Pointer;

  PAGLDevice = ^TAGLDevice;
  TAGLDevice = TGDHandle;

  PAGLDrawable = ^TAGLDrawable;
  TAGLDrawable = TCGrafPtr;

  TAGLPixelFormat = Pointer;

  TAGLContext = Pointer;

  function aglSetInt(ctx:TAGLContext; pname:GLenum; params:GLint):GLboolean;

var
  aglChoosePixelFormat : function(gdevs:PAGLDevice; ndev:GLint; attribs:PGLint):TAGLPixelFormat;cdecl;
  aglDestroyPixelFormat : procedure(pix:TAGLPixelFormat);cdecl;
  aglCreateContext : function(pix:TAGLPixelFormat; share:TAGLContext):TAGLContext;cdecl;
  aglDestroyContext : function(ctx:TAGLContext):GLboolean;cdecl;
  aglUpdateContext : function(ctx:TAGLContext):GLboolean;cdecl;
  aglSetCurrentContext : function(ctx:TAGLContext):GLboolean;cdecl;
  aglSetDrawable : function(ctx:TAGLContext; draw:TAGLDrawable):GLboolean;cdecl;
  aglGetDrawable : function(ctx:TAGLContext):TAGLDrawable;cdecl;
  aglSetFullScreen : function(ctx:TAGLContext; width:GLsizei; height:GLsizei; freq:GLsizei; device:GLint):GLboolean;cdecl;
  aglSwapBuffers : procedure(ctx:TAGLContext);cdecl;
  aglSetInteger : function(ctx:TAGLContext; pname:GLenum; params:PGLint):GLboolean;cdecl;
{$ENDIF}

var
  ogl_Library : {$IFDEF LINUX_OR_DARWIN} Pointer {$ENDIF} {$IFDEF WIN32} HMODULE {$ENDIF};
  {$IFDEF DARWIN}
  agl_Library : Pointer;
  {$ENDIF}

implementation
uses
  math,
  zgl_const,
  zgl_log,
  zgl_utils;

function InitGL;
begin
  Result := FALSE;

  // Страшно, да :)
  {$IFDEF FPC}
    { according to bug 7570, this is necessary on all x86 platforms,
      maybe we've to fix the sse control word as well }
    { Yes, at least for darwin/x86_64 (JM) }
    {$IF DEFINED(cpui386) or DEFINED(cpux86_64)}
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
    {$IFEND}
  {$ELSE}
    { according to bug 7570, this is necessary on all x86 platforms,
      maybe we've to fix the sse control word as well }
    {$IFDEF x86}
    Set8087CW($133F);
    {$ENDIF x86}
  {$ENDIF}

  ogl_Library := dlopen( libGL {$IFDEF LINUX_OR_DARWIN}, $001 {$ENDIF} );

  Result := ogl_Library <> LIB_ERROR;
end;

procedure FreeGL;
begin
  dlclose( ogl_Library );
end;

{$IFDEF DARWIN}
function aglSetInt;
  var
    i : Integer;
begin
  i := params;
  Result := aglSetInteger( ctx, pname, @i );
end;

function InitAGL;
begin
  agl_Library := dlopen( libAGL, $001 );
  if agl_Library <> nil Then
    begin
      aglChoosePixelFormat  := dlsym( agl_Library, 'aglChoosePixelFormat' );
      aglDestroyPixelFormat := dlsym( agl_Library, 'aglDestroyPixelFormat' );
      aglCreateContext      := dlsym( agl_Library, 'aglCreateContext' );
      aglDestroyContext     := dlsym( agl_Library, 'aglDestroyContext' );
      aglUpdateContext      := dlsym( agl_Library, 'aglUpdateContext' );
      aglSetCurrentContext  := dlsym( agl_Library, 'aglSetCurrentContext' );
      aglSetDrawable        := dlsym( agl_Library, 'aglSetDrawable' );
      aglGetDrawable        := dlsym( agl_Library, 'aglGetDrawable' );
      aglSetFullScreen      := dlsym( agl_Library, 'aglSetFullScreen' );
      aglSwapBuffers        := dlsym( agl_Library, 'aglSwapBuffers' );
      aglSetInteger         := dlsym( agl_Library, 'aglSetInteger' );
      Result := TRUE;
    end else
      Result := FALSE;
end;

procedure FreeAGL;
begin
  dlclose( agl_Library );
end;
{$ENDIF}

function gl_GetProc;
begin
  {$IFDEF WIN32}
  Result := wglGetProcAddress( Proc );
  if Result = nil Then
    Result := wglGetProcAddress( PChar( Proc + 'ARB' ) );
  {$ELSE}
  Result := dlsym( ogl_Library, Proc );
  if Result = nil Then
    Result := dlsym( ogl_Library, PChar( Proc + 'ARB' ) );
  {$ENDIF}
end;

function gl_IsSupported;
  var
    ExtPos: Integer;
begin
  ExtPos := Pos( Extension, searchIn );
  Result := ExtPos > 0;
  if Result Then
    Result := ( ( ExtPos + Length( Extension ) - 1 ) = Length( searchIn ) ) or
                ( searchIn[ ExtPos + Length( Extension ) ] = ' ' );
end;

end.
