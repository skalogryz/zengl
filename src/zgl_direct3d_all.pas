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
// TODO: glTexSubImage2D doesn't work properly with level parameter
unit zgl_direct3d_all;

{$I zgl_config.cfg}

interface
uses
  Windows,
  {$IFDEF USE_DIRECT3D8}
  DirectXGraphics,
  {$ENDIF}
  {$IFDEF USE_DIRECT3D9}
  Direct3D9,
  {$ENDIF}
  zgl_types
  ;

const
  libGLU = 'glu32.dll';

  GL_FALSE                          = 0;
  GL_TRUE                           = 1;
  GL_ZERO                           = 0;
  GL_ONE                            = 1;

  // DataType
  GL_UNSIGNED_BYTE                  = $1401;
  GL_UNSIGNED_SHORT                 = $1403;
  GL_UNSIGNED_INT                   = $1405;
  GL_FLOAT                          = $1406;
  GL_UNSIGNED_SHORT_4_4_4_4         = $8033;

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

  // Buffer Bit
  GL_DEPTH_BUFFER_BIT               = $00000100;
  GL_STENCIL_BUFFER_BIT             = $00000400;
  GL_COLOR_BUFFER_BIT               = $00004000;

  // Enable
  GL_LINE_SMOOTH                    = $0B20;
  GL_POLYGON_SMOOTH                 = $0B41;

  // glBegin/glEnd
  GL_POINTS                         = $0000;
  GL_LINES                          = $0001;
  GL_TRIANGLES                      = $0004;
  GL_TRIANGLE_STRIP                 = $0005;
  GL_TRIANGLE_FAN                   = $0006;
  GL_QUADS                          = $0007;

  // Texture
  GL_UNPACK_ROW_LENGTH              = $0CF2;
  GL_TEXTURE_2D                     = $0DE1;
  GL_TEXTURE0_ARB                   = $84C0;
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
  GL_COMPRESSED_RGB_S3TC_DXT1_EXT   = $83F0;
  GL_COMPRESSED_RGBA_S3TC_DXT1_EXT  = $83F1;
  GL_COMPRESSED_RGBA_S3TC_DXT3_EXT  = $83F2;
  GL_COMPRESSED_RGBA_S3TC_DXT5_EXT  = $83F3;
  // Texture Env Mode
  GL_MODULATE                       = $2100;
  GL_DECAL                          = $2101;
  // Texture Env Parameter
  GL_TEXTURE_ENV_MODE               = $2200;
  GL_TEXTURE_ENV_COLOR              = $2201;
  // Texture Env Target
  GL_TEXTURE_ENV                    = $2300;
  // Mipmaps
  GL_GENERATE_MIPMAP                = $8191;
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

  GL_COMBINE_ARB                    = $8570;
  GL_COMBINE_RGB_ARB                = $8571;
  GL_COMBINE_ALPHA_ARB              = $8572;
  GL_SOURCE0_RGB_ARB                = $8580;
  GL_SOURCE1_RGB_ARB                = $8581;
  GL_SOURCE2_RGB_ARB                = $8582;
  GL_SOURCE0_ALPHA_ARB              = $8588;
  GL_SOURCE1_ALPHA_ARB              = $8589;
  GL_SOURCE2_ALPHA_ARB              = $858A;
  GL_OPERAND0_RGB_ARB               = $8590;
  GL_OPERAND1_RGB_ARB               = $8591;
  GL_OPERAND2_RGB_ARB               = $8592;
  GL_OPERAND0_ALPHA_ARB             = $8598;
  GL_OPERAND1_ALPHA_ARB             = $8599;
  GL_OPERAND2_ALPHA_ARB             = $859A;
  GL_RGB_SCALE_ARB                  = $8573;
  GL_ADD_SIGNED_ARB                 = $8574;
  GL_INTERPOLATE_ARB                = $8575;
  GL_SUBTRACT_ARB                   = $84E7;
  GL_CONSTANT_ARB                   = $8576;
  GL_PRIMARY_COLOR_ARB              = $8577;
  GL_PREVIOUS_ARB                   = $8578;

  // d3d8_Matrices
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

  // Triangulation
  GLU_TESS_BEGIN                    = $18704;
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
  GLU_TESS_COMBINE_DATA             = $1870F;

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

// D3DFVF_XYZC
const
  D3DFVF_XYZC = D3DFVF_XYZ or D3DFVF_DIFFUSE;
type
  TXYZCVertex = record
    x, y, z : Single;
    c       : LongWord;
end;
const
  s_D3DFVF_XYZC = SizeOf( TXYZCVertex );

// D3DFVF_XYZCT
const
  D3DFVF_XYZCT = D3DFVF_XYZ or D3DFVF_DIFFUSE or D3DFVF_TEX1;
type
  TXYZCTVertex = record
    x, y, z : Single;
    c       : LongWord;
    u, v    : Single;
end;
const
  s_D3DFVF_XYZCT = SizeOf( TXYZCTVertex );

type
  zglD3DTexture = record
    Texture    : {$IFDEF USE_DIRECT3D8} IDirect3DTexture8 {$ENDIF}
                 {$IFDEF USE_DIRECT3D9} IDirect3DTexture9 {$ENDIF};
    Pool       : TD3DPool;
    AutoMipMap : Byte;
    MagFilter  : LongWord;
    MinFilter  : LongWord;
    MipFilter  : LongWord;
    Wrap       : LongWord;
end;

procedure glReadPixels(x, y: GLint; width, height: GLsizei; format, atype: GLenum; pixels: Pointer);
// Clear
procedure glClear(mask: GLbitfield);
// State
procedure glBegin(mode: GLenum);
procedure glEnd;
procedure glEnable(cap: GLenum);
procedure glDisable(cap: GLenum);
// Viewport
procedure glViewport(x, y: GLint; width, height: GLsizei);
procedure glOrtho(left, right, bottom, top, zNear, zFar: GLdouble);
procedure glScissor(x, y: GLint; width, height: GLsizei);
// Depth Buffer
procedure glDepthFunc(func: GLenum); {$IFDEF USE_INLINE} inline; {$ENDIF}
procedure glDepthMask(flag: GLboolean); {$IFDEF USE_INLINE} inline; {$ENDIF}
// Color
procedure glColor4ub(red, green, blue, alpha: GLubyte); {$IFDEF USE_INLINE} inline; {$ENDIF}
procedure glColor4ubv(v: PGLubyte); {$IFDEF USE_INLINE} inline; {$ENDIF}
procedure glColor4f(red, green, blue, alpha: GLfloat); {$IFDEF USE_INLINE} inline; {$ENDIF}
procedure glColorMask(red, green, blue, alpha: GLboolean); {$IFDEF USE_INLINE} inline; {$ENDIF}
// Alpha
procedure glAlphaFunc(func: GLenum; ref: GLclampf); {$IFDEF USE_INLINE} inline; {$ENDIF}
procedure glBlendFunc(sfactor, dfactor: GLenum); {$IFDEF USE_INLINE} inline; {$ENDIF}
//procedure glBlendEquationEXT(mode: GLenum);
procedure glBlendFuncSeparate(sfactorRGB: GLenum; dfactorRGB: GLenum; sfactorAlpha: GLenum; dfactorAlpha: GLenum); {$IFDEF USE_INLINE} inline; {$ENDIF}
// Matrix
procedure glPushMatrix;
procedure glPopMatrix;
procedure glMatrixMode(mode: GLenum);
procedure glLoadIdentity;
procedure gluPerspective(fovy, aspect, zNear, zFar: GLdouble);
procedure glFrustum(left, right, bottom, top, zNear, zFar: GLdouble);
procedure glRotatef(angle, x, y, z: GLfloat);
procedure glScalef(x, y, z: GLfloat);
procedure glTranslatef(x, y, z: GLfloat);
// Vertex
procedure glVertex2f(x, y: GLfloat);
procedure glVertex2fv(v: PGLfloat);
procedure glVertex3f(x, y, z: GLfloat);
// Texture
procedure glBindTexture(target: GLenum; texture: GLuint);
procedure glGenTextures(n: GLsizei; textures: PGLuint);
procedure glDeleteTextures(n: GLsizei; const textures: PGLuint);
procedure glTexParameterf(target: GLenum; pname: GLenum; param: GLfloat);
procedure glTexParameteri(target: GLenum; pname: GLenum; param: GLint);
procedure glPixelStorei(pname: GLenum; param: GLint);
procedure glTexImage2D(target: GLenum; level, internalformat: GLint; width, height: GLsizei; border: GLint; format, atype: GLenum; const pixels: Pointer);
procedure glCompressedTexImage2D(target: GLenum; level, internalformat: GLint; width, height: GLsizei; border: GLint; imageSize: GLsizei; const pixels: Pointer);
procedure glTexSubImage2D(target: GLenum; level, xoffset, yoffset: GLint; width, height: GLsizei; format, atype: GLenum; const pixels: Pointer);
procedure glGetTexImage(target: GLenum; level: GLint; format: GLenum; atype: GLenum; pixels: Pointer);
procedure glCopyTexSubImage2D(target: GLenum; level, xoffset, yoffset, x, y: GLint; width, height: GLsizei);
procedure glTexEnvi(target: GLenum; pname: GLenum; param: GLint);
function  gluBuild2DMipmaps(target: GLenum; components, width, height: GLint; format, atype: GLenum; const data: Pointer): Integer;
// TexCoords
procedure glTexCoord2f(s, t: GLfloat);
procedure glTexCoord2fv(v: PGLfloat);
// Triangulation
{$IFDEF USE_TRIANGULATION}
procedure gluDeleteTess(tess: Integer); stdcall external libGLU;
function  gluErrorString(error: Integer): PChar; stdcall external libGLU;
function  gluNewTess: Integer; stdcall external libGLU;
procedure gluTessBeginContour(tess: Integer); stdcall external libGLU;
procedure gluTessBeginPolygon(tess: Integer; data: Pointer); stdcall external libGLU;
procedure gluTessCallback(tess: Integer; which: Integer; fn: Pointer); stdcall external libGLU;
procedure gluTessEndContour(tess: Integer); stdcall external libGLU;
procedure gluTessEndPolygon(tess: Integer); stdcall external libGLU;
procedure gluTessVertex(tess: Integer; vertex: PDouble; data: Pointer); stdcall external libGLU;
{$ENDIF}

procedure d3d_FillTexture( Src, Dst : PByteArray; const Width, Height : Integer; const DstStride : Integer = 0 );

var
  d3dTexCount   : Integer;
  d3dTexArray   : array of zglD3DTexture;
  {$IFDEF USE_DIRECT3D8}
  d3dResArray   : array of IDirect3DTexture8;
  {$ENDIF}
  {$IFDEF USE_DIRECT3D9}
  d3dResArray   : array of IDirect3DSurface9;
  {$ENDIF}
  d3dMatrices   : array[ 0..23 ] of TD3DMatrix;
  d3dMatrixMode : {$IFDEF USE_DIRECT3D8} LongWord {$ENDIF}
                  {$IFDEF USE_DIRECT3D9} TD3DTransformStateType {$ENDIF};

  {$IFDEF USE_DIRECT3D8}
  // Scissor
  ScissorEnabled : Boolean;
  ScissorX       : Integer;
  ScissorY       : Integer;
  ScissorW       : Integer;
  ScissorH       : Integer;
  ScissorScaleX  : Single;
  ScissorScaleY  : Single;
  {$ENDIF}

implementation
uses
  zgl_direct3d,
  zgl_application,
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_render,
  zgl_camera_2d,
  zgl_render_target,
  zgl_textures,
  zgl_log,
  zgl_math_2d,
  math;

var
  RenderMode     : TD3DPrimitiveType;
  RenderQuad     : Boolean;
  RenderTextured : Boolean;
  RenderTexID    : Integer;
  // Matrices
  popMatrices : array of array[ 0..23 ] of TD3DMatrix;
  pushCount   : Integer;
  // Textures
  psiUnpackRowLength : Integer;
  lMagFilter : LongWord;
  lMinFilter : LongWord;
  lMipFilter : LongWord;
  lWrap      : LongWord;
  // Buffers
  newTriangle : Integer;
  bColor      : TD3DColor;
  bTVertices  : array of TXYZCTVertex; // Textured
  bTVCount    : Integer;
  bPVertices  : array of TXYZCVertex;  // Primitives
  bPVCount    : Integer;

procedure glReadPixels(x, y: GLint; width, height: GLsizei; format, atype: GLenum; pixels: Pointer);
  var
    i, j : Integer;
    pSrc, pDst : Ptr;
    a : TRect;
    r : TD3DLockedRect;
    d : TD3DSurface_Desc;
    {$IFDEF USE_DIRECT3D8}
    src, dst : IDirect3DSurface8;
    {$ENDIF}
    {$IFDEF USE_DIRECT3D9}
    src, dst : IDirect3DSurface9;
    {$ENDIF}
begin
  {$IFDEF USE_DIRECT3D8}
  d3dDevice.GetRenderTarget( src );
  src.GetDesc( d );
  d3dDevice.CreateImageSurface( d.Width, d.Height, d.Format, dst );
  d3dDevice.CopyRects( src, nil, 0, dst, nil );
  {$ENDIF}
  {$IFDEF USE_DIRECT3D9}
  d3dDevice.GetRenderTarget( 0, src );
  src.GetDesc( d );
  d3dDevice.CreateOffscreenPlainSurface( d.Width, d.Height, d.Format, D3DPOOL_SYSTEMMEM, dst, nil );
  d3dDevice.GetRenderTargetData( src, dst );
  {$ENDIF}

  y := ( d.Height - Height ) - y;

  a.Left   := x;
  a.Top    := y;
  a.Right  := x + width - 1;
  a.Bottom := y + height - 1;
  if dst.LockRect( r, @a, D3DLOCK_READONLY ) = D3D_OK Then
    begin
      pSrc := Ptr( r.pBits );
      pDst := Ptr( Ptr( pixels ) + width * ( height - 1 ) * 4 );
      for j := 0 to height - 1 do
        begin
          for i := 0 to width - 1 do
            begin
              PByte( pDst + 2 )^ := PByte( pSrc + 0 )^;
              PByte( pDst + 1 )^ := PByte( pSrc + 1 )^;
              PByte( pDst + 0 )^ := PByte( pSrc + 2 )^;
              PByte( pDst + 3 )^ := 255; // PByte( pSrc + 3 )^ всегда равно нулю O_o
              INC( pSrc, 4 );
              INC( pDst, 4 );
            end;
          DEC( pDst, width * 4 * 2 );
        end;
      dst.UnlockRect();
    end;

  dst := nil;
  src := nil;
end;

procedure glClear(mask: GLbitfield);
begin
  glViewPort( 0, 0, wndWidth, wndHeight );
  if mask and GL_DEPTH_BUFFER_BIT > 0 Then
    d3dDevice.Clear( 0, nil, D3DCLEAR_ZBUFFER, D3DCOLOR_XRGB( 0, 0, 0 ), 1, 0 );
  if mask and GL_STENCIL_BUFFER_BIT > 0 Then
    d3dDevice.Clear( 0, nil, D3DCLEAR_STENCIL, D3DCOLOR_XRGB( 0, 0, 0 ), 1, 0 );
  if mask and GL_COLOR_BUFFER_BIT > 0 Then
    d3dDevice.Clear( 0, nil, D3DCLEAR_TARGET, D3DCOLOR_ARGB( 0, 0, 0, 0 ), 1, 0 );
  SetCurrentMode;
end;

procedure glBegin(mode: GLenum);
begin
  bTVCount := 0;
  bPVCount := 0;
  RenderQuad  := FALSE;
  newTriangle := 0;

  case Mode of
    GL_POINTS: RenderMode := D3DPT_POINTLIST;
    GL_LINES: RenderMode := D3DPT_LINELIST;
    GL_TRIANGLES: RenderMode := D3DPT_TRIANGLELIST;
    GL_TRIANGLE_STRIP: RenderMode := D3DPT_TRIANGLESTRIP;
    GL_TRIANGLE_FAN: RenderMode := D3DPT_TRIANGLEFAN;
    GL_QUADS:
      begin
        RenderQuad := TRUE;
        RenderMode := D3DPT_TRIANGLELIST;
      end;
  end;
end;

procedure glEnd;
  var
    Count : Integer;
begin
  if RenderQuad Then
    begin
      if RenderTextured Then
      begin
        INC( bTVCount );
        if bTVCount + 1 > Length( bTVertices ) Then SetLength( bTVertices, bTVCount + 1 );
        bTVertices[ bTVCount - 1 ] := bTVertices[ 0 ];
      end else
        begin
          INC( bPVCount );
          if bPVCount + 1 > Length( bPVertices ) Then SetLength( bPVertices, bPVCount + 1 );
          bPVertices[ bPVCount - 1 ] := bPVertices[ 0 ];
        end;
    end;

  if RenderTextured Then
    Count := bTVCount
  else
    Count := bPVCount;

  if Count = 0 Then exit;

  case RenderMode of
    D3DPT_POINTLIST:;
    D3DPT_LINELIST: Count := Count div 2;
    D3DPT_TRIANGLELIST: Count := Count div 3;
    D3DPT_TRIANGLESTRIP:;
    D3DPT_TRIANGLEFAN: Count := Count - 2;
  end;

  {$IFDEF USE_DIRECT3D8}
  if RenderTextured Then
    begin
      d3dDevice.SetVertexShader( D3DFVF_XYZCT );
      d3dDevice.DrawPrimitiveUP( RenderMode, Count, @bTVertices[ 0 ], s_D3DFVF_XYZCT );
    end else
      begin
        d3dDevice.SetVertexShader( D3DFVF_XYZC );
        d3dDevice.DrawPrimitiveUP( RenderMode, Count, @bPVertices[ 0 ], s_D3DFVF_XYZC );
      end;
  {$ENDIF}
  {$IFDEF USE_DIRECT3D9}
  if RenderTextured Then
    begin
      d3dDevice.SetFVF( D3DFVF_XYZCT );
      d3dDevice.DrawPrimitiveUP( RenderMode, Count, bTVertices[ 0 ], s_D3DFVF_XYZCT );
    end else
      begin
        d3dDevice.SetFVF( D3DFVF_XYZC );
        d3dDevice.DrawPrimitiveUP( RenderMode, Count, bPVertices[ 0 ], s_D3DFVF_XYZC );
      end;
  {$ENDIF}
end;

procedure glEnable(cap: GLenum);
begin
  case cap of
    GL_TEXTURE_2D: RenderTextured := TRUE;
    GL_BLEND: d3dDevice.SetRenderState( D3DRS_ALPHABLENDENABLE, iTRUE );
    GL_ALPHA_TEST: d3dDevice.SetRenderState( D3DRS_ALPHATESTENABLE, iTRUE );
    GL_DEPTH_TEST: d3dDevice.SetRenderState( D3DRS_ZENABLE, D3DZB_TRUE );
    GL_SCISSOR_TEST:
      begin
        {$IFDEF USE_DIRECT3D8}
        ScissorEnabled := TRUE;
        {$ENDIF}
        {$IFDEF USE_DIRECT3D9}
        d3dDevice.SetRenderState( D3DRS_SCISSORTESTENABLE, iTRUE );
        {$ENDIF}
      end;
    {$IFDEF USE_DIRECT3D8}
    // MS sucks again! :)
    GL_LINE_SMOOTH, GL_POLYGON_SMOOTH:;// d3d_Device.SetRenderState( D3DRS_EDGEANTIALIAS, iTRUE );
    {$ENDIF}
    {$IFDEF USE_DIRECT3D9}
    GL_LINE_SMOOTH, GL_POLYGON_SMOOTH: d3dDevice.SetRenderState( D3DRS_ANTIALIASEDLINEENABLE, iTRUE );
    {$ENDIF}
  end;
end;

procedure glDisable(cap: GLenum);
begin
  case cap of
    GL_TEXTURE_2D:
      begin
        RenderTexID    := -1;
        RenderTextured := FALSE;
        d3dDevice.SetTexture( 0, nil );
      end;
    GL_BLEND: d3dDevice.SetRenderState( D3DRS_ALPHABLENDENABLE, iFALSE );
    GL_ALPHA_TEST: d3dDevice.SetRenderState( D3DRS_ALPHATESTENABLE, iFALSE );
    GL_DEPTH_TEST: d3dDevice.SetRenderState( D3DRS_ZENABLE, D3DZB_FALSE );
    GL_SCISSOR_TEST:
      begin
        {$IFDEF USE_DIRECT3D8}
        ScissorEnabled := FALSE;
        SetCurrentMode();
        {$ENDIF}
        {$IFDEF USE_DIRECT3D9}
        d3dDevice.SetRenderState( D3DRS_SCISSORTESTENABLE, iFALSe );
        {$ENDIF}
      end;
    {$IFDEF USE_DIRECT3D8}
    GL_LINE_SMOOTH, GL_POLYGON_SMOOTH:;// d3d_Device.SetRenderState( D3DRS_EDGEANTIALIAS, iFALSE );
    {$ENDIF}
    {$IFDEF USE_DIRECT3D9}
    GL_LINE_SMOOTH, GL_POLYGON_SMOOTH: d3dDevice.SetRenderState( D3DRS_ANTIALIASEDLINEENABLE, iFALSE );
    {$ENDIF}
  end;
end;

procedure glViewport(x, y: GLint; width, height: GLsizei);
begin
  {$IFDEF USE_DIRECT3D8}
  if not ScissorEnabled Then
    begin
  {$ENDIF}
      d3dViewport.X      := X;
      d3dViewport.Y      := Y;
      d3dViewport.Width  := Width;
      d3dViewport.Height := Height;
      if oglMode = 2 Then
        begin
          d3dViewport.MinZ := -1;
          d3dViewport.MaxZ := 1;
        end else
          begin
            d3dViewport.MinZ := oglzNear;
            d3dViewport.MaxZ := oglzFar;
          end;
      d3dDevice.SetViewport( d3dViewport );
  {$IFDEF USE_DIRECT3D8}
    end else
      begin
        if cam2d.Apply Then glPopMatrix();

        glDisable( GL_DEPTH_TEST );
        glMatrixMode( GL_PROJECTION );
        glLoadIdentity;
        if oglTarget = TARGET_SCREEN Then
          glOrtho( ScissorX, ScissorX + ScissorW, ScissorY + ScissorH, ScissorY, -1, 1 )
        else
          glOrtho( ScissorX, ScissorX + ScissorW, ScissorY, ScissorY + ScissorH, -1, 1 );
        glMatrixMode( GL_MODELVIEW );
        glLoadIdentity;
        if ( oglTarget = TARGET_SCREEN ) and ( appFlags and CORRECT_RESOLUTION > 0 ) Then
          begin
            glTranslatef( scrAddCX, scrAddCY, 0 );
            glScalef( scrResCX, scrResCY, 1 );
          end;

        ScissorEnabled := FALSE;
        if oglTarget = TARGET_SCREEN Then
          glViewPort( ScissorX, ScissorY, ScissorW, ScissorH )
        else
          glViewPort( Round( ScissorX / ( oglWidth / oglTargetW ) ), oglTargetH - Round( ( ScissorY + ScissorH ) / ( oglHeight / oglTargetH ) ),
                      Round( ScissorW / ( oglWidth / oglTargetW ) ), Round( ScissorH / ( oglHeight / oglTargetH ) ) );

        if cam2d.Apply Then cam2d_Set( cam2d.Global );

        ScissorEnabled := TRUE;
      end;
  {$ENDIF}
end;

procedure glOrtho(left, right, bottom, top, zNear, zFar: GLdouble);
begin
  left   := left + 0.5;
  right  := right + 0.5;
  if oglTarget = TARGET_SCREEN Then
    begin
      top    := top + 0.5;
      bottom := bottom + 0.5;
    end else
      begin
        top    := top - 0.5;
        bottom := bottom - 0.5;
      end;

  with d3dMatrices[ LongWord( d3dMatrixMode ) ] do
    begin
      _11 := 2 / ( right - left );
      _12 := 0;
      _13 := 0;
      _14 := 0;

      _21 := 0;
      _22 := 2 / ( top - bottom );
      _23 := 0;
      _24 := 0;

      _31 := 0;
      _32 := 0;
      _33 := 2 / ( zFar - znear );
      _34 := 0;

      _41 := -( right + left ) / ( right - left );
      _42 := -( top + bottom ) / ( top - bottom );
      _43 := -( zFar + zNear ) / ( zFar - znear );
      _44 := 1;
    end;
  d3dDevice.MultiplyTransform( d3dMatrixMode, d3dMatrices[ LongWord( d3dMatrixMode ) ] );
end;

procedure glScissor(x, y: GLint; width, height: GLsizei);
  {$IFDEF USE_DIRECT3D9}
  var
    r : TRect;
  {$ENDIF}
begin
  {$IFDEF USE_DIRECT3D8}
  if oglTarget = TARGET_SCREEN Then
    begin
      ScissorX := x;
      ScissorY := -y - height + oglTargetH;
      if ScissorX < scrAddCX Then
        begin
          ScissorW := ScissorX + width - scrAddCX;
          ScissorX := scrAddCX;
        end else ScissorW := width;
      if ScissorY < scrAddCY Then
        begin
          ScissorH := ScissorY + height - scrAddCY;
          ScissorY := scrAddCY;
        end else ScissorH := height;

      if ScissorX + ScissorW > oglTargetW - scrAddCX Then
        ScissorW := oglTargetW - ScissorX - scrAddCX;
      if ScissorY + ScissorH > oglTargetH - scrAddCY Then
        ScissorH := oglTargetH - ScissorY - scrAddCY;

      if ScissorX >= ScissorX + ScissorW Then exit;
      if ScissorY >= ScissorY + ScissorH Then exit;
    end else
      begin
        ScissorX := Round( x * ScissorScaleX );
        ScissorY := Round( ( -y - height + oglTargetH ) * ScissorScaleY );
        ScissorW := Round( width * ScissorScaleX );
        ScissorH := Round( height * ScissorScaleY );
      end;

  glViewPort( 0, 0, 0, 0 );
  {$ENDIF}
  {$IFDEF USE_DIRECT3D9}
  if oglTarget = TARGET_SCREEN Then
    y := -y + oglTargetH - Height;

  r.Left   := x;
  r.Right  := x + width;
  r.Top    := y;
  r.Bottom := y + height;
  d3dDevice.SetScissorRect( @r );
  {$ENDIF}
end;

procedure glDepthFunc(func: GLenum); {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  d3dDevice.SetRenderState( D3DRS_ZFUNC, func - $01FF );
end;

procedure glDepthMask(flag: GLboolean); {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  d3dDevice.SetRenderState( D3DRS_ZWRITEENABLE, flag );
end;

procedure glColor4ub(red, green, blue, alpha: GLubyte); {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  bColor := D3DCOLOR_ARGB( alpha, red, green, blue );
end;

procedure glColor4ubv(v: PGLubyte); {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  bColor := D3DCOLOR_ARGB( PByte( Ptr( v ) + 3 )^, PByte( v )^, PByte( Ptr( v ) + 1 )^, PByte( Ptr( v ) + 2 )^ );
end;

procedure glColor4f(red, green, blue, alpha: GLfloat); {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  bColor := D3DCOLOR_ARGB( Round( alpha * 255 ), Round( red * 255 ), Round( green * 255 ), Round( blue * 255 ) );
end;

procedure glColorMask(red, green, blue, alpha: GLboolean); {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  d3dDevice.SetRenderState( D3DRS_COLORWRITEENABLE, D3DCOLORWRITEENABLE_RED   * red or
                                                    D3DCOLORWRITEENABLE_GREEN * green or
                                                    D3DCOLORWRITEENABLE_BLUE  * blue or
                                                    D3DCOLORWRITEENABLE_ALPHA * alpha );
end;

procedure glAlphaFunc(func: GLenum; ref: GLclampf); {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  d3dDevice.SetRenderState( D3DRS_ALPHAREF,  Trunc( ref * 255 ) );
  d3dDevice.SetRenderState( D3DRS_ALPHAFUNC, func - $01FF );
end;

function d3d_GetBlendFactor( factor : GLenum ) : LongWord;
begin
  case factor of
    GL_ZERO: Result := D3DBLEND_ZERO;
    GL_ONE:  Result := D3DBLEND_ONE;
  else
    Result := factor - $02FD;
  end;
end;

procedure glBlendFunc(sfactor, dfactor: GLenum); {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF USE_DIRECT3D9}
  d3dDevice.SetRenderState( D3DRS_SEPARATEALPHABLENDENABLE, iFALSE );
  {$ENDIF}
  d3dDevice.SetRenderState( D3DRS_SRCBLEND,  d3d_GetBlendFactor( sfactor ) );
  d3dDevice.SetRenderState( D3DRS_DESTBLEND, d3d_GetBlendFactor( dfactor ) );
end;

procedure glBlendFuncSeparate(sfactorRGB: GLenum; dfactorRGB: GLenum; sfactorAlpha: GLenum; dfactorAlpha: GLenum); {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  d3dDevice.SetRenderState( D3DRS_SRCBLEND,  d3d_GetBlendFactor( sfactorRGB ) );
  d3dDevice.SetRenderState( D3DRS_DESTBLEND, d3d_GetBlendFactor( dfactorRGB ) );
  {$IFDEF USE_DIRECT3D9}
  d3dDevice.SetRenderState( D3DRS_SEPARATEALPHABLENDENABLE, iTRUE );
  d3dDevice.SetRenderState( D3DRS_SRCBLENDALPHA,  d3d_GetBlendFactor( sfactorAlpha ) );
  d3dDevice.SetRenderState( D3DRS_DESTBLENDALPHA, d3d_GetBlendFactor( dfactorAlpha ) );
  {$ENDIF}
end;

procedure glPushMatrix;
begin
  INC( pushCount );
  if pushCount > Length( popMatrices ) Then
    SetLength( popMatrices, Length( popMatrices ) + 16 );

  popMatrices[ pushCount - 1, LongWord( d3dMatrixMode ) ] := d3dMatrices[ LongWord( d3dMatrixMode ) ];
end;

procedure glPopMatrix;
begin
  if pushCount < 1 Then exit;
  d3dMatrices[ LongWord( d3dMatrixMode ) ] := popMatrices[ pushCount - 1, LongWord( d3dMatrixMode ) ];
  d3dDevice.SetTransform( d3dMatrixMode, d3dMatrices[ LongWord( d3dMatrixMode ) ] );
  DEC( pushCount );
end;

procedure glMatrixMode(mode: GLenum);
begin
  case mode of
    GL_MODELVIEW:  d3dMatrixMode := D3DTS_VIEW;
    GL_PROJECTION: d3dMatrixMode := D3DTS_PROJECTION;
    GL_TEXTURE:    d3dMatrixMode := D3DTS_TEXTURE0;
  end;
end;

procedure glLoadIdentity;
begin
  with d3dMatrices[ LongWord( d3dMatrixMode ) ] do
    begin
      _11 := 1;
      _12 := 0;
      _13 := 0;
      _14 := 0;

      _21 := 0;
      _22 := 1;
      _23 := 0;
      _24 := 0;

      _31 := 0;
      _32 := 0;
      _33 := 1;
      _34 := 0;

      _41 := 0;
      _42 := 0;
      _43 := 0;
      _44 := 1;
    end;
  d3dDevice.SetTransform( d3dMatrixMode, d3dMatrices[ LongWord( d3dMatrixMode ) ] );
end;

procedure gluPerspective(fovy, aspect, zNear, zFar: GLdouble);
  var
    f : Single;
begin
  f := 1 / tan( FOVY * pi / 360 );

  with d3dMatrices[ LongWord( d3dMatrixMode ) ] do
    begin
      _11 := f / aspect;
      _12 := 0;
      _13 := 0;
      _14 := 0;

      _21 := 0;
      _22 := f;
      _23 := 0;
      _24 := 0;

      _31 := 0;
      _32 := 0;
      _33 := ( zFar + zNear ) / ( zNear - zFar );
      _34 := -1;

      _41 := 0;
      _42 := 0;
      _43 := 2 * zFar * zNear / ( zNear - zFar );
      _44 := 0;
    end;
  d3dDevice.MultiplyTransform( d3dMatrixMode, d3dMatrices[ LongWord( d3dMatrixMode ) ] );
end;

procedure glFrustum(left, right, bottom, top, zNear, zFar: GLdouble);
begin
  with d3dMatrices[ LongWord( d3dMatrixMode ) ] do
    begin
      _11 := ( zNear * 2 ) / ( right - left );
      _12 := 0;
      _13 := 0;
      _14 := 0;

      _21 := 0;
      _22 := ( zNear * 2 ) / ( Top - Bottom );
      _23 := 0;
      _24 := 0;

      _31 := ( right + left ) / ( right - left );
      _32 := ( top + bottom ) / ( top - bottom );
      _33 := -( zFar + zNear ) / ( zFar - zNear );
      _34 := -1;

      _41 := 0;
      _42 := 0;
      _43 := -( zFar * zNear * 2 ) / ( zFar - zNear );
      _44 := 0;
    end;
  d3dDevice.MultiplyTransform( d3dMatrixMode, d3dMatrices[ LongWord( d3dMatrixMode ) ] );
end;

procedure glRotatef(angle, x, y, z: GLfloat);
  var
    sa, ca : Single;
    m      : TD3DMatrix;
  function matrix4f_Mul( const m1, m2 : TD3DMatrix ) : TD3DMatrix;
  begin
    Result._11 := m1._11 * m2._11 + m1._12 * m2._21 + m1._13 * m2._31 + m1._14 * m2._41;
    Result._12 := m1._11 * m2._12 + m1._12 * m2._22 + m1._13 * m2._32 + m1._14 * m2._42;
    Result._13 := m1._11 * m2._13 + m1._12 * m2._23 + m1._13 * m2._33 + m1._14 * m2._43;
    Result._14 := m1._11 * m2._14 + m1._12 * m2._24 + m1._13 * m2._34 + m1._14 * m2._44;
    Result._21 := m1._21 * m2._11 + m1._22 * m2._21 + m1._23 * m2._31 + m1._24 * m2._41;
    Result._22 := m1._21 * m2._12 + m1._22 * m2._22 + m1._23 * m2._32 + m1._24 * m2._42;
    Result._23 := m1._21 * m2._13 + m1._22 * m2._23 + m1._23 * m2._33 + m1._24 * m2._43;
    Result._24 := m1._21 * m2._14 + m1._22 * m2._24 + m1._23 * m2._34 + m1._24 * m2._44;
    Result._31 := m1._31 * m2._11 + m1._32 * m2._21 + m1._33 * m2._31 + m1._34 * m2._41;
    Result._32 := m1._31 * m2._12 + m1._32 * m2._22 + m1._33 * m2._32 + m1._34 * m2._42;
    Result._33 := m1._31 * m2._13 + m1._32 * m2._23 + m1._33 * m2._33 + m1._34 * m2._43;
    Result._34 := m1._31 * m2._14 + m1._32 * m2._24 + m1._33 * m2._34 + m1._34 * m2._44;
    Result._41 := m1._41 * m2._11 + m1._42 * m2._21 + m1._43 * m2._31 + m1._44 * m2._41;
    Result._42 := m1._41 * m2._12 + m1._42 * m2._22 + m1._43 * m2._32 + m1._44 * m2._42;
    Result._43 := m1._41 * m2._13 + m1._42 * m2._23 + m1._43 * m2._33 + m1._44 * m2._43;
    Result._44 := m1._41 * m2._14 + m1._42 * m2._24 + m1._43 * m2._34 + m1._44 * m2._44;
  end;
begin
  sa := sin( angle * deg2rad );
  ca := cos( angle * deg2rad );

  with m do
    begin
      _11 := ca + ( 1 - ca ) * x * x;
      _12 := ( 1 - ca ) * x * z + z * sa;
      _13 := ( 1 - ca ) * x * z - y * sa;
      _14 := 0;

      _21 := ( 1 - ca ) * x * y - z * sa;
      _22 := ca + ( 1 - ca ) * y * y;
      _23 := ( 1 - ca ) * y * z + x * sa;
      _24 := 0;

      _31 := ( 1 - ca ) * x * z + y * sa;
      _32 := ( 1 - ca ) * y * z - x * sa;
      _33 := ca + ( 1 - ca ) * z * z;
      _34 := 0;

      _41 := 0;
      _42 := 0;
      _43 := 0;
      _44 := 1;
    end;
  d3dMatrices[ LongWord( d3dMatrixMode ) ] := matrix4f_Mul( m, d3dMatrices[ LongWord( d3dMatrixMode ) ] );
  d3dDevice.SetTransform( d3dMatrixMode, d3dMatrices[ LongWord( d3dMatrixMode ) ] );
end;

procedure glScalef(x, y, z: GLfloat);
begin
  with d3dMatrices[ LongWord( d3dMatrixMode ) ] do
    begin
      _11 := x * _11;
      _12 := x * _12;
      _13 := x * _13;
      _14 := x * _14;

      _21 := y * _21;
      _22 := y * _22;
      _23 := y * _23;
      _24 := y * _24;

      _31 := z * _31;
      _32 := z * _32;
      _33 := z * _33;
      _34 := z * _34;
    end;
  d3dDevice.SetTransform( d3dMatrixMode, d3dMatrices[ LongWord( d3dMatrixMode ) ] );
end;

procedure glTranslatef(x, y, z: GLfloat);
begin
  with d3dMatrices[ LongWord( d3dMatrixMode ) ] do
    begin
      _41 := _11 * x + _21 * y + _31 * z + _41;
      _42 := _12 * x + _22 * y + _32 * z + _42;
      _43 := _13 * x + _23 * y + _33 * z + _43;
      _44 := _14 * x + _24 * y + _34 * z + _44;
    end;
  d3dDevice.SetTransform( d3dMatrixMode, d3dMatrices[ LongWord( d3dMatrixMode ) ] );
end;

procedure glVertex2f(x, y: GLfloat);
begin
  if RenderTextured Then
    begin
      bTVertices[ bTVCount ].z := 0;
      bTVertices[ bTVCount ].c := bColor;
      bTVertices[ bTVCount ].x := x;
      bTVertices[ bTVCount ].y := y;
      INC( bTVCount );
      if RenderQuad Then
        begin
          INC( newTriangle );
          if newTriangle = 3 Then
            begin
              if bTVCount = Length( bTVertices ) Then SetLength(  bTVertices, bTVCount + 1024 );
              bTVertices[ bTVCount  ] :=  bTVertices[ bTVCount - 1 ];

              INC( bTVCount );
            end else
              if newTriangle = 4 Then
                begin
                  if bTVCount > Length( bTVertices ) Then SetLength( bTVertices, bTVCount + 1024 );
                  bTVertices[ bTVCount ] := bTVertices[ bTVCount - 5 ];

                  INC( bTVCount );
                  newTriangle := 0;
                end;
        end;
    end else
      begin
        if bPVCount + 1 > Length( bPVertices ) Then SetLength( bPVertices, bPVCount + 1 );
        bPVertices[ bPVCount ].z := 0;
        bPVertices[ bPVCount ].c := bColor;
        bPVertices[ bPVCount ].x := x;
        bPVertices[ bPVCount ].y := y;
        INC( bPVCount );
        if RenderQuad Then
          begin
            INC( newTriangle );
            if newTriangle = 3 Then
              begin
                if bPVCount = Length( bPVertices ) Then SetLength(  bPVertices, bPVCount + 1024 );
                bPVertices[ bPVCount ] :=  bPVertices[ bPVCount - 1 ];

                INC( bPVCount );
              end else
                if newTriangle = 4 Then
                  begin
                    if bPVCount = Length( bPVertices ) Then SetLength( bPVertices, bPVCount + 1024 );
                    bPVertices[ bPVCount ] := bPVertices[ bPVCount - 5 ];

                    INC( bPVCount );
                    newTriangle := 0;
              end;
          end;
      end;
end;

procedure glVertex2fv(v: PGLfloat);
begin
  if RenderTextured Then
    begin
      bTVertices[ bTVCount ].z := 0;
      bTVertices[ bTVCount ].c := bColor;
      bTVertices[ bTVCount ].x := zglPPoint2D( v ).X;
      bTVertices[ bTVCount ].y := zglPPoint2D( v ).Y;
      INC( bTVCount );
      if RenderQuad Then
        begin
          INC( newTriangle );
          if newTriangle = 3 Then
            begin
              if bTVCount = Length( bTVertices ) Then SetLength(  bTVertices, bTVCount + 1024 );
              bTVertices[ bTVCount  ] :=  bTVertices[ bTVCount - 1 ];

              INC( bTVCount );
            end else
              if newTriangle = 4 Then
                begin
                  if bTVCount > Length( bTVertices ) Then SetLength( bTVertices, bTVCount + 1024 );
                  bTVertices[ bTVCount ] := bTVertices[ bTVCount - 5 ];

                  INC( bTVCount );
                  newTriangle := 0;
                end;
        end;
    end else
      begin
        if bPVCount + 1 > Length( bPVertices ) Then SetLength( bPVertices, bPVCount + 1 );
        bPVertices[ bPVCount ].z := 0;
        bPVertices[ bPVCount ].c := bColor;
        bPVertices[ bPVCount ].x := zglPPoint2D( v ).X;
        bPVertices[ bPVCount ].y := zglPPoint2D( v ).Y;
        INC( bPVCount );
        if RenderQuad Then
          begin
            INC( newTriangle );
            if newTriangle = 3 Then
              begin
                if bPVCount = Length( bPVertices ) Then SetLength(  bPVertices, bPVCount + 1024 );
                bPVertices[ bPVCount ] :=  bPVertices[ bPVCount - 1 ];

                INC( bPVCount );
              end else
                if newTriangle = 4 Then
                  begin
                    if bPVCount = Length( bPVertices ) Then SetLength( bPVertices, bPVCount + 1024 );
                    bPVertices[ bPVCount ] := bPVertices[ bPVCount - 5 ];

                    INC( bPVCount );
                    newTriangle := 0;
                  end;
          end;
      end;
end;

procedure glVertex3f(x, y, z: GLfloat);
begin
  if RenderTextured Then
    begin
      bTVertices[ bTVCount ].z := z;
      bTVertices[ bTVCount ].c := bColor;
      bTVertices[ bTVCount ].x := x;
      bTVertices[ bTVCount ].y := y;
      INC( bTVCount );
      if RenderQuad Then
        begin
          INC( newTriangle );
          if newTriangle = 3 Then
            begin
              if bTVCount = Length( bTVertices ) Then SetLength(  bTVertices, bTVCount + 1024 );
              bTVertices[ bTVCount  ] :=  bTVertices[ bTVCount - 1 ];

              INC( bTVCount );
            end else
              if newTriangle = 4 Then
                begin
                  if bTVCount > Length( bTVertices ) Then SetLength( bTVertices, bTVCount + 1024 );
                  bTVertices[ bTVCount ] := bTVertices[ bTVCount - 5 ];

                  INC( bTVCount );
                  newTriangle := 0;
                end;
        end;
    end else
      begin
        if bPVCount + 1 > Length( bPVertices ) Then SetLength( bPVertices, bPVCount + 1 );
        bPVertices[ bPVCount ].z := z;
        bPVertices[ bPVCount ].c := bColor;
        bPVertices[ bPVCount ].x := x;
        bPVertices[ bPVCount ].y := y;
        INC( bPVCount );
        if RenderQuad Then
          begin
            INC( newTriangle );
            if newTriangle = 3 Then
              begin
                if bPVCount = Length( bPVertices ) Then SetLength(  bPVertices, bPVCount + 1024 );
                bPVertices[ bPVCount ] :=  bPVertices[ bPVCount - 1 ];

                INC( bPVCount );
              end else
                if newTriangle = 4 Then
                  begin
                    if bPVCount = Length( bPVertices ) Then SetLength( bPVertices, bPVCount + 1024 );
                    bPVertices[ bPVCount ] := bPVertices[ bPVCount - 5 ];

                    INC( bPVCount );
                    newTriangle := 0;
                  end;
          end;
      end;
end;

procedure glBindTexture(target: GLenum; texture: GLuint);
begin
  if texture >= d3dTexCount Then
    begin
      d3dDevice.SetTexture( 0, nil );
      exit;
    end;

  {$IFDEF USE_DIRECT3D8}
  if d3dTexArray[ texture ].MagFilter > 0 Then
    d3dDevice.SetTextureStageState( 0, D3DTSS_MAGFILTER, d3dTexArray[ texture ].MagFilter );
  if d3dTexArray[ texture ].MinFilter > 0 Then
    d3dDevice.SetTextureStageState( 0, D3DTSS_MINFILTER, d3dTexArray[ texture ].MinFilter );
  if d3dTexArray[ texture ].MipFilter > 0 Then
    d3dDevice.SetTextureStageState( 0, D3DTSS_MIPFILTER, d3dTexArray[ texture ].MipFilter );
  if oglAnisotropy > 0 Then
    d3dDevice.SetTextureStageState( 0, D3DTSS_MAXANISOTROPY, oglAnisotropy );
  {$ENDIF}
  {$IFDEF USE_DIRECT3D9}
  if d3dTexArray[ texture ].MagFilter > 0 Then
    d3dDevice.SetSamplerState( 0, D3DSAMP_MAGFILTER, d3dTexArray[ texture ].MagFilter );
  if d3dTexArray[ texture ].MinFilter > 0 Then
    d3dDevice.SetSamplerState( 0, D3DSAMP_MINFILTER, d3dTexArray[ texture ].MinFilter );
  if d3dTexArray[ texture ].MipFilter > 0 Then
    d3dDevice.SetSamplerState( 0, D3DSAMP_MIPFILTER, d3dTexArray[ texture ].MipFilter );
  if oglAnisotropy > 0 Then
    d3dDevice.SetSamplerState( 0, D3DSAMP_MAXANISOTROPY, oglAnisotropy );
  {$ENDIF}

  case  d3dTexArray[ texture ].Wrap of
    GL_CLAMP_TO_EDGE:
      begin
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE );
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE );
      end;
    GL_REPEAT:
      begin
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT );
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT );
      end;
  end;

  RenderTexID := texture;
  d3dDevice.SetTexture( 0, d3dTexArray[ texture ].Texture );
end;

procedure glGenTextures(n: GLsizei; textures: PGLuint);
  var
    i : Integer;
begin
  RenderTexID := -1;
  for i := 1 to d3dTexCount - 1 do
    if d3dTexArray[ i ].Texture = nil Then
      begin
        RenderTexID := i;
        break;
      end;

  if RenderTexID = -1 Then
    begin
      INC( d3dTexCount );
      SetLength( d3dTexArray, d3dTexCount );
      SetLength( d3dResArray, d3dTexCount );
      RenderTexID := d3dTexCount - 1;
    end;
  d3dTexArray[ RenderTexID ].Texture := nil;
  d3dResArray[ RenderTexID ] := nil;
  textures^ := RenderTexID;
end;

procedure glDeleteTextures(n: GLsizei; const textures: PGLuint);
begin
  if textures^ >= d3dTexCount Then exit;

  if Assigned( d3dTexArray[ textures^ ].Texture ) Then
    d3dTexArray[ textures^ ].Texture := nil;
  d3dResArray[ textures^ ] := nil;

  textures^ := 0;
end;

procedure glTexParameterf(target: GLenum; pname: GLenum; param: GLfloat);
  label _exit;
  var
    {$IFDEF USE_DIRECT3D8}
    _type : TD3DTextureStageStateType;
    {$ENDIF}
    {$IFDEF USE_DIRECT3D9}
    _type : TD3DSamplerStateType;
    {$ENDIF}
    value : LongWord;
begin
  if pname = GL_TEXTURE_MAX_ANISOTROPY_EXT Then
    begin
      {$IFDEF USE_DIRECT3D8}
      d3dDevice.SetTextureStageState( 0, D3DTSS_MAGFILTER, D3DTEXF_ANISOTROPIC );
      d3dDevice.SetTextureStageState( 0, D3DTSS_MINFILTER, D3DTEXF_ANISOTROPIC );
      d3dDevice.SetTextureStageState( 0, D3DTSS_MIPFILTER, D3DTEXF_ANISOTROPIC );
      d3dDevice.SetTextureStageState( 0, D3DTSS_MAXANISOTROPY, oglAnisotropy );
      {$ENDIF}
      {$IFDEF USE_DIRECT3D9}
      d3dDevice.SetSamplerState( 0, D3DSAMP_MAGFILTER, D3DTEXF_ANISOTROPIC );
      d3dDevice.SetSamplerState( 0, D3DSAMP_MINFILTER, D3DTEXF_ANISOTROPIC );
      d3dDevice.SetSamplerState( 0, D3DSAMP_MIPFILTER, D3DTEXF_ANISOTROPIC );
      d3dDevice.SetSamplerState( 0, D3DSAMP_MAXANISOTROPY, oglAnisotropy );
      {$ENDIF}
      lMinFilter  := D3DTEXF_ANISOTROPIC;
      lMagFilter  := D3DTEXF_ANISOTROPIC;
      lMipFilter  := D3DTEXF_ANISOTROPIC;
      goto _exit;
    end;

  case pname of
    {$IFDEF USE_DIRECT3D8}
    GL_TEXTURE_MIN_FILTER: _type := D3DTSS_MINFILTER;
    GL_TEXTURE_MAG_FILTER: _type := D3DTSS_MAGFILTER;
    {$ENDIF}
    {$IFDEF USE_DIRECT3D9}
    GL_TEXTURE_MIN_FILTER: _type := D3DSAMP_MINFILTER;
    GL_TEXTURE_MAG_FILTER: _type := D3DSAMP_MAGFILTER;
    {$ENDIF}
  else
    log_Add( 'glTexParameterf: wrong pname' );
    exit;
  end;

  case Round( param ) of
    GL_NEAREST: value := D3DTEXF_POINT;
    GL_LINEAR: value := D3DTEXF_LINEAR;
    {$IFDEF USE_DIRECT3D8}
    GL_LINEAR_MIPMAP_NEAREST: value := D3DTEXF_FLATCUBIC;
    GL_LINEAR_MIPMAP_LINEAR:  value := D3DTEXF_GAUSSIANCUBIC;
    {$ENDIF}
    {$IFDEF USE_DIRECT3D9}
    // FIXME:
    GL_LINEAR_MIPMAP_NEAREST: value := D3DTEXF_PYRAMIDALQUAD;
    GL_LINEAR_MIPMAP_LINEAR:  value := D3DTEXF_GAUSSIANQUAD;
    {$ENDIF}
  else
    log_Add( 'glTexParameterf: wrong param' );
    exit;
  end;

  case pname of
    GL_TEXTURE_MIN_FILTER: lMinFilter := value;
    GL_TEXTURE_MAG_FILTER: lMagFilter := value;
  end;

  {$IFDEF USE_DIRECT3D8}
  d3dDevice.SetTextureStageState( 0, _type, value );
  {$ENDIF}
  {$IFDEF USE_DIRECT3D9}
  d3dDevice.SetSamplerState( 0, _type, value );
  {$ENDIF}

_exit:
  if RenderTexID <> -1 Then
    begin
      d3dTexArray[ RenderTexID ].MinFilter := lMinFilter;
      d3dTexArray[ RenderTexID ].MagFilter := lMagFilter;
      d3dTexArray[ RenderTexID ].MipFilter := lMipFilter;
    end;
end;

procedure glTexParameteri(target: GLenum; pname: GLenum; param: GLint);
  var
    {$IFDEF USE_DIRECT3D8}
    _type : TD3DTextureStageStateType;
    {$ENDIF}
    {$IFDEF USE_DIRECT3D9}
    _type : TD3DSamplerStateType;
    {$ENDIF}
    value : LongWord;
begin
  case pname of
    {$IFDEF USE_DIRECT3D8}
    GL_TEXTURE_WRAP_S: _type := D3DTSS_ADDRESSU;
    GL_TEXTURE_WRAP_T: _type := D3DTSS_ADDRESSV;
    {$ENDIF}
    {$IFDEF USE_DIRECT3D9}
    GL_TEXTURE_WRAP_S: _type := D3DSAMP_ADDRESSU;
    GL_TEXTURE_WRAP_T: _type := D3DSAMP_ADDRESSV;
    GL_GENERATE_MIPMAP:
      begin
        if RenderTexID <> -1 Then
          d3dTexArray[ RenderTexID ].AutoMipMap := param;
        exit;
      end;
    {$ENDIF}
  else
    log_Add( 'glTexParameteri: wrong pname' );
    exit;
  end;

  case param of
    GL_CLAMP_TO_EDGE: value := D3DTADDRESS_CLAMP;
    GL_REPEAT: value := D3DTADDRESS_WRAP;
  else
    log_Add( 'glTexParameteri: wrong param' );
    exit;
  end;

  lWrap := param;
  {$IFDEF USE_DIRECT3D8}
  d3dDevice.SetTextureStageState( 0, _type, value );
  {$ENDIF}
  {$IFDEF USE_DIRECT3D9}
  d3dDevice.SetSamplerState( 0, _type, value );
  {$ENDIF}

  if RenderTexID <> -1 Then
    d3dTexArray[ RenderTexID ].Wrap := lWrap;
end;

procedure d3d_FillTexture( Src, Dst : PByteArray; const Width, Height : Integer; const DstStride : Integer = 0 );
  var
    i, j, w, stride : Integer;
begin
  if psiUnpackRowLength > 0 Then
    w := psiUnpackRowLength
  else
    w := Width;
  if DstStride > 0 Then
    stride := DstStride - w * 4
  else
    stride := 0;

  if stride = 0 Then
    begin
      Move( Src^, Dst^, w * Height * 4 );
      for i := 0 to w * Height - 1 do
        begin
          Dst[ 2 ] := Src[ 0 ];
          Dst[ 0 ] := Src[ 2 ];
          INC( PByte( Dst ), 4 );
          INC( PByte( Src ), 4 );
        end;
    end else
      for j := 0 to Height - 1 do
        begin
          for i := 0 to w - 1 do
            begin
              Dst[ 2 ] := Src[ 0 ];
              Dst[ 1 ] := Src[ 1 ];
              Dst[ 0 ] := Src[ 2 ];
              Dst[ 3 ] := Src[ 3 ];
              INC( PByte( Dst ), 4 );
              INC( PByte( Src ), 4 );
            end;
          INC( PByte( Dst ), stride );
        end;
end;

procedure glPixelStorei(pname: GLenum; param: GLint);
begin
  case pname of
    GL_UNPACK_ROW_LENGTH: psiUnpackRowLength := param;
  end;
end;

procedure glTexImage2D(target: GLenum; level, internalformat: GLint; width, height: GLsizei; border: GLint; format, atype: GLenum; const pixels: Pointer);
  var
    r : TD3DLockedRect;
begin
  if target = GL_TEXTURE_2D Then
    begin
      d3dTexArray[ RenderTexID ].Pool      := D3DPOOL_MANAGED;
      d3dTexArray[ RenderTexID ].MagFilter := lMagFilter;
      d3dTexArray[ RenderTexID ].MinFilter := lMinFilter;
      d3dTexArray[ RenderTexID ].MipFilter := lMipFilter;
      d3dTexArray[ RenderTexID ].Wrap      := lWrap;
      {$IFDEF USE_DIRECT3D8}
      if d3dDevice.CreateTexture( width, height, 1, 0, D3DFMT_A8R8G8B8, D3DPOOL_MANAGED, d3dTexArray[ RenderTexID ].Texture ) <> D3D_OK Then
      {$ENDIF}
      {$IFDEF USE_DIRECT3D9}
      if d3dDevice.CreateTexture( width, height, 1, d3dDefaultUsage or D3DUSAGE_AUTOGENMIPMAP * d3dTexArray[ RenderTexID ].AutoMipMap, D3DFMT_A8R8G8B8, d3dDefaultPool,
                                  d3dTexArray[ RenderTexID ].Texture, nil ) <> D3D_OK Then
      {$ENDIF}
        begin
          log_Add( 'Can''t CreateTexture' );
          exit;
        end;
      if d3dTexArray[ RenderTexID ].Texture.LockRect( level, r, nil, 0 ) = D3D_OK Then
        begin
          d3d_FillTexture( pixels, r.pBits, width, height );
          d3dTexArray[ RenderTexID ].Texture.UnlockRect( level );
        end;
    end;
end;

procedure glCompressedTexImage2D(target: GLenum; level, internalformat: GLint; width, height: GLsizei; border: GLint; imageSize: GLsizei; const pixels: Pointer);
begin

end;

procedure glTexSubImage2D(target: GLenum; level, xoffset, yoffset: GLint; width, height: GLsizei; format, atype: GLenum; const pixels: Pointer);
  var
    r    : TD3DLockedRect;
    a    : TRect;
    s, d : TD3DSurface_Desc;
    {$IFDEF USE_DIRECT3D8}
    src, dst : IDirect3DSurface8;
    {$ENDIF}
    {$IFDEF USE_DIRECT3D9}
    src, dst : IDirect3DSurface9;
    {$ENDIF}
begin
  if ( RenderTexID > d3dTexCount ) or
     ( not Assigned( d3dTexArray[ RenderTexID ].Texture ) ) Then exit;

  d3dTexArray[ RenderTexID ].Texture.GetLevelDesc( level, d );
  if ( d.Pool = D3DPOOL_MANAGED ) or ( d.Usage = D3DUSAGE_DYNAMIC ) Then
    begin
      a.Left   := xoffset;
      a.Top    := yoffset;
      a.Right  := xoffset + width;
      a.Bottom := yoffset + height;
      if d3dTexArray[ RenderTexID ].Texture.LockRect( level, r, @a, 0 ) = D3D_OK Then
        begin
          d3d_FillTexture( pixels, r.pBits, width, height, d.Width * 4 );
          d3dTexArray[ RenderTexID ].Texture.UnlockRect( level );
        end;
    end else
      if d.Pool = D3DPOOL_DEFAULT Then
        begin
          {$IFDEF USE_DIRECT3D8}
          d3dTexArray[ RenderTexID ].Texture.GetLevelDesc( level, d );
          if Assigned( d3dResArray[ RenderTexID ] ) Then
            begin
              d3dResArray[ RenderTexID ].GetLevelDesc( level, s );
              if ( s.Width < d.Width ) or ( s.Height < d.Height ) or ( s.Format <> d.Format ) Then
                d3dResArray[ RenderTexID ] := nil;
            end;
          if not Assigned( d3dResArray[ RenderTexID ] ) Then
            d3dDevice.CreateTexture( d.Width, d.Height, 1, 0, d.Format, D3DPOOL_MANAGED, d3dResArray[ RenderTexID ] );

          d3dTexArray[ RenderTexID ].Texture.GetSurfaceLevel( level, src );
          d3dResArray[ RenderTexID ].GetSurfaceLevel( level, dst );
          d3dDevice.CopyRects( src, nil, 0, dst, nil );

          if d3dResArray[ RenderTexID ].LockRect( level, r, nil, 0 ) = D3D_OK Then
            begin
              d3d_FillTexture( pixels, r.pBits, d.Width, d.Height );
              d3dResArray[ RenderTexID ].UnlockRect( level );
            end;

          d3dTexArray[ RenderTexID ].Texture.GetSurfaceLevel( level, dst );
          d3dResArray[ RenderTexID ].GetSurfaceLevel( level, src );
          d3dDevice.CopyRects( src, nil, 0, dst, nil );
          {$ENDIF}
          {$IFDEF USE_DIRECT3D9}
          d3dTexArray[ RenderTexID ].Texture.GetLevelDesc( level, d );
          if Assigned( d3dResArray[ RenderTexID ] ) Then
            begin
              d3dResArray[ RenderTexID ].GetDesc( s );
              if ( s.Width < d.Width ) or ( s.Height < d.Height ) or ( s.Format <> d.Format ) Then
                d3dResArray[ RenderTexID ] := nil;
            end;
          if not Assigned( d3dResArray[ RenderTexID ] ) Then
            d3dDevice.CreateOffscreenPlainSurface( d.Width, d.Height, d.Format, D3DPOOL_SYSTEMMEM, d3dResArray[ RenderTexID ], nil );

          d3dTexArray[ RenderTexID ].Texture.GetSurfaceLevel( level, src );
          d3dDevice.GetRenderTargetData( src, d3dResArray[ RenderTexID ] );

          if d3dResArray[ RenderTexID ].LockRect( r, nil, 0 ) = D3D_OK Then
            begin
              d3d_FillTexture( pixels, r.pBits, d.Width, d.Height );
              d3dResArray[ RenderTexID ].UnlockRect();
            end;

          d3dTexArray[ RenderTexID ].Texture.GetSurfaceLevel( level, dst );
          d3dDevice.UpdateSurface( d3dResArray[ RenderTexID ], nil, dst, nil );
          {$ENDIF}

          src := nil;
          dst := nil;
        end;
end;

procedure glGetTexImage(target: GLenum; level: GLint; format: GLenum; atype: GLenum; pixels: Pointer);
  var
    r    : TD3DLockedRect;
    s, d : TD3DSurface_Desc;
    hr : HRESULT;
    {$IFDEF USE_DIRECT3D8}
    src, dst : IDirect3DSurface8;
    {$ENDIF}
    {$IFDEF USE_DIRECT3D9}
    src, dst : IDirect3DSurface9;
    {$ENDIF}
begin
  if ( RenderTexID > d3dTexCount ) or
     ( not Assigned( d3dTexArray[ RenderTexID ].Texture ) ) Then exit;

  d3dTexArray[ RenderTexID ].Texture.GetLevelDesc( level, d );
  if ( d.Pool = D3DPOOL_MANAGED ) or ( d.Usage = D3DUSAGE_DYNAMIC ) Then
    begin
      if d3dTexArray[ RenderTexID ].Texture.LockRect( level, r, nil, D3DLOCK_READONLY ) = D3D_OK Then
        begin
          d3d_FillTexture( r.pBits, pixels, d.Width, d.Height );
          d3dTexArray[ RenderTexID ].Texture.UnlockRect( level );
        end;
    end else
      if d.Pool = D3DPOOL_DEFAULT Then
        begin
          {$IFDEF USE_DIRECT3D8}
          d3dTexArray[ RenderTexID ].Texture.GetLevelDesc( 0, d );
          if Assigned( d3dResArray[ RenderTexID ] ) Then
            begin
              d3dResArray[ RenderTexID ].GetLevelDesc( 0, s );
              if ( s.Width < d.Width ) or ( s.Height < d.Height ) or ( s.Format <> d.Format ) Then
                d3dResArray[ RenderTexID ] := nil;
            end;
          if not Assigned( d3dResArray[ RenderTexID ] ) Then
            d3dDevice.CreateTexture( d.Width, d.Height, 1, 0, d.Format, D3DPOOL_MANAGED, d3dResArray[ RenderTexID ] );

          d3dTexArray[ RenderTexID ].Texture.GetSurfaceLevel( 0, src );
          d3dResArray[ RenderTexID ].GetSurfaceLevel( 0, dst );
          d3dDevice.CopyRects( src, nil, 0, dst, nil );
          {$ENDIF}
          {$IFDEF USE_DIRECT3D9}
          d3dTexArray[ RenderTexID ].Texture.GetLevelDesc( 0, d );
          if Assigned( d3dResArray[ RenderTexID ] ) Then
            begin
              d3dResArray[ RenderTexID ].GetDesc( s );
              if ( s.Width < d.Width ) or ( s.Height < d.Height ) or ( s.Format <> d.Format ) Then
                d3dResArray[ RenderTexID ] := nil;
            end;
          if not Assigned( d3dResArray[ RenderTexID ] ) Then
            d3dDevice.CreateOffscreenPlainSurface( d.Width, d.Height, d.Format, D3DPOOL_SYSTEMMEM, d3dResArray[ RenderTexID ], nil );

          d3dTexArray[ RenderTexID ].Texture.GetSurfaceLevel( 0, src );
          d3dDevice.GetRenderTargetData( src, d3dResArray[ RenderTexID ] );
          {$ENDIF}

          if d3dResArray[ RenderTexID ].LockRect( {$IFDEF USE_DIRECT3D8} 0, {$ENDIF} r, nil, D3DLOCK_READONLY ) = D3D_OK Then
            begin
              d3d_FillTexture( r.pBits, pixels, d.Width, d.Height );
              d3dResArray[ RenderTexID ].UnlockRect( {$IFDEF USE_DIRECT3D8} 0 {$ENDIF} );
            end;

          dst := nil;
          src := nil;
        end;
end;

procedure glCopyTexSubImage2D(target: GLenum; level, xoffset, yoffset, x, y: GLint; width, height: GLsizei);
begin
end;

procedure glTexEnvi(target: GLenum; pname: GLenum; param: GLint);
  var
    _type : TD3DTextureStageStateType;
    value : LongWord;
begin
  if target <> GL_TEXTURE_ENV Then exit;

  if ( pname = GL_TEXTURE_ENV_MODE ) and ( param = GL_MODULATE ) Then
    begin
      d3dDevice.SetTextureStageState( 0, D3DTSS_COLOROP, D3DTOP_MODULATE );
      d3dDevice.SetTextureStageState( 0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
      d3dDevice.SetTextureStageState( 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE );

      d3dDevice.SetTextureStageState( 0, D3DTSS_ALPHAOP, D3DTOP_MODULATE );
      d3dDevice.SetTextureStageState( 0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE );
      d3dDevice.SetTextureStageState( 0, D3DTSS_ALPHAARG2, D3DTA_DIFFUSE );

      exit;
    end;
  if ( pname = GL_TEXTURE_ENV_MODE ) and ( param = GL_COMBINE_ARB ) Then exit;

  case pname of
    GL_COMBINE_RGB_ARB:   _type := D3DTSS_COLOROP;
    GL_SOURCE0_RGB_ARB:   _type := D3DTSS_COLORARG0;
    GL_SOURCE1_RGB_ARB:   _type := D3DTSS_COLORARG1;
    GL_SOURCE2_RGB_ARB:   _type := D3DTSS_COLORARG2;
    GL_COMBINE_ALPHA_ARB: _type := D3DTSS_ALPHAOP;
    GL_SOURCE0_ALPHA_ARB: _type := D3DTSS_ALPHAARG0;
    GL_SOURCE1_ALPHA_ARB: _type := D3DTSS_ALPHAARG1;
    GL_SOURCE2_ALPHA_ARB: _type := D3DTSS_ALPHAARG2;
  else
    log_Add( 'glTexEnvi: wrong pname' );
    exit;
  end;

  case param of
    GL_REPLACE:           value := 25; // Хммм...
    GL_MODULATE:          value := D3DTOP_MODULATE;
    GL_TEXTURE:           value := D3DTA_TEXTURE;
    GL_PRIMARY_COLOR_ARB: value := D3DTA_DIFFUSE;
  else
    log_Add( 'glTexEnvi: wrong param' );
    exit;
  end;

  d3dDevice.SetTextureStageState( 0, _type, value );
end;

function gluBuild2DMipmaps(target: GLenum; components, width, height: GLint; format, atype: GLenum; const data: Pointer): Integer;
  var
    r : TD3DLockedRect;
begin
  if target = GL_TEXTURE_2D Then
    begin
      Result := 0;
      d3dTexArray[ d3dTexCount - 1 ].Pool       := D3DPOOL_MANAGED;
      d3dTexArray[ d3dTexCount - 1 ].MagFilter  := lMagFilter;
      d3dTexArray[ d3dTexCount - 1 ].MinFilter  := lMinFilter;
      d3dTexArray[ d3dTexCount - 1 ].MipFilter  := lMipFilter;
      d3dTexArray[ d3dTexCount - 1 ].Wrap       := lWrap;
      {$IFDEF USE_DIRECT3D8}
      d3dDevice.CreateTexture( width, height, 0, 0, D3DFMT_A8R8G8B8, D3DPOOL_MANAGED, d3dTexArray[ d3dTexCount - 1 ].Texture );
      {$ENDIF}
      {$IFDEF USE_DIRECT3D9}
      d3dDevice.CreateTexture( width, height, 0, d3dDefaultUsage, D3DFMT_A8R8G8B8, d3dDefaultPool, d3dTexArray[ d3dTexCount - 1 ].Texture, nil );
      {$ENDIF}
      d3dTexArray[ d3dTexCount - 1 ].Texture.LockRect( 0, r, nil, 0 );
      d3d_FillTexture( data, r.pBits, width, height );
      d3dTexArray[ d3dTexCount - 1 ].Texture.UnlockRect( 0 );
    end else
      Result := -1;
end;

procedure glTexCoord2f(s, t: GLfloat);
begin
  if bTVCount = Length( bTVertices ) Then SetLength( bTVertices, bTVCount + 1024 );
  bTVertices[ bTVCount ].u := s;
  bTVertices[ bTVCount ].v := t;
end;

procedure glTexCoord2fv(v: PGLfloat);
begin
  if bTVCount = Length( bTVertices ) Then SetLength( bTVertices, bTVCount + 1024 );
  bTVertices[ bTVCount ].u := zglPPoint2D( v ).X;
  bTVertices[ bTVCount ].v := zglPPoint2D( v ).Y;
end;

initialization
  // Страшно, да :)
  {$IFDEF FPC}
    { according to bug 7570, this is necessary on all x86 platforms,
      maybe we've to fix the sse control word as well }
    { Yes, at least for darwin/x86_64 (JM) }
    {$IF DEFINED(cpui386) or DEFINED(cpux86_64)}
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
    {$IFEND}
  {$ELSE}
    Set8087CW($133F);
  {$ENDIF}

end.
