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
unit zgl_textures;

{$I zgl_config.cfg}

interface

uses
  zgl_math_2d,
  zgl_memory;

const
  TEX_MIPMAP            = $000001;
  TEX_CLAMP             = $000002;
  TEX_REPEAT            = $000004;
  TEX_COMPRESS          = $000008;

  TEX_CONVERT_TO_POT    = $000010;
  TEX_CALCULATE_ALPHA   = $000020;

  TEX_GRAYSCALE         = $000040;
  TEX_INVERT            = $000080;
  TEX_CUSTOM_EFFECT     = $000100;

  TEX_FILTER_NEAREST    = $000200;
  TEX_FILTER_LINEAR     = $000400;
  TEX_FILTER_BILINEAR   = $000800;
  TEX_FILTER_TRILINEAR  = $001000;
  TEX_FILTER_ANISOTROPY = $002000;

  TEX_DEFAULT_2D        = TEX_CLAMP or TEX_FILTER_LINEAR or TEX_CONVERT_TO_POT or TEX_CALCULATE_ALPHA;

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
      Items   : Integer;
      Formats : Integer;
              end;
    First   : zglTTexture;
    Formats : array of zglTTextureFormat;
end;

function  tex_Add : zglPTexture;
procedure tex_Del( var Texture : zglPTexture );

procedure tex_Create( var Texture : zglTTexture; var pData : Pointer );
function  tex_CreateZero( Width, Height : Word; Color : LongWord = $000000; Flags : LongWord = TEX_DEFAULT_2D ) : zglPTexture;
function  tex_LoadFromFile( const FileName : String; TransparentColor : LongWord = $FF000000; Flags : LongWord = TEX_DEFAULT_2D ) : zglPTexture;
function  tex_LoadFromMemory( const Memory : zglTMemory; const Extension : String; TransparentColor : LongWord = $FF000000; Flags : LongWord = TEX_DEFAULT_2D ) : zglPTexture;
procedure tex_SetFrameSize( var Texture : zglPTexture; FrameWidth, FrameHeight : Word );
function  tex_SetMask( var Texture : zglPTexture; Mask : zglPTexture ) : zglPTexture;
procedure tex_CalcTexCoords( var Texture : zglTTexture );

procedure tex_Filter( Texture : zglPTexture; Flags : LongWord );
procedure tex_SetAnisotropy( Level : Byte );

procedure tex_CalcFlags( var Texture : zglTTexture; var pData : Pointer );
procedure tex_CalcPOT( var pData : Pointer; var Width, Height : Word; var U, V : Single );
procedure tex_CalcGrayScale( var pData : Pointer; Width, Height : Word );
procedure tex_CalcInvert( var pData : Pointer; Width, Height : Word );
procedure tex_CalcTransparent( var pData : Pointer; TransparentColor : LongWord; Width, Height : Word );
procedure tex_CalcAlpha( var pData : Pointer; Width, Height : Word );

procedure tex_SetData( Texture : zglPTexture; pData : Pointer; X, Y, Width, Height : Word; Stride : Integer = 0 );
procedure tex_GetData( Texture : zglPTexture; var pData : Pointer );

procedure zeroce( var pData : Pointer; Width, Height : Word );

var
  managerTexture       : zglTTextureManager;
  managerZeroTexture   : zglPTexture;
  tex_CalcCustomEffect : procedure( var pData : Pointer; Width, Height : Word ) = zeroce;

implementation
uses
  zgl_types,
  zgl_main,
  zgl_screen,
  zgl_opengl,
  zgl_opengl_all,
  zgl_render_2d,
  zgl_file,
  zgl_log,
  zgl_utils;

procedure zeroce; begin end;

function tex_Add : zglPTexture;
begin
  Result := @managerTexture.First;
  while Assigned( Result.next ) do
    Result := Result.next;

  zgl_GetMem( Pointer( Result.next ), SizeOf( zglTTexture ) );
  Result.next.prev := Result;
  Result.next.next := nil;
  Result := Result.next;
  INC( managerTexture.Count.Items );
end;

procedure tex_Del( var Texture : zglPTexture );
begin
  if ( not Assigned( Texture ) ) or ( Texture = managerZeroTexture ) Then
    begin
      Texture := nil;
      exit;
    end;

  glDeleteTextures( 1, @Texture.ID );
  if Assigned( Texture.prev ) Then
    Texture.prev.next := Texture.next;
  if Assigned( Texture.next ) Then
    Texture.next.prev := Texture.prev;
  SetLength( Texture.FramesCoord, 0 );
  FreeMem( Texture );
  Texture := nil;

  DEC( managerTexture.Count.Items );
end;

procedure tex_Create( var Texture : zglTTexture; var pData : Pointer );
  var
    cformat : LongWord;
begin
  tex_CalcFlags( Texture, pData );
  if Texture.Flags and TEX_COMPRESS >= 1 Then
    if ( not oglCanCompressE ) and ( not oglCanCompressA ) Then
      Texture.Flags := Texture.Flags xor TEX_COMPRESS;

  glEnable( GL_TEXTURE_2D );
  glGenTextures( 1, @Texture.ID );

  tex_Filter( @Texture, Texture.Flags );
  glBindTexture( GL_TEXTURE_2D, Texture.ID );

  if oglCanCompressE Then
    cformat := GL_COMPRESSED_RGBA_S3TC_DXT5_EXT
  else
    cformat := GL_COMPRESSED_RGBA_ARB;

  glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );

  if ( not oglCanAutoMipMap ) and ( Texture.Flags and TEX_MIPMAP > 0 ) Then
    begin
      if Texture.Flags and TEX_COMPRESS = 0 Then
        gluBuild2DMipmaps( GL_TEXTURE_2D, GL_RGBA, Texture.Width, Texture.Height, GL_RGBA, GL_UNSIGNED_BYTE, pData )
      else
        gluBuild2DMipmaps( GL_TEXTURE_2D, cformat, Texture.Width, Texture.Height, GL_RGBA, GL_UNSIGNED_BYTE, pData );
    end else
      begin
        if Texture.Flags and TEX_COMPRESS = 0 Then
          glTexImage2D( GL_TEXTURE_2D, 0, GL_RGBA, Texture.Width, Texture.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, pData )
        else
          glTexImage2D( GL_TEXTURE_2D, 0, cformat, Texture.Width, Texture.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, pData );
      end;

  glDisable( GL_TEXTURE_2D );

  if Texture.Flags and TEX_CONVERT_TO_POT > 0 Then
    begin
      Texture.Width  := Round( Texture.Width * Texture.U );
      Texture.Height := Round( Texture.Height * Texture.V );
    end;

  tex_CalcTexCoords( Texture );
end;

function tex_CreateZero( Width, Height : Word; Color, Flags : LongWord ) : zglPTexture;
  var
    i     : LongWord;
    pData : Pointer;
begin
  GetMem( pData, Width * Height * 4 );
  for i := 0 to Width * Height - 1 do
    Move( Color, Pointer( Ptr( pData ) + i * 4 )^, 4 );

  Result         := tex_Add();
  Result.Width   := Width;
  Result.Height  := Height;
  Result.U       := 1;
  Result.V       := 1;
  Result.FramesX := 1;
  Result.FramesY := 1;
  Result.Flags   := Flags;
  tex_Create( Result^, pData );

  FreeMem( pData );
end;

function tex_LoadFromFile( const FileName : String; TransparentColor, Flags : LongWord ) : zglPTexture;
  var
    i     : Integer;
    pData : Pointer;
    w, h  : Word;
begin
  Result := nil;
  pData  := nil;

  if not Assigned( managerZeroTexture ) Then
    managerZeroTexture := tex_CreateZero( 4, 4, $FFFFFFFF, TEX_DEFAULT_2D );
  Result := managerZeroTexture;

  if not file_Exists( FileName ) Then
    begin
      log_Add( 'Cannot read "' + FileName + '"' );
      exit;
    end;

  for i := managerTexture.Count.Formats - 1 downto 0 do
    if u_StrUp( file_GetExtension( FileName ) ) = managerTexture.Formats[ i ].Extension Then
      managerTexture.Formats[ i ].FileLoader( FileName, pData, w, h );

  if not Assigned( pData ) Then
    begin
      log_Add( 'Unable to load texture: "' + FileName + '"' );
      exit;
    end;

  Result         := tex_Add();
  Result.Width   := w;
  Result.Height  := h;
  Result.U       := 1;
  Result.V       := 1;
  Result.FramesX := 1;
  Result.FramesY := 1;
  Result.Flags   := Flags;
  if Result.Flags and TEX_CALCULATE_ALPHA > 0 Then
    begin
      tex_CalcTransparent( pData, TransparentColor, w, h );
      tex_CalcAlpha( pData, w, h );
    end else
      tex_CalcTransparent( pData, TransparentColor, w, h );
  tex_Create( Result^, pData );

  log_Add( 'Texture loaded: "' + FileName + '"' );

  FreeMem( pData );
end;

function tex_LoadFromMemory( const Memory : zglTMemory; const Extension : String; TransparentColor, Flags : LongWord ) : zglPTexture;
  var
    i     : Integer;
    pData : Pointer;
    w, h  : Word;
begin
  Result := nil;
  pData  := nil;

  if not Assigned( managerZeroTexture ) Then
    managerZeroTexture := tex_CreateZero( 4, 4, $FFFFFFFF, TEX_DEFAULT_2D );
  Result := managerZeroTexture;

  for i := managerTexture.Count.Formats - 1 downto 0 do
    if u_StrUp( Extension ) = managerTexture.Formats[ i ].Extension Then
      managerTexture.Formats[ i ].MemLoader( Memory, pData, w, h );

  if not Assigned( pData ) Then
    begin
      log_Add( 'Unable to load texture: From Memory' );
      exit;
    end;

  Result         := tex_Add();
  Result.Width   := w;
  Result.Height  := h;
  Result.U       := 1;
  Result.V       := 1;
  Result.FramesX := 1;
  Result.FramesY := 1;
  Result.Flags   := Flags;
  if Result.Flags and TEX_CALCULATE_ALPHA > 0 Then
    begin
      tex_CalcTransparent( pData, TransparentColor, w, h );
      tex_CalcAlpha( pData, w, h );
    end else
      tex_CalcTransparent( pData, TransparentColor, w, h );
  tex_Create( Result^, pData );

  FreeMem( pData );
end;

procedure tex_SetFrameSize( var Texture : zglPTexture; FrameWidth, FrameHeight : Word );
begin
  if not Assigned( Texture ) Then exit;

  Texture.FramesX := Round( Texture.Width ) div FrameWidth;
  Texture.FramesY := Round( Texture.Height ) div FrameHeight;
  if Texture.FramesX = 0 Then Texture.FramesX := 1;
  if Texture.FramesY = 0 Then Texture.FramesY := 1;
  tex_CalcTexCoords( Texture^ );
end;

function tex_SetMask( var Texture : zglPTexture; Mask : zglPTexture ) : zglPTexture;
  var
    i, j   : Integer;
    tData  : Pointer;
    mData  : Pointer;
    pData  : Pointer;
    rW, mW : Integer;
begin
  if ( not Assigned( Texture ) ) or ( not Assigned( Mask ) ) Then exit;

  rW := Round( Texture.Width / Texture.U );
  mW := Round( Mask.Width / Mask.U );

  tex_GetData( Texture, tData );
  tex_GetData( Mask, mData );
  GetMem( pData, Texture.Width * Texture.Height * 4 );

  for i := 0 to Texture.Width - 1 do
    for j := 0 to Texture.Height - 1 do
      begin
        PByte( Ptr( pData ) + i * 4 + j * Texture.Width * 4 + 0 )^ := PByte( Ptr( tData ) + i * 4 + j * rW * 4 + 0 )^;
        PByte( Ptr( pData ) + i * 4 + j * Texture.Width * 4 + 1 )^ := PByte( Ptr( tData ) + i * 4 + j * rW * 4 + 1 )^;
        PByte( Ptr( pData ) + i * 4 + j * Texture.Width * 4 + 2 )^ := PByte( Ptr( tData ) + i * 4 + j * rW * 4 + 2 )^;
        PByte( Ptr( pData ) + i * 4 + j * Texture.Width * 4 + 3 )^ := PByte( Ptr( mData ) + i * 4 + j * mW * 4 + 0 )^;
      end;

  Result         := tex_Add();
  Result.Width   := Texture.Width;
  Result.Height  := Texture.Height;
  Result.U       := 1;
  Result.V       := 1;
  Result.FramesX := 1;
  Result.FramesY := 1;
  Result.Flags   := Texture.Flags;
  if Result.Flags and TEX_GRAYSCALE > 0 Then
    Result.Flags := Result.Flags xor TEX_GRAYSCALE;
  if Result.Flags and TEX_INVERT > 0 Then
    Result.Flags := Result.Flags xor TEX_INVERT;
  tex_Create( Result^, pData );
  tex_Del( Texture );

  FreeMem( pData );
  FreeMem( tData );
  FreeMem( mData );
end;

procedure tex_CalcTexCoords( var Texture : zglTTexture );
  var
    i : Integer;
    tX, tY, u, v : Single;
begin
  SetLength( Texture.FramesCoord, Texture.FramesX * Texture.FramesY + 1 );
  u := Texture.U / Texture.FramesX;
  v := Texture.V / Texture.FramesY;

  Texture.FramesCoord[ 0, 0 ].X := 0;
  Texture.FramesCoord[ 0, 0 ].Y := Texture.V;
  Texture.FramesCoord[ 0, 1 ].X := Texture.U;
  Texture.FramesCoord[ 0, 1 ].Y := Texture.V;
  Texture.FramesCoord[ 0, 2 ].X := Texture.U;
  Texture.FramesCoord[ 0, 2 ].Y := 0;
  Texture.FramesCoord[ 0, 3 ].X := 0;
  Texture.FramesCoord[ 0, 3 ].Y := 0;
  for i := 1 to Texture.FramesX * Texture.FramesY do
    begin
      tY := i div Texture.FramesX;
      tX := i - tY * Texture.FramesX;
      tY := Texture.FramesY - tY;
      if tX = 0 Then
        begin
          tX := Texture.FramesX;
          tY := tY + 1;
        end;
      tX := tX * u;
      tY := tY * v;

      Texture.FramesCoord[ i, 0 ].X := tX - u;
      Texture.FramesCoord[ i, 0 ].Y := tY;

      Texture.FramesCoord[ i, 1 ].X := tX;
      Texture.FramesCoord[ i, 1 ].Y := tY;

      Texture.FramesCoord[ i, 2 ].X := tX;
      Texture.FramesCoord[ i, 2 ].Y := tY - v;

      Texture.FramesCoord[ i, 3 ].X := tX - u;
      Texture.FramesCoord[ i, 3 ].Y := tY - v;
    end;
end;

procedure tex_Filter( Texture : zglPTexture; Flags : LongWord );
  var
    currentFilter : LongWord;
begin
  batch2d_Flush();

  if Texture.Flags and TEX_FILTER_NEAREST > 0 Then
    currentFilter := TEX_FILTER_NEAREST
  else if Texture.Flags and TEX_FILTER_LINEAR > 0 Then
    currentFilter := TEX_FILTER_LINEAR
  else if Texture.Flags and TEX_FILTER_BILINEAR > 0 Then
    currentFilter := TEX_FILTER_BILINEAR
  else if Texture.Flags and TEX_FILTER_TRILINEAR > 0 Then
    currentFilter := TEX_FILTER_TRILINEAR
  else if Texture.Flags and TEX_FILTER_ANISOTROPY > 0 Then
    currentFilter := TEX_FILTER_ANISOTROPY
  else
    currentFilter := 0;
  Texture.Flags := Texture.Flags xor currentFilter;

  glBindTexture( GL_TEXTURE_2D, Texture.ID );

  if Flags and TEX_CLAMP > 0 Then
    begin
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE );
    end;
  if Flags and TEX_REPEAT > 0 Then
    begin
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT );
    end;

  if oglCanAutoMipMap Then
    glTexParameteri( GL_TEXTURE_2D, GL_GENERATE_MIPMAP, Byte( Flags and TEX_MIPMAP > 0 ) );

  if Flags and TEX_MIPMAP > 0 Then
    begin
      if Flags and TEX_FILTER_NEAREST > 0 Then
        begin
          Texture.Flags := Texture.Flags or TEX_FILTER_NEAREST;
          glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
          glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
        end else
          if Flags and TEX_FILTER_LINEAR > 0 Then
            begin
              Texture.Flags := Texture.Flags or TEX_FILTER_LINEAR;
              glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
              glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
            end else
              if Flags and TEX_FILTER_BILINEAR > 0 Then
                begin
                  Texture.Flags := Texture.Flags or TEX_FILTER_BILINEAR;
                  glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST );
                  glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
                end else
                  if ( Flags and TEX_FILTER_TRILINEAR > 0 ) or ( ( not oglCanAnisotropy ) and ( Flags and TEX_FILTER_ANISOTROPY > 0 ) ) Then
                    begin
                      Texture.Flags := Texture.Flags or TEX_FILTER_TRILINEAR;
                      glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR );
                      glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
                    end else
                      if Flags and TEX_FILTER_ANISOTROPY > 0 Then
                        begin
                          Texture.Flags := Texture.Flags or TEX_FILTER_ANISOTROPY;
                          glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR );
                          glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
                          glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, oglAnisotropy );
                        end;
    end else
      begin
        if ( Flags and TEX_FILTER_NEAREST > 0 ) or ( ( Flags and TEX_FILTER_LINEAR = 0 ) and ( Flags and TEX_FILTER_BILINEAR = 0 ) and
           ( Flags and TEX_FILTER_TRILINEAR = 0 ) and ( Flags and TEX_FILTER_ANISOTROPY = 0 ) ) Then
          begin
            Texture.Flags := Texture.Flags or TEX_FILTER_NEAREST;
            glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
            glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
          end else
            begin
              Texture.Flags := Texture.Flags or TEX_FILTER_LINEAR;
              glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
              glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
            end;
      end;
end;

procedure tex_SetAnisotropy( Level : Byte );
begin
  if Level > oglMaxAnisotropy Then
    oglAnisotropy := oglMaxAnisotropy
  else
    oglAnisotropy := Level;
end;

procedure tex_CalcFlags( var Texture : zglTTexture; var pData : Pointer );
begin
  if Texture.Flags and TEX_GRAYSCALE > 0 Then
    tex_CalcGrayScale( pData, Texture.Width, Texture.Height );
  if Texture.Flags and TEX_INVERT > 0 Then
    tex_CalcInvert( pData, Texture.Width, Texture.Height );
  if Texture.Flags and TEX_CUSTOM_EFFECT > 0 Then
    tex_CalcCustomEffect( pData, Texture.Width, Texture.Height );
  if Texture.Flags and TEX_CONVERT_TO_POT > 0 Then
    tex_CalcPOT( pData, Texture.Width, Texture.Height, Texture.U, Texture.V );
end;

procedure tex_CalcPOT( var pData : Pointer; var Width, Height : Word; var U, V : Single );
  var
    i, j : LongWord;
    w, h : Word;
    data : array of Byte;
begin
  w := u_GetPOT( Width );
  h := u_GetPOT( Height );
  if ( w = Width ) and ( h = Height ) Then
    begin
      U := 1;
      V := 1;
      exit;
    end;
  U := Width  / w;
  V := Height / h;

  SetLength( data, Width * Height * 4 );
  Move( pData^, Pointer( data )^, Width * Height * 4 );
  FreeMem( pData );
  GetMem( pData, w * h * 4 );

  for i := 0 to Height - 1 do
    Move( data[ i * Width * 4 ], PLongWord( Ptr( pData ) + i * w * 4 )^, Width * 4 );

  for i := Height to h - 1 do
    Move( PByte( Ptr( pData ) + ( Height - 1 ) * w * 4 )^, PByte( Ptr( pData ) + i * w * 4 )^, Width * 4 );
  for i := 0 to h - 1 do
    for j := Width to w - 1 do
      PLongWord( Ptr( pData ) + j * 4 + i * w * 4 )^ := PLongWord( Ptr( pData ) + ( Width - 1 ) * 4 + i * w * 4 )^;

  Width  := w;
  Height := h;
  SetLength( data, 0 );
end;

procedure tex_CalcGrayScale( var pData : Pointer; Width, Height : Word );
  var
    i    : Integer;
    p    : Ptr;
    gray : Byte;
begin
  for i := 0 to Width * Height - 1 do
    begin
      p := Ptr( pData ) + i * 4;
      gray := Round( PByte( p + 0 )^ * 0.299 + PByte( p + 1 )^ * 0.587 + PByte( p + 2 )^ * 0.114 );

      PByte( p + 0 )^ := gray;
      PByte( p + 1 )^ := gray;
      PByte( p + 2 )^ := gray;
    end;
end;

procedure tex_CalcInvert( var pData : Pointer; Width, Height : Word );
  var
    i : Integer;
    p : Ptr;
begin
  for i := 0 to Width * Height - 1 do
    begin
      p := Ptr( pData ) + i * 4;
      PByte( p + 0 )^ := 255 - PByte( p + 0 )^;
      PByte( p + 1 )^ := 255 - PByte( p + 1 )^;
      PByte( p + 2 )^ := 255 - PByte( p + 2 )^;
    end;
end;

procedure tex_CalcTransparent( var pData : Pointer; TransparentColor : LongWord; Width, Height : Word );
  var
    i       : Integer;
    r, g, b : Byte;
begin
  if TransparentColor = $FF000000 Then exit;

  r := ( TransparentColor and $FF0000 ) shr 16;
  g := ( TransparentColor and $FF00   ) shr 8;
  b := ( TransparentColor and $FF     );
  for i := 0 to Width * Height - 1 do
    if ( PByte( Ptr( pData ) + i * 4 )^ = r ) and ( PByte( Ptr( pData ) + i * 4 + 1 )^ = g ) and ( PByte( Ptr( pData ) + i * 4 + 2 )^ = b ) Then
      PByte( Ptr( pData ) + i * 4 + 3 )^ := 0;
end;

procedure tex_CalcAlpha( var pData : Pointer; Width, Height : Word );
  var
    i       : Integer;
    r, g, b : Byte;
begin
  for i := 0 to Width * Height - 1 do
    if ( PByte( Ptr( pData ) + i * 4 + 3 )^ = 0 ) Then
      begin
        PLongWord( Ptr( pData ) + i * 4 )^ := 0;

        // bottom
        if i + Width <= Width * Height - 1 Then
          if PByte( Ptr( pData ) + ( i + Width ) * 4 + 3 )^ > 0 Then
            begin
              PByte( Ptr( pData ) + i * 4 )^     := PByte( Ptr( pData ) + ( i + Width ) * 4 )^;
              PByte( Ptr( pData ) + i * 4 + 1 )^ := PByte( Ptr( pData ) + ( i + Width ) * 4 + 1 )^;
              PByte( Ptr( pData ) + i * 4 + 2 )^ := PByte( Ptr( pData ) + ( i + Width ) * 4 + 2 )^;
              continue;
            end;

        // bottom right
        if ( i + 1 <= Width * Height - 1 ) and ( i + Width <= Width * Height - 1 ) Then
          if PByte( Ptr( pData ) + ( i + Width + 1 ) * 4 + 3 )^ > 0 Then
            begin
              PByte( Ptr( pData ) + i * 4 )^     := PByte( Ptr( pData ) + ( i + Width + 1 ) * 4 )^;
              PByte( Ptr( pData ) + i * 4 + 1 )^ := PByte( Ptr( pData ) + ( i + Width + 1 ) * 4 + 1 )^;
              PByte( Ptr( pData ) + i * 4 + 2 )^ := PByte( Ptr( pData ) + ( i + Width + 1 ) * 4 + 2 )^;
              continue;
            end;

        // right
        if i + 1 <= Width * Height - 1 Then
          if PByte( Ptr( pData ) + ( i + 1 ) * 4 + 3 )^ > 0 Then
            begin
              PByte( Ptr( pData ) + i * 4 )^     := PByte( Ptr( pData ) + ( i + 1 ) * 4 )^;
              PByte( Ptr( pData ) + i * 4 + 1 )^ := PByte( Ptr( pData ) + ( i + 1 ) * 4 + 1 )^;
              PByte( Ptr( pData ) + i * 4 + 2 )^ := PByte( Ptr( pData ) + ( i + 1 ) * 4 + 2 )^;
              continue;
            end;

        // top right
        if ( i + 1 <= Width * Height - 1 ) and ( i - Width > 0 ) Then
          if PByte( Ptr( pData ) + ( i - Width + 1 ) * 4 + 3 )^ > 0 Then
            begin
              PByte( Ptr( pData ) + i * 4 )^     := PByte( Ptr( pData ) + ( i - Width + 1 ) * 4 )^;
              PByte( Ptr( pData ) + i * 4 + 1 )^ := PByte( Ptr( pData ) + ( i - Width + 1 ) * 4 + 1 )^;
              PByte( Ptr( pData ) + i * 4 + 2 )^ := PByte( Ptr( pData ) + ( i - Width + 1 ) * 4 + 2 )^;
              continue;
            end;

        // top
        if i - Width > 0 Then
          if PByte( Ptr( pData ) + ( i - Width ) * 4 + 3 )^ > 0 Then
            begin
              PByte( Ptr( pData ) + i * 4 )^     := PByte( Ptr( pData ) + ( i - Width ) * 4 )^;
              PByte( Ptr( pData ) + i * 4 + 1 )^ := PByte( Ptr( pData ) + ( i - Width ) * 4 + 1 )^;
              PByte( Ptr( pData ) + i * 4 + 2 )^ := PByte( Ptr( pData ) + ( i - Width ) * 4 + 2 )^;
              continue;
            end;

        // top left
        if ( i - 1 > 0 ) and ( i - Width > 0 ) Then
          if PByte( Ptr( pData ) + ( i - Width - 1 ) * 4 + 3 )^ > 0 Then
            begin
              PByte( Ptr( pData ) + i * 4 )^     := PByte( Ptr( pData ) + ( i - Width - 1 ) * 4 )^;
              PByte( Ptr( pData ) + i * 4 + 1 )^ := PByte( Ptr( pData ) + ( i - Width - 1 ) * 4 + 1 )^;
              PByte( Ptr( pData ) + i * 4 + 2 )^ := PByte( Ptr( pData ) + ( i - Width - 1 ) * 4 + 2 )^;
              continue;
            end;

        // left
        if i - 1 > 0 Then
          if PByte( Ptr( pData ) + ( i - 1 ) * 4 + 3 )^ > 0 Then
            begin
              PByte( Ptr( pData ) + i * 4 )^     := PByte( Ptr( pData ) + ( i - 1 ) * 4 )^;
              PByte( Ptr( pData ) + i * 4 + 1 )^ := PByte( Ptr( pData ) + ( i - 1 ) * 4 + 1 )^;
              PByte( Ptr( pData ) + i * 4 + 2 )^ := PByte( Ptr( pData ) + ( i - 1 ) * 4 + 2 )^;
              continue;
            end;

        // bottom left
        if ( i - 1 > 0 ) and ( i + Width <= Width * Height - 1 ) Then
          if PByte( Ptr( pData ) + ( i + Width - 1 ) * 4 + 3 )^ > 0 Then
            begin
              PByte( Ptr( pData ) + i * 4 )^     := PByte( Ptr( pData ) + ( i + Width - 1 ) * 4 )^;
              PByte( Ptr( pData ) + i * 4 + 1 )^ := PByte( Ptr( pData ) + ( i + Width - 1 ) * 4 + 1 )^;
              PByte( Ptr( pData ) + i * 4 + 2 )^ := PByte( Ptr( pData ) + ( i + Width - 1 ) * 4 + 2 )^;
              continue;
            end;
      end;
end;

procedure tex_SetData( Texture : zglPTexture; pData : Pointer; X, Y, Width, Height : Word; Stride : Integer = 0 );
begin
  batch2d_Flush();

  if ( not Assigned( Texture ) ) or ( not Assigned( pData ) ) Then exit;

  glEnable( GL_TEXTURE_2D );
  glPixelStorei( GL_UNPACK_ROW_LENGTH, Stride );
  glBindTexture( GL_TEXTURE_2D, Texture.ID );
  glTexSubImage2D( GL_TEXTURE_2D, 0, X, Texture.Height - Height - Y, Width, Height, GL_RGBA, GL_UNSIGNED_BYTE, pData );
  glPixelStorei( GL_UNPACK_ROW_LENGTH, 0 );
  glDisable( GL_TEXTURE_2D );
end;

procedure tex_GetData( Texture : zglPTexture; var pData : Pointer );
begin
  batch2d_Flush();

  if not Assigned( Texture ) Then
    begin
      pData := nil;
      exit;
    end;

  GetMem( pData, Round( Texture.Width / Texture.U ) * Round( Texture.Height / Texture.V ) * 4 );

  glEnable( GL_TEXTURE_2D );
  glBindTexture( GL_TEXTURE_2D, Texture.ID );
  glGetTexImage( GL_TEXTURE_2D, 0, GL_RGBA, GL_UNSIGNED_BYTE, pData );
  glDisable( GL_TEXTURE_2D );
end;

end.
