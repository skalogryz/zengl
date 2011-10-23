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
  TEX_FORMAT_RGBA       = $01;
  TEX_FORMAT_RGBA_4444  = $02;
  TEX_FORMAT_RGBA_PVR2  = $10;
  TEX_FORMAT_RGBA_PVR4  = $11;
  TEX_FORMAT_RGBA_DXT1  = $20;
  TEX_FORMAT_RGBA_DXT3  = $21;
  TEX_FORMAT_RGBA_DXT5  = $22;

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

  zglTTextureFileLoader = procedure( const FileName : String; var pData : Pointer; var W, H, Format : Word );
  zglTTextureMemLoader  = procedure( const Memory : zglTMemory; var pData : Pointer; var W, H, Format : Word );

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
    Format        : Word;

    prev, next    : zglPTexture;
end;

type
  zglPTextureFormat = ^zglTTextureFormat;
  zglTTextureFormat = record
    Extension  : String;
    FileLoader : zglTTextureFileLoader;
    MemLoader  : zglTTextureMemLoader;
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

function  tex_Create( var Texture : zglTTexture; pData : Pointer ) : Boolean;
function  tex_CreateZero( Width, Height : Word; Color : LongWord = $000000; Flags : LongWord = TEX_DEFAULT_2D ) : zglPTexture;
function  tex_LoadFromFile( const FileName : String; TransparentColor : LongWord = $FF000000; Flags : LongWord = TEX_DEFAULT_2D ) : zglPTexture;
function  tex_LoadFromMemory( const Memory : zglTMemory; const Extension : String; TransparentColor : LongWord = $FF000000; Flags : LongWord = TEX_DEFAULT_2D ) : zglPTexture;
procedure tex_SetFrameSize( var Texture : zglPTexture; FrameWidth, FrameHeight : Word );
function  tex_SetMask( var Texture : zglPTexture; Mask : zglPTexture ) : zglPTexture;
procedure tex_CalcTexCoords( var Texture : zglTTexture );

procedure tex_Filter( Texture : zglPTexture; Flags : LongWord );
procedure tex_SetAnisotropy( Level : Byte );

procedure tex_CalcFlags( var Texture : zglTTexture; var pData : Pointer );
procedure tex_CalcPOT( var pData : Pointer; var Width, Height : Word; var U, V : Single; PixelSize : Integer );
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
  {$IFNDEF USE_GLES}
  zgl_opengl,
  zgl_opengl_all,
  {$ELSE}
  zgl_opengles,
  zgl_opengles_all,
  {$ENDIF}
  zgl_render_2d,
  zgl_resources,
  zgl_file,
  zgl_log,
  zgl_utils;

procedure zeroce; begin end;

function tex_GetVRAM( Texture : zglPTexture ) : LongWord;
  var
    size : LongWord;
begin
  size := Round( Texture.Width / Texture.U ) * Round( Texture.Height / Texture.V );
  case Texture.Format of
    TEX_FORMAT_RGBA: Result := size * 4;
    TEX_FORMAT_RGBA_4444: Result := size * 2;
    {$IFDEF USE_GLES}
    TEX_FORMAT_RGBA_PVR2: Result := size div 4;
    TEX_FORMAT_RGBA_PVR4: Result := size div 2;
    {$ELSE}
    TEX_FORMAT_RGBA_DXT1: Result := size div 2;
    TEX_FORMAT_RGBA_DXT3: Result := size;
    TEX_FORMAT_RGBA_DXT5: Result := size;
    {$ENDIF}
  end;
end;

function tex_Add : zglPTexture;
begin
  Result := @managerTexture.First;
  while Assigned( Result.next ) do
    Result := Result.next;

  zgl_GetMem( Pointer( Result.next ), SizeOf( zglTTexture ) );
  Result.next.U       := 1;
  Result.next.V       := 1;
  Result.next.FramesX := 1;
  Result.next.FramesY := 1;
  Result.next.prev    := Result;
  Result.next.next    := nil;
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

  oglVRAMUsed := oglVRAMUsed - tex_GetVRAM( Texture );

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

function tex_Create( var Texture : zglTTexture; pData : Pointer ) : Boolean;
  var
    width  : Integer;
    height : Integer;
    size   : LongWord;
begin
  if Texture.Flags and TEX_COMPRESS >= 1 Then
  {$IFNDEF USE_GLES}
    if not oglCanS3TC Then
  {$ENDIF}
      Texture.Flags := Texture.Flags xor TEX_COMPRESS;

  width  := Round( Texture.Width / Texture.U );
  height := Round( Texture.Height / Texture.V );
  size   := tex_GetVRAM( @Texture );
  Result := FALSE;

  glEnable( GL_TEXTURE_2D );
  glGenTextures( 1, @Texture.ID );

  tex_Filter( @Texture, Texture.Flags );
  glBindTexture( GL_TEXTURE_2D, Texture.ID );

  {$IFDEF USE_GLES}
  if ( ( not oglCanPVRTC ) and ( ( Texture.Format = TEX_FORMAT_RGBA_DXT1 ) or ( Texture.Format = TEX_FORMAT_RGBA_DXT3 ) or ( Texture.Format = TEX_FORMAT_RGBA_DXT5 ) ) ) or
  {$ELSE}
  if ( ( not oglCanS3TC ) and ( ( Texture.Format = TEX_FORMAT_RGBA_PVR2 ) or ( Texture.Format = TEX_FORMAT_RGBA_PVR4 ) ) ) or
  {$ENDIF}
    ( ( width > oglMaxTexSize ) or ( height > oglMaxTexSize ) ) Then
    begin
      glDisable( GL_TEXTURE_2D );
      exit;
    end;

  glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );

  {$IFNDEF USE_GLES}
  if ( not oglCanAutoMipMap ) and ( Texture.Flags and TEX_MIPMAP > 0 ) Then
    begin
      if Texture.Flags and TEX_COMPRESS = 0 Then
        gluBuild2DMipmaps( GL_TEXTURE_2D, GL_RGBA, width, height, GL_RGBA, GL_UNSIGNED_BYTE, pData )
      else
        gluBuild2DMipmaps( GL_TEXTURE_2D, GL_COMPRESSED_RGBA_S3TC_DXT5_EXT, width, height, GL_RGBA, GL_UNSIGNED_BYTE, pData );
    end else
  {$ENDIF}
      begin
        if Texture.Flags and TEX_COMPRESS = 0 Then
          begin
            case Texture.Format of
              TEX_FORMAT_RGBA: glTexImage2D( GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, pData );
              TEX_FORMAT_RGBA_4444: glTexImage2D( GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA, GL_UNSIGNED_SHORT_4_4_4_4, pData );
              {$IFDEF USE_GLES}
              TEX_FORMAT_RGBA_PVR2: glCompressedTexImage2D( GL_TEXTURE_2D, 0, GL_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG, width, height, 0, size, pData );
              TEX_FORMAT_RGBA_PVR4: glCompressedTexImage2D( GL_TEXTURE_2D, 0, GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG, width, height, 0, size, pData );
              {$ELSE}
              TEX_FORMAT_RGBA_DXT1: glCompressedTexImage2D( GL_TEXTURE_2D, 0, GL_COMPRESSED_RGBA_S3TC_DXT1_EXT, width, height, 0, size, pData );
              TEX_FORMAT_RGBA_DXT3: glCompressedTexImage2D( GL_TEXTURE_2D, 0, GL_COMPRESSED_RGBA_S3TC_DXT3_EXT, width, height, 0, size, pData );
              TEX_FORMAT_RGBA_DXT5: glCompressedTexImage2D( GL_TEXTURE_2D, 0, GL_COMPRESSED_RGBA_S3TC_DXT5_EXT, width, height, 0, size, pData );
              {$ENDIF}
            end;
          {$IFDEF USE_GLES}
          end;
          {$ELSE}
          end else
            glTexImage2D( GL_TEXTURE_2D, 0, GL_COMPRESSED_RGBA_S3TC_DXT5_EXT, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, pData );
          {$ENDIF}
      end;

  oglVRAMUsed := oglVRAMUsed + size;
  glDisable( GL_TEXTURE_2D );
  Result := TRUE;
end;

function tex_CreateZero( Width, Height : Word; Color, Flags : LongWord ) : zglPTexture;
  var
    i     : LongWord;
    pData : Pointer;
begin
  GetMem( pData, Width * Height * 4 );
  for i := 0 to Width * Height - 1 do
    Move( Color, Pointer( Ptr( pData ) + i * 4 )^, 4 );

  Result        := tex_Add();
  Result.Width  := Width;
  Result.Height := Height;
  Result.Flags  := Flags;
  Result.Format := TEX_FORMAT_RGBA;
  tex_CalcFlags( Result^, pData );
  tex_CalcTexCoords( Result^ );
  if not tex_Create( Result^, pData ) Then
    begin
      tex_Del( Result );
      Result := managerZeroTexture;
    end;

  FreeMem( pData );
end;

function tex_LoadFromFile( const FileName : String; TransparentColor, Flags : LongWord ) : zglPTexture;
  var
    i      : Integer;
    pData  : Pointer;
    w, h   : Word;
    format : Word;
    res    : zglTTextureResource;
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
      if resUseThreaded Then
        begin
          Result               := tex_Add();
          res.FileName         := FileName;
          res.Texture          := Result;
          res.FileLoader       := managerTexture.Formats[ i ].FileLoader;
          res.TransparentColor := TransparentColor;
          res.Flags            := Flags;
          res_AddToQueue( RES_TEXTURE, TRUE, @res );
          exit;
        end else
          managerTexture.Formats[ i ].FileLoader( FileName, pData, w, h, format );

  if not Assigned( pData ) Then
    begin
      log_Add( 'Unable to load texture: "' + FileName + '"' );
      exit;
    end;

  Result        := tex_Add();
  Result.Width  := w;
  Result.Height := h;
  Result.Flags  := Flags;
  Result.Format := format;
  if Result.Format = TEX_FORMAT_RGBA Then
    begin
      if Result.Flags and TEX_CALCULATE_ALPHA > 0 Then
        begin
          tex_CalcTransparent( pData, TransparentColor, w, h );
          tex_CalcAlpha( pData, w, h );
        end else
          tex_CalcTransparent( pData, TransparentColor, w, h );
    end;
  tex_CalcFlags( Result^, pData );
  tex_CalcTexCoords( Result^ );
  if not tex_Create( Result^, pData ) Then
    begin
      tex_Del( Result );
      Result := managerZeroTexture;
    end;

  log_Add( 'Texture loaded: "' + FileName + '"' );

  FreeMem( pData );
end;

function tex_LoadFromMemory( const Memory : zglTMemory; const Extension : String; TransparentColor, Flags : LongWord ) : zglPTexture;
  var
    i      : Integer;
    pData  : Pointer;
    w, h   : Word;
    format : Word;
    res    : zglTTextureResource;
begin
  Result := nil;
  pData  := nil;

  if not Assigned( managerZeroTexture ) Then
    managerZeroTexture := tex_CreateZero( 4, 4, $FFFFFFFF, TEX_DEFAULT_2D );
  Result := managerZeroTexture;

  for i := managerTexture.Count.Formats - 1 downto 0 do
    if u_StrUp( Extension ) = managerTexture.Formats[ i ].Extension Then
      if resUseThreaded Then
        begin
          Result               := tex_Add();
          res.Memory           := Memory;
          res.Texture          := Result;
          res.MemLoader        := managerTexture.Formats[ i ].MemLoader;
          res.TransparentColor := TransparentColor;
          res.Flags            := Flags;
          res_AddToQueue( RES_TEXTURE, FALSE, @res );
          exit;
        end else
          managerTexture.Formats[ i ].MemLoader( Memory, pData, w, h, format );

  if not Assigned( pData ) Then
    begin
      log_Add( 'Unable to load texture: From Memory' );
      exit;
    end;

  Result        := tex_Add();
  Result.Width  := w;
  Result.Height := h;
  Result.Flags  := Flags;
  Result.Format := format;
  if Result.Format = TEX_FORMAT_RGBA Then
    begin
      if Result.Flags and TEX_CALCULATE_ALPHA > 0 Then
        begin
          tex_CalcTransparent( pData, TransparentColor, w, h );
          tex_CalcAlpha( pData, w, h );
        end else
          tex_CalcTransparent( pData, TransparentColor, w, h );
    end;
  tex_CalcFlags( Result^, pData );
  tex_CalcTexCoords( Result^ );
  if not tex_Create( Result^, pData ) Then
    begin
      tex_Del( Result );
      Result := managerZeroTexture;
    end;

  FreeMem( pData );
end;

procedure tex_SetFrameSize( var Texture : zglPTexture; FrameWidth, FrameHeight : Word );
  var
    res : zglTTextureFrameSizeResource;
begin
  if resUseThreaded Then
    begin
      res.Texture     := Texture;
      res.FrameWidth  := FrameWidth;
      res.FrameHeight := FrameHeight;
      res_AddToQueue( RES_TEXTURE_FRAMESIZE, TRUE, @res );
    end else
      begin
        if not Assigned( Texture ) Then exit;

        Texture.FramesX := Round( Texture.Width ) div FrameWidth;
        Texture.FramesY := Round( Texture.Height ) div FrameHeight;
        if Texture.FramesX = 0 Then Texture.FramesX := 1;
        if Texture.FramesY = 0 Then Texture.FramesY := 1;
        tex_CalcTexCoords( Texture^ );
      end;
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

  Result        := tex_Add();
  Result.Width  := Texture.Width;
  Result.Height := Texture.Height;
  Result.Flags  := Texture.Flags;
  if Result.Flags and TEX_GRAYSCALE > 0 Then
    Result.Flags := Result.Flags xor TEX_GRAYSCALE;
  if Result.Flags and TEX_INVERT > 0 Then
    Result.Flags := Result.Flags xor TEX_INVERT;
  tex_CalcFlags( Result^, pData );
  tex_CalcTexCoords( Result^ );
  if not tex_Create( Result^, pData ) Then
    begin
      tex_Del( Result );
      Result := managerZeroTexture;
    end;
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
  if Texture.Format = TEX_FORMAT_RGBA Then
    begin
      if Texture.Flags and TEX_GRAYSCALE > 0 Then
        tex_CalcGrayScale( pData, Texture.Width, Texture.Height );
      if Texture.Flags and TEX_INVERT > 0 Then
        tex_CalcInvert( pData, Texture.Width, Texture.Height );
      if Texture.Flags and TEX_CUSTOM_EFFECT > 0 Then
        tex_CalcCustomEffect( pData, Texture.Width, Texture.Height );
    end;

  if Texture.Flags and TEX_CONVERT_TO_POT > 0 Then
    begin
      if Texture.Format = TEX_FORMAT_RGBA Then
        tex_CalcPOT( pData, Texture.Width, Texture.Height, Texture.U, Texture.V, 4 )
      else
        if Texture.Format = TEX_FORMAT_RGBA_4444 Then
          tex_CalcPOT( pData, Texture.Width, Texture.Height, Texture.U, Texture.V, 2 )
        else
          exit;

      Texture.Width  := Round( Texture.Width * Texture.U );
      Texture.Height := Round( Texture.Height * Texture.V );
    end;
end;

procedure tex_CalcPOT( var pData : Pointer; var Width, Height : Word; var U, V : Single; PixelSize : Integer );
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

  SetLength( data, Width * Height * PixelSize );
  Move( pData^, Pointer( data )^, Width * Height * PixelSize );
  FreeMem( pData );
  GetMem( pData, w * h * PixelSize );

  for i := 0 to Height - 1 do
    Move( data[ i * Width * PixelSize ], PLongWord( Ptr( pData ) + i * w * PixelSize )^, Width * PixelSize );

  for i := Height to h - 1 do
    Move( PByte( Ptr( pData ) + ( Height - 1 ) * w * PixelSize )^, PByte( Ptr( pData ) + i * w * PixelSize )^, Width * PixelSize );

  if PixelSize = 4 Then
    begin
      for i := 0 to h - 1 do
        for j := Width to w - 1 do
          PLongWord( Ptr( pData ) + j * 4 + i * w * 4 )^ := PLongWord( Ptr( pData ) + ( Width - 1 ) * 4 + i * w * 4 )^;
    end else
      for i := 0 to h - 1 do
        for j := Width to w - 1 do
          PWord( Ptr( pData ) + j * 2 + i * w * 2 )^ := PWord( Ptr( pData ) + ( Width - 1 ) * 2 + i * w * 2 )^;

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
  {$IFNDEF USE_GLES}
  glPixelStorei( GL_UNPACK_ROW_LENGTH, Stride );
  {$ENDIF}
  glBindTexture( GL_TEXTURE_2D, Texture.ID );
  glTexSubImage2D( GL_TEXTURE_2D, 0, X, Texture.Height - Height - Y, Width, Height, GL_RGBA, GL_UNSIGNED_BYTE, pData );
  {$IFNDEF USE_GLES}
  glPixelStorei( GL_UNPACK_ROW_LENGTH, 0 );
  {$ENDIF}
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

  {$IFNDEF USE_GLES}
  glEnable( GL_TEXTURE_2D );
  glBindTexture( GL_TEXTURE_2D, Texture.ID );
  glGetTexImage( GL_TEXTURE_2D, 0, GL_RGBA, GL_UNSIGNED_BYTE, pData );
  glDisable( GL_TEXTURE_2D );
  {$ELSE}
  if not oglCanFBO Then exit;

  if oglReadPixelsFBO = 0 Then
    begin
      glGenFramebuffers( 1, @oglReadPixelsFBO );
      glBindFramebuffer( GL_FRAMEBUFFER, oglReadPixelsFBO );
    end;

  glFramebufferTexture2D( GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, Texture.ID, 0 );

  glReadPixels( 0, 0, Round( Texture.Width / Texture.U ), Round( Texture.Height / Texture.V ), GL_RGBA, GL_UNSIGNED_BYTE, pData );

  if oglTarget = TARGET_SCREEN Then
    begin
      {$IFNDEF iOS}
      glBindFramebuffer( GL_FRAMEBUFFER, 0 );
      {$ELSE}
      glBindFramebuffer( GL_FRAMEBUFFER, eglFramebuffer );
      glBindRenderbuffer( GL_RENDERBUFFER, eglRenderbuffer );
      glFramebufferRenderbuffer( GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_RENDERBUFFER, eglRenderbuffer );
      {$ENDIF}
    end else
      begin
        // TODO:
      end;
  {$ENDIF}
end;

end.
