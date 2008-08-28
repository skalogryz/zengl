{
 * Copyright © Kemka Andrey aka Andru
 * mail: dr.andru@gmail.com
 * site: http://andru.2x4.ru
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
}
unit zgl_textures;

{$I define.inc}

interface

uses
  GL, GLExt,

  zgl_opengl,
  zgl_const,
  zgl_global_var,
  zgl_log,
  zgl_types,
  zgl_math,

  zgl_file,
  Utils;

function  tex_Add : zglPTexture; extdecl;
procedure tex_Del( Texture : zglPTexture ); extdecl;
  
procedure tex_Create( var Texture : zglTTexture; var pData : Pointer ); extdecl;
function  tex_CreateZero( Width, Height : WORD; Color, Flags : DWORD ) : zglPTexture; extdecl;
function  tex_LoadFromFile( FileName : PChar; TransparentColor, Flags : DWORD ) : zglPTexture; extdecl;
procedure tex_SetFrameSize( Texture : zglPTexture; FrameWidth, FrameHeight : WORD ); extdecl;
function  tex_SetMask( Texture, Mask : zglPTexture ) : zglPTexture; extdecl;

procedure tex_Filter( Texture : zglPTexture; Flags : DWORD ); extdecl;
procedure tex_SetAnisotropy( Level : Byte ); extdecl;

procedure tex_CalcPOT( var pData : Pointer; var Width, Height : WORD; var U, V : Single );
procedure tex_CalcGrayScale( pData : Pointer; Width, Height : WORD );
procedure tex_CalcInvert( pData : Pointer; Width, Height : WORD );
procedure tex_CalcRGB( var pData : Pointer; Width, Height : WORD );
procedure tex_CalcTransparent( pData : Pointer; TransparentColor : Integer; Width, Height : WORD );

procedure tex_GetData( Texture : zglPTexture; var pData : Pointer; var pSize : Integer ); extdecl;

implementation
uses
  zgl_main;

function tex_Add;
begin
  Result := @managerTexture.First;
  while Assigned( Result.Next ) do
    Result := Result.Next;
      
  Result.Next := AllocMem( SizeOf( zglTTexture ) );
  FillChar( Result.Next^, SizeOf( zglTTexture ), 0 );
  Result.Next.Prev := Result;
  Result := Result.Next;
  INC( managerTexture.Count );
end;

procedure tex_Del;
begin
  glDeleteTextures( 1, @Texture.ID );
  if Assigned( Texture.Prev ) Then
    Texture.Prev.Next := Texture.Next;
  if Assigned( Texture.Next ) Then
    Texture.Next.Prev := Texture.Prev;
  Freememory( Texture );
  DEC( managerTexture.Count );
end;

procedure tex_Create;
  var
    format, iformat, cformat : DWORD;
begin
  if Texture.Flags and TEX_GRAYSCALE > 0 Then
    tex_CalcGrayScale( pData, Texture.Width, Texture.Height );
  if Texture.Flags and TEX_INVERT > 0 Then
    tex_CalcInvert( pData, Texture.Width, Texture.Height );
  if Texture.Flags and TEX_CONVERT_TO_POT > 0 Then
    tex_CalcPOT( pData, Texture.Width, Texture.Height, Texture.U, Texture.V );
  if Texture.Flags and TEX_RGB > 0 Then
    tex_CalcRGB( pData, Texture.Width, Texture.Height );

  if Texture.Flags and TEX_COMPRESS >= 1 Then
    if not ogl_CanCompress Then
      Texture.Flags := Texture.Flags xor TEX_COMPRESS;

  glEnable( GL_TEXTURE_2D );
  glGenTextures( 1, @Texture.ID );

  tex_Filter( @Texture, Texture.Flags );
  glBindTexture( GL_TEXTURE_2D, Texture.ID );
  
  if Texture.Flags and TEX_RGB > 0 Then
    begin
      format  := GL_RGB;
      iformat := GL_RGB * Byte( scr_BPP = 32 ) or GL_RGB16 * Byte( scr_BPP = 16 );
      cformat := GL_COMPRESSED_RGB_ARB;
    end else
      begin
        format  := GL_RGBA;
        iformat := GL_RGBA * Byte( scr_BPP = 32 ) or GL_RGBA16 * Byte( scr_BPP = 16 );
        cformat := GL_COMPRESSED_RGBA_ARB;
      end;
  
  glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );

  if Texture.Flags and TEX_MIPMAP = 0 Then
    begin
      if Texture.Flags and TEX_COMPRESS = 0 Then
        glTexImage2D( GL_TEXTURE_2D, 0, iformat, Texture.Width, Texture.Height, 0, format, GL_UNSIGNED_BYTE, pData )
      else
        glTexImage2D( GL_TEXTURE_2D, 0, cformat, Texture.Width, Texture.Height, 0, format, GL_UNSIGNED_BYTE, pData );
    end else
      begin
        if Texture.Flags and TEX_COMPRESS = 0 Then
          gluBuild2DMipmaps( GL_TEXTURE_2D, iformat, Texture.Width, Texture.Height, format, GL_UNSIGNED_BYTE, pData )
        else
          gluBuild2DMipmaps( GL_TEXTURE_2D, cformat, Texture.Width, Texture.Height, format, GL_UNSIGNED_BYTE, pData )
      end;
      
   glDisable( GL_TEXTURE_2D );
end;

function tex_CreateZero;
  var
    i       : DWORD;
    pData   : Pointer;
begin
  pData := AllocMem( Width * Height * 4 );
  for i := 0 to Width * Height - 1 do
    Move( Color, Pointer( pData + i * 4 )^, 4 );
  
  Result         := tex_Add;
  Result.Width   := Width;
  Result.Height  := Height;
  Result.U       := 1;
  Result.V       := 1;
  Result.FramesX := 1;
  Result.FramesY := 1;
  Result.Flags   := Flags;
  tex_Create( Result^, pData );
  
  FreeMemory( pData );
end;

function tex_LoadFromFile;
  var
    i      : Integer;
    pData  : Pointer;
    w, h   : WORD;
begin
  Result := nil;
  pData  := nil;
  
  if not file_Exists( FileName ) Then
    begin
      log_Add( 'Cannot read ' + FileName );
      zgl_Destroy;
      halt;
    end;

  for i := texNFCount - 1 downto 0 do
    if copy( StrUp( FileName ), length( FileName ) - 3, 4 ) = '.' + texNewFormats[ i ].Extension Then
      texNewFormats[ i ].Loader( FileName, pData, w, h );

  if not Assigned( pData ) Then
    begin
      log_Add( 'Unable to load texture: ' + FileName );
      zgl_Destroy;
      halt;
    end;

  Result         := tex_Add;
  Result.Width   := w;
  Result.Height  := h;
  Result.U       := 1;
  Result.V       := 1;
  Result.FramesX := 1;
  Result.FramesY := 1;
  Result.Flags   := Flags;
  if TransparentColor <> $FF000000 Then
    tex_CalcTransparent( pData, TransparentColor, w, h );
  tex_Create( Result^, pData );
  
  log_Add( 'Successful loading of texture: ' + FileName );
  
  FreeMemory( pData );
end;

procedure tex_SetFrameSize;
begin
if Texture.Flags and TEX_QUALITY_MEDIUM > 0 Then
  begin
    FrameWidth := FrameWidth div 2;
    FrameHeight := FrameHeight div 2;
  end else
    if Texture.Flags and TEX_QUALITY_LOW > 0 Then
      begin
        FrameWidth := FrameWidth div 4;
        FrameHeight := FrameHeight div 4;
      end;

  Texture.FramesX := m_Round( ( Texture.Width * Texture.U ) ) div FrameWidth;
  Texture.FramesY := m_Round( ( Texture.Height * Texture.V ) ) div FrameHeight;
end;

procedure tex_Filter;
begin
  Texture.Flags := Flags;
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
  if Flags and TEX_MIPMAP > 0 Then
    begin
      if Flags and TEX_FILTER_NEAREST > 0 Then
        begin
          glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
          glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
        end else
          if Flags and TEX_FILTER_LINEAR > 0 Then
            begin
              glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
              glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
            end else
              if Flags and TEX_FILTER_BILINEAR > 0 Then
                begin
                  glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST );
                  glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
                end else
                  if Flags and TEX_FILTER_TRILINEAR > 0 Then
                    begin
                      glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR );
                      glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
                    end else
                      if Flags and TEX_FILTER_ANISOTROPY > 0 Then
                        begin
                          glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR );
                          glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
                          glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, ogl_Anisotropy );
                        end;
    end else
      begin
        if ( Flags and TEX_FILTER_NEAREST      > 0 ) or
           ( ( Flags and TEX_FILTER_LINEAR     = 0 ) and
             ( Flags and TEX_FILTER_BILINEAR   = 0 ) and
             ( Flags and TEX_FILTER_TRILINEAR  = 0 ) and
             ( Flags and TEX_FILTER_ANISOTROPY = 0 ) ) Then
          begin
            glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
            glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
          end else
            begin
              glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
              glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
            end;
      end;

  glBindTexture( GL_TEXTURE_2D, 0 );
end;

procedure tex_SetAnisotropy;
begin
  if Level > ogl_MaxAnisotropy Then
    ogl_Anisotropy := ogl_MaxAnisotropy
  else
    ogl_Anisotropy := Level;
end;

procedure tex_CalcPOT;
  var
    i, j : DWORD;
    w, h : WORD;
    Data : array of Byte;
begin
  w := GetPOT( Width );
  h := GetPOT( Height );
  U := Width  / w;
  V := Height / h;
  
  SetLength( Data, Width * Height * 4 );
  Move( pData^, Pointer( Data )^, Width * Height * 4 );
  FreeMem( pData );
  pData := AllocMem( w * h * 4 );
  FillChar( pData^, w * h * 4, 0 );
  
  for i := 0 to Height - 1 do
    for j := 0 to Width - 1 do
      begin
        PByte( pData + j * 4 + i * w * 4 + 0 )^ := Data[ j * 4 + i * Width * 4 + 0 ];
        PByte( pData + j * 4 + i * w * 4 + 1 )^ := Data[ j * 4 + i * Width * 4 + 1 ];
        PByte( pData + j * 4 + i * w * 4 + 2 )^ := Data[ j * 4 + i * Width * 4 + 2 ];
        PByte( pData + j * 4 + i * w * 4 + 3 )^ := Data[ j * 4 + i * Width * 4 + 3 ];
      end;

  Width  := w;
  Height := h;
  SetLength( Data, 0 );
end;

procedure tex_CalcGrayScale;
  var
    i    : Integer;
    P    : Pointer;
    Gray : Byte;
begin
  for i := 0 to Width * Height - 1 do
    begin
      P := pData + i * 4;
      Gray := m_Round(
                       PByte( P + 0 )^ * 0.3  +
                       PByte( P + 1 )^ * 0.59 +
                       PByte( P + 2 )^ * 0.11
                     );

      PByte( P + 0 )^ := Gray;
      PByte( P + 1 )^ := Gray;
      PByte( P + 2 )^ := Gray;
    end;
end;

procedure tex_CalcInvert;
  var
    i : Integer;
    P : Pointer;
begin
  for i := 0 to Width * Height - 1 do
    begin
      P := pData + i * 4;
      PByte( P + 0 )^ := 255 - PByte( P + 0 )^;
      PByte( P + 1 )^ := 255 - PByte( P + 1 )^;
      PByte( P + 2 )^ := 255 - PByte( P + 2 )^;
    end;
end;

procedure tex_CalcRGB;
  var
    i, j : DWORD;
    Data : array of Byte;
begin
  SetLength( Data, Width * Height * 4 );
  Move( pData^, Pointer( Data )^, Width * Height * 4 );
  FreeMem( pData );
  pData := AllocMem( Width * Height * 3 );
  FillChar( pData^, Width * Height * 3, 0 );
  
  for i := 0 to Height - 1 do
    for j := 0 to Width - 1 do
      begin
        PByte( pData + j * 3 + i * Width * 3 + 0 )^ := Data[ j * 4 + i * Width * 4 + 0 ];
        PByte( pData + j * 3 + i * Width * 3 + 1 )^ := Data[ j * 4 + i * Width * 4 + 1 ];
        PByte( pData + j * 3 + i * Width * 3 + 2 )^ := Data[ j * 4 + i * Width * 4 + 2 ];
      end;

  SetLength( Data, 0 );
end;

procedure tex_CalcTransparent;
  var
    i       : Integer;
    r, g, b : Byte;
begin
  r := ( TransparentColor and $FF     );
  g := ( TransparentColor and $FF00   ) shr 8;
  b := ( TransparentColor and $FF0000 ) shr 16;
  for i := 0 to Width * Height - 1 do
    begin
      if ( PByte( pData + 0 + i * 4 )^ = b ) and
         ( PByte( pData + 1 + i * 4 )^ = g ) and
         ( PByte( pData + 2 + i * 4 )^ = r ) Then
        begin
          PByte( pData + 0 + i * 4 )^ := 0;
          PByte( pData + 1 + i * 4 )^ := 0;
          PByte( pData + 2 + i * 4 )^ := 0;
          PByte( pData + 3 + i * 4 )^ := 0;

          if i + Width <= Width * Height - 1 Then
            if PByte( pData + ( i + Width ) * 4 + 3 )^ > 0 Then
              begin
                PByte( pData + 0 + i * 4 )^ := PByte( pData + ( i + Width ) * 4 + 0 )^;
                PByte( pData + 1 + i * 4 )^ := PByte( pData + ( i + Width ) * 4 + 1 )^;
                PByte( pData + 2 + i * 4 )^ := PByte( pData + ( i + Width ) * 4 + 2 )^;
              end;

          if i + 1 <= Width * Height - 1 Then
            if PByte( pData + ( i + 1 ) * 4 + 3 )^ > 0 Then
              begin
                PByte( pData + 0 + i * 4 )^ := PByte( pData + ( i + 1 ) * 4 + 0 )^;
                PByte( pData + 1 + i * 4 )^ := PByte( pData + ( i + 1 ) * 4 + 1 )^;
                PByte( pData + 2 + i * 4 )^ := PByte( pData + ( i + 1 ) * 4 + 2 )^;
              end;

          if i - 1 > 0 Then
            if PByte( pData + ( i - 1 ) * 4 + 3 )^ > 0 Then
              begin
                PByte( pData + 0 + i * 4 )^ := PByte( pData + ( i - 1 ) * 4 + 0 )^;
                PByte( pData + 1 + i * 4 )^ := PByte( pData + ( i - 1 ) * 4 + 1 )^;
                PByte( pData + 2 + i * 4 )^ := PByte( pData + ( i - 1 ) * 4 + 2 )^;
              end;

          if i - Width > 0 Then
            if PByte( pData + ( i - Width ) * 4 + 3 )^ > 0 Then
              begin
                PByte( pData + 0 + i * 4 )^ := PByte( pData + ( i - Width ) * 4 + 0 )^;
                PByte( pData + 1 + i * 4 )^ := PByte( pData + ( i - Width ) * 4 + 1 )^;
                PByte( pData + 2 + i * 4 )^ := PByte( pData + ( i - Width ) * 4 + 2 )^;
              end;
        end;
    end;
end;

function tex_SetMask;
  var
    i            : DWORD;
    tSize, mSize : Integer;
    tData, mData : Pointer;
    pData        : Pointer;
begin
  tex_GetData( Texture, tData, tSize );
  tex_GetData( Mask, mData, mSize );
  pData := AllocMem( Texture.Width * Texture.Height * 4 );

  for i := 0 to Texture.Width * Texture.Height - 1 do
    begin
      PByte( pData + i * 4 + 0 )^ := PByte( tData + i * tSize + 0 )^;
      PByte( pData + i * 4 + 1 )^ := PByte( tData + i * tSize + 1 )^;
      PByte( pData + i * 4 + 2 )^ := PByte( tData + i * tSize + 2 )^;
      PByte( pData + i * 4 + 3 )^ := PByte( mData + i * mSize + 0 )^;
    end;

  Result         := tex_Add;
  Result.Width   := Texture.Width;
  Result.Height  := Texture.Height;
  Result.U       := Texture.U;
  Result.V       := Texture.V;
  Result.FramesX := 1;
  Result.FramesY := 1;
  Result.Flags   := Texture.Flags xor TEX_GRAYSCALE * Byte( Texture.Flags and TEX_GRAYSCALE > 0 ) 
                                  xor TEX_INVERT * Byte( Texture.Flags and TEX_INVERT > 0 )
                                  xor TEX_CONVERT_TO_POT * Byte( Texture.Flags and TEX_CONVERT_TO_POT > 0 );
  tex_Create( Result^, pData );
  tex_Del( Texture );

  FreeMem( pData );
  FreeMem( tData );
  FreeMem( mData );
end;

procedure tex_GetData;
begin
  pSize := 3 + Byte( Texture.Flags and TEX_RGB = 0 );
  pData := AllocMem( Texture.Width * Texture.Height * pSize );

  glEnable( GL_TEXTURE_2D );
  glBindTexture( GL_TEXTURE_2D, Texture.ID );
  if Texture.Flags and TEX_RGB > 0 Then
    glGetTexImage( GL_TEXTURE_2D, 0, GL_RGB, GL_UNSIGNED_BYTE, pData )
  else
    glGetTexImage( GL_TEXTURE_2D, 0, GL_RGBA, GL_UNSIGNED_BYTE, pData );
  glDisable( GL_TEXTURE_2D );
end;

end.
