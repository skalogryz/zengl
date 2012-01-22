{
 *  Copyright © Kemka Andrey aka Andru
 *  mail: dr.andru@gmail.com
 *  site: http://zengl.org
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
unit zgl_texture_atlas;

{$I zgl_config.cfg}

interface
uses
  zgl_textures,
  zgl_memory,
  zgl_math_2d;

type
  zglPAtlasNode = ^zglTAtlasNode;
  zglTAtlasNode = record
    Leaf     : Boolean;
    Texture  : zglPTexture;
    TexCoord : zglTTextureCoord;
    FramesX  : Word;
    FramesY  : Word;
    Rect     : zglTRect;
    child    : array[ 0..1 ] of zglPAtlasNode;
  end;

type
  zglPAtlas = ^zglTAtlas;
  zglTAtlas = record
    root       : zglTAtlasNode;
    Texture    : zglPTexture;
    Full       : Boolean;
    prev, next : zglPAtlas;
  end;

type
  zglPAtlasManager = ^zglTAtlasManager;
  zglTAtlasManager = record
    Count : Integer;
    First : zglTAtlas;
end;

function  atlas_Add( Width, Height : Word; Flags : LongWord ) : zglPAtlas;
procedure atlas_Del( var Atlas : zglPAtlas );

function  atlas_AddNode( Node : zglPAtlasNode; Texture : zglPTexture; Width, Height : Integer ) : zglPAtlasNode;
procedure atlas_DelNode( var Node : zglPAtlasNode );

procedure atlas_GetFrameCoord( Node : zglPAtlasNode; Frame : Word; var TexCoord : array of zglTPoint2D );
function  atlas_InsertFromTexture( Atlas : zglPAtlas; Texture : zglPTexture ) : zglPAtlasNode;
function  atlas_InsertFromFile( Atlas : zglPAtlas; const FileName : UTF8String; TransparentColor, Flags : LongWord ) : zglPAtlasNode;
function  atlas_InsertFromMemory( Atlas : zglPAtlas; const Memory : zglTMemory; const Extension : UTF8String; TransparentColor, Flags : LongWord ) : zglPAtlasNode;

procedure atals_InsertDataToNode( var Node : zglPAtlasNode; pData : Pointer; RowLength, Width, Height : Word );

var
  managerAtlas : zglTAtlasManager;

implementation
uses
  zgl_main,
  zgl_log,
  {$IFNDEF USE_GLES}
  zgl_opengl_all,
  {$ELSE}
  zgl_opengles_all,
  {$ENDIF}
  zgl_file,
  zgl_utils;

function atlas_Add( Width, Height : Word; Flags : LongWord ) : zglPAtlas;
  var
    atlas : zglPAtlas;
begin
  Result := nil;

  atlas := @managerAtlas.First;
  while Assigned( atlas.next ) do
    atlas := atlas.next;

  zgl_GetMem( Pointer( atlas.next ), SizeOf( zglTAtlas ) );
  atlas.next.prev        := atlas;
  atlas.next.next        := nil;
  atlas.next.Texture     := tex_CreateZero( Width, Height, $00000000, Flags );
  atlas.next.root.Leaf   := TRUE;
  atlas.next.root.Rect.W := Width;
  atlas.next.root.Rect.H := Height;
  atlas := atlas.next;
  INC( managerAtlas.Count );

  Result := atlas;
end;

procedure atlas_Del( var Atlas : zglPAtlas );
begin
  if not Assigned( Atlas ) Then exit;

  atlas_DelNode( Atlas.root.child[ 0 ] );
  atlas_DelNode( Atlas.root.child[ 1 ] );
  tex_Del( Atlas.Texture );
  if Assigned( Atlas.prev ) Then
    Atlas.prev.next := Atlas.next;
  if Assigned( Atlas.next ) Then
    Atlas.next.prev := Atlas.prev;
  FreeMem( Atlas );
  Atlas := nil;

  DEC( managerAtlas.Count );
end;

function atlas_AddNode( Node : zglPAtlasNode; Texture : zglPTexture; Width, Height : Integer ) : zglPAtlasNode;
  var
    dw, dh : Single;
    c1, c2 : zglPAtlasNode;
begin
  if not Node.Leaf Then
    begin
      Result := atlas_AddNode( Node.child[ 0 ], Texture, Width, Height );
      if not Assigned( Result ) Then
        Result := atlas_AddNode( Node.child[ 1 ], Texture, Width, Height );
    end else
      begin
        if Assigned( Node.Texture ) Then
          begin
            Result := nil;
            exit;
          end;

        if ( Width > Node.Rect.W ) or ( Height > Node.Rect.H ) Then
          begin
            Result := nil;
            exit;
          end;

        if ( Width = Node.Rect.W ) and ( Height = Node.Rect.H ) Then
          begin
            Result       := Node;
            Node.Texture := Texture;
            dw           := 1 / Texture.Width;
            dh           := 1 / Texture.Height;
            with Node^ do
              begin
                TexCoord[ 0 ].X := Rect.X * dw;
                TexCoord[ 0 ].Y := ( Rect.Y + Rect.H ) * dh;
                TexCoord[ 1 ].X := ( Rect.X + Rect.W ) * dw;
                TexCoord[ 1 ].Y := ( Rect.Y + Rect.H ) * dh;
                TexCoord[ 2 ].X := ( Rect.X + Rect.W ) * dw;
                TexCoord[ 2 ].Y := Rect.Y * dh;
                TexCoord[ 3 ].X := Rect.X * dw;
                TexCoord[ 3 ].Y := Rect.Y * dh;
              end;
            exit;
          end;

        zgl_GetMem( Pointer( Node.child[ 0 ] ), SizeOf( zglTAtlasNode ) );
        zgl_GetMem( Pointer( Node.child[ 1 ] ), SizeOf( zglTAtlasNode ) );
        Node.Leaf := FALSE;

        c1 := Node.child[ 0 ];
        c2 := Node.child[ 1 ];

        dw := Node.Rect.W - Width;
        dh := Node.Rect.H - Height;

        if dw > dh Then
          begin
            c1.Leaf   := TRUE;
            c1.Rect.X := Node.Rect.X;
            c1.Rect.Y := Node.Rect.Y;
            c1.Rect.W := Width;
            c1.Rect.H := Node.Rect.H;

            c2.Leaf   := TRUE;
            c2.Rect.X := Node.Rect.X + Width;
            c2.Rect.Y := Node.Rect.Y;
            c2.Rect.W := Node.Rect.W - Width;
            c2.Rect.H := Node.Rect.H;
          end else
            begin
              c1.Leaf   := TRUE;
              c1.Rect.X := Node.Rect.X;
              c1.Rect.Y := Node.Rect.Y;
              c1.Rect.W := Node.Rect.W;
              c1.Rect.H := Height;

              c2.Leaf   := TRUE;
              c2.Rect.X := Node.Rect.X;
              c2.Rect.Y := Node.Rect.Y + Height;
              c2.Rect.W := Node.Rect.W;
              c2.Rect.H := Node.Rect.H - Height;
            end;

        Result := atlas_AddNode( Node.child[ 0 ], Texture, Width, Height );
      end;
end;

procedure atlas_DelNode( var Node : zglPAtlasNode );
begin
  if not Assigned( Node ) Then exit;

  atlas_DelNode( Node.child[ 0 ] );
  atlas_DelNode( Node.child[ 1 ] );

  FreeMem( Node );
  Node := nil;
end;

procedure atlas_GetFrameCoord( Node : zglPAtlasNode; Frame : Word; var TexCoord : array of zglTPoint2D );
  var
    tX, tY, u, v : Single;
begin
  if not Assigned( Node ) Then exit;

  u := abs( Node.TexCoord[ 0 ].X - Node.TexCoord[ 1 ].X ) / Node.FramesX;
  v := abs( Node.TexCoord[ 1 ].Y - Node.TexCoord[ 2 ].Y ) / Node.FramesY;

  tY := Frame div Node.FramesX;
  tX := Frame - tY * Node.FramesX;
  tY := Node.FramesY - tY;
  if tX = 0 Then
    begin
      tX := Node.FramesX;
      tY := tY + 1;
    end;
  tX := Node.TexCoord[ 0 ].X + ( tX - 1 ) * u;
  tY := Node.TexCoord[ 0 ].Y - ( Node.FramesY - tY ) * v;

  TexCoord[ 0 ].X := tX;
  TexCoord[ 0 ].Y := tY;
  TexCoord[ 1 ].X := tX + u;
  TexCoord[ 1 ].Y := tY;
  TexCoord[ 2 ].X := tX + u;
  TexCoord[ 2 ].Y := tY - v;
  TexCoord[ 3 ].X := tX;
  TexCoord[ 3 ].Y := tY - v;
end;

function atlas_InsertFromTexture( Atlas : zglPAtlas; Texture : zglPTexture ) : zglPAtlasNode;
  var
    pData : Pointer;
begin
  if not Assigned( Texture ) Then exit;

  Result := atlas_AddNode( @Atlas.root, Atlas.Texture, Texture.Width, Texture.Height );
  Result.FramesX := Texture.FramesX;
  Result.FramesY := Texture.FramesY;
  tex_GetData( Texture, pData );
  atals_InsertDataToNode( Result, pData, Round( Texture.Width / Texture.U ), Texture.Width, Texture.Height );

  FreeMem( pData );
end;

function atlas_InsertFromFile( Atlas : zglPAtlas; const FileName : UTF8String; TransparentColor, Flags : LongWord ) : zglPAtlasNode;
  var
    i      : Integer;
    pData  : Pointer;
    tex    : zglTTexture;
    w, h   : Word;
    format : Word;
begin
  Result := nil;
  pData  := nil;

  if not file_Exists( FileName ) Then
    begin
      log_Add( 'Cannot read ' + FileName );
      exit;
    end;

  for i := managerTexture.Count.Formats - 1 downto 0 do
    if u_StrUp( file_GetExtension( FileName ) ) = managerTexture.Formats[ i ].Extension Then
      managerTexture.Formats[ i ].FileLoader( FileName, pData, w, h, format );

  if not Assigned( pData ) Then
    begin
      log_Add( 'Unable to load texture: "' + FileName + '"' );
      exit;
    end;

  tex.Width   := w;
  tex.Height  := h;
  tex.U       := 1;
  tex.V       := 1;
  tex.FramesX := 1;
  tex.FramesY := 1;
  tex.Flags   := Flags;
  tex.Format  := format;
  if ( Flags and TEX_CONVERT_TO_POT > 0 ) Then
    tex.Flags := tex.Flags xor TEX_CONVERT_TO_POT;
  if tex.Format = TEX_FORMAT_RGBA Then
    begin
      if tex.Flags and TEX_CALCULATE_ALPHA > 0 Then
        begin
          tex_CalcTransparent( pData, TransparentColor, w, h );
          tex_CalcAlpha( pData, w, h );
        end else
          tex_CalcTransparent( pData, TransparentColor, w, h );
    end;
  tex_CalcFlags( tex, pData );

  Result := atlas_AddNode( @Atlas.root, Atlas.Texture, tex.Width, tex.Height );
  Result.FramesX := tex.FramesX;
  Result.FramesY := tex.FramesY;
  atals_InsertDataToNode( Result, pData, tex.Width, tex.Width, tex.Height );

  FreeMem( pData );
end;

function atlas_InsertFromMemory( Atlas : zglPAtlas; const Memory : zglTMemory; const Extension : UTF8String; TransparentColor, Flags : LongWord ) : zglPAtlasNode;
  var
    i      : Integer;
    pData  : Pointer;
    tex    : zglTTexture;
    w, h   : Word;
    format : Word;
begin
  Result := nil;
  pData  := nil;

  for i := managerTexture.Count.Formats - 1 downto 0 do
    if u_StrUp( Extension ) = managerTexture.Formats[ i ].Extension Then
      managerTexture.Formats[ i ].MemLoader( Memory, pData, w, h, format );

  if not Assigned( pData ) Then
    begin
      log_Add( 'Unable to load texture: From Memory' );
      exit;
    end;

  tex.Width   := w;
  tex.Height  := h;
  tex.U       := 1;
  tex.V       := 1;
  tex.FramesX := 1;
  tex.FramesY := 1;
  tex.Flags   := Flags;
  tex.Format  := format;
  if ( Flags and TEX_CONVERT_TO_POT > 0 ) Then
    tex.Flags := tex.Flags xor TEX_CONVERT_TO_POT;
  if tex.Format = TEX_FORMAT_RGBA Then
    begin
      if tex.Flags and TEX_CALCULATE_ALPHA > 0 Then
        begin
          tex_CalcTransparent( pData, TransparentColor, w, h );
          tex_CalcAlpha( pData, w, h );
        end else
          tex_CalcTransparent( pData, TransparentColor, w, h );
    end;
  tex_CalcFlags( tex, pData );

  Result := atlas_AddNode( @Atlas.root, Atlas.Texture, tex.Width, tex.Height );
  Result.FramesX := tex.FramesX;
  Result.FramesY := tex.FramesY;
  atals_InsertDataToNode( Result, pData, tex.Width, tex.Width, tex.Height );

  FreeMem( pData );
end;

procedure atals_InsertDataToNode( var Node : zglPAtlasNode; pData : Pointer; RowLength, Width, Height : Word );
begin
  {$IFNDEF USE_GLES}
  glEnable( GL_TEXTURE_2D );
  glPixelStorei( GL_UNPACK_ROW_LENGTH, RowLength );
  glBindTexture( GL_TEXTURE_2D, Node.Texture.ID );
  glTexSubImage2D( GL_TEXTURE_2D, 0, Trunc( Node.Rect.X ), Trunc( Node.Rect.Y ), Width, Height, GL_RGBA, GL_UNSIGNED_BYTE, pData );
  glPixelStorei( GL_UNPACK_ROW_LENGTH, 0 );
  glDisable( GL_TEXTURE_2D );
  {$ENDIF}
end;

end.
