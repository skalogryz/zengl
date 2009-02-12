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
unit zgl_heightmap;

{$I define.inc}

interface
uses
  GL, GLext,
  zgl_const,
  zgl_global_var,
  zgl_mesh_file,
  zgl_types,
  zgl_textures,
  zgl_vbo,
  zgl_math,
  zgl_utils_3d,
  Utils;

function  heightmap_Build( const Texture : zglPTexture; const xScale, yScale, zScale : Single; const xDetail, yDetail : Integer; const Flags : DWORD ) : zglPHeightMap;
procedure heightmap_Draw( const HeightMap : zglPHeightMap );
procedure heightmap_Free( const HeightMap : zglPHeightMap );
function  heightmap_GetPlane( const HeightMap : zglPHeightMap; Position : zglTPoint3D ) : DWORD;
function  heightmap_GetYOffset( const HeightMap : zglPHeightMap; Position : zglTPoint3D ) : Single;

implementation
var
  tTexLevel : Byte;

function heightmap_Build;
  var
    i, j  : Integer;
    x, y  : Integer;
    pData : Pointer;
    pSize : Integer;
begin
  Result := AllocMem( SizeOf( zglTHeightMap ) );
  tex_GetData( Texture, pData, pSize );

  Result.FCount := ( Texture.Width - 1 ) * ( Texture.Height - 1 ) * 2;
  Result.VCount := Texture.Width * Texture.Height;
  Result.TCount := Texture.Width * Texture.Height;
  SetLength( Result.Vertices, Result.VCount );
  SetLength( Result.TexCoords, Result.TCount );
  SetLength( Result.MultiTexCoords, Result.TCount );
  SetLength( Result.Faces, Result.FCount );

  Result.Width  := Texture.Width;
  Result.Height := Texture.Height;
  Result.xScale := xScale;
  Result.zScale := zScale;

  i := 0;
  j := 0;
  for y := 0 to Texture.Height - 1 do
  for x := 0 to Texture.Width - 1 do
    begin
      Result.Vertices[ i ].X := x * xScale;
      Result.Vertices[ i ].Y := PByte( pData + x * pSize + y * Texture.Width * pSize )^ * yScale;
      Result.Vertices[ i ].Z := y * zScale;

      Result.TexCoords[ i ].X := x / Texture.Width;
      Result.TexCoords[ i ].Y := y / Texture.Height;
      Result.MultiTexCoords[ i ].X := Result.TexCoords[ i ].X * xDetail;
      Result.MultiTexCoords[ i ].Y := Result.TexCoords[ i ].Y * yDetail;

      if ( x < Texture.Width - 2 ) and ( y < Texture.Height - 2 ) Then
        begin
          Result.Faces[ j, 0 ] := i + Texture.Width;
          Result.Faces[ j, 1 ] := i + 1;
          Result.Faces[ j, 2 ] := i;
          INC( j );
          Result.Faces[ j, 0 ] := i + Texture.Width + 1;
          Result.Faces[ j, 1 ] := i + 1;
          Result.Faces[ j, 2 ] := i + Texture.Width;
          INC( j );
        end;
      INC( i );
    end;

  Result.Flags := Flags;
  vbo_Check( Result.Flags );

  if ( not Assigned( Result.Normals ) ) and ( Result.Flags and BUILD_SNORMALS > 0 ) Then
    begin
      Result.Flags := Result.Flags or USE_NORMALS;
      SetLength( Result.Normals, Result.VCount );
      BuildSNormals( Result.FCount, Result.VCount, Result.Faces, Result.Vertices, Result.Normals );
    end;

  Result.ICount := Result.FCount * 3;
  if Result.VCount < 65536 Then
    BuildIndices( Result.FCount, Result.Faces, Result.Indices, 2 )
  else
    BuildIndices( Result.FCount, Result.Faces, Result.Indices, 4 );

  if Result.Flags and USE_VBO > 0 Then
    vbo_Build( Result.IBuffer, Result.VBuffer, Result.ICount, Result.VCount,
               Result.Indices,
               Result.Vertices, Result.Normals,
               Result.TexCoords, Result.MultiTexCoords,
               Result.Flags );

  if Result.Flags and BUILD_PLANES > 0 Then
    begin
      Result.PCount := Result.FCount;
      SetLength( Result.Planes, Result.PCount );
      for i := 0 to Result.PCount - 1 do
        Result.Planes[ i ] := plane_Get( Result.Vertices[ Result.Faces[ i, 0 ] ],
                                            Result.Vertices[ Result.Faces[ i, 1 ] ],
                                            Result.Vertices[ Result.Faces[ i, 2 ] ] );
    end;

  FreeMem( pData );
end;

procedure heightmap_Draw;
  var
    i  : Byte;
    PV : Ptr = 0;
    PN : Ptr = 0;
    PT : Ptr = 0;
begin
  if ogl_MaxTexLevels > 0 Then
    tTexLevel := Byte( HeightMap.Flags and USE_MULTITEX1 > 0 ) +
                 Byte( HeightMap.Flags and USE_MULTITEX2 > 0 ) +
                 Byte( HeightMap.Flags and USE_MULTITEX3 > 0 );

  if HeightMap.Flags and USE_VBO > 0 Then
    begin
      PV := 0;
      if HeightMap.Flags and USE_NORMALS > 0 Then PN := PV + HeightMap.VCount * 12;
      if HeightMap.Flags and USE_TEXTURE > 0 Then PT := PV + PN + HeightMap.VCount * 12;
      glBindBufferARB( GL_ARRAY_BUFFER_ARB, HeightMap.VBuffer );
      glBindBufferARB( GL_ELEMENT_ARRAY_BUFFER_ARB, HeightMap.IBuffer );
    end else
      begin
        PV := Ptr( @HeightMap.Vertices[ 0 ] );
        if HeightMap.Flags and USE_NORMALS > 0 Then PN := Ptr( @HeightMap.Normals[ 0 ] );
        if HeightMap.Flags and USE_TEXTURE > 0 Then PT := Ptr( @HeightMap.TexCoords[ 0 ] );
      end;

  if ( HeightMap.Flags and USE_NORMALS > 0 ) and ( glIsEnabled( GL_LIGHTING ) = GL_TRUE ) Then
    begin
      glEnableClientState( GL_NORMAL_ARRAY );
      glNormalPointer( GL_FLOAT, 0, Pointer( PN ) );
    end;
  if HeightMap.Flags and USE_TEXTURE > 0 Then
    begin
      glClientActiveTextureARB( GL_TEXTURE0_ARB );
      glEnableClientState( GL_TEXTURE_COORD_ARRAY );
      glTexCoordPointer( 2, GL_FLOAT, 0, Pointer( PT ) );

      if ogl_MaxTexLevels > 0 Then
        for i := 1 to tTexLevel do
          begin
            glClientActiveTextureARB( GL_TEXTURE0_ARB + i );
            glEnableClientState( GL_TEXTURE_COORD_ARRAY );
            if HeightMap.Flags and USE_VBO > 0 Then
              glTexCoordPointer( 2, GL_FLOAT, 0, Pointer( PT + HeightMap.VCount * i * 8  ) )
            else
              glTexCoordPointer( 2, GL_FLOAT, 0, @HeightMap.MultiTexCoords[ 0 + HeightMap.VCount * ( i - 1 ) ] );
          end;
    end;

  glEnableClientState( GL_VERTEX_ARRAY );
  glVertexPointer( 3, GL_FLOAT, 0, Pointer( PV ) );

  if HeightMap.Flags and USE_VBO > 0 Then
    begin
      if HeightMap.VCount < 65536 Then
        glDrawElements( GL_TRIANGLES, HeightMap.ICount, GL_UNSIGNED_SHORT, nil )
      else
        glDrawElements( GL_TRIANGLES, HeightMap.ICount, GL_UNSIGNED_INT, nil );
    end else
      if HeightMap.VCount < 65536 Then
        glDrawElements( GL_TRIANGLES, HeightMap.ICount, GL_UNSIGNED_SHORT, HeightMap.Indices )
      else
        glDrawElements( GL_TRIANGLES, HeightMap.ICount, GL_UNSIGNED_INT, HeightMap.Indices );

  glDisableClientState( GL_VERTEX_ARRAY );
  glDisableClientState( GL_NORMAL_ARRAY );
  glDisableClientState( GL_TEXTURE_COORD_ARRAY );
  if ogl_CanVBO Then
    begin
      glBindBufferARB( GL_ARRAY_BUFFER_ARB, 0 );
      glBindBufferARB( GL_ELEMENT_ARRAY_BUFFER_ARB, 0 );
    end;
end;

procedure heightmap_Free;
begin
  if HeightMap.Flags and USE_VBO > 0 Then
    vbo_Free( HeightMap.IBuffer, HeightMap.VBuffer, HeightMap.ICount, HeightMap.VCount );
  SetLength( HeightMap.Vertices, 0 );
  SetLength( HeightMap.Normals, 0 );
  SetLength( HeightMap.TexCoords, 0 );
  SetLength( HeightMap.MultiTexCoords, 0 );
  SetLength( HeightMap.Faces, 0 );
  FreeMem( HeightMap.Indices );
  FreeMem( HeightMap );
end;

function heightmap_GetPlane;
begin
  if Position.X / HeightMap.xScale > HeightMap.Width - 2 Then
    Position.X := ( HeightMap.Width - 3 ) * HeightMap.xScale;
  if Position.X / HeightMap.xScale < 0 Then
    Position.X := 0;
  if Position.Z / HeightMap.zScale > HeightMap.Height - 2 Then
    Position.Z := ( HeightMap.Height - 3 ) * HeightMap.zScale;
  if Position.Z / HeightMap.zScale < 0 Then
    Position.Z := 0;

  Result := ( trunc( Position.X / HeightMap.xScale ) + trunc( Position.Z / HeightMap.zScale ) * ( HeightMap.Width - 2 ) ) * 2;
  with HeightMap.Vertices[ HeightMap.Faces[ Result, 0 ] ] do
    if ( HeightMap.Vertices[ HeightMap.Faces[ Result, 1 ] ].X - X ) * ( Position.Z - Z ) -
       ( HeightMap.Vertices[ HeightMap.Faces[ Result, 1 ] ].Z - Z ) * ( Position.X - X ) >= 0 Then INC( Result );
end;

function heightmap_GetYOffset;
  var
    p : DWORD;
begin
  if Position.X / HeightMap.xScale > HeightMap.Width - 2 Then
    Position.X := ( HeightMap.Width - 3 ) * HeightMap.xScale;
  if Position.X / HeightMap.xScale < 0 Then
    Position.X := 0;
  if Position.Z / HeightMap.zScale > HeightMap.Height - 2 Then
    Position.Z := ( HeightMap.Height - 3 ) * HeightMap.zScale;
  if Position.Z / HeightMap.zScale < 0 Then
    Position.Z := 0;

  p := ( trunc( Position.X / HeightMap.xScale ) + trunc( Position.Z / HeightMap.zScale ) * ( HeightMap.Width - 2 ) ) * 2;
  with HeightMap.Vertices[ HeightMap.Faces[ p, 0 ] ] do
    if ( HeightMap.Vertices[ HeightMap.Faces[ p, 1 ] ].X - X ) * ( Position.Z - Z ) -
       ( HeightMap.Vertices[ HeightMap.Faces[ p, 1 ] ].Z - Z ) * ( Position.X - X ) >= 0 Then INC( p );

  Result := ( - HeightMap.Planes[ p ].Normal.X * Position.X -
                HeightMap.Planes[ p ].Normal.Z * Position.Z - HeightMap.Planes[ p ].D ) / HeightMap.Planes[ p ].Normal.Y;
end;

end.
