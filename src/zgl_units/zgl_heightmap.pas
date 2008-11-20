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

procedure heightmap_Build( var HeightMap : zglPHeightMap; Texture : zglPTexture; xScale, yScale, zScale : Single; xDetail, yDetail : Integer; Flags : DWORD ); extdecl;
procedure heightmap_Draw( HeightMap : zglPHeightMap ); extdecl;
procedure heightmap_Free( var HeightMap : zglPHeightMap ); extdecl;
function  heightmap_GetPlane( HeightMap : zglPHeightMap; Position : zglTPoint3D ) : DWORD; extdecl;
function  heightmap_GetYOffset( HeightMap : zglPHeightMap; Position : zglTPoint3D ) : Single; extdecl;

implementation
var
  tTexLevel : Byte;

procedure heightmap_Build;
  var
    i, j  : Integer;
    x, y  : Integer;
    pData : Pointer;
    pSize : Integer;
begin
  HeightMap := AllocMem( SizeOf( zglTHeightMap ) );
  tex_GetData( Texture, pData, pSize );

  HeightMap.FCount := ( Texture.Width - 1 ) * ( Texture.Height - 1 ) * 2;
  HeightMap.VCount := Texture.Width * Texture.Height;
  HeightMap.TCount := Texture.Width * Texture.Height;
  SetLength( HeightMap.Vertices, HeightMap.VCount );
  SetLength( HeightMap.TexCoords, HeightMap.TCount );
  SetLength( HeightMap.MultiTexCoords, HeightMap.TCount );
  SetLength( HeightMap.Faces, HeightMap.FCount );

  HeightMap.Width  := Texture.Width;
  HeightMap.Height := Texture.Height;
  HeightMap.xScale := xScale;
  HeightMap.zScale := zScale;

  i := 0;
  j := 0;
  for y := 0 to Texture.Height - 1 do
  for x := 0 to Texture.Width - 1 do
    begin
      HeightMap.Vertices[ i ].X := x * xScale;
      HeightMap.Vertices[ i ].Y := PByte( pData + x * pSize + y * Texture.Width * pSize )^ * yScale;
      HeightMap.Vertices[ i ].Z := y * zScale;

      HeightMap.TexCoords[ i ].X := x / Texture.Width;
      HeightMap.TexCoords[ i ].Y := y / Texture.Height;
      HeightMap.MultiTexCoords[ i ].X := HeightMap.TexCoords[ i ].X * xDetail;
      HeightMap.MultiTexCoords[ i ].Y := HeightMap.TexCoords[ i ].Y * yDetail;

      if ( x < Texture.Width - 2 ) and ( y < Texture.Height - 2 ) Then
        begin
          HeightMap.Faces[ j, 0 ] := i + Texture.Width;
          HeightMap.Faces[ j, 1 ] := i + 1;
          HeightMap.Faces[ j, 2 ] := i;
          INC( j );
          HeightMap.Faces[ j, 0 ] := i + Texture.Width + 1;
          HeightMap.Faces[ j, 1 ] := i + 1;
          HeightMap.Faces[ j, 2 ] := i + Texture.Width;
          INC( j );
        end;
      INC( i );
    end;

  HeightMap.Flags := Flags;
  vbo_Check( HeightMap.Flags );

  if ( not Assigned( HeightMap.Normals ) ) and ( HeightMap.Flags and BUILD_SNORMALS > 0 ) Then
    begin
      HeightMap.Flags := HeightMap.Flags or USE_NORMALS;
      SetLength( HeightMap.Normals, HeightMap.VCount );
      BuildSNormals( HeightMap.FCount, HeightMap.VCount, HeightMap.Faces, HeightMap.Vertices, HeightMap.Normals );
    end;

  HeightMap.ICount := HeightMap.FCount * 3;
  if HeightMap.VCount < 65536 Then
    begin
      HeightMap.Indices := AllocMem( HeightMap.FCount * 2 * 3 );
      BuildIndices( HeightMap.FCount, HeightMap.Faces, HeightMap.Indices, 2 );
    end else
      begin
        HeightMap.Indices := AllocMem( HeightMap.FCount * 4 * 3 );
        BuildIndices( HeightMap.FCount, HeightMap.Faces, HeightMap.Indices, 4 );
      end;

  if HeightMap.Flags and USE_VBO > 0 Then
    vbo_Build( HeightMap.IBuffer, HeightMap.VBuffer, HeightMap.ICount, HeightMap.VCount,
               HeightMap.Indices, 
               HeightMap.Vertices, HeightMap.Normals,
               HeightMap.TexCoords, HeightMap.MultiTexCoords,
               HeightMap.Flags );

  if HeightMap.Flags and BUILD_PLANES > 0 Then
    begin
      HeightMap.PCount := HeightMap.FCount;
      SetLength( HeightMap.Planes, HeightMap.PCount );
      for i := 0 to HeightMap.PCount - 1 do
        HeightMap.Planes[ i ] := plane_Get( HeightMap.Vertices[ HeightMap.Faces[ i, 0 ] ],
                                            HeightMap.Vertices[ HeightMap.Faces[ i, 1 ] ],
                                            HeightMap.Vertices[ HeightMap.Faces[ i, 2 ] ] );
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
