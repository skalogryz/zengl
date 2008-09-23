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
unit zgl_utils_3d;

{$I define.inc}

interface
uses
  zgl_types,
  zgl_math;

procedure BuildIndices( FCount : DWORD; var Faces : array of zglTFace; Indices : Pointer; Size : Byte );
procedure BuildFNormals( FCount : DWORD; Faces : array of zglTFace; var Vertices, Normals : array of zglTPoint3D );
procedure BuildSNormals( FCount : DWORD; Faces : array of zglTFace; var Vertices, Normals : array of zglTPoint3D );

implementation

procedure BuildIndices;
  var
    i, j : DWORD;
begin
  if Size = 2 Then
    begin
      for i := 0 to FCount - 1 do
        for j := 0 to 2 do
          PWORD( Indices + ( i * 3 ) * Size + j * Size )^ := Faces[ i, j ];
    end else
      if Size = 4 Then
        begin
          for i := 0 to FCount - 1 do
            for j := 0 to 2 do
              PDWORD( Indices + ( i * 3 ) * Size + j * Size )^ := Faces[ i, j ];
        end;
end;

procedure BuildFNormals;
  var
    i      : DWORD;
    normal : zglTPoint3D;
begin
  for i := 0 to FCount - 1 do
    begin
      normal := tri_GetNormal( @Vertices[ Faces[ i, 0 ] ], @Vertices[ Faces[ i, 1 ] ], @Vertices[ Faces[ i, 2 ] ] );
      Normals[ Faces[ i, 0 ] ] := normal;
      Normals[ Faces[ i, 1 ] ] := normal;
      Normals[ Faces[ i, 2 ] ] := normal;
    end;
end;

procedure BuildSNormals;
  var
    i, j        : DWORD;
    v1, v2      : zglTPoint3D;
    TNormals    : array of zglTPoint3D;
    vPoly       : array[ 0..2 ] of zglTPoint3D;
    Shared      : Integer;
    vSum, vZero : zglTPoint3D;
begin
  vZero := vector_Get( 0, 0, 0 );
  vSum  := vector_Get( 0, 0, 0 );

  SetLength( TNormals, FCount );

  for i := 0 to FCount - 1 do
    begin
      vPoly[ 0 ] := Vertices[ Faces[ i, 0 ] ];
      vPoly[ 1 ] := Vertices[ Faces[ i, 1 ] ];
      vPoly[ 2 ] := Vertices[ Faces[ i, 2 ] ];

      v1 := vector_Sub( vPoly[ 0 ], vPoly[ 2 ] );
      v2 := vector_Sub( vPoly[ 2 ], vPoly[ 1 ] );

      TNormals[ i ] := vector_Cross( v1, v2 );
    end;

  Shared  := 0;
  for i := 0 to length( Vertices ) - 1 do
    begin
      for j := 0 to FCount - 1 do
        if ( Faces[ j, 0 ] = i ) or ( Faces[ j, 1 ] = i ) or ( Faces[ j, 2 ] = i ) Then
          begin
            vSum := vector_Add( vSum, TNormals[ j ] );
            INC( Shared );
          end;

      Normals[ i ] := vector_DivV( vSum, -Shared );
      Normals[ i ] := vector_Normalize( Normals[ i ] );
      vSum   := vZero;
      Shared := 0;
    end;

  SetLength( TNormals, 0 );
end;

end.
