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
unit zgl_octree;

{$I define.inc}

interface

uses
  GL, GLExt,
  zgl_const,
  zgl_types,
  zgl_global_var,
  zgl_object_3d,
  zgl_frustum,
  zgl_vbo,
  zgl_math,
  zgl_utils_3d;
  
procedure octree_Build( Octree : zglPOctree; MaxFacesPerNode, Flags : DWORD ); extdecl;
procedure octree_Free( Octree : zglPOctree ); extdecl;
procedure octree_SortFacesByTexture( Octree : zglPOctree; var Faces : array of DWORD; left, right : DWORD ); extdecl;

procedure octree_AddNode( Octree : zglPOctree; Node : zglPNode; FCount : DWORD; var Faces : array of DWORD ); extdecl;
procedure octree_AddNodeInside( Octree : zglPOctree; Node : zglPNode;
                                pFCount : DWORD; var pFaces : array of DWORD;
                                bFCount : DWORD; var bFaces : array of Boolean;
                                ID : Byte); extdecl;
procedure octree_DelNode( node : zglPNode ); extdecl;
procedure octree_MoveVerticesToNode( Octree : zglPOctree; Node : zglPNode; FCount : DWORD; var Faces : array of DWORD ); extdecl;
function octree_NodeCenter( Node : zglPNode; ID : Byte ): zglTPoint3D; extdecl;
function octree_FaceInNode( min, max, v1, v2, v3 : zglTPoint3D ) : Boolean; extdecl;

procedure octree_Draw( Octree : zglPOctree; Frustum : zglPFrustum ); extdecl;
procedure octree_DrawDebug( Octree : zglPOctree; Frustum : zglPFrustum ); extdecl;
procedure octree_DrawNode( Octree : zglPOctree; Node : zglPNode; Frustum : zglPFrustum ); extdecl;
procedure octree_DrawDFaces( Octree : zglPOctree; Node : zglPNode; Frustum : zglPFrustum ); extdecl;
function octree_FaceAlreadyDraw( Octree : zglPOctree; face_index : DWORD ) : Boolean; extdecl;

implementation

var
  tbuildVBO        : Boolean;
  tMaxFacesPerNode : DWORD;
  tDebugFirst      : Boolean;
  tDebugNode       : zglPNode;
  tTexLevel        : DWORD;
  tLastVCount      : DWORD;

function GetRenderDataSize( Octree : zglPOctree; FCount : DWORD; var Faces : array of DWORD ) : DWORD;
  var
    i : DWORD;
begin
  Result := 1;
  
  if FCount - 2 < 0 Then exit;
  
  for i := 0 to FCount - 2 do
    if Octree.Textures[ Faces[ i ] ] <> Octree.Textures[ Faces[ i + 1 ] ] Then INC( Result );
end;

procedure octree_AddIndices( Octree : zglPOctree; Indices : Pointer; Count : DWORD; Size : Byte );
begin
  if not Assigned( Octree.Indices ) Then
    Octree.Indices := AllocMem( Count * Size )
  else
    Octree.Indices := ReAllocMem( Octree.Indices, ( Octree.ICount + Count ) * Size );

  Move( Indices^, ( Octree.Indices + Octree.ICount * Size )^, Count * Size );
  INC( Octree.ICount, Count );
end;

procedure BuildRenderData( Octree : zglPOctree;
                           RDSize : DWORD; var RenderData : array of zglTRenderData;
                           FCount : DWORD; var Faces      : array of DWORD );
  var
    i, j, t, z : DWORD;
    rdata_id   : DWORD;
    face_id    : DWORD;
begin
  i := 0;
  z := 0;

  if RDSize = 1 Then
    begin
      RenderData[ 0 ].Texture := Octree.Textures[ Faces[ 0 ] ];
      RenderData[ 0 ].ICount  := FCount * 3;
      if tLastVCount < 65536 Then
        begin
          RenderData[ 0 ].Indices := AllocMem( RenderData[ 0 ].ICount * 2 );
          RenderData[ 0 ].IBType  := GL_UNSIGNED_SHORT;
        end else
          begin
            RenderData[ 0 ].Indices := AllocMem( RenderData[ 0 ].ICount * 4 );
            RenderData[ 0 ].IBType  := GL_UNSIGNED_INT;
          end;
      
      j := 0;
      if tLastVCount < 65536 Then
        begin
          for t := 0 to FCount - 1 do
            begin
              face_id := Faces[ t ];

              PWORD( RenderData[ 0 ].Indices + ( j     ) * 2 )^ := Octree.Faces[ face_id, 0 ];
              PWORD( RenderData[ 0 ].Indices + ( j + 1 ) * 2 )^ := Octree.Faces[ face_id, 1 ];
              PWORD( RenderData[ 0 ].Indices + ( j + 2 ) * 2 )^ := Octree.Faces[ face_id, 2 ];

              INC( j, 3 );
            end;
        end else
          for t := 0 to FCount - 1 do
            begin
              face_id := Faces[ t ];

              PDWORD( RenderData[ 0 ].Indices + ( j     ) * 4 )^ := Octree.Faces[ face_id, 0 ];
              PDWORD( RenderData[ 0 ].Indices + ( j + 1 ) * 4 )^ := Octree.Faces[ face_id, 1 ];
              PDWORD( RenderData[ 0 ].Indices + ( j + 2 ) * 4 )^ := Octree.Faces[ face_id, 2 ];

              INC( j, 3 );
            end;
    end else
      for rdata_id := 0 to RDSize - 1 do
        begin
          while ( i < FCount - 1 ) and ( Octree.Textures[ Faces[ i ] ] = Octree.Textures[ Faces[ i + 1 ] ] ) do INC( i );

          RenderData[ rdata_id ].Texture := Octree.Textures[ Faces[ i ] ];
          RenderData[ rdata_id ].ICount  := ( i - z + 1 ) * 3;
          if tLastVCount < 65536 Then
            begin
              RenderData[ rdata_id ].Indices := AllocMem( RenderData[ rdata_id ].ICount * 2 );
              RenderData[ rdata_id ].IBType  := GL_UNSIGNED_SHORT;
            end else
              begin
                RenderData[ rdata_id ].Indices := AllocMem( RenderData[ rdata_id ].ICount * 4 );
                RenderData[ rdata_id ].IBType  := GL_UNSIGNED_INT;
              end;

          j := 0;
          if tLastVCount < 65536 Then
            begin
              for t := z to i do
                begin
                  face_id := Faces[ t ];

                  PWORD( RenderData[ rdata_id ].Indices + ( j     ) * 2 )^ := Octree.Faces[ face_id, 0 ];
                  PWORD( RenderData[ rdata_id ].Indices + ( j + 1 ) * 2 )^ := Octree.Faces[ face_id, 1 ];
                  PWORD( RenderData[ rdata_id ].Indices + ( j + 2 ) * 2 )^ := Octree.Faces[ face_id, 2 ];

                  INC( j, 3 );
                end;
            end else
              for t := z to i do
                begin
                  face_id := Faces[ t ];

                  PDWORD( RenderData[ rdata_id ].Indices + ( j     ) * 4 )^ := Octree.Faces[ face_id, 0 ];
                  PDWORD( RenderData[ rdata_id ].Indices + ( j + 1 ) * 4 )^ := Octree.Faces[ face_id, 1 ];
                  PDWORD( RenderData[ rdata_id ].Indices + ( j + 2 ) * 4 )^ := Octree.Faces[ face_id, 2 ];

                  INC( j, 3 );
                end;
                
          INC( i );
          z := i;
        end;
  if tbuildVBO Then
    if Octree.Flags and BUILD_VBO > 0 Then
      begin
        for i := 0 to RDSize - 1 do
          begin
            if tLastVCount < 65536 Then
              begin
                t := Octree.ICount * 2;
                octree_AddIndices( Octree, RenderData[ i ].Indices, RenderData[ i ].ICount, 2 );
                FreeMem( RenderData[ i ].Indices );
                RenderData[ i ].Indices := Pointer( t );
              end else
                begin
                  t := Octree.ICount * 4;
                  octree_AddIndices( Octree, RenderData[ i ].Indices, RenderData[ i ].ICount, 4 );
                  FreeMem( RenderData[ i ].Indices ); 
                  RenderData[ i ].Indices := Pointer( t );
                end;
          end;
      end else
        for i := 0 to RDSize - 1 do
          begin
            if tLastVCount < 65536 Then
              begin
                t := Octree.ICount * 2;
                octree_AddIndices( Octree, RenderData[ i ].Indices, RenderData[ i ].ICount, 2 );
                FreeMem( RenderData[ i ].Indices );
                RenderData[ i ].Indices := Pointer( Octree.Indices + t );
              end else
                begin
                  t := Octree.ICount * 4;
                  octree_AddIndices( Octree, RenderData[ i ].Indices, RenderData[ i ].ICount, 4 );
                  FreeMem( RenderData[ i ].Indices ); 
                  RenderData[ i ].Indices := Pointer( Octree.Indices + t );
                end;
          end;
end;

{------------------------------------------------------------------------------}
{------------------------------- zgl_octree.pp --------------------------------}
{------------------------------------------------------------------------------}
procedure octree_Build;
  var
    i     : DWORD;
    Faces : array of DWORD;
    max   : zglTPoint3D;
    min   : zglTPoint3D;
    v     : zglPPoint3D;
begin
  tLastVCount := Octree.VCount;
  tMaxFacesPerNode := MaxFacesPerNode;
  
  if Flags and USE_MULTITEX3 > 0 Then
    begin
      Flags := Flags or USE_MULTITEX2;
      Flags := Flags or USE_MULTITEX1;
    end else
      if Flags and USE_MULTITEX2 > 0 Then
        Flags := Flags or USE_MULTITEX1;
  Octree.Flags := Flags;
  
  tTexLevel := Byte( Octree.Flags and USE_MULTITEX1 > 0 ) +
               Byte( Octree.Flags and USE_MULTITEX2 > 0 ) +
               Byte( Octree.Flags and USE_MULTITEX3 > 0 );

  SetLength( Faces, Octree.FCount );
  for i := 0 to Octree.FCount - 1 do Faces[ i ] := i;

  Octree.MainNode := AllocMem( SizeOf( zglTNode ) );
  FillChar( Octree.MainNode^, SizeOf( zglTNode ), 0 );

  max := Octree.Vertices[ 0 ];
  min := Octree.Vertices[ 0 ];
  for i := 0 to Octree.VCount - 1 do
    begin
      v := @Octree.Vertices[ i ];

      if v.x > max.x Then max.x := v.x;
      if v.y > max.y Then max.y := v.y;
      if v.z > max.z Then max.z := v.z;

      if v.x < min.x Then min.x := v.x;
      if v.y < min.y Then min.y := v.y;
      if v.z < min.z Then min.z := v.z;
    end;
  Octree.MainNode.Cube.Position.X := ( max.x + min.x ) / 2;
  Octree.MainNode.Cube.Position.Y := ( max.y + min.y ) / 2;
  Octree.MainNode.Cube.Position.Z := ( max.z + min.z ) / 2;

  Octree.MainNode.Cube.Size := vector_Sub( max, min );
  Octree.MainNode.Cube.Size.Y := Octree.MainNode.Cube.Size.X;
  Octree.MainNode.Cube.Size.Z := Octree.MainNode.Cube.Size.X;
  if max.y - min.y > Octree.MainNode.Cube.Size.X Then
    begin
      Octree.MainNode.Cube.Size.X := max.y - min.y;
      Octree.MainNode.Cube.Size.Y := max.y - min.y;
      Octree.MainNode.Cube.Size.Z := max.y - min.y;
    end;
  if max.z - min.z > Octree.MainNode.Cube.Size.X Then
    begin
      Octree.MainNode.Cube.Size.X := max.z - min.z;
      Octree.MainNode.Cube.Size.Y := max.z - min.z;
      Octree.MainNode.Cube.Size.Z := max.z - min.z;
    end;

  if not Assigned( Octree.Normals ) Then
    begin
      if Octree.Flags and BUILD_FNORMALS > 0 Then
        begin
          Octree.Flags := Octree.Flags or USE_NORMALS;
          SetLength( Octree.Normals, Octree.VCount );
          BuildFNormals( Octree.FCount, Octree.Faces, Octree.Vertices, Octree.Normals );
        end else
      if Octree.Flags and BUILD_SNORMALS > 0 Then
        begin
          Octree.Flags := Octree.Flags or USE_NORMALS;
          SetLength( Octree.Normals, Octree.VCount );
          BuildSNormals( Octree.FCount, Octree.Faces, Octree.Vertices, Octree.Normals );
        end;
    end;

  vbo_Check( Octree.Flags );
    
  if Octree.Flags and BUILD_PLANES > 0 Then
    begin
      SetLength( Octree.Planes, Octree.FCount );
      for i := 0 to Octree.FCount - 1 do
        Octree.Planes[ i ] := plane_Get( Octree.Vertices[ Octree.Faces[ i, 0 ] ],
                                         Octree.Vertices[ Octree.Faces[ i, 1 ] ],
                                         Octree.Vertices[ Octree.Faces[ i, 2 ] ] );
    end;

  if Octree.Flags and BUILD_VBO > 0 Then tbuildVBO := TRUE;
    
  octree_AddNode( Octree, Octree.MainNode, Octree.FCount, Faces );
  SetLength( Faces, 0 );

  if Octree.Flags and BUILD_VBO > 0 Then
    begin
      vbo_Build( Octree.IBuffer, Octree.VBuffer, Octree.ICount, Octree.VCount,
                 Octree.Indices, 
                 Octree.Vertices, Octree.Normals,
                 Octree.TexCoords, Octree.MultiTexCoords,
                 Octree.Flags );
    end;

  SetLength( Octree.r_DFacesAlready, Octree.r_DFacesACount );
  SetLength( Octree.DFaces,          Octree.MaxDFaces   );
  tbuildVBO := FALSE;
end;

procedure octree_Free;
begin
  SetLength( Octree.Vertices,  0 );
  SetLength( Octree.TexCoords, 0 );
  SetLength( Octree.Faces,     0 );

  SetLength( Octree.DFaces, 0 );
  SetLength( Octree.r_DFacesAlready,  0 );

  if Assigned( Octree.MainNode ) Then
    octree_DelNode( Octree.MainNode );
    
  if Octree.Flags and BUILD_VBO > 0 Then
    vbo_Free( Octree.IBuffer, Octree.VBuffer, Octree.VCount, Octree.ICount );
end;

procedure octree_SortFacesByTexture;
  var
    i, j    : Integer;
    c, temp : DWORD;
begin
  i := left;
  j := right;
  c := Faces[ ( i + j ) div 2 ];

  repeat
    while ( Octree.Textures[ Faces[ i ] ] < Octree.Textures[ c ] ) and ( i < right ) do INC( i );
    while ( Octree.Textures[ c ] < Octree.Textures[ Faces[ j ] ] ) and ( j > left  ) do DEC( j );

    if i <= j Then
      begin
        if Octree.Textures[ Faces[ i ] ] <> Octree.Textures[ Faces[ j ] ] Then
          begin
            temp       := Faces[ i ];
            Faces[ i ] := Faces[ j ];
            Faces[ j ] := temp;
          end;

        INC( i );
        DEC( j );
      end;
  until i > j;

  if left < j  Then octree_SortFacesByTexture( Octree, Faces, left, j );
  if i < right Then octree_SortFacesByTexture( OCtree, Faces, i, right );
end;

procedure octree_AddNode;
  var
    i, j    : DWORD;
    ID      : Byte;

    bFCount : array[ 0..7 ] of DWORD;
    bFaces  : array[ 0..7 ] of array of Boolean;
    vertex  : zglPPoint3D;
begin
  if FCount <= tMaxFacesPerNode Then
     octree_MoveVerticesToNode( Octree, Node, FCount, Faces )
  else
    begin
      Node.NInside := TRUE;

      for ID := 0 to 7 do
        begin
          SetLength( bFaces[ ID ], FCount );
          bFCount[ ID ] := 0;
        end;

      for i := 0 to FCount - 1 do
        for j := 0 to 2 do
          begin
            vertex := @Octree.Vertices[ Octree.Faces[ Faces[ i ], j ] ];

            // top
            if vertex.y >= Node.Cube.Position.Y Then
              begin
                if vertex.x <= Node.Cube.Position.X Then
                  begin
                    if vertex.z >= Node.Cube.Position.Z Then
                      bFaces[ 0, i ] := TRUE // left_front
                    else
                      bFaces[ 1, i ] := TRUE // left_back
                  end else
                    begin
                      if vertex.x >= Node.Cube.Position.X Then
                        begin
                          if vertex.z <= Node.Cube.Position.Z Then
                            bFaces[ 2, i ] := TRUE  // right_back
                          else
                            bFaces[ 3, i ] := TRUE; // right_front
                        end;
                    end;
              end else
                begin
                  // bottom
                  if vertex.y < Node.Cube.Position.Y Then
                    begin
                      if vertex.x <= Node.Cube.Position.X Then
                        begin
                          if vertex.z >= Node.Cube.Position.Z Then
                            bFaces[ 4, i ] := TRUE // left_front
                          else
                            bFaces[ 5, i ] := TRUE // left_back
                        end else
                          begin
                            if vertex.x >= Node.Cube.Position.X Then
                              begin
                                if vertex.z <= Node.Cube.Position.Z Then
                                  bFaces[ 6, i ] := TRUE  // right_back
                                else
                                  bFaces[ 7, i ] := TRUE; // right_front
                              end;
                          end;
                    end;
                end;
          end;
          
      for ID := 0 to 7 do
        for i := 0 to FCount - 1 do
          if bFaces[ ID, i ] Then
            INC( bFCount[ ID ] );

      for ID := 0 to 7 do
        begin
          if bFCount[ ID ] > 0 Then
            octree_AddNodeInside( Octree, Node, FCount, Faces, bFCount[ ID ], bFaces[ ID ], ID );

          SetLength( bFaces[ ID ], 0 );
        end;
    end;
end;

procedure octree_AddNodeInside;
  var
    i, j  : DWORD;
    Faces : array of DWORD;
begin
  SetLength( Faces, bFCount );

  j := 0;
  for i := 0 to pFCount - 1 do
    if bFaces[ i ] Then
      begin
        Faces[ j ] := pFaces[ i ];
        INC( j );
      end;

  Node.SubNodes[ ID ] := AllocMem( SizeOf( zglTNode ) );
  FillChar( Node.SubNodes[ ID ]^, SizeOf( zglTNode ), 0 );

  Node.SubNodes[ ID ].Cube.Position := Octree_NodeCenter( Node, ID );
  Node.SubNodes[ ID ].Cube.Size.X   := Node.Cube.Size.X / 2;
  Node.SubNodes[ ID ].Cube.Size.Y   := Node.Cube.Size.Y / 2;
  Node.SubNodes[ ID ].Cube.Size.Z   := Node.Cube.Size.Z / 2;

  octree_AddNode( Octree, Node.SubNodes[ ID ], bFCount, Faces );
  SetLength( Faces, 0 );
end;

procedure octree_DelNode;
  var
    i : DWORD;
begin
  if Node.NInside Then
    for i := 0 to 7 do
      if Assigned( Node.SubNodes[ i ] ) Then
        octree_DelNode( Node.SubNodes[ i ] );

  SetLength( Node.RenderData, 0 );
  SetLength( Node.DFaces, 0 );
  SetLength( Node.Planes, 0 );

  FreeMem( Node );
end;

procedure octree_MoveVerticesToNode;
  var
    i          : DWORD;

    IFCount    : DWORD;
    IFaces     : array of DWORD;
    DFCount    : DWORD;
    DFaces     : array of DWORD;

    min, max   : zglTPoint3D;
    v1, v2, v3 : zglTPoint3D;
begin
  Node.NInside := FALSE;
  SetLength( IFaces, FCount );
  SetLength( DFaces, FCount );

  min := vector_Sub( Node.Cube.Position, Node.Cube.Size );
  max := vector_Add( Node.Cube.Position, Node.Cube.Size );

  IFCount := 0;
  DFCount := 0;
  for i := 0 to FCount - 1 do
    begin
      v1 := Octree.Vertices[ Octree.Faces[ Faces[ i ], 0 ] ];
      v2 := Octree.Vertices[ Octree.Faces[ Faces[ i ], 1 ] ];
      v3 := Octree.Vertices[ Octree.Faces[ Faces[ i ], 2 ] ];

      if octree_FaceInNode( min, max, v1, v2, v3 ) Then
        begin
          IFaces[ IFCount ] := Faces[ i ];
          INC( IFCount );
        end else
          begin
            DFaces[ DFCount ] := Faces[ i ];
            INC( DFCount );
          end;
    end;

  if IFCount = 0 Then
    Node.RDSize := 0
  else
    begin
      SetLength( IFaces, IFCount );
      octree_SortFacesByTexture( Octree, IFaces, 0, IFCount - 1 );

      Node.RDSize := GetRenderDataSize( Octree, IFCount, IFaces );
      SetLength( Node.RenderData, Node.RDSize );
      BuildRenderData( Octree, Node.RDSize, Node.RenderData, IFCount, IFaces );
      
      if Octree.Flags and BUILD_PLANES > 0 Then
        begin
          Node.PCount := IFCount + DFCount;
          SetLength( Node.Planes, Node.PCount );
          if IFCount > 0 Then
            for i := 0 to IFCount - 1 do
              Node.Planes[ i ] := IFaces[ i ];
          if DFCount > 0 Then
            for i := 0 to DFCount - 1 do
              Node.Planes[ IFCount + i ] := DFaces[ i ];
        end;
    end;

  if DFCount = 0 Then
    Node.DFCount := 0
  else
    begin
      SetLength( DFaces, DFCount );
      octree_SortFacesByTexture( Octree, DFaces, 0, DFCount - 1 );

      SetLength( Node.DFaces, DFCount );
      Move( DFaces[ 0 ], Node.DFaces[ 0 ], DFCount * 4 );
      Node.DFCount := DFCount;

      INC( Octree.r_DFacesACount, DFCount );

      if Octree.MaxDFaces < DFCount Then
        Octree.MaxDFaces := DFCount;
    end;

  SetLength( IFaces, 0 );
  SetLength( DFaces, 0 );
end;

function octree_NodeCenter;
  var
    x, y, z, s : Single;
begin
  x := Node.Cube.Position.X;
  y := Node.Cube.Position.Y;
  z := Node.Cube.Position.Z;
  s := Node.Cube.Size.X / 4;

  case ID of
    // top left front
    0 : Result := vector_Get( x - s, y + s, z + s );
    // top left back
    1 : Result := vector_Get( x - s, y + s, z - s );
    // top right back
    2 : Result := vector_Get( x + s, y + s, z - s );
    // top right front
    3 : Result := vector_Get( x + s, y + s, z + s );
    // bottom left front
    4 : Result := vector_Get( x - s, y - s, z + s );
    // bottom left back
    5 : Result := vector_Get( x - s, y - s, z - s );
    // bottom right back
    6 : Result := vector_Get( x + s, y - s, z - s );
    // bottom right front
    7 : Result := vector_Get( x + s, y - s, z + s );
  end;
end;

function octree_FaceInNode;
begin
  Result := FALSE;

  if ( v1.x < min.x ) or ( v1.y < min.y ) or ( v1.z < min.z ) or
     ( v1.x > max.x ) or ( v1.y > max.y ) or ( v1.z > max.z ) Then exit;

  if ( v2.x < min.x ) or ( v2.y < min.y ) or ( v2.z < min.z ) or
     ( v2.x > max.x ) or ( v2.y > max.y ) or ( v2.z > max.z ) Then exit;

  if ( v3.x < min.x ) or ( v3.y < min.y ) or ( v3.z < min.z ) or
     ( v3.x > max.x ) or ( v3.y > max.y ) or ( v3.z > max.z ) Then exit;

  Result := TRUE;
end;

procedure octree_Draw;
  var
    i  : Integer;
    PV : Ptr = 0;
    PN : Ptr = 0;
    PT : Ptr = 0;
begin
  Octree.r_DFacesCount := 0;
  Octree.r_NodeACount  := 0;
  
  if Octree.Flags and BUILD_VBO > 0 Then
    begin
      PV := 0;
      if Octree.Flags and USE_NORMALS > 0 Then PN := PV + Octree.VCount * 12;
      if Octree.Flags and USE_TEXTURE > 0 Then PT := PV + PN + Octree.VCount * 12;
      glBindBufferARB( GL_ARRAY_BUFFER_ARB, Octree.VBuffer );
      glBindBufferARB( GL_ELEMENT_ARRAY_BUFFER_ARB, Octree.IBuffer );
    end else
      begin
        PV := Ptr( @Octree.Vertices[ 0 ] );
        if Octree.Flags and USE_NORMALS > 0 Then PN := Ptr( @Octree.Normals[ 0 ] );
        if Octree.Flags and USE_TEXTURE > 0 Then PT := Ptr( @Octree.TexCoords[ 0 ] );
      end;

  if ( Octree.Flags and USE_NORMALS > 0 ) and ( glIsEnabled( GL_LIGHTING ) = GL_TRUE ) Then
    begin
      glEnableClientState( GL_NORMAL_ARRAY );
      glNormalPointer( GL_FLOAT, 0, Pointer( PN ) );
    end;

  if Octree.Flags and USE_TEXTURE > 0 Then
    begin
      glClientActiveTextureARB( GL_TEXTURE0_ARB );
      glEnableClientState( GL_TEXTURE_COORD_ARRAY );
      glTexCoordPointer( 2, GL_FLOAT, 0, Pointer( PT ) );
  
      if ogl_MaxTexLevels > 0 Then
        begin
          tTexLevel := Byte( Octree.Flags and USE_MULTITEX1 > 0 ) +
                       Byte( Octree.Flags and USE_MULTITEX2 > 0 ) +
                       Byte( Octree.Flags and USE_MULTITEX3 > 0 );
  
          for i := 1 to tTexLevel do
            begin
              glClientActiveTextureARB( GL_TEXTURE0_ARB + i );
              glEnableClientState( GL_TEXTURE_COORD_ARRAY );
              if Octree.Flags and BUILD_VBO > 0 Then
                glTexCoordPointer( 2, GL_FLOAT, 0, Pointer( PT + Octree.VCount * i * 8  ) )
              else
                glTexCoordPointer( 2, GL_FLOAT, 0, @Octree.MultiTexCoords[ 0 + Octree.VCount * ( i - 1 ) ] )
            end;
        end;
    end;

  glEnableClientState( GL_VERTEX_ARRAY );
  glVertexPointer( 3, GL_FLOAT, 0, Pointer( PV ) );

  octree_DrawNode( Octree, Octree.MainNode, Frustum );

  if Octree.Flags and BUILD_VBO > 0 Then
    glBindBufferARB( GL_ELEMENT_ARRAY_BUFFER_ARB, 0 );

  octree_DrawDFaces( Octree, Octree.MainNode, Frustum );
  
  if Octree.Flags and BUILD_VBO > 0 Then
    glBindBufferARB( GL_ARRAY_BUFFER_ARB, 0 );

  glDisableClientState( GL_VERTEX_ARRAY );
  glDisableClientState( GL_NORMAL_ARRAY );
  glDisableClientState( GL_TEXTURE_COORD_ARRAY );
  
  tTexLevel := 0;
end;

procedure octree_DrawDebug;
  var
    i          : DWORD;
    j          : Integer;
    x, y, z, s : Single;
    Node       : zglPNode;
begin
  if not tDebugFirst Then
    begin
      Node        := Octree.MainNode;
      tDebugFirst := TRUE;
      glBegin( GL_LINES );
    end else
      Node := tDebugNode;
      
  x := Node.Cube.Position.X;
  y := Node.Cube.Position.Y;
  z := Node.Cube.Position.Z;
  s := Node.Cube.Size.X / 2;

  if not frustum_CubeIn( Frustum, x, y, z, Node.Cube.Size.X ) Then exit;

  if Node.NInside Then
    for i := 0 to 7 do
      if Assigned( Node.SubNodes[ i ] ) Then
        begin
          tDebugNode := Node.SubNodes[ i ];
          octree_DrawDebug( Octree, Frustum );
        end;
  
  // Т.к. писать в лоб все координаты линий - раздувать библиотеку, то поиздеваюсь немного :)
  for j := -1 to 1 do
    begin
      if i = 0 Then continue;
      glVertex3f( x - s * j, y - s, z - s );
      glVertex3f( x - s * j, y - s, z + s );
      glVertex3f( x - s * j, y - s, z + s );
      glVertex3f( x - s * j, y + s, z + s );
      glVertex3f( x - s * j, y + s, z + s );
      glVertex3f( x - s * j, y + s, z - s );
      glVertex3f( x - s * j, y + s, z - s );
      glVertex3f( x - s * j, y - s, z - s );
    end;
  for j := -1 to 1 do
    begin
      if j = 0 Then continue;
      glVertex3f( x - s, y - s, z - s * j );
      glVertex3f( x + s, y - s, z - s * j );
      glVertex3f( x - s, y + s, z - s * j );
      glVertex3f( x + s, y + s, z - s * j );
    end;
    
  if Node = Octree.MainNode Then
    begin
      tDebugFirst := FALSE;
      glEnd;
    end;
end;

procedure octree_DrawNode;
  var
    i, j       : DWORD;
begin
  if not frustum_CubeIn( Frustum, Node.Cube.Position.X, Node.Cube.Position.Y, Node.Cube.Position.Z, Node.Cube.Size.X ) Then exit;

  if Node.NInside Then
    for i := 0 to 7 do
      if Assigned( Node.SubNodes[ i ] ) Then
        octree_DrawNode( Octree, Node.SubNodes[ i ], Frustum );

  INC( Octree.r_NodeACount );

  if Node.RDSize > 0 Then
    begin
      for j := 0 to Node.RDSize - 1 do
        begin
          if tLastFlags and OBJ3D_TEXTURING > 0 Then
            if Node.RenderData[ j ].Texture <> 0 Then
              begin
                glActiveTextureARB( GL_TEXTURE0_ARB );
                glEnable( GL_TEXTURE_2D );
                glBindTexture( GL_TEXTURE_2D, Node.RenderData[ j ].Texture );
              end;

          glDrawElements( GL_TRIANGLES, Node.RenderData[ j ].ICount, Node.RenderData[ j ].IBType, Node.RenderData[ j ].Indices );
        end;
    end;
end;

//TODO: оптимизировать(хранить индексы для таких фейсов заранее в памяти а не генерировать динамически)
procedure octree_DrawDFaces;
  var
    i, j       : DWORD;
    DFCount    : DWORD;
    RDSize     : DWORD;
    RenderData : array of zglTRenderData;
begin
  if not frustum_CubeIn( Frustum, Node.Cube.Position.X, Node.Cube.Position.Y, Node.Cube.Position.Z, Node.Cube.Size.X ) Then exit;

  if Node.NInside Then
    for i := 0 to 7 do
      if Assigned( Node.SubNodes[ i ] ) Then
        octree_DrawDFaces( Octree, Node.SubNodes[ i ], Frustum );

  if Node.DFCount > 0 Then
    begin
      DFCount := 0;
      for j := 0 to Node.DFCount - 1 do
        begin
          if octree_FaceAlreadyDraw( Octree, Node.DFaces[ j ] ) Then continue;

          Octree.DFaces[ DFCount ] := Node.DFaces[ j ];
          INC( DFCount );
        end;

      if DFCount = 0 Then exit;

      RDSize := GetRenderDataSize( Octree, DFCount, Octree.DFaces );
      SetLength( RenderData, RDSize );
      BuildRenderData( Octree, RDSize, RenderData, DFCount, Octree.DFaces );

      for j := 0 to RDSize - 1 do
        begin
          if tLastFlags and OBJ3D_TEXTURING > 0 Then
            if RenderData[ j ].Texture <> 0 Then
              begin
                glActiveTextureARB( GL_TEXTURE0_ARB );
                glEnable( GL_TEXTURE_2D );
                glBindTexture( GL_TEXTURE_2D, RenderData[ j ].Texture );
              end;

          glDrawElements( GL_TRIANGLES, RenderData[ j ].ICount, RenderData[ j ].IBType, RenderData[ j ].Indices );
          FreeMem( RenderData[ j ].Indices );
        end;

      SetLength( RenderData, 0 );
    end;
end;

function octree_FaceAlreadyDraw;
  var
    i : DWORD;
begin
  Result := FALSE;

  if Octree.r_DFacesCount = 0 Then exit;

  for i := 0 to Octree.r_DFacesCount - 1 do
    if Octree.r_DFacesAlready[ i ] = face_index Then
      begin
        Result := TRUE;
        exit;
      end;

  Octree.r_DFacesAlready[ Octree.r_DFacesCount ] := face_index;
  INC( Octree.r_DFacesCount );
end;

end.
