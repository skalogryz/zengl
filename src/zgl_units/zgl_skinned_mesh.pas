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
unit zgl_skinned_mesh;

{$I define.inc}

interface

uses
  GL, GLext,
  zgl_file,
  zgl_memory,
  zgl_log,
  zgl_const,
  zgl_types,
  zgl_global_var,
  zgl_timers,
  zgl_mesh_file,
  zgl_math,
  zgl_utils_3d;

function  skmesh_LoadFromFile( var Mesh : zglPSkMesh; FileName : PChar; Flags : DWORD ) : Boolean; extdecl;
procedure skmesh_Animate( Mesh : zglPSkMesh; State : zglPSkeletonState ); extdecl;
procedure skmesh_Draw( Mesh : zglPSkMesh; State : zglPSkeletonState ); extdecl;
procedure skmesh_DrawGroup( Mesh : zglPSkMesh; State : zglPSkeletonState; Group : DWORD ); extdecl;
procedure skmesh_DrawSkelet( Mesh : zglPSkMesh; State : zglPSkeletonState ); extdecl;
procedure skmesh_Free( var Mesh : zglPSkMesh ); extdecl;

procedure skmesh_CalcQuats( var Frame : zglTSkeletonFrame );
procedure skmesh_CalcFrame( var Frame : zglTSkeletonFrame; Bones : array of zglTBone );
procedure skmesh_CalcVerts( Mesh : zglPSkMesh; var State : zglTSkeletonState; Frame : zglTSkeletonFrame );

implementation

var
  tTexLevel : Byte;

function skmesh_LoadFromFile;
  var
    i, j     : DWORD;
    M        : zglTMemory;
    DataID   : Byte;
    TexLayer : Byte = 0;
begin
  Mesh := AllocMem( SizeOf( zglTSkMesh ) );
  Result := FALSE;

  if not file_Exists( FileName ) Then
    begin
      log_Add( 'Cannot read ' + FileName );
      exit;
    end;

  mem_LoadFromFile( M, FileName );

  if not zmf_ReadHeader( M ) Then
    begin
      log_Add( FileName + ' - not a ZenGL Mesh File' );
      mem_Free( M );
      exit;
    end;

  Mesh.Flags  := zmfHeader.Flags or Flags;
  Mesh.VCount := zmfHeader.VCount;
  Mesh.TCount := zmfHeader.TCount;
  Mesh.FCount := zmfHeader.FCount;
  Mesh.GCount := zmfHeader.GCount;
  Mesh.BCount := zmfHeader.BCount;
  Mesh.ACount := 0;

  SetLength( Mesh.Vertices, Mesh.VCount );
  if zmfHeader.Flags and USE_NORMALS > 0 Then SetLength( Mesh.Normals, Mesh.VCount );
  if zmfHeader.Flags and USE_TEXTURE > 0 Then
    begin
      SetLength( Mesh.TexCoords, Mesh.TCount );
      SetLength( Mesh.MultiTexCoords, ( zmfHeader.TLayers - 1 ) * Mesh.VCount );
    end;
  SetLength( Mesh.Faces, Mesh.FCount );
  SetLength( Mesh.Groups, Mesh.GCount );
  SetLength( Mesh.Bones, Mesh.BCount );
  SetLength( Mesh.Weights, Mesh.VCount );
  SetLength( Mesh.Skeleton.BonePos, Mesh.BCount );

  while M.Position < M.Size do
    begin
      mem_Read( M, DataID, 1 );
      case DataID of
        ZMF_VERTICES:
          begin
            zmf_ReadVertices( M, Mesh.Vertices );
          end;
        ZMF_NORMALS:
          begin
            zmf_ReadNormals( M, Mesh.Normals );
          end;
        ZMF_TEXCOORDS:
          begin
            INC( TexLayer );
            zmf_ReadTexCoords( M, Mesh.TexCoords, TexLayer );
          end;
        ZMF_FACES:
          begin
            zmf_ReadFaces( M, Mesh.Faces );
          end;
        ZMF_GROUPS:
          begin
            zmf_ReadGroups( M, Mesh.Groups );
          end;
        ZMF_PACKED_VERTICES:
          begin
            zmf_ReadPackedVertices( M, Mesh.Vertices );
          end;
        ZMF_PACKED_NORMALS:
          begin
            zmf_ReadPackedNormals( M, Mesh.Normals );
          end;
        ZMF_PACKED_TEXCOORDS:
          begin
            INC( TexLayer );
            zmf_ReadPackedTexCoords( M, Mesh.TexCoords, TexLayer );
          end;
        ZMF_FACES_RANGE_WORD:
          begin
            zmf_ReadFacesW( M, Mesh.Faces );
          end;
        ZMF_GROUPS_RANGE_WORD:
          begin
            zmf_ReadGroupsW( M, Mesh.Groups );
          end;
        ZMF_BONES:
          begin
            zmf_ReadBones( M, Mesh.Bones );
          end;
        ZMF_WEIGHTS:
          begin
            SetLength( Mesh.WCount, Mesh.VCount );
            mem_Read( M, Mesh.WCount[ 0 ], Mesh.VCount );
            SetLength( Mesh.Weights, Mesh.VCount );
            for i := 0 to Mesh.VCount - 1 do
              SetLength( Mesh.Weights[ i ], Mesh.WCount[ i ] );
            zmf_ReadWeights( M, Mesh.Weights, Mesh.WCount );
          end;
        ZMF_ACTION:
          begin
            SetLength( Mesh.Actions, Mesh.ACount + 1 );
            mem_Read( M, Mesh.Actions[ Mesh.ACount ].FCount, 4 );
            SetLength( Mesh.Actions[ Mesh.ACount ].Frames, Mesh.Actions[ Mesh.ACount ].FCount );
            zmf_ReadAction( M, Mesh.Actions[ Mesh.ACount ] );
            INC( Mesh.ACount );
          end;
        ZMF_SKELETON:
          begin
            zmf_ReadSkeleton( M, Mesh.Skeleton );
          end;
      end;
    end;
  mem_Free( M );

  if not Assigned( Mesh.Normals ) Then
    begin
      if Mesh.Flags and BUILD_FNORMALS > 0 Then
        begin
          Mesh.Flags := Mesh.Flags or USE_NORMALS;
          SetLength( Mesh.Normals, Mesh.VCount );
          BuildFNormals( Mesh.FCount, Mesh.Faces, Mesh.Vertices, Mesh.Normals );
        end else
      if Mesh.Flags and BUILD_SNORMALS > 0 Then
        begin
          Mesh.Flags := Mesh.Flags or USE_NORMALS;
          SetLength( Mesh.Normals, Mesh.VCount );
          BuildSNormals( Mesh.FCount, Mesh.Faces, Mesh.Vertices, Mesh.Normals );
        end;
    end;

  if zmfHeader.TLayers > 0 Then
    begin
      SetLength( Mesh.TexCoords, Mesh.VCount );
      BuildTexCoords( Mesh.FCount, Mesh.Faces, Mesh.VCount, Mesh.TexCoords );
    end;

  if Mesh.VCount < 65536 Then
    begin
      Mesh.Indices := AllocMem( Mesh.FCount * 2 * 3 );
      BuildIndices( Mesh.FCount, Mesh.Faces, Mesh.Indices, 2 );
      for i := 0 to Mesh.GCount - 1 do
        Mesh.Groups[ i ].Indices := Mesh.Indices + Mesh.Groups[ i ].IFace * 3 * 2;
    end else
      begin
        Mesh.Indices := AllocMem( Mesh.FCount * 4 * 3 );
        BuildIndices( Mesh.FCount, Mesh.Faces, Mesh.Indices, 4 );
        for i := 0 to Mesh.GCount - 1 do
          Mesh.Groups[ i ].Indices := Mesh.Indices + Mesh.Groups[ i ].IFace * 3 * 4;
      end;

  skmesh_CalcQuats( Mesh.Skeleton );
  skmesh_CalcFrame( Mesh.Skeleton, Mesh.Bones );

  SetLength( Mesh.State.Vertices, Mesh.VCount );

  Result := TRUE;
end;

procedure skmesh_Animate;
  var
    i : Integer;
begin
  if length( State.Vertices ) < Mesh.VCount Then
    SetLength( State.Vertices, Mesh.VCount );
  if ( length( State.Normals ) < Mesh.VCount ) and ( Mesh.Flags and USE_NORMALS > 0 ) Then
    SetLength( State.Normals, Mesh.VCount );
  if State.Delta <> State.prevDelta Then
  with State^ do
    begin
      prevDelta := Delta;

      if length( Frame.BonePos ) = 0 Then
        SetLength( Frame.BonePos, length( Mesh.Actions[ nAction ].Frames[ nFrame ].BonePos ) );

      skmesh_CalcQuats( Mesh.Actions[ nAction ].Frames[ nFrame ] );
      skmesh_CalcQuats( Mesh.Actions[ nAction ].Frames[ nFrame + 1 ] );

      for i := 0 to length( Frame.BonePos ) - 1 do        with Mesh.Actions[ nAction ].Frames[ nFrame ].BonePos[ i ] do
          begin
            Frame.BonePos[ i ].Translation := vector_Lerp( Translation,
                                                           Mesh.Actions[ nAction ].Frames[ nFrame + 1 ].BonePos[ i ].Translation,                                                           Delta );
            Frame.BonePos[ i ].Quaternion  := quater_Lerp( Quaternion,
                                                           Mesh.Actions[ nAction ].Frames[ nFrame + 1 ].BonePos[ i ].Quaternion,
                                                           Delta );
            Frame.BonePos[ i ].Matrix      := quater_GetM4f( Frame.BonePos[ i ].Quaternion );
          end;

      skmesh_CalcFrame( Frame, Mesh.Bones );
      skmesh_CalcVerts( Mesh, State^, Frame );
    end;
end;

procedure skmesh_Draw;
  var
    i : Byte;
begin
  if ogl_MaxTexLevels > 0 Then
    tTexLevel := Byte( Mesh.Flags and USE_MULTITEX1 > 0 ) +
                 Byte( Mesh.Flags and USE_MULTITEX2 > 0 ) +
                 Byte( Mesh.Flags and USE_MULTITEX3 > 0 );
                 
  if Mesh.Flags and USE_NORMALS > 0 Then
    begin
      glEnableClientState( GL_NORMAL_ARRAY );
      glNormalPointer( GL_FLOAT, 0, @State.Normals[ 0 ] );
    end;
  if Mesh.Flags and USE_TEXTURE > 0 Then
    begin
      glClientActiveTextureARB( GL_TEXTURE0_ARB );
      glEnableClientState( GL_TEXTURE_COORD_ARRAY );
      glTexCoordPointer( 2, GL_FLOAT, 0, @Mesh.TexCoords[ 0 ] );

      if ogl_MaxTexLevels > 0 Then
        for i := 1 to tTexLevel do
          begin
            glClientActiveTextureARB( GL_TEXTURE0_ARB + i );
            glEnableClientState( GL_TEXTURE_COORD_ARRAY );
            glTexCoordPointer( 2, GL_FLOAT, 0, @Mesh.MultiTexCoords[ 0 + Mesh.VCount * ( i - 1 ) ] );
          end;
    end;

  glEnableClientState( GL_VERTEX_ARRAY );
  glVertexPointer( 3, GL_FLOAT, 0, @State.Vertices[ 0 ] );

  if Mesh.VCount < 65536 Then
    glDrawElements( GL_TRIANGLES, Mesh.FCount * 3, GL_UNSIGNED_SHORT, Mesh.Indices )
  else
    glDrawElements( GL_TRIANGLES, Mesh.FCount * 3, GL_UNSIGNED_INT, Mesh.Indices );
    
  glDisableClientState( GL_VERTEX_ARRAY );
  glDisableClientState( GL_NORMAL_ARRAY );
  glDisableClientState( GL_TEXTURE_COORD_ARRAY );
end;

procedure skmesh_DrawGroup;
  var
    i : Byte;
begin
  if ogl_MaxTexLevels > 0 Then
    tTexLevel := Byte( Mesh.Flags and USE_MULTITEX1 > 0 ) +
                 Byte( Mesh.Flags and USE_MULTITEX2 > 0 ) +
                 Byte( Mesh.Flags and USE_MULTITEX3 > 0 );
                 
  if Mesh.Flags and USE_NORMALS > 0 Then
    begin
      glEnableClientState( GL_NORMAL_ARRAY );
      glNormalPointer( GL_FLOAT, 0, @State.Normals[ 0 ] );
    end;
  if Mesh.Flags and USE_TEXTURE > 0 Then
    begin
      glClientActiveTextureARB( GL_TEXTURE0_ARB );
      glEnableClientState( GL_TEXTURE_COORD_ARRAY );
      glTexCoordPointer( 2, GL_FLOAT, 0, @Mesh.TexCoords[ 0 ] );

      if ogl_MaxTexLevels > 0 Then
        for i := 1 to tTexLevel do
          begin
            glClientActiveTextureARB( GL_TEXTURE0_ARB + i );
            glEnableClientState( GL_TEXTURE_COORD_ARRAY );
            glTexCoordPointer( 2, GL_FLOAT, 0, @Mesh.MultiTexCoords[ 0 + Mesh.VCount * ( i - 1 ) ] );
          end;
    end;

  glEnableClientState( GL_VERTEX_ARRAY );
  glVertexPointer( 3, GL_FLOAT, 0, @State.Vertices[ 0 ] );
    
  if Mesh.VCount < 65536 Then
    glDrawElements( GL_TRIANGLES, Mesh.Groups[ Group ].FCount * 3, GL_UNSIGNED_SHORT, Mesh.Groups[ Group ].Indices )
  else
    glDrawElements( GL_TRIANGLES, Mesh.Groups[ Group ].FCount * 3, GL_UNSIGNED_INT, Mesh.Groups[ Group ].Indices );
    
  glDisableClientState( GL_VERTEX_ARRAY );
  glDisableClientState( GL_NORMAL_ARRAY );
  glDisableClientState( GL_TEXTURE_COORD_ARRAY );
end;

procedure skmesh_DrawSkelet;
  var
    i : Integer;
begin
  glBegin( GL_LINES );
  for i := 0 to length( State.Frame.BonePos ) - 1 do
    if Mesh.Bones[ i ].Parent >= 0 Then
      begin
        glVertex3fv( @State.Frame.BonePos[ i ].Point );
        glVertex3fv( @State.Frame.BonePos[ Mesh.Bones[ i ].Parent ].Point );
      end;
  glEnd;

  glPointSize( 3 );
  glBegin(GL_POINTS);
  for i := 0 to length( State.Frame.BonePos ) - 1 do
    glVertex3fv( @State.Frame.BonePos[ i ].Point );
  glEnd;
  glPointSize( 1 );
end;

procedure skmesh_Free;
  var
    i, j : Integer;
begin
  SetLength( Mesh.Vertices,  0 );
  SetLength( Mesh.Normals,   0 );
  SetLength( Mesh.TexCoords, 0 );
  SetLength( Mesh.MultiTexCoords, 0 );
  SetLength( Mesh.Faces,     0 );
  SetLength( Mesh.Groups,    0 );
  SetLength( Mesh.Bones,     0 );
  for i := 0 to Mesh.VCount - 1 do
    SetLength( Mesh.Weights[ i ], 0 );
  SetLength( Mesh.Weights,   0 );
  SetLength( Mesh.WCount,    0 );
  for i := 0 to Mesh.ACount - 1 do
    begin
      for j := 0 to Mesh.Actions[ i ].FCount - 1 do
        SetLength( Mesh.Actions[ i ].Frames[ j ].BonePos, 0 );
      SetLength( Mesh.Actions[ i ].Frames, 0 );
    end;
  SetLength( Mesh.Actions,   0 );
  SetLength( Mesh.Skeleton.BonePos, 0 );
  FreeMem( Mesh.Indices );
  FreeMem( Mesh );
end;

procedure skmesh_CalcQuats;
  var
    i : Integer;
begin
  for i := 0 to length( Frame.BonePos ) - 1 do
    begin
      Frame.BonePos[ i ].Quaternion := quater_FromRotation( Frame.BonePos[ i ].Rotation );
      Frame.BonePos[ i ].Matrix     := quater_GetM4f( Frame.BonePos[ i ].Quaternion );
    end;
end;

procedure skmesh_CalcFrame;
  var
    i : Integer;
begin
  for i := 0 to length( Frame.BonePos ) - 1 do
    begin
      Frame.BonePos[ i ].Point.X := 0;
      Frame.BonePos[ i ].Point.Y := 0;
      Frame.BonePos[ i ].Point.Z := 0;

      Frame.BonePos[ i ].Matrix[ 0, 3 ] := Frame.BonePos[ i ].Translation.X;
      Frame.BonePos[ i ].Matrix[ 1, 3 ] := Frame.BonePos[ i ].Translation.Y;
      Frame.BonePos[ i ].Matrix[ 2, 3 ] := Frame.BonePos[ i ].Translation.Z;
      if Bones[ i ].Parent >= 0 Then
        Frame.BonePos[ i ].Matrix := matrix4f_Mul( @Frame.BonePos[ Bones[ i ].Parent ].Matrix, @Frame.BonePos[ i ].Matrix );
      Frame.BonePos[ i ].Point := vector_MulInvM4f( Frame.BonePos[ i ].Point, @Frame.BonePos[ i ].Matrix );
    end;
end;

procedure skmesh_CalcVerts;
  var
    i, j    : Integer;
    t1, t2  : zglTPoint3D;
    p       : zglPPoint3D;
    Matrix  : zglPMatrix4f;
    rMatrix : zglTMAtrix4f;
    Weight  : Single;
begin
  for i := 0 to Mesh.VCount - 1 do
    begin
      t2.X := 0;
      t2.Y := 0;
      t2.Z := 0;
      for j := 0 to Mesh.WCount[ i ] - 1 do
        begin
          Matrix := @Mesh.Skeleton.BonePos[ Mesh.Weights[ i ][ j ].boneID ].Matrix;
          t1.X   := Mesh.Vertices[ i ].X - Matrix[ 0, 3 ];
          t1.Y   := Mesh.Vertices[ i ].Y - Matrix[ 1, 3 ];
          t1.Z   := Mesh.Vertices[ i ].Z - Matrix[ 2, 3 ];
          t1     := vector_MulM4f( t1, Matrix );

          Matrix := @Frame.BonePos[ Mesh.Weights[ i ][ j ].boneID ].Matrix;
          t1     := vector_MulInvM4f( t1, Matrix );
          Weight := Mesh.Weights[ i ][ j ].Weight;

          t2.X := t2.X + t1.X * Weight;
          t2.Y := t2.Y + t1.Y * Weight;
          t2.Z := t2.Z + t1.Z * Weight;
        end;
      State.Vertices[ i ] := t2;
    end;

  if Mesh.Flags and USE_NORMALS > 0 Then
    for i := 0 to Mesh.VCount - 1 do
      begin
        t2.X := 0;
        t2.Y := 0;
        t2.Z := 0;

        for j := 0 to Mesh.WCount[ i ] - 1 do
          begin
            t1 := Mesh.Normals[ i ];

            rMatrix := Mesh.Skeleton.BonePos[ Mesh.Weights[ i ][ j ].BoneID ].Matrix;
            rMatrix[ 0, 3 ] := 0;
            rMatrix[ 1, 3 ] := 0;
            rMatrix[ 2, 3 ] := 0;
            rMatrix[ 3, 0 ] := 0;
            rMatrix[ 3, 1 ] := 0;
            rMatrix[ 3, 2 ] := 0;
            rMatrix[ 3, 3 ] := 0;

            t1 := vector_MulM4f( t1, @rMatrix );

            rMatrix := Frame.BonePos[ Mesh.Weights[ i ][ j ].BoneID ].Matrix;
            rMatrix[ 0, 3 ] := 0;
            rMatrix[ 1, 3 ] := 0;
            rMatrix[ 2, 3 ] := 0;
            rMatrix[ 3, 0 ] := 0;
            rMatrix[ 3, 1 ] := 0;
            rMatrix[ 3, 2 ] := 0;
            rMatrix[ 3, 3 ] := 0;

            t1 := vector_MulInvM4f( t1, @rMatrix );

            Weight := Mesh.Weights[ i ][ j ].Weight;

            t2.X := t2.X + t1.X * Weight;
            t2.Y := t2.Y + t1.Y * Weight;
            t2.Z := t2.Z + t1.Z * Weight;
          end;

        State.Normals[ i ] := t2;
      end;
end;

end.
