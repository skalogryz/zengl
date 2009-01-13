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
  zgl_vbo,
  zgl_mesh_file,
  zgl_math,
  zgl_utils_3d,
  Utils;

function  skmesh_LoadFromFile( var Mesh : zglPSkMesh; const FileName : PChar; const Flags : DWORD ) : Boolean;
procedure skmesh_Animate( const Mesh : zglPSkMesh; var State : zglTSkeletonState );
procedure skmesh_Draw( const Mesh : zglPSkMesh; const State : zglPSkeletonState );
procedure skmesh_DrawGroup( const Mesh : zglPSkMesh; const State : zglPSkeletonState; const Group : DWORD );
procedure skmesh_DrawSkelet( const Mesh : zglPSkMesh; const State : zglPSkeletonState );
procedure skmesh_Free( const Mesh : zglPSkMesh );

procedure skmesh_CalcQuats( var BonePos : array of zglTBonePos );
procedure skmesh_CalcFrame( var BonePos : array of zglTBonePos; const Bones : array of zglTBone );
procedure skmesh_CalcVerts( const Mesh : zglPSkMesh; var State : zglTSkeletonState; const BonePos : array of zglTBonePos );

implementation

var
  tTexLevel : Byte;

function skmesh_LoadFromFile;
  var
    i, j     : DWORD;
    M        : zglTMemory;
    DataID   : Byte;
    TexLayer : Byte = 0;
    function getVI( ID : DWORD ) : DWORD;
      var
        k : DWORD;
    begin
      for k := 0 to Mesh.VCount - 1 do
        if ( Mesh.Vertices[ ID ].X = Mesh.Vertices[ k ].X ) and
           ( Mesh.Vertices[ ID ].Y = Mesh.Vertices[ k ].Y ) and
           ( Mesh.Vertices[ ID ].Z = Mesh.Vertices[ k ].Z ) Then
          begin
            Result := k;
            if ( k <> ID ) and ( Mesh.RVCount = 0 ) Then Mesh.RVCount := ID;
            break;
          end;
    end;
    procedure calcVI;
      var
        k : DWORD;
    begin
      SetLength( Mesh.RIndices, Mesh.VCount );
      for k := 0 to Mesh.VCount - 1 do
        Mesh.RIndices[ k ] := getVI( k );
    end;
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
  SetLength( Mesh.BonePos, Mesh.BCount );

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
        ZMF_BONEPOS:
          begin
            zmf_ReadBonePos( M, Mesh.BonePos );
          end;
      end;
    end;
  mem_Free( M );
  calcVI;

  vbo_Check( Mesh.Flags );

  if ( not Assigned( Mesh.Normals ) ) and ( Mesh.Flags and BUILD_SNORMALS > 0 ) Then
    begin
      Mesh.Flags := Mesh.Flags or USE_NORMALS;
      SetLength( Mesh.Normals, Mesh.VCount );
      BuildSNormals( Mesh.FCount, Mesh.VCount, Mesh.Faces, Mesh.Vertices, Mesh.Normals );
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

  if Mesh.Flags and USE_VBO > 0 Then
    vbo_Build( Mesh.IBuffer, Mesh.VBuffer, Mesh.FCount * 3, Mesh.VCount,
               Mesh.Indices,
               Mesh.Vertices, Mesh.Normals,
               Mesh.TexCoords, Mesh.MultiTexCoords,
               Mesh.Flags );

  skmesh_CalcQuats( Mesh.BonePos );
  skmesh_CalcFrame( Mesh.BonePos, Mesh.Bones );

  Result := TRUE;
end;

procedure skmesh_Animate;
  var
    i : Integer;
    b : Boolean;
    d : Single;
    prevFrame : Integer;
    nextFrame : Integer;
begin
  with State do
    begin
      CalcFrame( Delta, prevDelta, d, Time, Frame, prevFrame, nextFrame, 0, Mesh.Actions[ Action ].FCount );

      if length( BonePos ) = 0 Then
        SetLength( BonePos, length( Mesh.Actions[ Action ].Frames[ prevFrame ] ) );

      skmesh_CalcQuats( Mesh.Actions[ Action ].Frames[ prevFrame ] );
      skmesh_CalcQuats( Mesh.Actions[ Action ].Frames[ nextFrame ] );

      for i := 0 to length( BonePos ) - 1 do        with Mesh.Actions[ Action ].Frames[ prevFrame, i ] do
          begin
            BonePos[ i ].Translation := vector_Lerp( Translation,
                                                     Mesh.Actions[ Action ].Frames[ nextFrame, i ].Translation,                                                     d );
            BonePos[ i ].Quaternion  := quater_Lerp( Quaternion,
                                                     Mesh.Actions[ Action ].Frames[ nextFrame, i ].Quaternion,
                                                     d );
            BonePos[ i ].Matrix      := quater_GetM4f( BonePos[ i ].Quaternion );
          end;

      skmesh_CalcFrame( BonePos, Mesh.Bones );
      skmesh_CalcVerts( Mesh, State, BonePos );
    end;
end;

procedure skmesh_Draw;
  var
    i  : Byte;
    PV : Ptr = 0;
    PN : Ptr = 0;
    PT : Ptr = 0;
begin
  if ogl_MaxTexLevels > 0 Then
    tTexLevel := Byte( Mesh.Flags and USE_MULTITEX1 > 0 ) +
                 Byte( Mesh.Flags and USE_MULTITEX2 > 0 ) +
                 Byte( Mesh.Flags and USE_MULTITEX3 > 0 );

  if Mesh.Flags and USE_VBO > 0 Then
    begin
      PV := 0;
      if Mesh.Flags and USE_NORMALS > 0 Then PN := PV + Mesh.VCount * 12;
      if Mesh.Flags and USE_TEXTURE > 0 Then PT := PV + PN + Mesh.VCount * 12;
      if glIsBufferARB( State.VBuffer ) = GL_FALSE Then
        glBindBufferARB( GL_ARRAY_BUFFER_ARB, Mesh.VBuffer )
      else
        glBindBufferARB( GL_ARRAY_BUFFER_ARB, State.VBuffer );
      glBindBufferARB( GL_ELEMENT_ARRAY_BUFFER_ARB, Mesh.IBuffer );
    end else
      begin
        PV := Ptr( @State.Vertices[ 0 ] );
        if Mesh.Flags and USE_NORMALS > 0 Then PN := Ptr( @State.Normals[ 0 ] );
        if Mesh.Flags and USE_TEXTURE > 0 Then PT := Ptr( @Mesh.TexCoords[ 0 ] );
      end;
                 
  if Mesh.Flags and USE_NORMALS > 0 Then
    begin
      glEnableClientState( GL_NORMAL_ARRAY );
      glNormalPointer( GL_FLOAT, 0, Pointer( PN ) );
    end;
  if Mesh.Flags and USE_TEXTURE > 0 Then
    begin
      glClientActiveTextureARB( GL_TEXTURE0_ARB );
      glEnableClientState( GL_TEXTURE_COORD_ARRAY );
      glTexCoordPointer( 2, GL_FLOAT, 0, Pointer( PT ) );

      if ogl_MaxTexLevels > 0 Then
        for i := 1 to tTexLevel do
          begin
            glClientActiveTextureARB( GL_TEXTURE0_ARB + i );
            glEnableClientState( GL_TEXTURE_COORD_ARRAY );
            if Mesh.Flags and USE_VBO > 0 Then
              glTexCoordPointer( 2, GL_FLOAT, 0, Pointer( PT + Mesh.VCount * i * 8  ) )
            else
              glTexCoordPointer( 2, GL_FLOAT, 0, @Mesh.MultiTexCoords[ 0 + Mesh.VCount * ( i - 1 ) ] );
          end;
    end;

  glEnableClientState( GL_VERTEX_ARRAY );
  glVertexPointer( 3, GL_FLOAT, 0, Pointer( PV ) );

  if Mesh.Flags and USE_VBO > 0 Then
    begin
      if Mesh.VCount < 65536 Then
        glDrawElements( GL_TRIANGLES, Mesh.FCount * 3, GL_UNSIGNED_SHORT, nil )
      else
        glDrawElements( GL_TRIANGLES, Mesh.FCount * 3, GL_UNSIGNED_INT, nil );
    end else
      begin
        if Mesh.VCount < 65536 Then
          glDrawElements( GL_TRIANGLES, Mesh.FCount * 3, GL_UNSIGNED_SHORT, Mesh.Indices )
        else
          glDrawElements( GL_TRIANGLES, Mesh.FCount * 3, GL_UNSIGNED_INT, Mesh.Indices );
      end;
    
  glDisableClientState( GL_VERTEX_ARRAY );
  glDisableClientState( GL_NORMAL_ARRAY );
  glDisableClientState( GL_TEXTURE_COORD_ARRAY );
  if ogl_CanVBO Then
    begin
      glBindBufferARB( GL_ARRAY_BUFFER_ARB, 0 );
      glBindBufferARB( GL_ELEMENT_ARRAY_BUFFER_ARB, 0 );
    end;
end;

procedure skmesh_DrawGroup;
  var
    i  : Byte;
    PV : Ptr = 0;
    PN : Ptr = 0;
    PT : Ptr = 0;
begin
  if ogl_MaxTexLevels > 0 Then
    tTexLevel := Byte( Mesh.Flags and USE_MULTITEX1 > 0 ) +
                 Byte( Mesh.Flags and USE_MULTITEX2 > 0 ) +
                 Byte( Mesh.Flags and USE_MULTITEX3 > 0 );

  if Mesh.Flags and USE_VBO > 0 Then
    begin
      PV := 0;
      if Mesh.Flags and USE_NORMALS > 0 Then PN := PV + Mesh.VCount * 12;
      if Mesh.Flags and USE_TEXTURE > 0 Then PT := PV + PN + Mesh.VCount * 12;
      if glIsBufferARB( State.VBuffer ) = GL_FALSE Then
        glBindBufferARB( GL_ARRAY_BUFFER_ARB, Mesh.VBuffer )
      else
        glBindBufferARB( GL_ARRAY_BUFFER_ARB, State.VBuffer );
      glBindBufferARB( GL_ELEMENT_ARRAY_BUFFER_ARB, Mesh.IBuffer );
    end else
      begin
        PV := Ptr( @State.Vertices[ 0 ] );
        if Mesh.Flags and USE_NORMALS > 0 Then PN := Ptr( @State.Normals[ 0 ] );
        if Mesh.Flags and USE_TEXTURE > 0 Then PT := Ptr( @Mesh.TexCoords[ 0 ] );
      end;
                 
  if Mesh.Flags and USE_NORMALS > 0 Then
    begin
      glEnableClientState( GL_NORMAL_ARRAY );
      glNormalPointer( GL_FLOAT, 0, Pointer( PN ) );
    end;
  if Mesh.Flags and USE_TEXTURE > 0 Then
    begin
      glClientActiveTextureARB( GL_TEXTURE0_ARB );
      glEnableClientState( GL_TEXTURE_COORD_ARRAY );
      glTexCoordPointer( 2, GL_FLOAT, 0, Pointer( PT ) );

      if ogl_MaxTexLevels > 0 Then
        for i := 1 to tTexLevel do
          begin
            glClientActiveTextureARB( GL_TEXTURE0_ARB + i );
            glEnableClientState( GL_TEXTURE_COORD_ARRAY );
            if Mesh.Flags and USE_VBO > 0 Then
              glTexCoordPointer( 2, GL_FLOAT, 0, Pointer( PT + Mesh.VCount * i * 8  ) )
            else
              glTexCoordPointer( 2, GL_FLOAT, 0, @Mesh.MultiTexCoords[ 0 + Mesh.VCount * ( i - 1 ) ] );
          end;
    end;

  glEnableClientState( GL_VERTEX_ARRAY );
  glVertexPointer( 3, GL_FLOAT, 0, Pointer( PV ) );

  if Mesh.Flags and USE_VBO > 0 Then
    begin
      if Mesh.VCount < 65536 Then
        glDrawElements( GL_TRIANGLES, Mesh.Groups[ Group ].FCount * 3, GL_UNSIGNED_SHORT, Pointer( Mesh.Groups[ Group ].IFace * 3 * 2 ) )
      else
        glDrawElements( GL_TRIANGLES, Mesh.Groups[ Group ].FCount * 3, GL_UNSIGNED_INT, Pointer( Mesh.Groups[ Group ].IFace * 3 * 4 ) );
    end else
      begin
        if Mesh.VCount < 65536 Then
          glDrawElements( GL_TRIANGLES, Mesh.Groups[ Group ].FCount * 3, GL_UNSIGNED_SHORT, Mesh.Groups[ Group ].Indices )
        else
          glDrawElements( GL_TRIANGLES, Mesh.Groups[ Group ].FCount * 3, GL_UNSIGNED_INT, Mesh.Groups[ Group ].Indices );
      end;
    
  glDisableClientState( GL_VERTEX_ARRAY );
  glDisableClientState( GL_NORMAL_ARRAY );
  glDisableClientState( GL_TEXTURE_COORD_ARRAY );
  if ogl_CanVBO Then
    begin
      glBindBufferARB( GL_ARRAY_BUFFER_ARB, 0 );
      glBindBufferARB( GL_ELEMENT_ARRAY_BUFFER_ARB, 0 );
    end;
end;

procedure skmesh_DrawSkelet;
  var
    i : Integer;
begin
  glBegin( GL_LINES );
  for i := 0 to length( State.BonePos ) - 1 do
    if Mesh.Bones[ i ].Parent >= 0 Then
      begin
        glVertex3fv( @State.BonePos[ i ].Point );
        glVertex3fv( @State.BonePos[ Mesh.Bones[ i ].Parent ].Point );
      end;
  glEnd;

  glPointSize( 3 );
  glBegin(GL_POINTS);
  for i := 0 to length( State.BonePos ) - 1 do
    glVertex3fv( @State.BonePos[ i ].Point );
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
        SetLength( Mesh.Actions[ i ].Frames[ j ], 0 );
      SetLength( Mesh.Actions[ i ].Frames, 0 );
    end;
  SetLength( Mesh.Actions, 0 );
  SetLength( Mesh.BonePos, 0 );
  FreeMem( Mesh.Indices );
  SetLength( Mesh.RIndices, 0 );
  FreeMem( Mesh );
end;

procedure skmesh_CalcQuats;
  var
    i : Integer;
begin
  for i := 0 to length( BonePos ) - 1 do
    begin
      BonePos[ i ].Quaternion := quater_FromRotation( BonePos[ i ].Rotation );
      BonePos[ i ].Matrix     := quater_GetM4f( BonePos[ i ].Quaternion );
    end;
end;

procedure skmesh_CalcFrame;
  var
    i : Integer;
begin
  for i := 0 to length( BonePos ) - 1 do
    begin
      BonePos[ i ].Point.X := 0;
      BonePos[ i ].Point.Y := 0;
      BonePos[ i ].Point.Z := 0;

      BonePos[ i ].Matrix.a14 := BonePos[ i ].Translation.X;
      BonePos[ i ].Matrix.a24 := BonePos[ i ].Translation.Y;
      BonePos[ i ].Matrix.a34 := BonePos[ i ].Translation.Z;
      if Bones[ i ].Parent >= 0 Then
        BonePos[ i ].Matrix := matrix4f_Mul( BonePos[ Bones[ i ].Parent ].Matrix, BonePos[ i ].Matrix );
      BonePos[ i ].Point := vector_MulInvM4f( BonePos[ i ].Point, BonePos[ i ].Matrix );
    end;
end;

procedure skmesh_CalcVerts;
  var
    i, j    : Integer;
    t1, t2  : zglTPoint3D;
    Matrix  : zglPMatrix4f;
    rMatrix : zglTMatrix4f;
    Weight  : Single;
    vb, vb2 : Pointer;
begin
  if length( State.Vertices ) < Mesh.VCount Then
    SetLength( State.Vertices, Mesh.VCount );
  if ( length( State.Normals ) < Mesh.VCount ) and ( Mesh.Flags and USE_NORMALS > 0 ) Then
    SetLength( State.Normals, Mesh.VCount );

  for i := 0 to Mesh.RVCount - 1 do
    begin
      t2.X := 0;
      t2.Y := 0;
      t2.Z := 0;
      for j := 0 to Mesh.WCount[ i ] - 1 do
        begin
          Matrix := @Mesh.BonePos[ Mesh.Weights[ i, j ].boneID ].Matrix;
          t1.X   := Mesh.Vertices[ i ].X - Matrix.a14;
          t1.Y   := Mesh.Vertices[ i ].Y - Matrix.a24;
          t1.Z   := Mesh.Vertices[ i ].Z - Matrix.a34;
          t1     := vector_MulM4f( t1, Matrix^ );

          Matrix := @BonePos[ Mesh.Weights[ i, j ].boneID ].Matrix;
          t1     := vector_MulInvM4f( t1, Matrix^ );
          Weight := Mesh.Weights[ i, j ].Weight;

          t2.X := t2.X + t1.X * Weight;
          t2.Y := t2.Y + t1.Y * Weight;
          t2.Z := t2.Z + t1.Z * Weight;
        end;
      State.Vertices[ i ] := t2;
    end;

  for i := Mesh.RVCount to Mesh.VCount - 1 do
    State.Vertices[ i ] := State.Vertices[ Mesh.RIndices[ i ] ];

  if Mesh.Flags and USE_NORMALS > 0 Then
    begin
      for i := 0 to Mesh.RVCount - 1 do
        begin
          t2.X := 0;
          t2.Y := 0;
          t2.Z := 0;

          for j := 0 to Mesh.WCount[ i ] - 1 do
            begin
              t1 := Mesh.Normals[ i ];

              rMatrix := Mesh.BonePos[ Mesh.Weights[ i, j ].BoneID ].Matrix;
              rMatrix.a14 := 0;
              rMatrix.a24 := 0;
              rMatrix.a34 := 0;
              rMatrix.a41 := 0;
              rMatrix.a42 := 0;
              rMatrix.a43 := 0;
              rMatrix.a44 := 0;

              t1 := vector_MulM4f( t1, rMatrix );

              rMatrix := BonePos[ Mesh.Weights[ i, j ].BoneID ].Matrix;
              rMatrix.a14 := 0;
              rMatrix.a24 := 0;
              rMatrix.a34 := 0;
              rMatrix.a41 := 0;
              rMatrix.a42 := 0;
              rMatrix.a43 := 0;
              rMatrix.a44 := 0;

              t1 := vector_MulInvM4f( t1, rMatrix );

              Weight := Mesh.Weights[ i, j ].Weight;

              t2.X := t2.X + t1.X * Weight;
              t2.Y := t2.Y + t1.Y * Weight;
              t2.Z := t2.Z + t1.Z * Weight;
            end;

          State.Normals[ i ] := t2;
        end;

      for i := Mesh.RVCount to Mesh.VCount - 1 do
        State.Normals[ i ] := State.Normals[ Mesh.RIndices[ i ] ];
    end;

  if Mesh.Flags and BUILD_VBO_STREAM > 0 Then
    begin
      if glIsBufferARB( State.VBuffer ) = GL_FALSE Then
        begin
          glBindBufferARB( GL_ARRAY_BUFFER_ARB, Mesh.VBuffer );
          glGetBufferParameterivARB( GL_ARRAY_BUFFER_ARB, GL_BUFFER_SIZE_ARB, @i );
          vb := glMapBufferARB( GL_ARRAY_BUFFER_ARB, GL_READ_ONLY_ARB );
          vb2 := AllocMem( i );
          Move( vb^, vb2^, i );
          glUnmapBufferARB( GL_ARRAY_BUFFER_ARB );

          glGenBuffersARB( 1, @State.VBuffer );
          glBindBufferARB( GL_ARRAY_BUFFER_ARB, State.VBuffer );
          glBufferDataARB( GL_ARRAY_BUFFER_ARB, i, vb2, GL_STREAM_DRAW_ARB );
        end;

      glBindBufferARB( GL_ARRAY_BUFFER_ARB, State.VBuffer );
      vb := glMapBufferARB( GL_ARRAY_BUFFER_ARB, GL_WRITE_ONLY_ARB );

      Move( State.Vertices[ 0 ], vb^, SizeOf( zglTPoint3D ) * Mesh.VCount );
      if Mesh.Flags and USE_NORMALS > 0 Then
        Move( State.Normals[ 0 ], Pointer( vb + SizeOf( zglTPoint3D ) * Mesh.VCount )^, SizeOf( zglTPoint3D ) * Mesh.VCount );
      glUnmapBufferARB( GL_ARRAY_BUFFER_ARB );
      glBindBufferARB( GL_ARRAY_BUFFER_ARB, 0 );
    end;
end;

end.
