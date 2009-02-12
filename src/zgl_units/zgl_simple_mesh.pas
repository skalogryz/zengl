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
unit zgl_simple_mesh;

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
  zgl_vbo,
  zgl_math,
  zgl_utils_3d;

function  smesh_LoadFromFile( const FileName : String; const Flags : DWORD ) : zglPSMesh;
function  smesh_LoadFromMemory( var Memory : zglTMemory; const Flags : DWORD ) : zglPSMesh;
procedure smesh_Animate( const Mesh : zglPSMesh; var State : zglTSimpleState );
procedure smesh_Draw( const Mesh : zglPSMesh; const State : zglPSimpleState );
procedure smesh_DrawGroup( const Mesh : zglPSMesh; const State : zglPSimpleState; const Group : DWORD );
procedure smesh_Free( const Mesh : zglPSMesh );

implementation

var
  tTexLevel : Byte;

function smesh_LoadFromFile;
  var
    Memory : zglTMemory;
begin
  if not file_Exists( FileName ) Then
    begin
      log_Add( 'Cannot read ' + FileName );
      Result := nil;
      exit;
    end;

  mem_LoadFromFile( Memory, FileName );
  Result := smesh_LoadFromMemory( Memory, Flags );
  mem_Free( Memory );
end;

function smesh_LoadFromMemory;
  var
    i, f      : DWORD;
    DataID, t : Byte;
    TexLayer  : Byte = 0;
begin
  if not zmf_ReadHeader( Memory ) Then
    begin
      log_Add( 'not a ZenGL Mesh File' );
      exit;
    end;
  Result := AllocMem( SizeOf( zglTSMesh ) );

  Result.Flags  := zmfHeader.Flags or Flags;
  Result.VCount := zmfHeader.VCount;
  Result.TCount := zmfHeader.TCount;
  Result.FCount := zmfHeader.FCount;
  Result.GCount := zmfHeader.GCount;
  Result.ACount := 0;
  Result.Frames := 1;

  SetLength( Result.Vertices, Result.VCount );
  if zmfHeader.Flags and USE_NORMALS > 0 Then SetLength( Result.Normals, Result.VCount );
  if zmfHeader.Flags and USE_TEXTURE > 0 Then
    begin
      SetLength( Result.TexCoords, Result.TCount );
      SetLength( Result.MultiTexCoords, ( zmfHeader.TLayers - 1 ) * Result.VCount );
    end;
  SetLength( Result.Faces, Result.FCount );
  SetLength( Result.Groups, Result.GCount );

  while Memory.Position < Memory.Size do
    begin
      mem_Read( Memory, DataID, 1 );
      case DataID of
        ZMF_VERTICES:
          begin
            zmf_ReadVertices( Memory, Result.Vertices );
          end;
        ZMF_NORMALS:
          begin
            zmf_ReadNormals( Memory, Result.Normals );
          end;
        ZMF_TEXCOORDS:
          begin
            INC( TexLayer );
            zmf_ReadTexCoords( Memory, Result.TexCoords, TexLayer )
          end;
        ZMF_FACES:
          begin
            zmf_ReadFaces( Memory, Result.Faces );
          end;
        ZMF_GROUPS:
          begin
            zmf_ReadGroups( Memory, Result.Groups );
          end;
        ZMF_FRAME:
          begin
            INC( Result.Frames );
            SetLength( Result.Vertices, Result.VCount * Result.Frames );
            if zmfHeader.Flags and USE_NORMALS > 0 Then
              SetLength( Result.Normals, Result.VCount * Result.Frames );
            zmf_ReadFrame( Memory, Result.Vertices, Result.Normals );
          end;
        ZMF_PACKED_VERTICES:
          begin
            zmf_ReadPackedVertices( Memory, Result.Vertices );
          end;
        ZMF_PACKED_NORMALS:
          begin
            zmf_ReadPackedNormals( Memory, Result.Normals );
          end;
        ZMF_PACKED_TEXCOORDS:
          begin
            INC( TexLayer );
            zmf_ReadPackedTexCoords( Memory, Result.TexCoords, TexLayer );
          end;
        ZMF_FACES_RANGE_WORD:
          begin
            zmf_ReadFacesW( Memory, Result.Faces );
          end;
        ZMF_GROUPS_RANGE_WORD:
          begin
            zmf_ReadGroupsW( Memory, Result.Groups );
          end;
        ZMF_PACKED_FRAME:
          begin
            INC( Result.Frames );
            SetLength( Result.Vertices, Result.VCount * Result.Frames );
            if zmfHeader.Flags and USE_NORMALS > 0 Then
              SetLength( Result.Normals, Result.VCount * Result.Frames );
            zmf_ReadPackedFrame( Memory, Result.Vertices, Result.Normals );
          end;
        ZMF_ACTION:
          begin
            SetLength( Result.Actions, Result.ACount + 1 );
            mem_Read( Memory, t, 1 );
            SetLength( Result.Actions[ Result.ACount ].Name, t );
            mem_Read( Memory, Result.Actions[ Result.ACount ].Name,   t );
            mem_Read( Memory, Result.Actions[ Result.ACount ].FPS,    4 );
            mem_Read( Memory, Result.Actions[ Result.ACount ].FCount, 4 );
            mem_Read( Memory, Result.Actions[ Result.ACount ].FFrame, 4 );
            INC( Result.ACount );
          end;
      end;
    end;

  SetLength( Result.RIndices, Result.VCount );
  Result.RVCount := CalcRVC( Result.VCount, Result.Vertices, Result.RIndices );

  if Result.Frames > 1 Then
    for f := 0 to Result.Frames - 1 do
      begin
        for i := Result.RVCount to Result.VCount - 1 do
          Result.Vertices[ Result.VCount * f + i ] := Result.Vertices[ Result.VCount * f + Result.RIndices[ i ] ];
        if zmfHeader.Flags and USE_NORMALS > 0 Then
          for i := Result.RVCount to Result.VCount - 1 do
            Result.Normals[ Result.VCount * f + i ] := Result.Normals[ Result.VCount * f + Result.RIndices[ i ] ];
      end;

  vbo_Check( Result.Flags );

  if ( not Assigned( Result.Normals ) ) and ( Result.Flags and BUILD_SNORMALS > 0 ) Then
    begin
      Result.Flags := Result.Flags or USE_NORMALS;
      SetLength( Result.Normals, Result.VCount * Result.Frames );
      for i := 0 to Result.Frames - 1 do
        BuildSNormals( Result.FCount, Result.VCount, Result.Faces, Result.Vertices[ i * Result.VCount ], Result.Normals[ i * Result.VCount ] );
    end;

  if Result.VCount < 65536 Then
    begin
      BuildIndices( Result.FCount, Result.Faces, Result.Indices, 2 );
      for i := 0 to Result.GCount - 1 do
        Result.Groups[ i ].Indices := Result.Indices + Result.Groups[ i ].IFace * 3 * 2;
    end else
      begin
        BuildIndices( Result.FCount, Result.Faces, Result.Indices, 4 );
        for i := 0 to Result.GCount - 1 do
          Result.Groups[ i ].Indices := Result.Indices + Result.Groups[ i ].IFace * 3 * 4;
      end;

  if Result.Flags and USE_VBO > 0 Then
    vbo_Build( Result.IBuffer, Result.VBuffer, Result.FCount * 3, Result.VCount{ * Result.Frames},
               Result.Indices,
               Result.Vertices, Result.Normals,
               Result.TexCoords, Result.MultiTexCoords,
               Result.Flags );
end;

procedure smesh_Animate;
  var
    i, j : Integer;
    prevFrame, nextFrame : Integer;
    d   : Single;
    b   : Boolean;
    vb  : Pointer;
    vb2 : Pointer;
begin
  if length( State.Vertices ) < Mesh.VCount Then
    SetLength( State.Vertices, Mesh.VCount );
  if ( length( State.Normals ) < Mesh.VCount ) and ( Mesh.Flags and USE_NORMALS > 0 ) Then
    SetLength( State.Normals, Mesh.VCount );

  with State do
    begin
      CalcFrame( Delta, prevDelta, d, Time, Frame, prevFrame, nextFrame, Mesh.Actions[ Action ].FFrame, Mesh.Actions[ Action ].FCount );

      if Mesh.Flags and BUILD_VBO_STATIC > 0 Then exit;

      for i := 0 to Mesh.RVCount - 1 do
        Vertices[ i ] := vector_Lerp( Mesh.Vertices[ i + prevFrame * Mesh.VCount ], Mesh.Vertices[ i + nextFrame * Mesh.VCount ], d );
      for i := Mesh.RVCount to Mesh.VCount - 1 do
        Vertices[ i ] := Vertices[ Mesh.RIndices[ i ] ];

      if Mesh.Flags and USE_NORMALS > 0 Then
        begin
          for i := 0 to Mesh.RVCount - 1 do
            Normals[ i ] := vector_Lerp( Mesh.Normals[ i + prevFrame * Mesh.VCount ], Mesh.Normals[ i + nextFrame * Mesh.VCount ], d );
          for i := Mesh.RVCount to Mesh.VCount - 1 do
            Normals[ i ] := Normals[ Mesh.RIndices[ i ] ];
        end;
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

procedure smesh_Draw;
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
      if Mesh.Flags and USE_NORMALS > 0 Then PN := PV + Mesh.VCount * 12{ * Mesh.Frames};
      if Mesh.Flags and USE_TEXTURE > 0 Then PT := PV + PN + Mesh.VCount * 12{ * Mesh.Frames};
      if glIsBufferARB( State.VBuffer ) = GL_FALSE Then
        glBindBufferARB( GL_ARRAY_BUFFER_ARB, Mesh.VBuffer )
      else
        glBindBufferARB( GL_ARRAY_BUFFER_ARB, State.VBuffer );
      glBindBufferARB( GL_ELEMENT_ARRAY_BUFFER_ARB, Mesh.IBuffer );
    end else
      begin
        if Assigned( State ) Then
          begin
            PV := Ptr( @State.Vertices[ 0 ] );
            if Mesh.Flags and USE_NORMALS > 0 Then PN := Ptr( @State.Normals[ 0 ] );
            if Mesh.Flags and USE_TEXTURE > 0 Then PT := Ptr( @Mesh.TexCoords[ 0 ] );
          end else
            begin
              PV := Ptr( @Mesh.Vertices[ 0 ] );
              if Mesh.Flags and USE_NORMALS > 0 Then PN := Ptr( @Mesh.Normals[ 0 ] );
              if Mesh.Flags and USE_TEXTURE > 0 Then PT := Ptr( @Mesh.TexCoords[ 0 ] );
            end;
      end;

  {if Assigned( State ) and ( Mesh.Flags and BUILD_VBO_STATIC > 0 ) Then
    PV := PV + Mesh.VCount * 12 * ( Mesh.Actions[ State.nAction ].FFrame + State.nFrame );}

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

procedure smesh_DrawGroup;
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
      if Mesh.Flags and USE_NORMALS > 0 Then PN := PV + Mesh.VCount * 12 * Mesh.Frames;
      if Mesh.Flags and USE_TEXTURE > 0 Then PT := PV + PN + Mesh.VCount * 12 * Mesh.Frames;
      if glIsBufferARB( State.VBuffer ) = GL_FALSE Then
        glBindBufferARB( GL_ARRAY_BUFFER_ARB, Mesh.VBuffer )
      else
        glBindBufferARB( GL_ARRAY_BUFFER_ARB, State.VBuffer );
      glBindBufferARB( GL_ELEMENT_ARRAY_BUFFER_ARB, Mesh.IBuffer );
    end else
      begin
        if Assigned( State ) Then
          begin
            PV := Ptr( @State.Vertices[ 0 ] );
            if Mesh.Flags and USE_NORMALS > 0 Then PN := Ptr( @State.Normals[ 0 ] );
            if Mesh.Flags and USE_TEXTURE > 0 Then PT := Ptr( @Mesh.TexCoords[ 0 ] );
          end else
            begin
              PV := Ptr( @Mesh.Vertices[ 0 ] );
              if Mesh.Flags and USE_NORMALS > 0 Then PN := Ptr( @Mesh.Normals[ 0 ] );
              if Mesh.Flags and USE_TEXTURE > 0 Then PT := Ptr( @Mesh.TexCoords[ 0 ] );
            end;
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

procedure smesh_Free;
begin
  SetLength( Mesh.Vertices, 0 );
  SetLength( Mesh.Normals, 0 );
  SetLength( Mesh.TexCoords, 0 );
  SetLength( Mesh.MultiTexCoords, 0 );
  SetLength( Mesh.Faces, 0 );
  SetLength( Mesh.Groups, 0 );
  FreeMem( Mesh.Indices );
  SetLength( Mesh.RIndices, 0 );
  SetLength( Mesh.Actions, 0 );
  FreeMem( Mesh );
end;

end.
