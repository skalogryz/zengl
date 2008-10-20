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
unit zgl_static_mesh;

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
  zgl_mesh_file,
  zgl_utils_3d;
  
function  smesh_LoadFromFile( var Mesh : zglPSMesh; FileName : PChar; Flags : DWORD ) : Boolean; extdecl;
procedure smesh_Draw( Mesh : zglPSMesh ); extdecl;
procedure smesh_DrawGroup( Mesh : zglPSMesh; Group : DWORD ); extdecl;
procedure smesh_Free( var Mesh : zglPSMesh ); extdecl;

implementation

var
  tTexLevel : Byte;

function smesh_LoadFromFile;
  var
    i        : DWORD;
    M        : zglTMemory;
    DataID   : Byte;
    TexLayer : Byte = 0;
begin
  Mesh := AllocMem( SizeOf( zglTSMesh ) );
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

  SetLength( Mesh.Vertices, Mesh.VCount );
  if zmfHeader.Flags and USE_NORMALS > 0 Then SetLength( Mesh.Normals, Mesh.VCount );
  if zmfHeader.Flags and USE_TEXTURE > 0 Then
    begin
      SetLength( Mesh.TexCoords, Mesh.TCount );
      SetLength( Mesh.MultiTexCoords, ( zmfHeader.TLayers - 1 ) * Mesh.VCount );
    end;
  SetLength( Mesh.Faces, Mesh.FCount );
  SetLength( Mesh.Groups, Mesh.GCount );
  
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
            zmf_ReadTexCoords( M, Mesh.TexCoords, TexLayer )
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

  Result := TRUE;
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

  if Mesh.Flags and BUILD_VBO > 0 Then
    begin
      PV := 0;
      if Mesh.Flags and USE_NORMALS > 0 Then PN := PV + Mesh.VCount * 12;
      if Mesh.Flags and USE_TEXTURE > 0 Then PT := PV + PN + Mesh.VCount * 12;
      glBindBufferARB( GL_ARRAY_BUFFER_ARB, Mesh.VBuffer );
      glBindBufferARB( GL_ELEMENT_ARRAY_BUFFER_ARB, Mesh.IBuffer );
    end else
      begin
        PV := Ptr( @Mesh.Vertices[ 0 ] );
        if Mesh.Flags and USE_NORMALS > 0 Then PN := Ptr( @Mesh.Normals[ 0 ] );
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
            if Mesh.Flags and BUILD_VBO > 0 Then
              glTexCoordPointer( 2, GL_FLOAT, 0, Pointer( PT + Mesh.VCount * i * 8  ) )
            else
              glTexCoordPointer( 2, GL_FLOAT, 0, @Mesh.MultiTexCoords[ 0 + Mesh.VCount * ( i - 1 ) ] );
          end;
    end;

  glEnableClientState( GL_VERTEX_ARRAY );
  glVertexPointer( 3, GL_FLOAT, 0, Pointer( PV ) );

  if Mesh.Flags and BUILD_VBO > 0 Then
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

  if Mesh.Flags and BUILD_VBO > 0 Then
    begin
      PV := 0;
      if Mesh.Flags and USE_NORMALS > 0 Then PN := PV + Mesh.VCount * 12;
      if Mesh.Flags and USE_TEXTURE > 0 Then PT := PV + PN + Mesh.VCount * 12;
      glBindBufferARB( GL_ARRAY_BUFFER_ARB, Mesh.VBuffer );
      glBindBufferARB( GL_ELEMENT_ARRAY_BUFFER_ARB, Mesh.IBuffer );
    end else
      begin
        PV := Ptr( @Mesh.Vertices[ 0 ] );
        if Mesh.Flags and USE_NORMALS > 0 Then PN := Ptr( @Mesh.Normals[ 0 ] );
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
            if Mesh.Flags and BUILD_VBO > 0 Then
              glTexCoordPointer( 2, GL_FLOAT, 0, Pointer( PT + Mesh.VCount * i * 8  ) )
            else
              glTexCoordPointer( 2, GL_FLOAT, 0, @Mesh.MultiTexCoords[ 0 + Mesh.VCount * ( i - 1 ) ] );
          end;
    end;

  glEnableClientState( GL_VERTEX_ARRAY );
  glVertexPointer( 3, GL_FLOAT, 0, Pointer( PV ) );

  if Mesh.Flags and BUILD_VBO > 0 Then
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
  FreeMem( Mesh );
end;

end.
