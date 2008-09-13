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
unit zgl_mesh_file;

{$I define.inc}
{.$DEFINE ZMF_WRITE_DATA}

interface

uses
  zgl_memory,
  zgl_const,
  zgl_types;
  
type
  zglPZMFHeader = ^zglTZMFHeader;
  zglTZMFHeader = record
    ID      : array[ 0..14 ] of Char;
    Flags   : DWORD;
    VCount  : DWORD; // Vertices
    TCount  : DWORD; // TexCoords
    FCount  : DWORD; // Faces
    GCount  : DWORD; // Groups
    BCount  : DWORD; // Bones
    TLayers : Byte;  // Texture Layers
end;

const
  ZMF_ID                = 'ZENGL_MESH_FILE';
  ZMF_VERTICES          = $01;
  ZMF_NORMALS           = $02;
  ZMF_TEXCOORDS         = $03;
  ZMF_FACES             = $04;
  ZMF_GROUPS            = $05;
  ZMF_FRAME             = $06;
  ZMF_PACKED_VERTICES   = $07;
  ZMF_PACKED_NORMALS    = $08;
  ZMF_PACKED_TEXCOORDS  = $09;
  ZMF_FACES_RANGE_WORD  = $0A;
  ZMF_GROUPS_RANGE_WORD = $0B;
  ZMF_FRAME_RANGE_WORD  = $0C;
  ZMF_BONES             = $0D;
  ZMF_SKELETON          = $0E;
  ZMF_WEIGHTS           = $0F;
  ZMF_ACTION            = $10;

function  zmf_ReadHeader( var Memory : zglTMemory ) : Boolean;
procedure zmf_ReadVertices( var Memory : zglTMemory; var Vertices : array of zglTPoint3D );
procedure zmf_ReadNormals( var Memory : zglTMemory; var Normals : array of zglTPoint3D );
procedure zmf_ReadTexCoords( var Memory : zglTMemory; var TexCoords : array of zglTPoint2D; Layer : Byte );
procedure zmf_ReadFaces( var Memory : zglTMemory; var Faces : array of zglTFace );
procedure zmf_ReadGroups( var Memory : zglTMemory; var Groups : array of zglTGroup );
procedure zmf_ReadFrame( var Memory : zglTMemory; var Frame : zglTFrame );
procedure zmf_ReadPackedVertices( var Memory : zglTMemory; var Vertices : array of zglTPoint3D );
procedure zmf_ReadPackedNormals( var Memory : zglTMemory; var Normals : array of zglTPoint3D );
procedure zmf_ReadPackedTexCoords( var Memory : zglTMemory; var TexCoords : array of zglTPoint2D; Layer : Byte );
procedure zmf_ReadFacesW( var Memory : zglTMemory; var Faces : array of zglTFace );
procedure zmf_ReadGroupsW( var Memory : zglTMemory; var Groups : array of zglTGroup );
procedure zmf_ReadFrameW( var Memory : zglTMemory; var Frame : zglTFrame );
procedure zmf_ReadBones( var Memory : zglTMemory; var Bones : array of zglTBone );
procedure zmf_ReadWeights( var Memory : zglTMemory; var Weights : zglTBonesWeights; WCount : array of Byte );
procedure zmf_ReadAction( var Memory : zglTMemory; var Action : zglTSkeletonAction );
procedure zmf_ReadSkeleton( var Memory : zglTMemory; var Skeleton : zglTSkeletonFrame );

{$IFDEF ZMF_WRITE_DATA}
procedure zmf_WriteHeader( var Memory : zglTMemory; zmfHeader : zglTZMFHeader );
procedure zmf_WriteVertices( var Memory : zglTMemory; Vertices : array of zglTPoint3D );
procedure zmf_WriteNormals( var Memory : zglTMemory; Normals : array of zglTPoint3D  );
procedure zmf_WriteTexCoords( var Memory : zglTMemory; TexCoords : array of zglTPoint2D  );
procedure zmf_WriteFaces( var Memory : zglTMemory; Faces : array of zglTFace );
procedure zmf_WriteGroups( var Memory : zglTMemory; Groups : array of zglTGroup );
procedure zmf_WriteFrame( var Memory : zglTMemory; Frame : zglTFrame );
procedure zmf_WritePackedVertices( var Memory : zglTMemory; Vertices : array of zglTPoint3D; Size : Byte );
procedure zmf_WritePackedNormals( var Memory : zglTMemory; Normals : array of zglTPoint3D; Size : Byte );
procedure zmf_WritePackedTexCoords( var Memory : zglTMemory; TexCoords : array of zglTPoint2D; Size : Byte );
procedure zmf_WriteFacesW( var Memory : zglTMemory; Faces : array of zglTFace );
procedure zmf_WriteGroupsW( var Memory : zglTMemory; Groups : array of zglTGroup );
procedure zmf_WriteFrameW( var Memory : zglTMemory; Frame : zglTFrame );
procedure zmf_WriteBones( var Memory : zglTMemory; Bones : array of zglTBone );
procedure zmf_WriteWeights( var Memory : zglTMemory; Weights : zglTBonesWeights; WCount : array of Byte );
procedure zmf_WriteAction( var Memory : zglTMemory; Action : zglTSkeletonAction );
procedure zmf_WriteSkeleton( var Memory : zglTMemory; Skeleton : zglTSkeletonFrame );
{$ENDIF}

var
  zmfHeader : zglTZMFHeader;

implementation

function zmf_ReadHeader;
begin
  mem_Read( Memory, zmfHeader, SizeOf( zglTZMFHeader ) );
  if zmfHeader.ID <> ZMF_ID Then
    Result := FALSE
  else
    Result := TRUE;
end;

procedure zmf_ReadVertices;
begin
  mem_Read( Memory, Vertices[ 0 ], SizeOf( zglTPoint3D ) * zmfHeader.VCount );
end;

procedure zmf_ReadNormals;
begin
  mem_Read( Memory, Normals[ 0 ], SizeOf( zglTPoint3D ) * zmfHeader.VCount );
end;

procedure zmf_ReadTexCoords;
  var
    i : DWORD;
begin
  if Layer = 1 Then
    mem_Read( Memory, TexCoords[ 0 ], SizeOf( zglTPoint2D ) * zmfHeader.TCount )
  else
    for i := 0 to zmfHeader.VCount - 1 do
      mem_Read( Memory, TexCoords[ i - zmfHeader.VCount * ( Layer - 2 ) ], SizeOf( zglTPoint2D ) );
end;

procedure zmf_ReadFaces;
begin
  mem_Read( Memory, Faces[ 0 ], SizeOf( zglTFace ) * zmfHeader.FCount );
end;

procedure zmf_ReadGroups;
  var
    i : DWORD;
begin
  for i := 0 to zmfHeader.GCount - 1 do
    begin
      mem_Read( Memory, Groups[ i ].FCount, 4 );
      mem_Read( Memory, Groups[ i ].IFace, 4 );
    end;
end;

procedure zmf_ReadFrame;
begin
end;

procedure zmf_ReadPackedVertices;
begin
end;

procedure zmf_ReadPackedNormals;
begin
end;

procedure zmf_ReadPackedTexCoords;
begin
end;

procedure zmf_ReadFacesW;
begin
end;

procedure zmf_ReadGroupsW;
begin
end;

procedure zmf_ReadFrameW;
begin
end;

procedure zmf_ReadBones;
  var
    i : Integer;
    j : Byte;
begin
  for i := 0 to zmfHeader.BCount - 1 do
    begin
      mem_Read( Memory, j, 1 );
      SetLength( Bones[ i ].Name, j );
      mem_Read( Memory, Bones[ i ].Name[ 1 ], j );
      mem_Read( Memory, Bones[ i ].Parent, 4 );
    end;
end;

procedure zmf_ReadWeights;
  var
    i, j : Integer;
begin
  for i := 0 to zmfHeader.VCount - 1 do
    for j := 0 to WCount[ i ] - 1 do
      begin
        mem_Read( Memory, Weights[ i ][ j ].boneID, 4 );
        mem_Read( Memory, Weights[ i ][ j ].Weight, 4 );
      end;
end;

procedure zmf_ReadAction;
  var
    i, j : Integer;
    c    : DWORD;
begin
  for i := 0 to Action.FCount - 1 do
    begin
      mem_Read( Memory, c, 4 );
      SetLength( Action.Frames[ i ].BonePos, c );
      mem_Read( Memory, Action.FPS, 4 );
      for j := 0 to c - 1 do
        begin
          mem_Read( Memory, Action.Frames[ i ].BonePos[ j ].Translation, SizeOf( zglTPoint3D ) );
          mem_Read( Memory, Action.Frames[ i ].BonePos[ j ].Rotation,    SizeOf( zglTPoint3D ) );
        end;
    end;
end;

procedure zmf_ReadSkeleton;
  var
    i, j : Integer;
begin
  mem_Read( Memory, j, 4 );
  for i := 0 to j - 1 do
    begin
      mem_Read( Memory, Skeleton.BonePos[ i ].Translation, SizeOf( zglTPoint3D ) );
      mem_Read( Memory, Skeleton.BonePos[ i ].Rotation,    SizeOf( zglTPoint3D ) );
    end;
end;

{------------------------------------------------------------------------------}
{--------------------------------- Write Data ---------------------------------}
{------------------------------------------------------------------------------}
{$IFDEF ZMF_WRITE_DATA}
procedure zmf_WriteHeader;
begin
  mem_Write( Memory, zmfHeader, SizeOf( zglTZMFHeader ) );
end;

procedure zmf_WriteVertices;
begin
  mem_Write( Memory, ZMF_VERTICES, 1 );
  mem_Write( Memory, Vertices[ 0 ], SizeOf( zglTPoint3D ) * length( Vertices ) );
end;

procedure zmf_WriteNormals;
begin
  mem_Write( Memory, ZMF_NORMALS, 1 );
  mem_Write( Memory, Normals[ 0 ], SizeOf( zglTPoint3D ) * length( Normals ) );
end;

procedure zmf_WriteTexCoords;
begin
  mem_Write( Memory, ZMF_TEXCOORDS, 1 );
  mem_Write( Memory, TexCoords[ 0 ], SizeOf( zglTPoint2D ) * length( Texcoords ) );
end;

procedure zmf_WriteFaces;
begin
  mem_Write( Memory, ZMF_FACES, 1 );
  mem_Write( Memory, Faces[ 0 ], SizeOf( zglTFace ) * length( Faces ) );
end;

procedure zmf_WriteGroups;
  var
    i : DWORD;
begin
  mem_Write( Memory, ZMF_GROUPS, 1 );
  for i := 0 to length( Groups ) - 1 do
    begin
      mem_Write( Memory, Groups[ i ].FCount, 4 );
      mem_Write( Memory, Groups[ i ].IFace, 4 );
    end;
end; 

procedure zmf_WriteFrame;
begin
end;

procedure zmf_WritePackedVertices;
begin
end;

procedure zmf_WritePackedNormals;
begin
end;

procedure zmf_WritePackedTexCoords;
begin
end;

procedure zmf_WriteFacesW;
begin
end;

procedure zmf_WriteGroupsW;
begin
end;

procedure zmf_WriteFrameW;
begin
end;

procedure zmf_WriteBones;
  var
    i : Integer;
    j : Byte;
begin
  mem_Write( Memory, ZMF_BONES, 1 );
  for i := 0 to length( Bones ) - 1 do
    begin
      j := length( Bones[ i ].Name );
      mem_Write( Memory, j, 1 );
      mem_Write( Memory, Bones[ i ].Name[ 1 ], j );
      mem_Write( Memory, Bones[ i ].Parent, 4 );
    end;
end;

procedure zmf_WriteWeights;
  var
    i, j : Integer;
begin
  mem_Write( Memory, ZMF_WEIGHTS, 1 );
  mem_Write( Memory, WCount[ 0 ], length( WCount ) );
  for i := 0 to length( WCount ) - 1 do
    for j := 0 to WCount[ i ] - 1 do
    begin
      mem_Write( Memory, Weights[ i ][ j ].boneID, 4 );
      mem_Write( Memory, Weights[ i ][ j ].Weight, 4 );
    end;
end;

procedure zmf_WriteAction;
  var
    i, j : Integer;
begin
  mem_Write( Memory, ZMF_ACTION, 1 );
  mem_Write( Memory, Action.FCount, 4 );
  for i := 0 to Action.FCount - 1 do
    begin
      j := length( Action.Frames[ i ].BonePos );
      mem_Write( Memory, j, 4 );
      mem_Write( Action.FPS );
      for j := 0 to length( Action.Frames[ i ].BonePos ) - 1 do
        begin
          mem_Write( Memory, Action.Frames[ i ].BonePos[ j ].Translation, SizeOf( zglTPoint3D ) );
          mem_Write( Memory, Action.Frames[ i ].BonePos[ j ].Rotation,    SizeOf( zglTPoint3D ) );
          mem_Write( Memory, Action.Frames[ i ].BonePos[ j ].Point,       SizeOf( zglTPoint3D ) );
        end;
    end;
end;

procedure zmf_WriteSkeleton;
  var
    i, j : Integer;
begin
  mem_Write( Memory, ZMF_SKELETON, 1 );
  j := length( Skeleton.BonePos );
  mem_Write( Memory, j, 4 );
  for i := 0 to j - 1 do
    begin
      mem_Write( Memory, Skeleton.BonePos[ i ].Point,       SizeOf( zglTPoint3D ) );
      mem_Write( Memory, Skeleton.BonePos[ i ].Translation, SizeOf( zglTPoint3D ) );
      mem_Write( Memory, Skeleton.BonePos[ i ].Rotation,    SizeOf( zglTPoint3D ) );
    end;
end;
{$ENDIF}

end.
