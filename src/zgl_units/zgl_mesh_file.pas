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
  ZMF_PACKED_FRAME      = $0C;
  ZMF_BONES             = $0D;
  ZMF_BONEPOS           = $0E;
  ZMF_WEIGHTS           = $0F;
  ZMF_ACTION            = $10;

function  zmf_ReadHeader( var Memory : zglTMemory ) : Boolean;
procedure zmf_ReadVertices( var Memory : zglTMemory; var Vertices : array of zglTPoint3D );
procedure zmf_ReadNormals( var Memory : zglTMemory; var Normals : array of zglTPoint3D );
procedure zmf_ReadTexCoords( var Memory : zglTMemory; var TexCoords : array of zglTPoint2D; Layer : Byte );
procedure zmf_ReadFaces( var Memory : zglTMemory; var Faces : array of zglTFace );
procedure zmf_ReadGroups( var Memory : zglTMemory; var Groups : array of zglTGroup );
procedure zmf_ReadFrame( var Memory : zglTMemory; var Vertices, Normals : array of zglTPoint3D );
procedure zmf_ReadPackedVertices( var Memory : zglTMemory; var Vertices : array of zglTPoint3D );
procedure zmf_ReadPackedNormals( var Memory : zglTMemory; var Normals : array of zglTPoint3D );
procedure zmf_ReadPackedTexCoords( var Memory : zglTMemory; var TexCoords : array of zglTPoint2D; Layer : Byte );
procedure zmf_ReadFacesW( var Memory : zglTMemory; var Faces : array of zglTFace );
procedure zmf_ReadGroupsW( var Memory : zglTMemory; var Groups : array of zglTGroup );
procedure zmf_ReadPackedFrame( var Memory : zglTMemory; var Vertices, Normals : array of zglTPoint3D );
procedure zmf_ReadBones( var Memory : zglTMemory; var Bones : array of zglTBone );
procedure zmf_ReadBonePos( var Memory : zglTMemory; var BonePos : array of zglTBonePos );
procedure zmf_ReadWeights( var Memory : zglTMemory; var Weights : zglTBonesWeights; WCount : array of Byte );
procedure zmf_ReadAction( var Memory : zglTMemory; var Action : zglTSkeletonAction );

{$IFDEF ZMF_WRITE_DATA}
procedure zmf_WriteHeader( var Memory : zglTMemory; zmfHeader : zglTZMFHeader );
procedure zmf_WriteVertices( var Memory : zglTMemory; VCount : DWORD; Vertices : array of zglTPoint3D );
procedure zmf_WriteNormals( var Memory : zglTMemory; VCount : DWORD; Normals : array of zglTPoint3D  );
procedure zmf_WriteTexCoords( var Memory : zglTMemory; TCount : DWORD; TexCoords : array of zglTPoint2D  );
procedure zmf_WriteFaces( var Memory : zglTMemory; Faces : array of zglTFace );
procedure zmf_WriteGroups( var Memory : zglTMemory; Groups : array of zglTGroup );
procedure zmf_WriteFrame( var Memory : zglTMemory; VCount, VPerFrame, Frame : DWORD; Vertices, Normals : array of zglTPoint3D );
procedure zmf_WritePackedVertices( var Memory : zglTMemory; VCount : DWORD; Vertices : array of zglTPoint3D );
procedure zmf_WritePackedNormals( var Memory : zglTMemory; VCount : DWORD; Normals : array of zglTPoint3D );
procedure zmf_WritePackedTexCoords( var Memory : zglTMemory; TCount : DWORD; TexCoords : array of zglTPoint2D );
procedure zmf_WriteFacesW( var Memory : zglTMemory; Faces : array of zglTFace );
procedure zmf_WriteGroupsW( var Memory : zglTMemory; Groups : array of zglTGroup );
procedure zmf_WritePackedFrame( var Memory : zglTMemory; VCount, VPerFrame, Frame : DWORD; Vertices, Normals : array of zglTPoint3D );
procedure zmf_WriteBones( var Memory : zglTMemory; Bones : array of zglTBone );
procedure zmf_WriteBonePos( var Memory : zglTMemory; Count : Integer; BonePos : array of zglTBonePos );
procedure zmf_WriteWeights( var Memory : zglTMemory; Weights : zglTBonesWeights; WCount : array of Byte );
procedure zmf_WriteAction( var Memory : zglTMemory; Action : zglTSkeletonAction );
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
  var
    i, j : DWORD;
begin
  mem_Read( Memory, i, 4 );
  mem_Read( Memory, j, 4 );
  mem_Read( Memory, Vertices[ ( i - 1 ) * zmfHeader.VCount ], SizeOf( zglTPoint3D ) * j );
  if zmfHeader.Flags and USE_NORMALS > 0 Then
    mem_Read( Memory, Normals[ ( i - 1 ) * zmfHeader.VCount ], SizeOf( zglTPoint3D ) * j );
end;

procedure zmf_ReadPackedVertices;
  var
    i : DWORD;
    j : WORD;
    x, y, z : Single;
    tx, ty, tz : Single;
begin
  mem_Read( Memory, x, 4 );
  mem_Read( Memory, y, 4 );
  mem_Read( Memory, z, 4 );
  mem_Read( Memory, tx, 4 );
  mem_Read( Memory, ty, 4 );
  mem_Read( Memory, tz, 4 );
  for i := 0 to zmfHeader.VCount - 1 do
    begin
      mem_Read( Memory, j, 2 );
      Vertices[ i ].X := j / x + tx;
      mem_Read( Memory, j, 2 );
      Vertices[ i ].Y := j / y + ty;
      mem_Read( Memory, j, 2 );
      Vertices[ i ].Z := j / z + tz;
    end;
end;

procedure zmf_ReadPackedNormals;
  var
    i : DWORD;
    j : WORD;
    x, y, z : Single;
    tx, ty, tz : Single;
begin
  mem_Read( Memory, x, 4 );
  mem_Read( Memory, y, 4 );
  mem_Read( Memory, z, 4 );
  mem_Read( Memory, tx, 4 );
  mem_Read( Memory, ty, 4 );
  mem_Read( Memory, tz, 4 );
  for i := 0 to zmfHeader.VCount - 1 do
    begin
      mem_Read( Memory, j, 2 );
      Normals[ i ].X := j / x + tx;
      mem_Read( Memory, j, 2 );
      Normals[ i ].Y := j / y + ty;
      mem_Read( Memory, j, 2 );
      Normals[ i ].Z := j / z + tz;
    end;
end;

procedure zmf_ReadPackedTexCoords;
  var
    i : DWORD;
    j : WORD;
    x, y : Single;
begin
  mem_Read( Memory, x, 4 );
  mem_Read( Memory, y, 4 );
  for i := 0 to zmfHeader.TCount - 1 do
    begin
      mem_Read( Memory, j, 2 );
      TexCoords[ i ].X := j / x;
      mem_Read( Memory, j, 2 );
      TexCoords[ i ].Y := j / y;
    end;
end;

procedure zmf_ReadFacesW;
  var
    i : DWORD;
    j : WORD;
begin
  for i := 0 to zmfHeader.FCount - 1 do
    begin
      mem_Read( Memory, j, 2 );
      Faces[ i, 0 ] := j;
      mem_Read( Memory, j, 2 );
      Faces[ i, 1 ] := j;
      mem_Read( Memory, j, 2 );
      Faces[ i, 2 ] := j;
    end;
end;

procedure zmf_ReadGroupsW;
begin
end;

procedure zmf_ReadPackedFrame;
  var
    i, j : DWORD;
    f    : DWORD;
    k : WORD;
    x, y, z : Single;
    tx, ty, tz : Single;
begin
  mem_Read( Memory, f, 4 );
  mem_Read( Memory, j, 4 );

  mem_Read( Memory, x, 4 );
  mem_Read( Memory, y, 4 );
  mem_Read( Memory, z, 4 );
  mem_Read( Memory, tx, 4 );
  mem_Read( Memory, ty, 4 );
  mem_Read( Memory, tz, 4 );
  for i := 0 to j - 1 do
    begin
      mem_Read( Memory, k, 2 );
      Vertices[ ( f - 1 ) * zmfHeader.VCount + i ].X := k / x + tx;
      mem_Read( Memory, k, 2 );
      Vertices[ ( f - 1 ) * zmfHeader.VCount + i ].Y := k / y + ty;
      mem_Read( Memory, k, 2 );
      Vertices[ ( f - 1 ) * zmfHeader.VCount + i ].Z := k / z + tz;
    end;

  if zmfHeader.Flags and USE_NORMALS > 0 Then
    mem_Read( Memory, Normals[ ( f - 1 ) * zmfHeader.VCount ], SizeOf( zglTPoint3D ) * j );
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

procedure zmf_ReadBonePos;
  var
    i, j : Integer;
begin
  mem_Read( Memory, j, 4 );
  for i := 0 to j - 1 do
    begin
      mem_Read( Memory, BonePos[ i ].Translation, SizeOf( zglTPoint3D ) );
      mem_Read( Memory, BonePos[ i ].Rotation,    SizeOf( zglTPoint3D ) );
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
      SetLength( Action.Frames[ i ], c );
      mem_Read( Memory, Action.FPS, 4 );
      for j := 0 to c - 1 do
        begin
          mem_Read( Memory, Action.Frames[ i, j ].Translation, SizeOf( zglTPoint3D ) );
          mem_Read( Memory, Action.Frames[ i, j ].Rotation,    SizeOf( zglTPoint3D ) );
        end;
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
  mem_Write( Memory, Vertices[ 0 ], SizeOf( zglTPoint3D ) * VCount );
end;

procedure zmf_WriteNormals;
begin
  mem_Write( Memory, ZMF_NORMALS, 1 );
  mem_Write( Memory, Normals[ 0 ], SizeOf( zglTPoint3D ) * VCount );
end;

procedure zmf_WriteTexCoords;
begin
  mem_Write( Memory, ZMF_TEXCOORDS, 1 );
  mem_Write( Memory, TexCoords[ 0 ], SizeOf( zglTPoint2D ) * TCount );
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
  var
    i : DWORD;
begin
  i := Frame + 1;
  mem_Write( Memory, ZMF_FRAME, 1 );
  mem_Write( Memory, i, 4 );
  mem_Write( Memory, VPerFrame, 4 );
  mem_Write( Memory, Vertices[ Frame * VCount ], SizeOf( zglTPoint3D ) * VPerFrame );
  if zmfHeader.Flags and USE_NORMALS > 0 Then
    mem_Write( Memory, Normals[ Frame * VCount ], SizeOf( zglTPoint3D ) * VPerFrame );
end;

procedure zmf_WritePackedVertices;
  var
    i : DWORD;
    minX, minY, minZ : Single;
    maxX, maxY, maxZ : Single;
    x, y, z          : WORD;
begin
  mem_Write( Memory, ZMF_PACKED_VERTICES, 1 );
  minX := 0;
  minY := 0;
  minZ := 0;
  maxX := 0;
  maxY := 0;
  maxZ := 0;
  for i := 0 to VCount - 1 do
    begin
      if Vertices[ i ].X < minX Then minX := Vertices[ i ].X;
      if Vertices[ i ].Y < minY Then minY := Vertices[ i ].Y;
      if Vertices[ i ].Z < minZ Then minZ := Vertices[ i ].Z;

      if Vertices[ i ].X > maxX Then maxX := Vertices[ i ].X;
      if Vertices[ i ].Y > maxY Then maxY := Vertices[ i ].Y;
      if Vertices[ i ].Z > maxZ Then maxZ := Vertices[ i ].Z;
    end;
  maxX := 65535 / ( maxX - minX );
  maxY := 65535 / ( maxY - minY );
  maxZ := 65535 / ( maxZ - minZ );
  mem_Write( Memory, maxX, 4 );
  mem_Write( Memory, maxY, 4 );
  mem_Write( Memory, maxZ, 4 );
  mem_Write( Memory, minX, 4 );
  mem_Write( Memory, minY, 4 );
  mem_Write( Memory, minZ, 4 );
  for i := 0 to VCount - 1 do
    begin
      x := Round( ( Vertices[ i ].X - minX ) * maxX );
      mem_Write( Memory, x, 2 );

      y := Round( ( Vertices[ i ].Y - minY ) * maxY );
      mem_Write( Memory, y, 2 );

      z := Round( ( Vertices[ i ].Z - minZ ) * maxZ );
      mem_Write( Memory, z, 2 );
    end;
end;

procedure zmf_WritePackedNormals;
  var
    i : DWORD;
    minX, minY, minZ : Single;
    maxX, maxY, maxZ : Single;
    x, y, z          : WORD;
begin
  mem_Write( Memory, ZMF_PACKED_NORMALS, 1 );
  minX := 0;
  minY := 0;
  minZ := 0;
  maxX := 0;
  maxY := 0;
  maxZ := 0;
  for i := 0 to VCount - 1 do
    begin
      if Normals[ i ].X < minX Then minX := Normals[ i ].X;
      if Normals[ i ].Y < minY Then minY := Normals[ i ].Y;
      if Normals[ i ].Z < minZ Then minZ := Normals[ i ].Z;

      if Normals[ i ].X > maxX Then maxX := Normals[ i ].X;
      if Normals[ i ].Y > maxY Then maxY := Normals[ i ].Y;
      if Normals[ i ].Z > maxZ Then maxZ := Normals[ i ].Z;
    end;
  maxX := 65535 / ( maxX - minX );
  maxY := 65535 / ( maxY - minY );
  maxZ := 65535 / ( maxZ - minZ );
  mem_Write( Memory, maxX, 4 );
  mem_Write( Memory, maxY, 4 );
  mem_Write( Memory, maxZ, 4 );
  mem_Write( Memory, minX, 4 );
  mem_Write( Memory, minY, 4 );
  mem_Write( Memory, minZ, 4 );
  for i := 0 to VCount - 1 do
    begin
      x := Round( ( Normals[ i ].X - minX ) * maxX );
      mem_Write( Memory, x, 2 );

      y := Round( ( Normals[ i ].Y - minY ) * maxY );
      mem_Write( Memory, y, 2 );

      z := Round( ( Normals[ i ].Z - minZ ) * maxZ );
      mem_Write( Memory, z, 2 );
    end;
end;

procedure zmf_WritePackedTexCoords;
  var
    i : DWORD;
    maxX, maxY : Single;
    x, y       : WORD;
begin
  mem_Write( Memory, ZMF_PACKED_TEXCOORDS, 1 );
  maxX := 0;
  maxY := 0;
  for i := 0 to TCount - 1 do
    begin
      if TexCoords[ i ].X > maxX Then maxX := TexCoords[ i ].X;
      if TexCoords[ i ].Y > maxY Then maxY := TexCoords[ i ].Y;
    end;
  maxX := 65535 / maxX;
  maxY := 65535 / maxY;
  mem_Write( Memory, maxX, 4 );
  mem_Write( Memory, maxY, 4 );
  for i := 0 to TCount - 1 do
    begin
      x := Round( TexCoords[ i ].X * maxX );
      mem_Write( Memory, x, 2 );

      y := Round( TexCoords[ i ].Y * maxY );
      mem_Write( Memory, y, 2 );
    end;
end;

procedure zmf_WriteFacesW;
  var
    i : DWORD;
    j : WORD;
begin
  mem_Write( Memory, ZMF_FACES_RANGE_WORD, 1 );
  for i := 0 to length( Faces ) - 1 do
    begin
      j := Faces[ i, 0 ];
      mem_Write( Memory, j, 2 );
      j := Faces[ i, 1 ];
      mem_Write( Memory, j, 2 );
      j := Faces[ i, 2 ];
      mem_Write( Memory, j, 2 );
    end;
end;

procedure zmf_WriteGroupsW;
begin
end;

procedure zmf_WritePackedFrame;
  var
    i : DWORD;
    minX, minY, minZ : Single;
    maxX, maxY, maxZ : Single;
    x, y, z          : WORD;
begin
  i := Frame + 1;
  mem_Write( Memory, ZMF_PACKED_FRAME, 1 );
  mem_Write( Memory, i, 4 );
  mem_Write( Memory, VPerFrame, 4 );

  minX := 0;
  minY := 0;
  minZ := 0;
  maxX := 0;
  maxY := 0;
  maxZ := 0;
  for i := 0 to VPerFrame - 1 do
    begin
      if Vertices[ Frame * VCount + i ].X < minX Then minX := Vertices[ Frame * VCount + i ].X;
      if Vertices[ Frame * VCount + i ].Y < minY Then minY := Vertices[ Frame * VCount + i ].Y;
      if Vertices[ Frame * VCount + i ].Z < minZ Then minZ := Vertices[ Frame * VCount + i ].Z;

      if Vertices[ Frame * VCount + i ].X > maxX Then maxX := Vertices[ Frame * VCount + i ].X;
      if Vertices[ Frame * VCount + i ].Y > maxY Then maxY := Vertices[ Frame * VCount + i ].Y;
      if Vertices[ Frame * VCount + i ].Z > maxZ Then maxZ := Vertices[ Frame * VCount + i ].Z;
    end;
  maxX := 65535 / ( maxX - minX );
  maxY := 65535 / ( maxY - minY );
  maxZ := 65535 / ( maxZ - minZ );
  mem_Write( Memory, maxX, 4 );
  mem_Write( Memory, maxY, 4 );
  mem_Write( Memory, maxZ, 4 );
  mem_Write( Memory, minX, 4 );
  mem_Write( Memory, minY, 4 );
  mem_Write( Memory, minZ, 4 );
  for i := 0 to VPerFrame - 1 do
    begin
      x := Round( ( Vertices[ Frame * VCount + i ].X - minX ) * maxX );
      mem_Write( Memory, x, 2 );

      y := Round( ( Vertices[ Frame * VCount + i ].Y - minY ) * maxY );
      mem_Write( Memory, y, 2 );

      z := Round( ( Vertices[ Frame * VCount + i ].Z - minZ ) * maxZ );
      mem_Write( Memory, z, 2 );
    end;

  if zmfHeader.Flags and USE_NORMALS > 0 Then
    mem_Write( Memory, Normals[ Frame * VCount ], SizeOf( zglTPoint3D ) * VPerFrame );
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

procedure zmf_WriteBonePos;
  var
    i : Integer;
begin
  mem_Write( Memory, ZMF_BONEPOS, 1 );
  mem_Write( Memory, Count, 4 );
  for i := 0 to Count - 1 do
    begin
      mem_Write( Memory, BonePos[ i ].Translation, SizeOf( zglTPoint3D ) );
      mem_Write( Memory, BonePos[ i ].Rotation,    SizeOf( zglTPoint3D ) );
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
      mem_Write( Memory, Weights[ i, j ].boneID, 4 );
      mem_Write( Memory, Weights[ i, j ].Weight, 4 );
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
      j := length( Action.Frames[ i ] );
      mem_Write( Memory, j, 4 );
      mem_Write( Memory, Action.FPS, 4 );
      for j := 0 to j - 1 do
        begin
          mem_Write( Memory, Action.Frames[ i, j ].Translation, SizeOf( zglTPoint3D ) );
          mem_Write( Memory, Action.Frames[ i, j ].Rotation,    SizeOf( zglTPoint3D ) );
        end;
    end;
end;
{$ENDIF}

end.
