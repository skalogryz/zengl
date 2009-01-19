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
  zgl_timers,
  zgl_math;

procedure BuildIndices( const FCount : DWORD; var Faces : array of zglTFace; var Indices : Pointer; const Size : Byte );
procedure BuildSNormals( const FCount, VCount : DWORD; const Faces : array of zglTFace; var Vertices, Normals : array of zglTPoint3D );

function  CalcRVC( const VCount : DWORD; const Vertices : array of zglTPoint3D; var RIndices : array of DWORD ) : DWORD;
procedure CalcFrame( var Delta, prevDelta, outDelta : Single; var Time : Double; var Frame, prevFrame, nextFrame : Integer; const FFrame, FCount : Integer );

implementation

procedure BuildIndices;
  var
    i, j : DWORD;
begin
  Indices := AllocMem( FCount * Size * 3 );
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
  for i := 0 to VCount - 1 do
    begin
      for j := 0 to FCount - 1 do
        if ( ( Vertices[ Faces[ j, 0 ] ].X = Vertices[ i ].X ) and
             ( Vertices[ Faces[ j, 0 ] ].Y = Vertices[ i ].Y ) and
             ( Vertices[ Faces[ j, 0 ] ].Z = Vertices[ i ].Z ) ) or
           ( ( Vertices[ Faces[ j, 1 ] ].X = Vertices[ i ].X ) and
             ( Vertices[ Faces[ j, 1 ] ].Y = Vertices[ i ].Y ) and
             ( Vertices[ Faces[ j, 1 ] ].Z = Vertices[ i ].Z ) ) or
           ( ( Vertices[ Faces[ j, 2 ] ].X = Vertices[ i ].X ) and
             ( Vertices[ Faces[ j, 2 ] ].Y = Vertices[ i ].Y ) and
             ( Vertices[ Faces[ j, 2 ] ].Z = Vertices[ i ].Z ) ) Then
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

function CalcRVC;
  var
    i, j : DWORD;
begin
  Result := 0;
  for i := 0 to VCount - 1 do
    for j := 0 to VCount - 1 do
      if ( Vertices[ i ].X = Vertices[ j ].X ) and
         ( Vertices[ i ].Y = Vertices[ j ].Y ) and
         ( Vertices[ i ].Z = Vertices[ j ].Z ) Then
        begin
          RIndices[ i ] := j;
          if ( j <> i ) and ( Result = 0 ) Then Result := i;
          break;
        end;
end;

procedure CalcFrame;
  var
    b : Boolean;
begin
  if Time = 0 Then
    begin
      Delta     := 0;
      prevDelta := 0;
    end;
  Time := timer_GetTicks;

  b := Delta < prevDelta;
  while Delta < 0 do
    begin
      Delta := Delta + 1;
      DEC( Frame );
    end;
  while Delta >= 1 do
    begin
      Delta := Delta - 1;
      INC( Frame );
    end;
  while Frame < 0 do
    Frame := Frame + FCount;
  while Frame > FCount - 1 do
    Frame := Frame - ( FCount - 1 );

  if b Then
    begin
      prevFrame := FFrame + Frame;
      nextFrame := prevFrame - 1;
      outDelta  := 1 - Delta;

      if nextFrame < FFrame Then
        nextFrame := nextFrame + FCount;
    end else
      begin
        prevFrame := FFrame + Frame;
        nextFrame := prevFrame + 1;
        outDelta  := Delta;

        if nextFrame > FFrame + FCount - 1 Then
          nextFrame := nextFrame - ( FCount - 1 );
      end;
  prevDelta := Delta;
end;

end.
