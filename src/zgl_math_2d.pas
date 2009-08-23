{
 * Copyright © Kemka Andrey aka Andru
 * mail: dr.andru@gmail.com
 * site: http://andru-kun.inf.ua
 *
 * This file is part of ZenGL
 *
 * ZenGL is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * ZenGL is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
}
unit zgl_math_2d;

{$I zgl_config.cfg}

interface

const
  pi      = 3.1415926;
  rad2deg = 57.29578049;
  deg2rad = 0.017453292;

  ORIENTATION_LEFT  = -1;
  ORIENTATION_RIGHT = 1;
  ORIENTATION_ZERO  = 0;

type
  zglPPoint2D = ^zglTPoint2D;
  zglTPoint2D = record
    x, y : Single;
end;

type
  zglPPoints2D = ^zglTPoints2D;
  zglTPoints2D = array[ 0..0 ] of zglTPoint2D;

type
  zglPLine = ^zglTLine;
  zglTLine = record
    x0, y0 : Single;
    x1, y1 : Single;
end;

type
  zglPRect = ^zglTRect;
  zglTRect = record
    x, y, w, h : Single;
end;

type
  zglPCircle = ^zglTCircle;
  zglTCircle = record
    cX, cY : Single;
    radius : Single;
end;

procedure InitCosSinTables;
function  m_Cos( Angle : Integer ) : Single;
function  m_Sin( Angle : Integer ) : Single;
function  m_Distance( const x1, y1, x2, y2 : Single ) : Single;
function  m_FDistance( const x1, y1, x2, y2 : Single ) : Single;
function  m_Angle( const x1, y1, x2, y2 : Single ) : Single;
function  m_Orientation( const x, y, x1, y1, x2, y2 : Single ) : Integer;

procedure tess_Triangulate( const Contour : zglPPoints2D; const iLo, iHi : Integer; const AddHoles : Boolean = FALSE );
procedure tess_AddHole( const Contour : zglPPoints2D; const iLo, iHi : Integer; const LastHole : Boolean = TRUE );
function  tess_GetData( var TriPoints : zglPPoints2D ) : Integer;

var
  CosTable : array[ 0..360 ] of Single;
  SinTable : array[ 0..360 ] of Single;

implementation
uses
  zgl_main,
  zgl_opengl_all,
  zgl_log,
  zgl_utils;

var
  tess        : Integer;
  tessMode    : Integer;
  tessHoles   : Boolean;
  tessFinish  : Boolean;
  tessCurrent : Integer;
  tessVertex  : array[ 0..2 ] of zglTPoint2D;
  tessVCount  : Integer;
  tessVerts   : array of zglTPoint2D;

function ArcTan2( const dx, dy : Single ) : Single;
begin
  Result := abs( ArcTan( dy / dx ) * ( 180 / pi ) );
end;

procedure InitCosSinTables;
  var
    i         : Integer;
    rad_angle : Single;
begin
  for i := 0 to 360 do
    begin
      rad_angle := i * ( pi / 180 );
      CosTable[ i ] := cos( rad_angle );
      SinTable[ i ] := sin( rad_angle );
    end;
end;

function m_Cos;
begin
  while Angle > 360 do Angle := Angle - 360;
  while Angle < 0   do Angle := Angle + 360;
  Result := CosTable[ Angle ];
end;

function m_Sin;
begin
  while Angle > 360 do Angle := Angle - 360;
  while Angle < 0   do Angle := Angle + 360;
  Result := SinTable[ Angle ];
end;

function m_Distance;
begin
  Result := Sqrt( sqr( x1 - x2 ) + sqr( y1 - y2 ) );
end;

function m_FDistance;
begin
  Result := sqr( x1 - x2 ) + sqr( y1 - y2 );
end;

function m_Angle;
  var
    dx, dy : Single;
begin
  dx := ( X1 - X2 );
  dy := ( Y1 - Y2 );

  if dx = 0 Then
    begin
      if dy > 0 Then
        Result := 90
      else
        Result := 270;
      exit;
    end;

  if dy = 0 Then
    begin
      if dx > 0 Then
        Result := 0
      else
        Result := 180;
      exit;
    end;

  if ( dx < 0 ) and ( dy > 0 ) Then { 2nd quad }
    Result := 180 - ArcTan2( dx, dy )
  else
    if ( dx < 0 ) and ( dy < 0 ) Then { 3rd quad }
      Result := 180 + ArcTan2( dx, dy )
    else
      if ( dx > 0 ) and ( dy < 0 ) Then { 4th quad }
        Result := 360 - ArcTan2( dx, dy )
      else                                { 1st quad }
        Result := ArcTan2( dx, dy )
end;

function m_Orientation;
  var
    Orientation : Single;
begin
  Orientation := ( x2 - x1 ) * ( y - y1 ) - ( x - x1 ) * ( y2 - y1 );

  if Orientation > 0 Then
    Result := ORIENTATION_RIGHT
  else
    if Orientation < 0 Then
      Result := ORIENTATION_LEFT
    else
      Result := ORIENTATION_ZERO;
end;

// GLU Triangulation
{$IFDEF LINUX_OR_DARWIN}
  {$DEFINE stdcall := cdecl}
{$ENDIF}

procedure tessBegin( Mode : Integer ); stdcall;
begin
  tessMode    := Mode;
  tessCurrent := 0;
end;

procedure tessVertex2f( Vertex : zglPPoint2D ); stdcall;
begin
  if not Assigned( Vertex ) Then exit;

  if tessVCount + 3 > length( tessVerts ) Then
    SetLength( tessVerts, length( tessVerts ) + 65536 );

  tessVertex[ tessCurrent ] := Vertex^;
  INC( tessCurrent );
  if tessCurrent <> 3 Then exit;

  case tessMode of
    GL_TRIANGLES:
      begin
        tessVerts[ tessVCount ] := tessVertex[ 0 ]; INC( tessVCount );
        tessVerts[ tessVCount ] := tessVertex[ 1 ]; INC( tessVCount );
        tessVerts[ tessVCount ] := tessVertex[ 2 ]; INC( tessVCount );

        tessCurrent := 0;
      end;
    GL_TRIANGLE_STRIP:
      begin
        tessVerts[ tessVCount ] := tessVertex[ 1 ]; INC( tessVCount );
        tessVerts[ tessVCount ] := tessVertex[ 2 ]; INC( tessVCount );
        tessVerts[ tessVCount ] := tessVertex[ 0 ]; INC( tessVCount );

        tessVertex[ 0 ] := tessVertex[ 1 ];
        tessVertex[ 1 ] := tessVertex[ 2 ];
        tessCurrent    := 2;
      end;
    GL_TRIANGLE_FAN:
      begin
        tessVerts[ tessVCount ] := tessVertex[ 0 ]; INC( tessVCount );
        tessVerts[ tessVCount ] := tessVertex[ 1 ]; INC( tessVCount );
        tessVerts[ tessVCount ] := tessVertex[ 2 ]; INC( tessVCount );

        tessVertex[ 1 ] := tessVertex[ 2 ];
        tessCurrent    := 2;
      end;
  end;
end;

procedure tess_Triangulate;
  var
    i : Integer;
    v : array[ 0..2 ] of Double;
begin
  tessFinish  := FALSE;
  tessHoles   := AddHoles;
  tessVCount  := 0;
  tessCurrent := 0;
  v[ 2 ]      := 0;

  gluTessBeginPolygon( tess, nil );
  gluTessBeginContour( tess );
  for i := iLo to iHi do
    begin
      v[ 0 ] := Contour[ i ].X;
      v[ 1 ] := Contour[ i ].Y;
      gluTessVertex( tess, @v[ 0 ], @Contour[ i ] );
    end;
  gluTessEndContour( tess );
  if not AddHoles Then
    gluTessEndPolygon( tess );
end;

procedure tess_AddHole;
  var
    i : Integer;
    v : array[ 0..2 ] of Double;
begin
  if not tessHoles Then exit;
  v[ 2 ] := 0;

  gluTessBeginContour( tess );
  for i := iLo to iHi do
    begin
      v[ 0 ] := Contour[ i ].X;
      v[ 1 ] := Contour[ i ].Y;
      gluTessVertex( tess, @v[ 0 ], @Contour[ i ] );
    end;
  gluTessEndContour( tess );
  if LastHole Then
    begin
      tessFinish := TRUE;
      tessHoles  := FALSE;
      gluTessEndPolygon( tess );
    end;
end;

function tess_GetData;
begin
  if not tessFinish Then
    begin
      tessFinish := TRUE;
      gluTessEndPolygon( tess );
    end;
  FreeMemory( TriPoints );
  zgl_GetMem( Pointer( TriPoints ), tessVCount * SizeOf( zglTPoint2D ) );
  Move( tessVerts[ 0 ], TriPoints[ 0 ], tessVCount * SizeOf( zglTPoint2D ) );
  Result := tessVCount;
end;

initialization
  InitCosSinTables;
  tess := gluNewTess;
  gluTessCallBack( tess, GLU_TESS_BEGIN,  @tessBegin    );
  gluTessCallBack( tess, GLU_TESS_VERTEX, @tessVertex2f );

finalization
  gluDeleteTess( tess );

end.
