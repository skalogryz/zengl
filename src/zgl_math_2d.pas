{
 * Copyright © Kemka Andrey aka Andru
 * mail: dr.andru@gmail.com
 * site: http://andru-kun.ru
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
uses
  zgl_const;

const
  pi      = 3.1415926;
  rad2deg = 0.017453292;

type
  zglPPoint2D = ^zglTPoint2D;
  zglTPoint2D = record
    x, y : Single;
end;

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

var
  CosTable : array[ 0..360 ] of Single;
  SinTable : array[ 0..360 ] of Single;

implementation

function ArcTan2( const dx, dy : Single ) : Single;
begin
  Result := abs( ArcTan( dy / dx ) * ( 180 / 3.14159 ) );
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

initialization
  InitCosSinTables;

end.
