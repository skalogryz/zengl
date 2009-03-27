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
unit zgl_collision_2d;

{$I zgl_config.cfg}

interface

uses
  zgl_math_2d;

// point 2d
function col2d_PointInRect  ( const X, Y : Single; const Rect : zglTRect   ) : Boolean;
function col2d_PointInCircle( const X, Y : Single; const Circ : zglTCircle ) : Boolean;
// line 2d
function col2d_Line          ( const A, B : zglTLine ) : Boolean;
function col2d_LineVsRect    ( const A : zglTLine; const Rect : zglTRect ) : Boolean;
function col2d_LineVsCircle  ( const L : zglTLine; const Circ : zglTCircle ) : Boolean;
function col2d_LineVsCircleXY( const L : zglTLine; const Circ : zglTCircle; const Precision : Byte ) : Boolean;
// rect
function col2d_Rect        ( const Rect1, Rect2 : zglTRect ) : Boolean;
function col2d_ClipRect    ( const Rect1, Rect2 : zglTRect ) : zglTRect;
function col2d_RectInRect  ( const Rect1, Rect2 : zglTRect ) : Boolean;
function col2d_RectInCircle( const Rect : zglTRect; const Circ : zglTCircle ) : Boolean;
function col2d_RectVsCircle( const Rect : zglTRect; const Circ : zglTCircle ) : Boolean;
// circle
function col2d_Circle        ( const Circ1, Circ2 : zglTCircle ) : Boolean;
function col2d_CircleInCircle( const Circ1, Circ2 : zglTCircle ) : Boolean;
function col2d_CircleInRect  ( const Circ : zglTCircle; const Rect : zglTRect ) : Boolean;
// extended
function col2dEx_LastX : Single;
function col2dEx_LastY : Single;
// line
procedure col2dEx_CalcLineCross( const A, B : zglTLine );

implementation

var
  lX, lY   : Single;
  lS, lT   : Single;

function Dist( Ax, Ay, Bx, By, Cx, Cy : Single ) : Single;
  var
    dx, dy : Single;
    D      : Single;
begin
  dx := Ax - Bx;
  dy := Ay - By;

  D := dx * ( Cy - Ay ) - dy * ( Cx - Ax );

  if D > 0 Then
    begin
      Result := sqr( D ) / ( sqr( dx ) + sqr( dy ) );
      Result := sqr( Result );
    end else
      begin
        Result := sqr( D ) / ( sqr( dx ) + sqr( dy ) );
        Result := -sqr( Result );
      end;
end;

function col2d_PointInRect;
begin
  Result := ( X > Rect.X ) and ( X < Rect.X + Rect.W ) and
            ( Y > Rect.Y ) and ( Y < Rect.Y + Rect.H );
end;

function col2d_PointInCircle;
begin
  Result := sqr( Circ.cX - X ) + sqr( Circ.cY - Y ) < sqr( Circ.Radius );
end;

function col2d_Line;
  var
    tmp    : Single;
    S1, S2 : array[ 0..1 ] of Single;
    s, t   : Single;
begin
  Result := FALSE;

  S1[ 0 ] := A.x1 - A.x0;
  S1[ 1 ] := A.y1 - A.y0;
  S2[ 0 ] := B.x1 - B.x0;
  S2[ 1 ] := B.y1 - B.y0;

  s := ( s2[ 0 ] * ( - S1[ 1 ] ) - ( - S1[ 0 ] ) * S2[ 1 ] );
  if s <> 0 Then
    tmp := 1 / ( S2[ 0 ] * ( - S1[ 1 ] ) - ( - S1[ 0 ] ) * S2[ 1 ] )
  else
    exit;

  s := ( ( A.x0 - B.x0 ) * ( - S1[ 1 ] ) - ( - S1[ 0 ] ) * ( A.y0 - B.y0 ) ) * tmp;
  t := ( S2[ 0 ] * ( A.y0 - B.y0 ) - ( A.x0 - B.x0 ) * S2[ 1 ] ) * tmp;

  Result := ( s >= 0 ) and ( s <= 1 ) and ( t >= 0 ) and ( t <= 1 );

  lS := s;
  lT := t;
end;

function col2d_LineVsRect;
  var
    L : zglTLine;
begin
  Result := col2d_PointInRect( A.x0, A.y0, Rect ) or col2d_PointInRect( A.x1, A.y1, Rect );
  if not Result Then
    begin
      L.x0 := Rect.X;
      L.y0 := Rect.Y;
      L.x1 := Rect.X + Rect.W;
      L.y1 := Rect.Y + Rect.H;
      Result := col2d_Line( A, L );
      if not Result Then
        begin
          L.x0 := Rect.X;
          L.y0 := Rect.Y + Rect.H;
          L.x1 := Rect.X + Rect.W;
          L.y1 := Rect.Y;
          Result := col2d_Line( A, L );
        end;
    end;
end;

function col2d_LineVsCircle;
  var
    p1, p2  : array[ 0..1 ] of Single;
    dx, dy  : Single;
    a, b, c : Single;
begin
  p1[ 0 ] := L.x0 - Circ.cX;
  p1[ 1 ] := L.y0 - Circ.cY;
  p2[ 0 ] := L.x1 - Circ.cX;
  p2[ 1 ] := L.y1 - Circ.cY;

  dx := p2[ 0 ] - p1[ 0 ];
  dy := p2[ 1 ] - p1[ 1 ];

  a := sqr( dx ) + sqr( dy );
  b := 2.0 * ( p1[ 0 ] * dx + p1[ 1 ] * dy );
  c := sqr( p1[ 0 ] ) + sqr( p1[ 1 ] ) - sqr( Circ.Radius );

  if -b < 0 Then
    Result := c < 0
  else
    if -b < ( 2.0 * a ) Then
      Result := 4.0 * a * c - sqr( b )  < 0
    else
      Result := a + b + c < 0;
end;

function col2d_LineVsCircleXY;
  var
    p1      : array of zglTPoint2D;
    l1      : zglTLine;
    i, t, k : Integer;
    x, y    : Single;
begin
  if not col2d_LineVsCircle( L, Circ ) Then
    begin
      Result := FALSE;
      exit;
    end;
  Result := TRUE;

  t := 0;
  k := Round( 360 / Precision );
  SetLength( p1, Precision + 1 );
  for i := 0 to Precision - 1 do
    begin
      p1[ i ].X := Circ.cX + m_Cos( k * i ) * Circ.Radius;
      p1[ i ].Y := Circ.cY + m_Sin( k * i ) * Circ.Radius;
    end;
  p1[ Precision ].X := p1[ 0 ].X;
  p1[ Precision ].Y := p1[ 0 ].Y;


  for i := 0 to Precision - 1 do
    begin
      l1.x0 := p1[ i     ].X;
      l1.y0 := p1[ i     ].Y;
      l1.x1 := p1[ i + 1 ].X;
      l1.y1 := p1[ i + 1 ].Y;
      if col2d_Line( l1, L ) Then
        begin
          INC( t );
          if t = 1 Then
            col2dEx_CalcLineCross( l1, L );
          if t = 2 Then
            begin
              x   := lX;
              y   := lY;
              col2dEx_CalcLineCross( l1, L );
              lX := ( x + lX ) / 2;
              lY := ( y + lY ) / 2;
              exit;
            end;
        end;
    end;
end;

function col2d_Rect;
begin
  Result := ( Rect1.X + Rect1.W >= Rect2.X ) and ( Rect1.X <= Rect2.X + Rect2.W ) and
            ( Rect1.Y + Rect1.H >= Rect2.Y ) and ( Rect1.Y <= Rect2.Y + Rect2.H );
end;

function col2d_ClipRect;
begin
  if ( Rect1.X < Rect2.X ) or ( Rect1.X > Rect2.X + Rect2.W ) Then
    Result.X := Rect2.X
  else
    Result.X := Rect1.X;
  if ( Rect1.Y < Rect2.Y ) or ( Rect1.Y > Rect2.Y + Rect2.H ) Then
    Result.Y := Rect2.Y
  else
    Result.Y := Rect1.Y;

  Result.W := ( Rect1.X + Rect1.W ) - Result.X;
  Result.H := ( Rect1.Y + Rect1.H ) - Result.Y;

  if Result.X + Result.W > Rect2.X + Rect2.W Then
    Result.W := ( Rect2.X + Rect2.W ) - Result.X - 1;
  if Result.Y + Result.H > Rect2.Y + Rect2.H Then
    Result.H := ( Rect2.Y + Rect2.H ) - Result.Y - 1;
end;

function col2d_RectInRect;
begin
  Result := ( Rect1.X > Rect2.X ) and ( Rect1.X + Rect1.W < Rect2.X + Rect2.W ) and
            ( Rect1.Y > Rect2.Y ) and ( Rect1.Y + Rect1.H < Rect2.Y + Rect2.H );
end;

function col2d_RectInCircle;
begin
  Result := col2d_PointInCircle( Rect.X, Rect.Y, Circ ) and
            col2d_PointInCircle( Rect.X + Rect.W, Rect.Y + Rect.H, Circ );
end;

function col2d_RectVsCircle;
begin
  // бред сидого программера :)
  Result := ( col2d_PointInCircle( Rect.X, Rect.Y, Circ ) or
              col2d_PointInCircle( Rect.X + Rect.W, Rect.Y, Circ ) or
              col2d_PointInCircle( Rect.X + Rect.W, Rect.Y + Rect.H, Circ ) or
              col2d_PointInCircle( Rect.X, Rect.Y + Rect.H, Circ ) ) or
            ( col2d_PointInRect  ( Circ.cX, Circ.cY - Circ.Radius, Rect ) or
              col2d_PointInRect  ( Circ.cX + Circ.Radius, Circ.cY - Circ.Radius, Rect ) or
              col2d_PointInRect  ( Circ.cX,  Circ.cY + Circ.Radius, Rect ) or
              col2d_PointInRect  ( Circ.cX - Circ.Radius, Circ.cY, Rect ) )
end;


function col2d_Circle;
begin
  Result := sqr( Circ1.cX - Circ2.cX ) +
            sqr( Circ1.cY - Circ2.cY ) <= sqr( Circ1.Radius + Circ2.Radius );
end;

function col2d_CircleInCircle;
begin
  Result := sqr( Circ1.cX - Circ2.cX ) +
            sqr( Circ1.cY - Circ2.cY ) < sqr( Circ1.Radius - Circ2.Radius );
end;

function col2d_CircleInRect;
begin
  Result := col2d_PointInRect( Circ.cX + Circ.Radius, Circ.cY + Circ.Radius, Rect ) and
            col2d_PointInRect( Circ.cX - Circ.Radius, Circ.cY + Circ.Radius, Rect ) and
            col2d_PointInRect( Circ.cX - Circ.Radius, Circ.cY - Circ.Radius, Rect ) and
            col2d_PointInRect( Circ.cX + Circ.Radius, Circ.cY - Circ.Radius, Rect );
end;

function col2dEx_LastX;
begin
  Result := lX;
end;

function col2dEx_LastY;
begin
  Result := lY;
end;

procedure col2dEx_CalcLineCross;
  var
    S1 : array[ 0..1 ] of Single;
begin
  S1[ 0 ] := A.x1 - A.x0;
  S1[ 1 ] := A.y1 - A.y0;

  lX := A.x0 + lT * S1[ 0 ];
  lY := A.y0 + lT * S1[ 1 ];
end;

end.
