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
unit zgl_collision_2d;

{$I define.inc}

interface

uses
  zgl_types,
  zgl_math;
  
// point 2d
function col2d_PointInRect    ( X, Y : Single; Rect : zglPRect   ) : Boolean; extdecl;
function col2d_PointInCircle  ( X, Y : Single; Circ : zglPCircle ) : Boolean; extdecl;
function col2d_PointInPolyLine( X, Y : Single; PL : zglPPolyLine ) : Boolean; extdecl;
// line 2d
function col2d_Line          ( A, B : zglPLine ) : Boolean; extdecl;
function col2d_LineVsRect    ( A : zglPLine; Rect : zglPRect ) : Boolean; extdecl;
function col2d_LineVsCircle  ( L : zglPLine; Circ : zglPCircle ) : Boolean; extdecl;
function col2d_LineVsCircleXY( L : zglPLine; Circ : zglPCircle; Precision : Byte ) : Boolean; extdecl;
function col2d_LineVsPolyLine( A : zglPLine; B : zglPPolyLine ) : Boolean; extdecl;
// polyline
function col2d_PolyLine          ( A, B : zglPPolyLine ) : Boolean; extdecl;
function col2d_PolyLineVsRect    ( A : zglPPolyLine; Rect : zglPRect ) : Boolean; extdecl;
function col2d_PolyLineVsCircle  ( A : zglPPolyLine; Circ : zglPCircle ) : Boolean; extdecl;
function col2d_PolyLineVsCircleXY( A : zglPPolyLine; Circ : zglPCircle; Precision : Integer ) : Boolean; extdecl;
// rect
function col2d_Rect        ( Rect1, Rect2 : zglPRect ) : Boolean; extdecl;
function col2d_RectInRect  ( Rect1, Rect2 : zglPRect ) : Boolean; extdecl;
function col2d_RectInCircle( Rect : zglPRect; Circ : zglPCircle ) : Boolean; extdecl;
function col2d_RectVsCircle( Rect : zglPRect; Circ : zglPCircle ) : Boolean; extdecl;
// circle
function col2d_Circle        ( Circ1, Circ2 : zglPCircle ) : Boolean; extdecl;
function col2d_CircleInCircle( Circ1, Circ2 : zglPCircle ) : Boolean; extdecl;
function col2d_CircleInRect  ( Circ : zglPCircle; Rect : zglPRect ) : Boolean; extdecl;
// ectended
function col2dEx_LastX : Single; extdecl;
function col2dEx_LastY : Single; extdecl;
function col2dEx_LastLineA : Integer; extdecl;
function col2dEx_LastLineB : Integer; extdecl;
// polyline transformations
procedure col2dEx_PolyRotate( A, B : zglPPolyLine; Angle : Single ); extdecl;
procedure col2dEx_PolyScale( A : zglPPolyLine; ScaleX, ScaleY : Single ); extdecl;
procedure col2dEx_PolyMove( A, B : zglPPolyLine; X, Y : Single ); extdecl;
procedure col2dEx_PolyCenter( A : zglPPolyLine ); extdecl;
procedure col2dEx_PolyRect( A : zglPPolyLine; Rect : zglPRect ); extdecl;
// line
procedure col2dEx_CalcLineCross( A, B : zglPLine ); extdecl;

implementation

var
  lX, lY   : Single;
  lS, lT   : Single;
  lLA, lLB : Integer;

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

function col2d_PointInPolyLine;
  var
    i : Integer;
begin
  Result := FALSE;
  for i := 0 to PL.Count - 1 do
    if Dist( PL.Points[ i ].X, PL.Points[ i ].Y, PL.Points[ i + 1 ].X, PL.Points[ i + 1 ].Y, PL.cX, PL.cY  ) > 0 Then
      begin
        if Dist( PL.Points[ i ].X, PL.Points[ i ].Y, PL.Points[ i + 1 ].X, PL.Points[ i + 1 ].Y, X, Y  ) < 0 Then
          exit;
      end else
        if Dist( PL.Points[ i ].X, PL.Points[ i ].Y, PL.Points[ i + 1 ].X, PL.Points[ i + 1 ].Y, X, Y  ) > 0 Then
          exit;

  if Dist( PL.Points[ i ].X, PL.Points[ i ].Y, PL.Points[ 0 ].X, PL.Points[ 0 ].Y, PL.cX, PL.cY  ) > 0 Then
    begin
      if Dist( PL.Points[ i ].X, PL.Points[ i ].Y, PL.Points[ 0 ].X, PL.Points[ 0 ].Y, X, Y  ) < 0 Then
        exit;
    end else
      if Dist( PL.Points[ i ].X, PL.Points[ i ].Y, PL.Points[ 0 ].X, PL.Points[ 0 ].Y, X, Y  ) > 0 Then
        exit;
  Result := TRUE;
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
      Result := col2d_Line( A, @L );
      if not Result Then
        begin
          L.x0 := Rect.X;
          L.y0 := Rect.Y + Rect.H;
          L.x1 := Rect.X + Rect.W;
          L.y1 := Rect.Y;
          Result := col2d_Line( A, @L );
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
    p1      : zglTPolyLine;
    l1      : zglTLine;
    i, t, k : Integer;
    x, y    : Single;
begin
  lLA := -1;
  lLB := -1;

  if not col2d_LineVsCircle( L, Circ ) Then
    begin
      Result := FALSE;
      exit;
    end;
  Result := TRUE;

  t := 0;
  p1.cX := Circ.cX;
  p1.cY := Circ.cY;
  p1.Count := Precision;
  k := m_Round( 360 / p1.Count );
  SetLength( p1.Points, p1.Count + 1 );
  for i := 0 to p1.Count - 1 do
    begin
      p1.Points[ i ].X := p1.cX + M_Cos( k * i ) * Circ.Radius;
      p1.Points[ i ].Y := p1.cY + M_Sin( k * i ) * Circ.Radius;
    end;
  p1.Points[ p1.Count ].X := p1.Points[ 0 ].X;
  p1.Points[ p1.Count ].Y := p1.Points[ 0 ].Y;


  for i := 0 to p1.Count - 1 do
    begin
      l1.x0 := p1.Points[ i     ].X;
      l1.y0 := p1.Points[ i     ].Y;
      l1.x1 := p1.Points[ i + 1 ].X;
      l1.y1 := p1.Points[ i + 1 ].Y;
      if col2d_Line( @l1, L ) Then
        begin
          INC( t );
          if t = 1 Then
            begin
              lLA := i;
              col2dEx_CalcLineCross( @l1, L );
            end;
          if t = 2 Then
            begin
              lLB := i;
              x   := lX;
              y   := lY;
              col2dEx_CalcLineCross( @l1, L );
              lX := ( x + lX ) / 2;
              lY := ( y + lY ) / 2;
              exit;
            end;
        end;
    end;
end;

function col2d_LineVsPolyLine;
  var
    L : zglTLine;
    i : Integer;
begin
  lLA := -1;
  lLB := -1;

  for i := 0 to B.Count - 1 do
    begin
      L.x0 := B.Points[ i     ].X;
      L.y0 := B.Points[ i     ].Y;
      L.x1 := B.Points[ i + 1 ].X;
      L.y1 := B.Points[ i + 1 ].Y;
      if col2d_Line( @L, A ) Then
        begin
          Result := TRUE;
          lLA    := i;
          exit;
        end;
    end;
  Result := FALSE;
end;


function col2d_PolyLine;
  var
    L1, L2 : zglTLine;
    i, j   : Integer;
begin
  lLA := -1;
  lLB := -1;

  for i := 0 to A.Count - 1 do
    begin
      L1.x0 := A.Points[ i     ].X;
      L1.y0 := A.Points[ i     ].Y;
      L1.x1 := A.Points[ i + 1 ].X;
      L1.y1 := A.Points[ i + 1 ].Y;
      for j := 0 to B.Count - 1 do
        begin
          L2.x0 := B.Points[ j     ].X;
          L2.y0 := B.Points[ j     ].Y;
          L2.x1 := B.Points[ j + 1 ].X;
          L2.y1 := B.Points[ j + 1 ].Y;
          if col2d_Line( @L1, @L2 ) Then
            begin
              Result := TRUE;
              lLA    := i;
              lLB    := j;
              exit;
            end;
        end;
    end;
  Result := FALSE;
end;

function col2d_PolyLineVsRect;
  var
    L : zglTLine;
    i : Integer;
begin
  lLA := -1;
  lLB := -1;

  for i := 0 to A.Count - 1 do
    begin
      L.x0 := A.Points[ i     ].X;
      L.y0 := A.Points[ i     ].Y;
      L.x1 := A.Points[ i + 1 ].X;
      L.y1 := A.Points[ i + 1 ].Y;
      if col2d_LineVsRect( @L, Rect ) Then
        begin
          Result := TRUE;
          lLA    := i;
          exit;
        end;
    end;
  Result := FALSE;
end;

function col2d_PolyLineVsCircle;
  var
    L : zglTLine;
    i : Integer;
begin
  lLA := -1;
  lLB := -1;

  for i := 0 to A.Count - 1 do
    begin
      L.x0 := A.Points[ i     ].X;
      L.y0 := A.Points[ i     ].Y;
      L.x1 := A.Points[ i + 1 ].X;
      L.y1 := A.Points[ i + 1 ].Y;
      if col2d_LineVsCircle( @L, Circ ) Then
        begin
          Result := TRUE;
          lLA    := i;
          exit;
        end;
    end;
  Result := FALSE;
end;

function col2d_PolyLineVsCircleXY;
  var
    L : zglTLine;
    i : Integer;
begin
  lLA := -1;
  lLB := -1;

  for i := 0 to A.Count - 1 do
    begin
      L.x0 := A.Points[ i     ].X;
      L.y0 := A.Points[ i     ].Y;
      L.x1 := A.Points[ i + 1 ].X;
      L.y1 := A.Points[ i + 1 ].Y;
      if col2d_LineVsCircleXY( @L, Circ, Precision ) Then
        begin
          Result := TRUE;
          lLA    := i;
          exit;
        end;
    end;
  Result := FALSE;
end;


function col2d_Rect;
begin
  Result := ( Rect1.X + Rect1.W >= Rect2.X ) and ( Rect1.X <= Rect2.X + Rect2.W ) and
            ( Rect1.Y + Rect1.H >= Rect2.Y ) and ( Rect1.Y <= Rect2.Y + Rect2.H );
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

function col2dEx_LastLineA;
begin
  Result := lLA;
end;

function col2dEx_LastLineB;
begin
  Result := lLB;
end;

procedure col2dEx_PolyRotate;
  var
    i : DWORD;
begin
  // Незнаю почему, но почему-то полилинии после поворота начинают уменьшатсо 8)
  // Кроме как искуственно возвращать размер пока не нашол другого решения...
  for i := 0 to A.Count do
    begin
      B.Points[ i ].X := A.cX + ( A.Points[ i ].X - A.cX ) * m_Cos( m_Round( Angle ) ) * 1.000153 - ( A.Points[ i ].Y - A.cY ) * m_Sin( m_Round( Angle ) ) * 1.000153;
      B.Points[ i ].Y := A.cY + ( A.Points[ i ].X - A.cX ) * m_Sin( m_Round( Angle ) ) * 1.000153 + ( A.Points[ i ].Y - A.cY ) * m_Cos( m_Round( Angle ) ) * 1.000153;
    end;
end;

procedure col2dEx_PolyScale;
  var
    i : Integer;
begin
  for i := 0 to A^.Count do
    begin
      A.Points[ i ].X := A.cX + ( A.Points[ i ].X - A.cX ) * ScaleX;
      A.Points[ i ].Y := A.cY + ( A.Points[ i ].Y - A.cY ) * ScaleY;
    end;
end;

procedure col2dEx_PolyMove;
  var
    i : Integer;
begin
  B.cX := A.cX + X;
  B.cY := A.cY + Y;
  for i := 0 to A.Count do
    begin
      B.Points[ i ].X := A.Points[ i ].X + X;
      B.Points[ i ].Y := A.Points[ i ].Y + Y;
    end;
end;

procedure col2dEx_PolyCenter;
  var
    i      : DWORD;
    Ex, Ey : Single;
begin
  Ex := 0;
  Ey := 0;
  
  for i := 0 to A.Count - 1 do
    Ex := Ex + A.Points[ i ].X;
  for i := 0 to A.Count - 1 do
    Ey := Ey + A.Points[ i ].Y;

  A.cX := Ex / A.Count;
  A.cY := Ey / A.Count;
end;

procedure col2dEx_PolyRect;
  var
    i          : DWORD;
    minX, maxX : Single;
    minY, maxY : Single;
begin
  minX := A.Points[ 0 ].X;
  minY := A.Points[ 0 ].Y;
  maxX := 0;
  maxY := 0;
  for i := 0 to A.Count - 1 do
    begin
      if A.Points[ i ].X < minX Then minX := A.Points[ i ].X;
      if A.Points[ i ].Y < minY Then minY := A.Points[ i ].Y;
      if A.Points[ i ].X > maxX Then maxX := A.Points[ i ].X;
      if A.Points[ i ].Y > maxY Then maxY := A.Points[ i ].Y;
    end;

  Rect.X := minX;
  Rect.Y := minY;
  Rect.W := maxX - minX;
  Rect.H := maxY - minY;
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
