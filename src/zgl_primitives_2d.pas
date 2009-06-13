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
unit zgl_primitives_2d;

{$I zgl_config.cfg}

interface
uses
  zgl_types;

const
  PR2D_FILL   = $000001;
  PR2D_SMOOTH = $000002;

procedure pr2d_Pixel( const X, Y : Single; const Color : DWORD; const Alpha : Byte = 255 );
procedure pr2d_Line( const X1, Y1, X2, Y2 : Single; const Color : DWORD; const Alpha : Byte = 255; const FX : DWORD = 0 );
procedure pr2d_Rect( const X, Y, W, H : Single; const Color : DWORD; const Alpha : Byte = 255; const FX : DWORD = 0 );
procedure pr2d_Circle( const X, Y, Radius : Single; const Color : DWORD; const Alpha : Byte = 255; const Quality : WORD = 32; const FX : DWORD = 0 );
procedure pr2d_Ellipse( const X, Y, xRadius, yRadius : Single; const Color : DWORD; const Alpha : Byte = 255; const Quality : WORD = 32; const FX : DWORD = 0 );

implementation
uses
  zgl_opengl_all,
  zgl_math_2d,
  zgl_fx;

procedure pr2d_Pixel;
begin
  glColor4ub( Color and $FF, ( Color and $FF00 ) shr 8, ( Color and $FF0000 ) shr 16, Alpha );

  glEnable( GL_BLEND );

  glBegin( GL_POINTS );
    gl_Vertex2f( X + 0.5, Y + 0.5 );
  glEnd;

  glDisable( GL_BLEND );
end;

procedure pr2d_Line;
  var
    _x1, _y1 : Single;
    _x2, _y2 : Single;
begin
  if X1 < X2 Then
    begin
      _x1 := X1 + 0.5;
      _x2 := X2 - 0.5;
    end else
      if X1 > X2 Then
        begin
          _x1 := X1 - 0.5;
          _x2 := X2 + 0.5;
        end else
          begin
            _x1 := X1 + 0.5;
            _x2 := X2 + 0.5;
          end;
  if Y1 < Y2 Then
    begin
      _y1 := Y1 + 0.5;
      _y2 := Y2 - 0.5;
    end else
      if Y1 > Y2 Then
        begin
          _y1 := Y1 - 0.5;
          _y2 := Y2 + 0.5;
        end else
          begin
            _y1 := Y1 + 0.5;
            _y2 := Y2 + 0.5;
          end;
  glColor4ub( Color and $FF, ( Color and $FF00 ) shr 8, ( Color and $FF0000 ) shr 16, Alpha );

  if FX and PR2D_SMOOTH > 0 Then
    begin
      glEnable( GL_LINE_SMOOTH    );
      glEnable( GL_POLYGON_SMOOTH );
    end;
  glEnable( GL_BLEND );

  glBegin( GL_LINES );
    if FX and FX2D_VCA > 0 Then glColor4ub( FX2D_VR1, FX2D_VG1, FX2D_VB1, FX2D_VA1 );
    gl_Vertex2f( _x1, _y1 );
    if FX and FX2D_VCA > 0 Then glColor4ub( FX2D_VR2, FX2D_VG2, FX2D_VB2, FX2D_VA2 );
    gl_Vertex2f( _x2, _y2 );
  glEnd;

  if FX and PR2D_SMOOTH > 0 Then
    begin
      glDisable( GL_LINE_SMOOTH    );
      glDisable( GL_POLYGON_SMOOTH );
    end;
  glDisable( GL_BLEND );
end;

procedure pr2d_Rect;
begin
  glColor4ub( Color and $FF, ( Color and $FF00 ) shr 8, ( Color and $FF0000 ) shr 16, Alpha );

  glEnable( GL_BLEND );

 if FX and PR2D_FILL > 0 Then
   begin
      glBegin( GL_QUADS );
        if FX and FX2D_VCA > 0 Then glColor4ub( FX2D_VR1, FX2D_VG1, FX2D_VB1, FX2D_VA1 );
        gl_Vertex2f( X,     Y );
        if FX and FX2D_VCA > 0 Then glColor4ub( FX2D_VR2, FX2D_VG2, FX2D_VB2, FX2D_VA2 );
        gl_Vertex2f( X + W, Y );
        if FX and FX2D_VCA > 0 Then glColor4ub( FX2D_VR3, FX2D_VG3, FX2D_VB3, FX2D_VA3 );
        gl_Vertex2f( X + W, Y + H );
        if FX and FX2D_VCA > 0 Then glColor4ub( FX2D_VR4, FX2D_VG4, FX2D_VB4, FX2D_VA4 );
        gl_Vertex2f( X,     Y + H );
      glEnd;
   end else
    begin
      glBegin( GL_LINES );
        if FX and FX2D_VCA > 0 Then glColor4ub( FX2D_VR1, FX2D_VG1, FX2D_VB1, FX2D_VA1 );
        gl_Vertex2f( X + 0.5,     Y + 0.5 );
        if FX and FX2D_VCA > 0 Then glColor4ub( FX2D_VR2, FX2D_VG2, FX2D_VB2, FX2D_VA2 );
        gl_Vertex2f( X + W - 0.5, Y + 0.5 );

        gl_Vertex2f( X + W - 0.5, Y + 0.5 );
        if FX and FX2D_VCA > 0 Then glColor4ub( FX2D_VR3, FX2D_VG3, FX2D_VB3, FX2D_VA3 );
        gl_Vertex2f( X + W - 0.5, Y + H - 0.5 );

        gl_Vertex2f( X + W - 0.5, Y + H - 0.5 );
        if FX and FX2D_VCA > 0 Then glColor4ub( FX2D_VR4, FX2D_VG4, FX2D_VB4, FX2D_VA4 );
        gl_Vertex2f( X + 0.5,     Y + H - 0.5 );

        gl_Vertex2f( X + 0.5,     Y + H - 0.5 );
        if FX and FX2D_VCA > 0 Then glColor4ub( FX2D_VR1, FX2D_VG1, FX2D_VB1, FX2D_VA1 );
        gl_Vertex2f( X + 0.5,     Y + 0.5 );
      glEnd;
    end;

  glDisable( GL_BLEND );
end;

procedure pr2d_Circle;
  var
    i : Integer;
    k : Single;
begin
  glColor4ub( Color and $FF, ( Color and $FF00 ) shr 8, ( Color and $FF0000 ) shr 16, Alpha );

  if FX and PR2D_SMOOTH > 0 Then
    begin
      glEnable( GL_LINE_SMOOTH    );
      glEnable( GL_POLYGON_SMOOTH );
    end;
  glEnable( GL_BLEND );

  if Quality > 360 Then
    k := 360
  else
    k := 360 / Quality;

  if FX and PR2D_FILL = 0 Then
    begin
      glBegin( GL_LINES );
      for i := 0 to Quality - 1 do
        begin
          gl_Vertex2f( X + Radius * CosTable[ Round( i * k ) ], Y + Radius * SinTable[ Round( i * k ) ] );
          gl_Vertex2f( X + Radius * CosTable[ Round( ( i + 1 ) * k ) ], Y + Radius * SinTable[ Round( ( i + 1 ) * k ) ] );
        end;
    end else
      begin
        glBegin( GL_TRIANGLE_STRIP );
        for i := 0 to Quality - 1 do
          begin
            gl_Vertex2f( X, Y );
            gl_Vertex2f( X + Radius * CosTable[ Round( i * k ) ], Y + Radius * SinTable[ Round( i * k ) ] );
            gl_Vertex2f( X + Radius * CosTable[ Round( ( i + 1 ) * k ) ], Y + Radius * SinTable[ Round( ( i + 1 ) * k ) ] );
          end;
      end;
  glEnd;

  if FX and PR2D_SMOOTH > 0 Then
    begin
      glDisable( GL_LINE_SMOOTH    );
      glDisable( GL_POLYGON_SMOOTH );
    end;
  glDisable( GL_BLEND );
end;

procedure pr2d_Ellipse;
  var
    i : Integer;
    k : Single;
begin
  glColor4ub( Color and $FF, ( Color and $FF00 ) shr 8, ( Color and $FF0000 ) shr 16, Alpha );

  if FX and PR2D_SMOOTH > 0 Then
    begin
      glEnable( GL_LINE_SMOOTH    );
      glEnable( GL_POLYGON_SMOOTH );
    end;
  glEnable( GL_BLEND );

  if Quality > 360 Then
    k := 360
  else
    k := 360 / Quality;

  if FX and PR2D_FILL = 0 Then
    begin
      glBegin( GL_LINES );
      for i := 0 to Quality - 1 do
        begin
          gl_Vertex2f( X + xRadius * CosTable[ Round( i * k ) ], Y + xRadius * SinTable[ Round( i * k ) ] );
          gl_Vertex2f( X + yRadius * CosTable[ Round( ( i + 1 ) * k ) ], Y + yRadius * SinTable[ Round( ( i + 1 ) * k ) ] );
        end;
    end else
      begin
        glBegin( GL_TRIANGLE_STRIP );
        for i := 0 to Quality do
          begin
            gl_Vertex2f( X, Y );
            gl_Vertex2f( X + xRadius * CosTable[ Round( i * k ) ], Y + xRadius * SinTable[ Round( i * k ) ] );
            gl_Vertex2f( X + yRadius * CosTable[ Round( ( i + 1 ) * k ) ], Y + yRadius * SinTable[ Round( ( i + 1 ) * k ) ] );
          end;
      end;
  glEnd;

  if FX and PR2D_SMOOTH > 0 Then
    begin
      glDisable( GL_LINE_SMOOTH    );
      glDisable( GL_POLYGON_SMOOTH );
    end;
  glDisable( GL_BLEND );
end;

end.
