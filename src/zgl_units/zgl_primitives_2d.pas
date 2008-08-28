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
unit zgl_primitives_2d;

{$I define.inc}

interface

uses
  GL,
  zgl_opengl,
  zgl_fx_2d,
  zgl_math;
  
const
  PR2D_FILL   = $000001;
  PR2D_SMOOTH = $000002;
  
procedure pr2d_Pixel( X, Y : Single; Color : DWORD; Alpha : Byte ); extdecl;
procedure pr2d_Line( X1, Y1, X2, Y2 : Single; Color : DWORD; Alpha : Byte; FX : DWORD ); extdecl;
procedure pr2d_Triangle; extdecl;
procedure pr2d_Rect( X, Y, W, H : Single; Color : DWORD; Alpha : Byte; FX : DWORD ); extdecl;
procedure pr2d_Circle( X, Y, Radius : Single; Color : DWORD; Alpha : Byte; Quality : WORD; FX : DWORD ); extdecl;
procedure pr2d_Ellipse( X, Y, xRadius, yRadius : Single; Color : DWORD; Alpha : Byte; Quality : WORD; FX : DWORD ); extdecl;

implementation

procedure pr2d_Pixel;
begin
  glColor4ub( Color and $FF, ( Color and $FF00 ) shr 8, ( Color and $FF0000 ) shr 16, Alpha );

  glEnable( GL_BLEND );
  
  glBegin( GL_POINTS );
    gl_Vertex2f( X, Y + 1 );
  glEnd;
  
  glDisable( GL_BLEND );
end;

procedure pr2d_Line;
begin
  glColor4ub( Color and $FF, ( Color and $FF00 ) shr 8, ( Color and $FF0000 ) shr 16, Alpha );

  if FX and PR2D_SMOOTH > 0 Then
    begin
      glEnable( GL_LINE_SMOOTH    );
      glEnable( GL_POLYGON_SMOOTH );
    end;
  glEnable( GL_BLEND );
  
  glBegin( GL_LINES );
    if FX and FX2D_VCA > 0 Then glColor4ub( FX2D_VR1, FX2D_VG1, FX2D_VB1, FX2D_VA1 );
    gl_Vertex2f( X1, Y1 + 1 );
    if FX and FX2D_VCA > 0 Then glColor4ub( FX2D_VR2, FX2D_VG2, FX2D_VB2, FX2D_VA2 );
    gl_Vertex2f( X2, Y2 + 1 );
  glEnd;
  
  if FX and PR2D_SMOOTH > 0 Then
    begin
      glDisable( GL_LINE_SMOOTH    );
      glDisable( GL_POLYGON_SMOOTH );
    end;
  glDisable( GL_BLEND );
end;

procedure pr2d_Triangle;
begin
end;

procedure pr2d_Rect;
begin
  glColor4ub( Color and $FF, ( Color and $FF00   ) shr 8, ( Color and $FF0000 ) shr 16, Alpha );

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
        gl_Vertex2f( X,         Y + 1 );
        if FX and FX2D_VCA > 0 Then glColor4ub( FX2D_VR2, FX2D_VG2, FX2D_VB2, FX2D_VA2 );
        gl_Vertex2f( X + W,     Y + 1 );
        
        gl_Vertex2f( X + W - 1, Y + 1 );
        if FX and FX2D_VCA > 0 Then glColor4ub( FX2D_VR3, FX2D_VG3, FX2D_VB3, FX2D_VA3 );
        gl_Vertex2f( X + W - 1, Y + H );
        
        gl_Vertex2f( X + W - 1, Y + H );
        if FX and FX2D_VCA > 0 Then glColor4ub( FX2D_VR4, FX2D_VG4, FX2D_VB4, FX2D_VA4 );
        gl_Vertex2f( X,         Y + H );
        
        gl_Vertex2f( X,         Y + H );
        if FX and FX2D_VCA > 0 Then glColor4ub( FX2D_VR1, FX2D_VG1, FX2D_VB1, FX2D_VA1 );
        gl_Vertex2f( X,         Y + 1 );
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

  if Quality > 360 Then Quality := 360;
  k := 360 / Quality;
  
  if FX and PR2D_FILL = 0 Then
    begin
      glBegin( GL_LINES );
      for i := 0 to Quality - 1 do
        begin
          gl_Vertex2f( X + Radius * CosTable[ m_Round( i * k ) ], Y + Radius * SinTable[ m_Round( i * k ) ] );
          gl_Vertex2f( X + Radius * CosTable[ m_Round( ( i + 1 ) * k ) ], Y + Radius * SinTable[ m_Round( ( i + 1 ) * k ) ] );
        end;
    end else
      begin
        glBegin( GL_TRIANGLE_STRIP );
        for i := 0 to Quality do
          begin
            gl_Vertex2f( X, Y );
            gl_Vertex2f( X + Radius * CosTable[ m_Round( i * k ) ], Y + Radius * SinTable[ m_Round( i * k ) ] );
            gl_Vertex2f( X + Radius * CosTable[ m_Round( ( i + 1 ) * k ) ], Y + Radius * SinTable[ m_Round( ( i + 1 ) * k ) ] );
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

  if Quality > 360 Then Quality := 360;
  k := 360 / Quality;
  
  if FX and PR2D_FILL = 0 Then
    begin
      glBegin( GL_LINES );
      for i := 0 to Quality - 1 do
        begin
          gl_Vertex2f( X + xRadius * CosTable[ m_Round( i * k ) ], Y + xRadius * SinTable[ m_Round( i * k ) ] );
          gl_Vertex2f( X + yRadius * CosTable[ m_Round( ( i + 1 ) * k ) ], Y + yRadius * SinTable[ m_Round( ( i + 1 ) * k ) ] );
        end;
    end else
      begin
        glBegin( GL_TRIANGLE_STRIP );
        for i := 0 to Quality do
          begin
            gl_Vertex2f( X, Y );
            gl_Vertex2f( X + xRadius * CosTable[ m_Round( i * k ) ], Y + xRadius * SinTable[ m_Round( i * k ) ] );
            gl_Vertex2f( X + yRadius * CosTable[ m_Round( ( i + 1 ) * k ) ], Y + yRadius * SinTable[ m_Round( ( i + 1 ) * k ) ] );
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
