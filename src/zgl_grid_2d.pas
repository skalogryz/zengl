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
unit zgl_grid_2d;

{$I zgl_config.cfg}

interface

uses
  zgl_types,
  zgl_textures,
  zgl_fx,
  zgl_math_2d;

type
  zglPGrid2D = ^zglTGrid2D;
  zglTGrid2D = record
    Cols : Integer;
    Rows : Integer;
    Grid : array of array of zglTPoint2D;
  end;

procedure sgrid2d_Draw( const Texture : zglPTexture; const X, Y : Single; const Grid : zglTGrid2D; const Alpha : Byte = 255; const FX : DWORD = FX_BLEND );
procedure agrid2d_Draw( const Texture : zglPTexture; const X, Y : Single; const Grid : zglTGrid2D; const Frame : Integer; const Alpha : Byte = 255; const FX : DWORD = FX_BLEND );
procedure cgrid2d_Draw( const Texture : zglPTexture; const X, Y : Single; const Grid : zglTGrid2D; const CutRect : zglTRect; const Alpha : Byte = 255; const FX : DWORD = FX_BLEND );

implementation
uses
  zgl_application,
  zgl_main,
  zgl_screen,
  zgl_opengl,
  zgl_opengl_all,
  zgl_render_2d,
  zgl_camera_2d;

procedure sgrid2d_Draw;
  var
    Quad : array[ 0..3 ] of zglTPoint2D;
    i, j : Integer;

    u, v : Single;
    iU, jV, iiU, ijV : Integer;
begin
  if not Assigned( Texture ) Then exit;

  // Текстурные координаты
  if FX and FX2D_FLIPX > 0 Then
    begin
      iU  := ( Grid.Cols - 1 );
      iiU := -1;
    end else
      begin
        iU  := 0;
        iiU := 1;
      end;
  if FX and FX2D_FLIPY > 0 Then
    begin
      jV  := ( Grid.Rows - 1 );
      ijV := -1;
    end else
      begin
        jV  := 0;
        ijV := 1;
      end;
  u := Texture^.U / ( Grid.Cols - 1 );
  v := Texture^.V / ( Grid.Rows - 1 );

  if ( not b2d_Started ) or batch2d_Check( GL_TRIANGLES, FX, Texture ) Then
    begin
      if FX and FX_BLEND > 0 Then
        glEnable( GL_BLEND )
      else
        glEnable( GL_ALPHA_TEST );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, Texture.ID );

      if FX and FX2D_COLORSET > 0 Then
        begin
          glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB );
          glTexEnvi( GL_TEXTURE_ENV, GL_COMBINE_RGB_ARB,  GL_REPLACE );
          glTexEnvi( GL_TEXTURE_ENV, GL_SOURCE0_RGB_ARB,  GL_PRIMARY_COLOR_ARB );
        end else
          glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );

      glBegin( GL_TRIANGLES );
    end;

  if ( FX and FX2D_COLORMIX > 0 ) or ( FX and FX2D_COLORSET > 0 ) Then
    glColor4ub( FX2D_R, FX2D_G, FX2D_B, Alpha )
  else
    glColor4ub( 255, 255, 255, Alpha );

  for i := 0 to Grid.Cols - 2 do
  begin
    for j := 0 to Grid.Rows - 2 do
      begin
        // Позиция/Трансформация
        Quad[ 0 ].X := X + Grid.Grid[ i, j ].X;
        Quad[ 0 ].Y := Y + Grid.Grid[ i, j ].Y;
        Quad[ 1 ].X := X + Grid.Grid[ i + 1, j ].X;
        Quad[ 1 ].Y := Y + Grid.Grid[ i + 1, j ].Y;
        Quad[ 2 ].X := X + Grid.Grid[ i + 1, j + 1 ].X;
        Quad[ 2 ].Y := Y + Grid.Grid[ i + 1, j + 1 ].Y;
        Quad[ 3 ].X := X + Grid.Grid[ i, j + 1 ].X;
        Quad[ 3 ].Y := Y + Grid.Grid[ i, j + 1 ].Y;

        glTexCoord2f( iU * u, Texture^.V - jV * v );
        gl_Vertex2fv( @Quad[ 0 ] );

        glTexCoord2f( ( iU + iiU ) * u, Texture^.V - jV * v );
        gl_Vertex2fv( @Quad[ 1 ] );

        glTexCoord2f( ( iU + iiU ) * u, Texture^.V - ( jV + ijV ) * v );
        gl_Vertex2fv( @Quad[ 2 ] );

        glTexCoord2f( ( iU + iiU ) * u, Texture^.V - ( jV + ijV ) * v );
        gl_Vertex2fv( @Quad[ 2 ] );

        glTexCoord2f( iU * u, Texture^.V - ( jV + ijV ) * v );
        gl_Vertex2fv( @Quad[ 3 ] );

        glTexCoord2f( iU * u, Texture^.V - jV * v );
        gl_Vertex2fv( @Quad[ 0 ] );

        INC( jV, ijV );
      end;
    INC( iU, iiU );
    DEC( jV, ijV * ( Grid.Rows - 1 ) );
  end;

  if not b2d_Started Then
    begin
      glEnd;

      glDisable( GL_TEXTURE_2D );
      glDisable( GL_BLEND );
      glDisable( GL_ALPHA_TEST );
    end;
end;

procedure agrid2d_Draw;
  var
    Quad : array[ 0..3 ] of zglTPoint2D;
    i, j : Integer;

    tX, tY, u, v : Single;
    iU, jV, iiU, ijV : Integer;
begin
  if not Assigned( Texture ) Then exit;

  if FX and FX2D_FLIPX > 0 Then
    begin
      iU  := ( Grid.Cols - 1 );
      iiU := -1;
    end else
      begin
        iU  := 0;
        iiU := 1;
      end;
  if FX and FX2D_FLIPY > 0 Then
    begin
      jV  := ( Grid.Rows - 1 );
      ijV := -1;
    end else
      begin
        jV  := 0;
        ijV := 1;
      end;

  u := Texture.U / Texture.FramesX;
  v := Texture.V / Texture.FramesY;
  tY := ( Frame - 1 ) div Texture.FramesX;
  tX := ( Frame - 1 ) - tY * Texture.FramesX;
  tX := tX * u;
  tY := tY * v;
  u := u / ( Grid.Cols - 1 );
  v := v / ( Grid.Rows - 1 );

  if ( not b2d_Started ) or batch2d_Check( GL_TRIANGLES, FX, Texture ) Then
    begin
      if FX and FX_BLEND > 0 Then
        glEnable( GL_BLEND )
      else
        glEnable( GL_ALPHA_TEST );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, Texture.ID );

      if FX and FX2D_COLORSET > 0 Then
        begin
          glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB );
          glTexEnvi( GL_TEXTURE_ENV, GL_COMBINE_RGB_ARB,  GL_REPLACE );
          glTexEnvi( GL_TEXTURE_ENV, GL_SOURCE0_RGB_ARB,  GL_PRIMARY_COLOR_ARB );
        end else
          glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );

      glBegin( GL_TRIANGLES );
    end;

  if ( FX and FX2D_COLORMIX > 0 ) or ( FX and FX2D_COLORSET > 0 ) Then
    glColor4ub( FX2D_R, FX2D_G, FX2D_B, Alpha )
  else
    glColor4ub( 255, 255, 255, Alpha );

  for i := 0 to Grid.Cols - 2 do
  begin
    for j := 0 to Grid.Rows - 2 do
      begin
        Quad[ 0 ].X := X + Grid.Grid[ i, j ].X;
        Quad[ 0 ].Y := Y + Grid.Grid[ i, j ].Y;
        Quad[ 1 ].X := X + Grid.Grid[ i + 1, j ].X;
        Quad[ 1 ].Y := Y + Grid.Grid[ i + 1, j ].Y;
        Quad[ 2 ].X := X + Grid.Grid[ i + 1, j + 1 ].X;
        Quad[ 2 ].Y := Y + Grid.Grid[ i + 1, j + 1 ].Y;
        Quad[ 3 ].X := X + Grid.Grid[ i, j + 1 ].X;
        Quad[ 3 ].Y := Y + Grid.Grid[ i, j + 1 ].Y;

        glTexCoord2f( iU * u + tX, Texture^.V - jV * v - tY );
        gl_Vertex2fv( @Quad[ 0 ] );

        glTexCoord2f( ( iU + iiU ) * u + tX, Texture^.V - jV * v - tY );
        gl_Vertex2fv( @Quad[ 1 ] );

        glTexCoord2f( ( iU + iiU ) * u + tX, Texture^.V - ( jV + ijV ) * v - tY );
        gl_Vertex2fv( @Quad[ 2 ] );

        glTexCoord2f( ( iU + iiU ) * u + tX, Texture^.V - ( jV + ijV ) * v - tY );
        gl_Vertex2fv( @Quad[ 2 ] );

        glTexCoord2f( iU * u + tX, Texture^.V - ( jV + ijV ) * v - tY );
        gl_Vertex2fv( @Quad[ 3 ] );

        glTexCoord2f( iU * u + tX, Texture^.V - jV * v - tY );
        gl_Vertex2fv( @Quad[ 0 ] );

        INC( jV, ijV );
      end;
    INC( iU, iiU );
    DEC( jV, ijV * ( Grid.Rows - 1 ) );
  end;

  if not b2d_Started Then
    begin
      glEnd;

      glDisable( GL_TEXTURE_2D );
      glDisable( GL_BLEND );
      glDisable( GL_ALPHA_TEST );
    end;
end;

procedure cgrid2d_Draw;
  var
    Quad : array[ 0..3 ] of zglTPoint2D;
    i, j : Integer;

    tX, tY, u, v : Single;
    iU, jV, iiU, ijV : Integer;
begin
  if not Assigned( Texture ) Then exit;

  if FX and FX2D_FLIPX > 0 Then
    begin
      iU  := ( Grid.Cols - 1 );
      iiU := -1;
    end else
      begin
        iU  := 0;
        iiU := 1;
      end;
  if FX and FX2D_FLIPY > 0 Then
    begin
      jV  := ( Grid.Rows - 1 );
      ijV := -1;
    end else
      begin
        jV  := 0;
        ijV := 1;
      end;

  u  := 1 / ( Texture.Width  / Texture.U / Texture.U );
  v  := 1 / ( Texture.Height / Texture.V / Texture.V );
  tX := u * ( CutRect.X / Texture.U );
  tY := v * ( CutRect.Y / Texture.V );
  u  := u * ( CutRect.W / Texture.U ) / ( Grid.Cols - 1 );
  v  := v * ( CutRect.H / Texture.V ) / ( Grid.Rows - 1 );

  if ( not b2d_Started ) or batch2d_Check( GL_TRIANGLES, FX, Texture ) Then
    begin
      if FX and FX_BLEND > 0 Then
        glEnable( GL_BLEND )
      else
        glEnable( GL_ALPHA_TEST );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, Texture.ID );

      if FX and FX2D_COLORSET > 0 Then
        begin
          glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB );
          glTexEnvi( GL_TEXTURE_ENV, GL_COMBINE_RGB_ARB,  GL_REPLACE );
          glTexEnvi( GL_TEXTURE_ENV, GL_SOURCE0_RGB_ARB,  GL_PRIMARY_COLOR_ARB );
        end else
          glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );

      glBegin( GL_TRIANGLES );
    end;

  if ( FX and FX2D_COLORMIX > 0 ) or ( FX and FX2D_COLORSET > 0 ) Then
    glColor4ub( FX2D_R, FX2D_G, FX2D_B, Alpha )
  else
    glColor4ub( 255, 255, 255, Alpha );

  for i := 0 to Grid.Cols - 2 do
  begin
    for j := 0 to Grid.Rows - 2 do
      begin
        Quad[ 0 ].X := X + Grid.Grid[ i, j ].X;
        Quad[ 0 ].Y := Y + Grid.Grid[ i, j ].Y;
        Quad[ 1 ].X := X + Grid.Grid[ i + 1, j ].X;
        Quad[ 1 ].Y := Y + Grid.Grid[ i + 1, j ].Y;
        Quad[ 2 ].X := X + Grid.Grid[ i + 1, j + 1 ].X;
        Quad[ 2 ].Y := Y + Grid.Grid[ i + 1, j + 1 ].Y;
        Quad[ 3 ].X := X + Grid.Grid[ i, j + 1 ].X;
        Quad[ 3 ].Y := Y + Grid.Grid[ i, j + 1 ].Y;

        glTexCoord2f( iU * u + tX, Texture^.V - jV * v - tY );
        gl_Vertex2fv( @Quad[ 0 ] );

        glTexCoord2f( ( iU + iiU ) * u + tX, Texture^.V - jV * v - tY );
        gl_Vertex2fv( @Quad[ 1 ] );

        glTexCoord2f( ( iU + iiU ) * u + tX, Texture^.V - ( jV + ijV ) * v - tY );
        gl_Vertex2fv( @Quad[ 2 ] );

        glTexCoord2f( ( iU + iiU ) * u + tX, Texture^.V - ( jV + ijV ) * v - tY );
        gl_Vertex2fv( @Quad[ 2 ] );

        glTexCoord2f( iU * u + tX, Texture^.V - ( jV + ijV ) * v - tY );
        gl_Vertex2fv( @Quad[ 3 ] );

        glTexCoord2f( iU * u + tX, Texture^.V - jV * v - tY );
        gl_Vertex2fv( @Quad[ 0 ] );

        INC( jV, ijV );
      end;
    INC( iU, iiU );
    DEC( jV, ijV * ( Grid.Rows - 1 ) );
  end;

  if not b2d_Started Then
    begin
      glEnd;

      glDisable( GL_TEXTURE_2D );
      glDisable( GL_BLEND );
      glDisable( GL_ALPHA_TEST );
    end;
end;

end.
