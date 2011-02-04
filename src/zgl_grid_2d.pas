{
 *  Copyright © Kemka Andrey aka Andru
 *  mail: dr.andru@gmail.com
 *  site: http://andru-kun.inf.ua
 *
 *  This file is part of ZenGL.
 *
 *  ZenGL is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as
 *  published by the Free Software Foundation, either version 3 of
 *  the License, or (at your option) any later version.
 *
 *  ZenGL is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with ZenGL. If not, see http://www.gnu.org/licenses/
}
unit zgl_grid_2d;

{$I zgl_config.cfg}

interface

uses
  zgl_fx,
  zgl_textures,
  zgl_math_2d;

type
  zglPGrid2D = ^zglTGrid2D;
  zglTGrid2D = record
    Cols : Integer;
    Rows : Integer;
    Grid : array of array of zglTPoint2D;
  end;

procedure sgrid2d_Draw( Texture : zglPTexture; X, Y : Single; Grid : zglPGrid2D; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
procedure agrid2d_Draw( Texture : zglPTexture; X, Y : Single; Grid : zglPGrid2D; Frame : Integer; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
procedure cgrid2d_Draw( Texture : zglPTexture; X, Y : Single; Grid : zglPGrid2D; const CutRect : zglTRect; Alpha : Byte = 255; FX : LongWord = FX_BLEND );

implementation
uses
  zgl_application,
  zgl_screen,
  zgl_opengl,
  zgl_opengl_all,
  zgl_render_2d,
  zgl_camera_2d;

procedure sgrid2d_Draw( Texture : zglPTexture; X, Y : Single; Grid : zglPGrid2D; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
  var
    quad : array[ 0..3 ] of zglTPoint2D;
    i, j : Integer;

    u, v : Single;
    iU, jV, iiU, ijV : Integer;
begin
  if ( not Assigned( Texture ) ) or ( not Assigned( Grid ) ) Then exit;

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

  if ( not b2dStarted ) or batch2d_Check( GL_QUADS, FX, Texture ) Then
    begin
      if FX and FX_BLEND > 0 Then
        glEnable( GL_BLEND )
      else
        glEnable( GL_ALPHA_TEST );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, Texture.ID );

      glBegin( GL_QUADS );
    end;

  if FX and FX_COLOR > 0 Then
    begin
      fx2dAlpha^ := Alpha;
      glColor4ubv( @fx2dColor[ 0 ] );
    end else
      begin
        fx2dAlphaDef^ := Alpha;
        glColor4ubv( @fx2dColorDef[ 0 ] );
      end;

  for i := 0 to Grid.Cols - 2 do
    begin
      for j := 0 to Grid.Rows - 2 do
        begin
          quad[ 0 ].X := X + Grid.Grid[ i, j ].X;
          quad[ 0 ].Y := Y + Grid.Grid[ i, j ].Y;
          quad[ 1 ].X := X + Grid.Grid[ i + 1, j ].X;
          quad[ 1 ].Y := Y + Grid.Grid[ i + 1, j ].Y;
          quad[ 2 ].X := X + Grid.Grid[ i + 1, j + 1 ].X;
          quad[ 2 ].Y := Y + Grid.Grid[ i + 1, j + 1 ].Y;
          quad[ 3 ].X := X + Grid.Grid[ i, j + 1 ].X;
          quad[ 3 ].Y := Y + Grid.Grid[ i, j + 1 ].Y;

          glTexCoord2f( iU * u, Texture^.V - jV * v );
          glVertex2fv( @quad[ 0 ] );

          glTexCoord2f( ( iU + iiU ) * u, Texture^.V - jV * v );
          glVertex2fv( @quad[ 1 ] );

          glTexCoord2f( ( iU + iiU ) * u, Texture^.V - ( jV + ijV ) * v );
          glVertex2fv( @quad[ 2 ] );

          glTexCoord2f( iU * u, Texture^.V - ( jV + ijV ) * v );
          glVertex2fv( @quad[ 3 ] );

          INC( jV, ijV );
        end;
      INC( iU, iiU );
      DEC( jV, ijV * ( Grid.Rows - 1 ) );
  end;

  if not b2dStarted Then
    begin
      glEnd();

      glDisable( GL_TEXTURE_2D );
      glDisable( GL_BLEND );
      glDisable( GL_ALPHA_TEST );
    end;
end;

procedure agrid2d_Draw( Texture : zglPTexture; X, Y : Single; Grid : zglPGrid2D; Frame : Integer; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
  var
    quad : array[ 0..3 ] of zglTPoint2D;
    i, j : Integer;

    tX, tY, u, v : Single;
    iU, jV, iiU, ijV : Integer;
begin
  if ( not Assigned( Texture ) ) or ( not Assigned( Grid ) ) Then exit;

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

  if ( not b2dStarted ) or batch2d_Check( GL_QUADS, FX, Texture ) Then
    begin
      if FX and FX_BLEND > 0 Then
        glEnable( GL_BLEND )
      else
        glEnable( GL_ALPHA_TEST );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, Texture.ID );

      glBegin( GL_QUADS );
    end;

  if FX and FX_COLOR > 0 Then
    begin
      fx2dAlpha^ := Alpha;
      glColor4ubv( @fx2dColor[ 0 ] );
    end else
      begin
        fx2dAlphaDef^ := Alpha;
        glColor4ubv( @fx2dColorDef[ 0 ] );
      end;

  for i := 0 to Grid.Cols - 2 do
    begin
      for j := 0 to Grid.Rows - 2 do
        begin
          quad[ 0 ].X := X + Grid.Grid[ i, j ].X;
          quad[ 0 ].Y := Y + Grid.Grid[ i, j ].Y;
          quad[ 1 ].X := X + Grid.Grid[ i + 1, j ].X;
          quad[ 1 ].Y := Y + Grid.Grid[ i + 1, j ].Y;
          quad[ 2 ].X := X + Grid.Grid[ i + 1, j + 1 ].X;
          quad[ 2 ].Y := Y + Grid.Grid[ i + 1, j + 1 ].Y;
          quad[ 3 ].X := X + Grid.Grid[ i, j + 1 ].X;
          quad[ 3 ].Y := Y + Grid.Grid[ i, j + 1 ].Y;

          glTexCoord2f( iU * u + tX, Texture^.V - jV * v - tY );
          glVertex2fv( @quad[ 0 ] );

          glTexCoord2f( ( iU + iiU ) * u + tX, Texture^.V - jV * v - tY );
          glVertex2fv( @quad[ 1 ] );

          glTexCoord2f( ( iU + iiU ) * u + tX, Texture^.V - ( jV + ijV ) * v - tY );
          glVertex2fv( @quad[ 2 ] );

          glTexCoord2f( iU * u + tX, Texture^.V - ( jV + ijV ) * v - tY );
          glVertex2fv( @quad[ 3 ] );

          INC( jV, ijV );
        end;
      INC( iU, iiU );
      DEC( jV, ijV * ( Grid.Rows - 1 ) );
    end;

  if not b2dStarted Then
    begin
      glEnd();

      glDisable( GL_TEXTURE_2D );
      glDisable( GL_BLEND );
      glDisable( GL_ALPHA_TEST );
    end;
end;

procedure cgrid2d_Draw( Texture : zglPTexture; X, Y : Single; Grid : zglPGrid2D; const CutRect : zglTRect; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
  var
    quad : array[ 0..3 ] of zglTPoint2D;
    i, j : Integer;

    tX, tY, u, v : Single;
    iU, jV, iiU, ijV : Integer;
begin
  if ( not Assigned( Texture ) ) or ( not Assigned( Grid ) ) Then exit;

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

  if ( not b2dStarted ) or batch2d_Check( GL_QUADS, FX, Texture ) Then
    begin
      if FX and FX_BLEND > 0 Then
        glEnable( GL_BLEND )
      else
        glEnable( GL_ALPHA_TEST );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, Texture.ID );

      glBegin( GL_QUADS );
    end;

  if FX and FX_COLOR > 0 Then
    begin
      fx2dAlpha^ := Alpha;
      glColor4ubv( @fx2dColor[ 0 ] );
    end else
      begin
        fx2dAlphaDef^ := Alpha;
        glColor4ubv( @fx2dColorDef[ 0 ] );
      end;

  for i := 0 to Grid.Cols - 2 do
    begin
      for j := 0 to Grid.Rows - 2 do
        begin
          quad[ 0 ].X := X + Grid.Grid[ i, j ].X;
          quad[ 0 ].Y := Y + Grid.Grid[ i, j ].Y;
          quad[ 1 ].X := X + Grid.Grid[ i + 1, j ].X;
          quad[ 1 ].Y := Y + Grid.Grid[ i + 1, j ].Y;
          quad[ 2 ].X := X + Grid.Grid[ i + 1, j + 1 ].X;
          quad[ 2 ].Y := Y + Grid.Grid[ i + 1, j + 1 ].Y;
          quad[ 3 ].X := X + Grid.Grid[ i, j + 1 ].X;
          quad[ 3 ].Y := Y + Grid.Grid[ i, j + 1 ].Y;

          glTexCoord2f( iU * u + tX, Texture^.V - jV * v - tY );
          glVertex2fv( @quad[ 0 ] );

          glTexCoord2f( ( iU + iiU ) * u + tX, Texture^.V - jV * v - tY );
          glVertex2fv( @quad[ 1 ] );

          glTexCoord2f( ( iU + iiU ) * u + tX, Texture^.V - ( jV + ijV ) * v - tY );
          glVertex2fv( @quad[ 2 ] );

          glTexCoord2f( iU * u + tX, Texture^.V - ( jV + ijV ) * v - tY );
          glVertex2fv( @quad[ 3 ] );

          INC( jV, ijV );
        end;
      INC( iU, iiU );
      DEC( jV, ijV * ( Grid.Rows - 1 ) );
    end;

  if not b2dStarted Then
    begin
      glEnd();

      glDisable( GL_TEXTURE_2D );
      glDisable( GL_BLEND );
      glDisable( GL_ALPHA_TEST );
    end;
end;

end.
