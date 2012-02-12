{
 *  Copyright © Kemka Andrey aka Andru
 *  mail: dr.andru@gmail.com
 *  site: http://zengl.org
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
unit zgl_sprite_2d;

{$I zgl_config.cfg}

interface

uses
  zgl_types,
  zgl_fx,
  zgl_textures,
  zgl_math_2d;

type
  zglPTiles2D = ^zglTTiles2D;
  zglTTiles2D = record
    Count : record
      X, Y : Integer;
            end;
    Size  : record
      W, H : Single;
            end;
    Tiles : array of array of Integer;
  end;

procedure texture2d_Draw( Texture : zglPTexture; const TexCoord : array of zglTPoint2D; X, Y, W, H, Angle : Single; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
procedure ssprite2d_Draw( Texture : zglPTexture; X, Y, W, H, Angle : Single; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
procedure asprite2d_Draw( Texture : zglPTexture; X, Y, W, H, Angle : Single; Frame : Word; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
procedure csprite2d_Draw( Texture : zglPTexture; X, Y, W, H, Angle : Single; const CutRect : zglTRect; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
procedure tiles2d_Draw( Texture : zglPTexture; X, Y : Single; Tiles : zglPTiles2D; Alpha : Byte = 255; FX : LongWord = FX_BLEND );

implementation
uses
  zgl_application,
  zgl_screen,
  {$IFNDEF USE_GLES}
  zgl_opengl,
  zgl_opengl_all,
  {$ELSE}
  zgl_opengles,
  zgl_opengles_all,
  {$ENDIF}
  zgl_render_2d,
  zgl_camera_2d;

const
  FLIP_TEXCOORD : array[ 0..3 ] of zglTTexCoordIndex = ( ( 0, 1, 2, 3 ), ( 1, 0, 3, 2 ), ( 3, 2, 1, 0 ), ( 2, 3, 0, 1 ) );

procedure texture2d_Draw( Texture : zglPTexture; const TexCoord : array of zglTPoint2D; X, Y, W, H, Angle : Single; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
  var
    quad : array[ 0..3 ] of zglTPoint2D;
    tci  : zglPTexCoordIndex;
    p    : zglPPoint2D;
    mode : Integer;

    x1, x2 : Single;
    y1, y2 : Single;
    cX, cY : Single;
    c, s   : Single;
    mX, mY : Single;
    mW, mH : Single;
begin
  if not Assigned( Texture ) Then exit;

  if FX and FX2D_SCALE > 0 Then
    begin
      X := X + ( W - W * fx2dSX ) / 2;
      Y := Y + ( H - H * fx2dSY ) / 2;
      W := W * fx2dSX;
      H := H * fx2dSY;
    end;

  if render2dClip Then
    if FX and FX2D_VCHANGE = 0 Then
      begin
        if not sprite2d_InScreen( X, Y, W, H, Angle ) Then Exit;
      end else
        begin
          mX := min( X + fx2dVX1, min( X + W + fx2dVX2, min( X + W + fx2dVX3, X + fx2dVX4 ) ) );
          mY := min( Y + fx2dVY1, min( Y + fx2dVY2, min( Y + H + fx2dVY3, Y + H + fx2dVY4 ) ) );
          mW := max( X + fx2dVX1, max( X + W + fx2dVX2, max( X + W + fx2dVX3, X + fx2dVX4 ) ) ) - mx;
          mH := max( Y + fx2dVY1, max( Y + fx2dVY2, max( Y + H + fx2dVY3, Y + H + fx2dVY4 ) ) ) - mY;
          if not sprite2d_InScreen( mX, mY, mW + abs( X - mX ) + abs( mW - W ), mH + abs( Y - mY ) + abs( mH - H ), Angle ) Then Exit;
        end;

  // Текстурные координаты
  tci := @FLIP_TEXCOORD[ FX and FX2D_FLIPX + FX and FX2D_FLIPY ];

  // Позиция/Трансформация
  if Angle <> 0 Then
    begin
      if FX and FX2D_RPIVOT = 0 Then
        begin
          x1 := -W / 2;
          y1 := -H / 2;
          x2 := -x1;
          y2 := -y1;
          cX :=  X + x2;
          cY :=  Y + y2;
        end else
          begin
            x1 := -fx2dRPX;
            y1 := -fx2dRPY;
            x2 := W + x1;
            y2 := H + y1;
            cX := X + fx2dRPX;
            cY := Y + fx2dRPY;
          end;

      m_SinCos( Angle * deg2rad, s, c );

      if FX and FX2D_VCHANGE = 0 Then
        begin
          p := @quad[ 0 ];
          p.X := x1 * c - y1 * s + cX;
          p.Y := x1 * s + y1 * c + cY;
          INC( p );
          p.X := x2 * c - y1 * s + cX;
          p.Y := x2 * s + y1 * c + cY;
          INC( p );
          p.X := x2 * c - y2 * s + cX;
          p.Y := x2 * s + y2 * c + cY;
          INC( p );
          p.X := x1 * c - y2 * s + cX;
          p.Y := x1 * s + y2 * c + cY;
        end else
          begin
            p := @quad[ 0 ];
            p.X := ( x1 + fx2dVX1 ) * c - ( y1 + fx2dVY1 ) * s + cX;
            p.Y := ( x1 + fx2dVX1 ) * s + ( y1 + fx2dVY1 ) * c + cY;
            INC( p );
            p.X := ( x2 + fx2dVX2 ) * c - ( y1 + fx2dVY2 ) * s + cX;
            p.Y := ( x2 + fx2dVX2 ) * s + ( y1 + fx2dVY2 ) * c + cY;
            INC( p );
            p.X := ( x2 + fx2dVX3 ) * c - ( y2 + fx2dVY3 ) * s + cX;
            p.Y := ( x2 + fx2dVX3 ) * s + ( y2 + fx2dVY3 ) * c + cY;
            INC( p );
            p.X := ( x1 + fx2dVX4 ) * c - ( y2 + fx2dVY4 ) * s + cX;
            p.Y := ( x1 + fx2dVX4 ) * s + ( y2 + fx2dVY4 ) * c + cY;
          end;
    end else
      if FX and FX2D_VCHANGE = 0 Then
        begin
          p := @quad[ 0 ];
          p.X := X;
          p.Y := Y;
          INC( p );
          p.X := X + W;
          p.Y := Y;
          INC( p );
          p.X := X + W;
          p.Y := Y + H;
          INC( p );
          p.X := X;
          p.Y := Y + H;
        end else
          begin
            p := @quad[ 0 ];
            p.X := X     + fx2dVX1;
            p.Y := Y     + fx2dVY1;
            INC( p );
            p.X := X + W + fx2dVX2;
            p.Y := Y     + fx2dVY2;
            INC( p );
            p.X := X + W + fx2dVX3;
            p.Y := Y + H + fx2dVY3;
            INC( p );
            p.X := X     + fx2dVX4;
            p.Y := Y + H + fx2dVY4;
          end;

  if FX and FX2D_VCA > 0 Then
    mode := GL_TRIANGLES
  else
    mode := GL_QUADS;
  if ( not b2dStarted ) or batch2d_Check( mode, FX, Texture ) Then
    begin
      if FX and FX_BLEND > 0 Then
        glEnable( GL_BLEND )
      else
        glEnable( GL_ALPHA_TEST );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, Texture.ID );

      glBegin( mode );
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

  if FX and FX2D_VCA > 0 Then
    begin
      glColor4ubv( @fx2dVCA1[ 0 ] );
      glTexCoord2fv( @TexCoord[ tci[ 0 ] ] );
      glVertex2fv( @quad[ 0 ] );

      glColor4ubv( @fx2dVCA2[ 0 ] );
      glTexCoord2fv( @TexCoord[ tci[ 1 ] ] );
      glVertex2fv( @quad[ 1 ] );

      glColor4ubv( @fx2dVCA3[ 0 ] );
      glTexCoord2fv( @TexCoord[ tci[ 2 ] ] );
      glVertex2fv( @quad[ 2 ] );

      glColor4ubv( @fx2dVCA3[ 0 ] );
      glTexCoord2fv( @TexCoord[ tci[ 2 ] ] );
      glVertex2fv( @quad[ 2 ] );

      glColor4ubv( @fx2dVCA4[ 0 ] );
      glTexCoord2fv( @TexCoord[ tci[ 3 ] ] );
      glVertex2fv( @quad[ 3 ] );

      glColor4ubv( @fx2dVCA1[ 0 ] );
      glTexCoord2fv( @TexCoord[ tci[ 0 ] ] );
      glVertex2fv( @quad[ 0 ] );
    end else
      begin
        glTexCoord2fv( @TexCoord[ tci[ 0 ] ] );
        glVertex2fv( @quad[ 0 ] );

        glTexCoord2fv( @TexCoord[ tci[ 1 ] ] );
        glVertex2fv( @quad[ 1 ] );

        glTexCoord2fv( @TexCoord[ tci[ 2 ] ] );
        glVertex2fv( @quad[ 2 ] );

        glTexCoord2fv( @TexCoord[ tci[ 3 ] ] );
        glVertex2fv( @quad[ 3 ] );
      end;

  if not b2dStarted Then
    begin
      glEnd();

      glDisable( GL_TEXTURE_2D );
      glDisable( GL_BLEND );
      glDisable( GL_ALPHA_TEST );
    end;
end;

procedure ssprite2d_Draw( Texture : zglPTexture; X, Y, W, H, Angle : Single; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
  var
    quad : array[ 0..3 ] of zglTPoint2D;
    p    : zglPPoint2D;
    tc   : zglPTextureCoord;
    tci  : zglPTexCoordIndex;
    mode : Integer;

    x1, x2 : Single;
    y1, y2 : Single;
    cX, cY : Single;
    c, s   : Single;
    mX, mY : Single;
    mW, mH : Single;
begin
  if not Assigned( Texture ) Then exit;

  if FX and FX2D_SCALE > 0 Then
    begin
      X := X + ( W - W * fx2dSX ) / 2;
      Y := Y + ( H - H * fx2dSY ) / 2;
      W := W * fx2dSX;
      H := H * fx2dSY;
    end;

  if render2dClip Then
    if FX and FX2D_VCHANGE = 0 Then
      begin
        if not sprite2d_InScreen( X, Y, W, H, Angle ) Then Exit;
      end else
        begin
          mX := min( X + fx2dVX1, min( X + W + fx2dVX2, min( X + W + fx2dVX3, X + fx2dVX4 ) ) );
          mY := min( Y + fx2dVY1, min( Y + fx2dVY2, min( Y + H + fx2dVY3, Y + H + fx2dVY4 ) ) );
          mW := max( X + fx2dVX1, max( X + W + fx2dVX2, max( X + W + fx2dVX3, X + fx2dVX4 ) ) ) - mx;
          mH := max( Y + fx2dVY1, max( Y + fx2dVY2, max( Y + H + fx2dVY3, Y + H + fx2dVY4 ) ) ) - mY;
          if not sprite2d_InScreen( mX, mY, mW + abs( X - mX ) + abs( mW - W ), mH + abs( Y - mY ) + abs( mH - H ), Angle ) Then Exit;
        end;

  // Текстурные координаты
  tci := @FLIP_TEXCOORD[ FX and FX2D_FLIPX + FX and FX2D_FLIPY ];
  tc  := @Texture.FramesCoord[ 0 ];

  // Позиция/Трансформация
  if Angle <> 0 Then
    begin
      if FX and FX2D_RPIVOT = 0 Then
        begin
          x1 := -W / 2;
          y1 := -H / 2;
          x2 := -x1;
          y2 := -y1;
          cX :=  X + x2;
          cY :=  Y + y2;
        end else
          begin
            x1 := -fx2dRPX;
            y1 := -fx2dRPY;
            x2 := W + x1;
            y2 := H + y1;
            cX := X + fx2dRPX;
            cY := Y + fx2dRPY;
          end;

      m_SinCos( Angle * deg2rad, s, c );

      if FX and FX2D_VCHANGE = 0 Then
        begin
          p := @quad[ 0 ];
          p.X := x1 * c - y1 * s + cX;
          p.Y := x1 * s + y1 * c + cY;
          INC( p );
          p.X := x2 * c - y1 * s + cX;
          p.Y := x2 * s + y1 * c + cY;
          INC( p );
          p.X := x2 * c - y2 * s + cX;
          p.Y := x2 * s + y2 * c + cY;
          INC( p );
          p.X := x1 * c - y2 * s + cX;
          p.Y := x1 * s + y2 * c + cY;
        end else
          begin
            p := @quad[ 0 ];
            p.X := ( x1 + fx2dVX1 ) * c - ( y1 + fx2dVY1 ) * s + cX;
            p.Y := ( x1 + fx2dVX1 ) * s + ( y1 + fx2dVY1 ) * c + cY;
            INC( p );
            p.X := ( x2 + fx2dVX2 ) * c - ( y1 + fx2dVY2 ) * s + cX;
            p.Y := ( x2 + fx2dVX2 ) * s + ( y1 + fx2dVY2 ) * c + cY;
            INC( p );
            p.X := ( x2 + fx2dVX3 ) * c - ( y2 + fx2dVY3 ) * s + cX;
            p.Y := ( x2 + fx2dVX3 ) * s + ( y2 + fx2dVY3 ) * c + cY;
            INC( p );
            p.X := ( x1 + fx2dVX4 ) * c - ( y2 + fx2dVY4 ) * s + cX;
            p.Y := ( x1 + fx2dVX4 ) * s + ( y2 + fx2dVY4 ) * c + cY;
          end;
    end else
      if FX and FX2D_VCHANGE = 0 Then
        begin
          p := @quad[ 0 ];
          p.X := X;
          p.Y := Y;
          INC( p );
          p.X := X + W;
          p.Y := Y;
          INC( p );
          p.X := X + W;
          p.Y := Y + H;
          INC( p );
          p.X := X;
          p.Y := Y + H;
        end else
          begin
            p := @quad[ 0 ];
            p.X := X     + fx2dVX1;
            p.Y := Y     + fx2dVY1;
            INC( p );
            p.X := X + W + fx2dVX2;
            p.Y := Y     + fx2dVY2;
            INC( p );
            p.X := X + W + fx2dVX3;
            p.Y := Y + H + fx2dVY3;
            INC( p );
            p.X := X     + fx2dVX4;
            p.Y := Y + H + fx2dVY4;
          end;

  if FX and FX2D_VCA > 0 Then
    mode := GL_TRIANGLES
  else
    mode := GL_QUADS;
  if ( not b2dStarted ) or batch2d_Check( mode, FX, Texture ) Then
    begin
      if FX and FX_BLEND > 0 Then
        glEnable( GL_BLEND )
      else
        glEnable( GL_ALPHA_TEST );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, Texture.ID );

      glBegin( mode );
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

  if FX and FX2D_VCA > 0 Then
    begin
      glColor4ubv( @fx2dVCA1[ 0 ] );
      glTexCoord2fv( @tc[ tci[ 0 ] ] );
      glVertex2fv( @quad[ 0 ] );

      glColor4ubv( @fx2dVCA2[ 0 ] );
      glTexCoord2fv( @tc[ tci[ 1 ] ] );
      glVertex2fv( @quad[ 1 ] );

      glColor4ubv( @fx2dVCA3[ 0 ] );
      glTexCoord2fv( @tc[ tci[ 2 ] ] );
      glVertex2fv( @quad[ 2 ] );

      glColor4ubv( @fx2dVCA3[ 0 ] );
      glTexCoord2fv( @tc[ tci[ 2 ] ] );
      glVertex2fv( @quad[ 2 ] );

      glColor4ubv( @fx2dVCA4[ 0 ] );
      glTexCoord2fv( @tc[ tci[ 3 ] ] );
      glVertex2fv( @quad[ 3 ] );

      glColor4ubv( @fx2dVCA1[ 0 ] );
      glTexCoord2fv( @tc[ tci[ 0 ] ] );
      glVertex2fv( @quad[ 0 ] );
    end else
      begin
        glTexCoord2fv( @tc[ tci[ 0 ] ] );
        glVertex2fv( @quad[ 0 ] );

        glTexCoord2fv( @tc[ tci[ 1 ] ] );
        glVertex2fv( @quad[ 1 ] );

        glTexCoord2fv( @tc[ tci[ 2 ] ] );
        glVertex2fv( @quad[ 2 ] );

        glTexCoord2fv( @tc[ tci[ 3 ] ] );
        glVertex2fv( @quad[ 3 ] );
      end;

  if not b2dStarted Then
    begin
      glEnd();

      glDisable( GL_TEXTURE_2D );
      glDisable( GL_BLEND );
      glDisable( GL_ALPHA_TEST );
    end;
end;

procedure asprite2d_Draw( Texture : zglPTexture; X, Y, W, H, Angle : Single; Frame : Word; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
  var
    quad : array[ 0..3 ] of zglTPoint2D;
    p    : zglPPoint2D;
    tc   : zglPTextureCoord;
    tci  : zglPTexCoordIndex;
    fc   : Integer;
    mode : Integer;

    x1, x2 : Single;
    y1, y2 : Single;
    cX, cY : Single;
    c, s   : Single;
    mX, mY : Single;
    mW, mH : Single;
begin
  if not Assigned( Texture ) Then exit;

  if FX and FX2D_SCALE > 0 Then
    begin
      X := X + ( W - W * fx2dSX ) / 2;
      Y := Y + ( H - H * fx2dSY ) / 2;
      W := W * fx2dSX;
      H := H * fx2dSY;
    end;

  if render2dClip Then
    if FX and FX2D_VCHANGE = 0 Then
      begin
        if not sprite2d_InScreen( X, Y, W, H, Angle ) Then Exit;
      end else
        begin
          mX := min( X + fx2dVX1, min( X + W + fx2dVX2, min( X + W + fx2dVX3, X + fx2dVX4 ) ) );
          mY := min( Y + fx2dVY1, min( Y + fx2dVY2, min( Y + H + fx2dVY3, Y + H + fx2dVY4 ) ) );
          mW := max( X + fx2dVX1, max( X + W + fx2dVX2, max( X + W + fx2dVX3, X + fx2dVX4 ) ) ) - mx;
          mH := max( Y + fx2dVY1, max( Y + fx2dVY2, max( Y + H + fx2dVY3, Y + H + fx2dVY4 ) ) ) - mY;
          if not sprite2d_InScreen( mX, mY, mW + abs( X - mX ) + abs( mW - W ), mH + abs( Y - mY ) + abs( mH - H ), Angle ) Then Exit;
        end;

  // Текстурные координаты
  fc := length( Texture.FramesCoord ) - 1;
  if Frame > fc Then
    DEC( Frame, ( ( Frame - 1 ) div fc ) * fc )
  else
    if Frame < 1 Then
      INC( Frame, ( abs( Frame ) div fc + 1 ) * fc );
  tci := @FLIP_TEXCOORD[ FX and FX2D_FLIPX + FX and FX2D_FLIPY ];
  tc  := @Texture.FramesCoord[ Frame ];

  // Позиция/Трансформация
  if Angle <> 0 Then
    begin
      if FX and FX2D_RPIVOT = 0 Then
        begin
          x1 := -W / 2;
          y1 := -H / 2;
          x2 := -x1;
          y2 := -y1;
          cX :=  X + x2;
          cY :=  Y + y2;
        end else
          begin
            x1 := -fx2dRPX;
            y1 := -fx2dRPY;
            x2 := W + x1;
            y2 := H + y1;
            cX := X + fx2dRPX;
            cY := Y + fx2dRPY;
          end;

      m_SinCos( Angle * deg2rad, s, c );

      if FX and FX2D_VCHANGE = 0 Then
        begin
          p := @quad[ 0 ];
          p.X := x1 * c - y1 * s + cX;
          p.Y := x1 * s + y1 * c + cY;
          INC( p );
          p.X := x2 * c - y1 * s + cX;
          p.Y := x2 * s + y1 * c + cY;
          INC( p );
          p.X := x2 * c - y2 * s + cX;
          p.Y := x2 * s + y2 * c + cY;
          INC( p );
          p.X := x1 * c - y2 * s + cX;
          p.Y := x1 * s + y2 * c + cY;
        end else
          begin
            p := @quad[ 0 ];
            p.X := ( x1 + fx2dVX1 ) * c - ( y1 + fx2dVY1 ) * s + cX;
            p.Y := ( x1 + fx2dVX1 ) * s + ( y1 + fx2dVY1 ) * c + cY;
            INC( p );
            p.X := ( x2 + fx2dVX2 ) * c - ( y1 + fx2dVY2 ) * s + cX;
            p.Y := ( x2 + fx2dVX2 ) * s + ( y1 + fx2dVY2 ) * c + cY;
            INC( p );
            p.X := ( x2 + fx2dVX3 ) * c - ( y2 + fx2dVY3 ) * s + cX;
            p.Y := ( x2 + fx2dVX3 ) * s + ( y2 + fx2dVY3 ) * c + cY;
            INC( p );
            p.X := ( x1 + fx2dVX4 ) * c - ( y2 + fx2dVY4 ) * s + cX;
            p.Y := ( x1 + fx2dVX4 ) * s + ( y2 + fx2dVY4 ) * c + cY;
          end;
    end else
      if FX and FX2D_VCHANGE = 0 Then
        begin
          p := @quad[ 0 ];
          p.X := X;
          p.Y := Y;
          INC( p );
          p.X := X + W;
          p.Y := Y;
          INC( p );
          p.X := X + W;
          p.Y := Y + H;
          INC( p );
          p.X := X;
          p.Y := Y + H;
        end else
          begin
            p := @quad[ 0 ];
            p.X := X     + fx2dVX1;
            p.Y := Y     + fx2dVY1;
            INC( p );
            p.X := X + W + fx2dVX2;
            p.Y := Y     + fx2dVY2;
            INC( p );
            p.X := X + W + fx2dVX3;
            p.Y := Y + H + fx2dVY3;
            INC( p );
            p.X := X     + fx2dVX4;
            p.Y := Y + H + fx2dVY4;
          end;

  if FX and FX2D_VCA > 0 Then
    mode := GL_TRIANGLES
  else
    mode := GL_QUADS;
  if ( not b2dStarted ) or batch2d_Check( mode, FX, Texture ) Then
    begin
      if FX and FX_BLEND > 0 Then
        glEnable( GL_BLEND )
      else
        glEnable( GL_ALPHA_TEST );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, Texture^.ID );

      glBegin( mode );
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

  if FX and FX2D_VCA > 0 Then
    begin
      glColor4ubv( @fx2dVCA1[ 0 ] );
      glTexCoord2fv( @tc[ tci[ 0 ] ] );
      glVertex2fv( @quad[ 0 ] );

      glColor4ubv( @fx2dVCA2[ 0 ] );
      glTexCoord2fv( @tc[ tci[ 1 ] ] );
      glVertex2fv( @quad[ 1 ] );

      glColor4ubv( @fx2dVCA3[ 0 ] );
      glTexCoord2fv( @tc[ tci[ 2 ] ] );
      glVertex2fv( @quad[ 2 ] );

      glColor4ubv( @fx2dVCA3[ 0 ] );
      glTexCoord2fv( @tc[ tci[ 2 ] ] );
      glVertex2fv( @quad[ 2 ] );

      glColor4ubv( @fx2dVCA4[ 0 ] );
      glTexCoord2fv( @tc[ tci[ 3 ] ] );
      glVertex2fv( @quad[ 3 ] );

      glColor4ubv( @fx2dVCA1[ 0 ] );
      glTexCoord2fv( @tc[ tci[ 0 ] ] );
      glVertex2fv( @quad[ 0 ] );
    end else
      begin
        glTexCoord2fv( @tc[ tci[ 0 ] ] );
        glVertex2fv( @quad[ 0 ] );

        glTexCoord2fv( @tc[ tci[ 1 ] ] );
        glVertex2fv( @quad[ 1 ] );

        glTexCoord2fv( @tc[ tci[ 2 ] ] );
        glVertex2fv( @quad[ 2 ] );

        glTexCoord2fv( @tc[ tci[ 3 ] ] );
        glVertex2fv( @quad[ 3 ] );
      end;

  if not b2dStarted Then
    begin
      glEnd();

      glDisable( GL_TEXTURE_2D );
      glDisable( GL_BLEND );
      glDisable( GL_ALPHA_TEST );
    end;
end;

procedure csprite2d_Draw( Texture : zglPTexture; X, Y, W, H, Angle : Single; const CutRect : zglTRect; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
  var
    quad : array[ 0..3 ] of zglTPoint2D;
    p    : zglPPoint2D;
    mode : Integer;

    tU, tV, tX, tY, tW, tH : Single;

    x1, x2 : Single;
    y1, y2 : Single;
    cX, cY : Single;
    c, s   : Single;
    mX, mY : Single;
    mW, mH : Single;
begin
  if not Assigned( Texture ) Then exit;

  if FX and FX2D_SCALE > 0 Then
    begin
      X := X + ( W - W * fx2dSX ) / 2;
      Y := Y + ( H - H * fx2dSY ) / 2;
      W := W * fx2dSX;
      H := H * fx2dSY;
    end;

  if render2dClip Then
    if FX and FX2D_VCHANGE = 0 Then
      begin
        if not sprite2d_InScreen( X, Y, W, H, Angle ) Then Exit;
      end else
        begin
          mX := min( X + fx2dVX1, min( X + W + fx2dVX2, min( X + W + fx2dVX3, X + fx2dVX4 ) ) );
          mY := min( Y + fx2dVY1, min( Y + fx2dVY2, min( Y + H + fx2dVY3, Y + H + fx2dVY4 ) ) );
          mW := max( X + fx2dVX1, max( X + W + fx2dVX2, max( X + W + fx2dVX3, X + fx2dVX4 ) ) ) - mx;
          mH := max( Y + fx2dVY1, max( Y + fx2dVY2, max( Y + H + fx2dVY3, Y + H + fx2dVY4 ) ) ) - mY;
          if not sprite2d_InScreen( mX, mY, mW + abs( X - mX ) + abs( mW - W ), mH + abs( Y - mY ) + abs( mH - H ), Angle ) Then Exit;
        end;

  // Текстурные координаты
  // бред, ога :)
  tU := 1 / ( Texture.Width  / Texture.U / Texture.U );
  tV := 1 / ( Texture.Height / Texture.V / Texture.V );
  tX := tU * ( CutRect.X / Texture.U );
  tY := tV * ( Texture.Height / Texture.V - CutRect.Y / Texture.V );
  tW := tX + tU * ( CutRect.W / Texture.U );
  tH := tY + tV * ( -CutRect.H / Texture.V );

  if FX and FX2D_FLIPX > 0 Then tU := tW - tX else tU := 0;
  if FX and FX2D_FLIPY > 0 Then tV := tH - tY else tV := 0;

  // Позиция/Трансформация
  if Angle <> 0 Then
    begin
      if FX and FX2D_RPIVOT = 0 Then
        begin
          x1 := -W / 2;
          y1 := -H / 2;
          x2 := -x1;
          y2 := -y1;
          cX :=  X + x2;
          cY :=  Y + y2;
        end else
          begin
            x1 := -fx2dRPX;
            y1 := -fx2dRPY;
            x2 := W + x1;
            y2 := H + y1;
            cX := X + fx2dRPX;
            cY := Y + fx2dRPY;
          end;

      m_SinCos( Angle * deg2rad, s, c );

      if FX and FX2D_VCHANGE = 0 Then
        begin
          p := @quad[ 0 ];
          p.X := x1 * c - y1 * s + cX;
          p.Y := x1 * s + y1 * c + cY;
          INC( p );
          p.X := x2 * c - y1 * s + cX;
          p.Y := x2 * s + y1 * c + cY;
          INC( p );
          p.X := x2 * c - y2 * s + cX;
          p.Y := x2 * s + y2 * c + cY;
          INC( p );
          p.X := x1 * c - y2 * s + cX;
          p.Y := x1 * s + y2 * c + cY;
        end else
          begin
            p := @quad[ 0 ];
            p.X := ( x1 + fx2dVX1 ) * c - ( y1 + fx2dVY1 ) * s + cX;
            p.Y := ( x1 + fx2dVX1 ) * s + ( y1 + fx2dVY1 ) * c + cY;
            INC( p );
            p.X := ( x2 + fx2dVX2 ) * c - ( y1 + fx2dVY2 ) * s + cX;
            p.Y := ( x2 + fx2dVX2 ) * s + ( y1 + fx2dVY2 ) * c + cY;
            INC( p );
            p.X := ( x2 + fx2dVX3 ) * c - ( y2 + fx2dVY3 ) * s + cX;
            p.Y := ( x2 + fx2dVX3 ) * s + ( y2 + fx2dVY3 ) * c + cY;
            INC( p );
            p.X := ( x1 + fx2dVX4 ) * c - ( y2 + fx2dVY4 ) * s + cX;
            p.Y := ( x1 + fx2dVX4 ) * s + ( y2 + fx2dVY4 ) * c + cY;
          end;
    end else
      if FX and FX2D_VCHANGE = 0 Then
        begin
          p := @quad[ 0 ];
          p.X := X;
          p.Y := Y;
          INC( p );
          p.X := X + W;
          p.Y := Y;
          INC( p );
          p.X := X + W;
          p.Y := Y + H;
          INC( p );
          p.X := X;
          p.Y := Y + H;
        end else
          begin
            p := @quad[ 0 ];
            p.X := X     + fx2dVX1;
            p.Y := Y     + fx2dVY1;
            INC( p );
            p.X := X + W + fx2dVX2;
            p.Y := Y     + fx2dVY2;
            INC( p );
            p.X := X + W + fx2dVX3;
            p.Y := Y + H + fx2dVY3;
            INC( p );
            p.X := X     + fx2dVX4;
            p.Y := Y + H + fx2dVY4;
          end;

  if FX and FX2D_VCA > 0 Then
    mode := GL_TRIANGLES
  else
    mode := GL_QUADS;
  if ( not b2dStarted ) or batch2d_Check( mode, FX, Texture ) Then
    begin
      if FX and FX_BLEND > 0 Then
        glEnable( GL_BLEND )
      else
        glEnable( GL_ALPHA_TEST );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, Texture^.ID );

      glBegin( mode );
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

  if FX and FX2D_VCA > 0 Then
    begin
      glColor4ubv( @fx2dVCA1[ 0 ] );
      glTexCoord2f( tX + tU, tY + tV );
      glVertex2fv( @quad[ 0 ] );

      glColor4ubv( @fx2dVCA2[ 0 ] );
      glTexCoord2f( tW - tU, tY + tV );
      glVertex2fv( @quad[ 1 ] );

      glColor4ubv( @fx2dVCA3[ 0 ] );
      glTexCoord2f( tW - tU, tH - tV );
      glVertex2fv( @quad[ 2 ] );

      glColor4ubv( @fx2dVCA3[ 0 ] );
      glTexCoord2f( tW - tU, tH - tV );
      glVertex2fv( @quad[ 2 ] );

      glColor4ubv( @fx2dVCA4[ 0 ] );
      glTexCoord2f( tX + tU, tH - tV );
      glVertex2fv( @quad[ 3 ] );

      glColor4ubv( @fx2dVCA1[ 0 ] );
      glTexCoord2f( tX + tU, tY + tV );
      glVertex2fv( @quad[ 0 ] );
    end else
      begin
        glTexCoord2f( tX + tU, tY + tV );
        glVertex2fv( @quad[ 0 ] );

        glTexCoord2f( tW - tU, tY + tV );
        glVertex2fv( @quad[ 1 ] );

        glTexCoord2f( tW - tU, tH - tV );
        glVertex2fv( @quad[ 2 ] );

        glTexCoord2f( tX + tU, tH - tV );
        glVertex2fv( @quad[ 3 ] );
      end;

  if not b2dStarted Then
    begin
      glEnd();

      glDisable( GL_TEXTURE_2D );
      glDisable( GL_BLEND );
      glDisable( GL_ALPHA_TEST );
    end;
end;

procedure tiles2d_Draw( Texture : zglPTexture; X, Y : Single; Tiles : zglPTiles2D; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
  var
    w, h, tX, tY, tU, tV, u, v   : Single;
    i, j, aI, aJ, bI, bJ : Integer;
    s, c, x1, y1, x2, y2, x3, y3, x4, y4 : Single;
    tc  : zglPTextureCoord;
    tci : zglPTexCoordIndex;
begin
  if ( not Assigned( Texture ) ) or ( not Assigned( Tiles ) ) Then exit;

  i := Round( Tiles.Size.W );
  j := Round( Tiles.Size.H );

  if X < 0 Then
    begin
      aI := Round( -X ) div i;
      bI := render2dClipW div i + aI;
    end else
      begin
        aI := 0;
        bI := render2dClipW div i - Round( X ) div i;
      end;

  if Y < 0 Then
    begin
      aJ := Round( -Y ) div j;
      bJ := render2dClipH div j + aJ;
    end else
      begin
        aJ := 0;
        bJ := render2dClipH div j - Round( Y ) div j;
      end;

  if not cam2d.OnlyXY Then
    begin
      tX := -cam2d.CX;
      tY := -cam2d.CY;
      tU := render2dClipW + tX;
      tV := render2dClipH + tY;
      u  := cam2d.CX;
      v  := cam2d.CY;

      m_SinCos( -cam2d.Global.Angle * deg2rad, s, c );

      x1 := tX * c - tY * s + u;
      y1 := tX * s + tY * c + v;
      x2 := tU * c - tY * s + u;
      y2 := tU * s + tY * c + v;
      x3 := tU * c - tV * s + u;
      y3 := tU * s + tV * c + v;
      x4 := tX * c - tV * s + u;
      y4 := tX * s + tV * c + v;

      if x1 > x2 Then tX := x2 else tX := x1;
      if tX > x3 Then tX := x3;
      if tX > x4 Then tX := x4;
      if y1 > y2 Then tY := y2 else tY := y1;
      if tY > y3 Then tY := y3;
      if tY > y4 Then tY := y4;
      if x1 < x2 Then tU := x2 else tU := x1;
      if tU < x3 Then tU := x3;
      if tU < x4 Then tU := x4;
      if y1 < y2 Then tV := y2 else tV := y1;
      if tV < y3 Then tV := y3;
      if tV < y4 Then tV := y4;

      DEC( aI, Round( -tX / i ) );
      INC( bI, Round( ( ( tU - render2dClipW ) ) / i ) );
      DEC( aJ, Round( -tY / j ) );
      INC( bJ, Round( ( tV - render2dClipH ) / j ) );

      x1 := cam2d.Global.X * c - cam2d.Global.Y * s;
      y1 := cam2d.Global.X * s + cam2d.Global.Y * c;
      INC( aI, Round( x1 / i ) - 1 );
      INC( bI, Round( x1 / i ) + 1 );
      INC( aJ, Round( y1 / j ) - 1 );
      INC( bJ, Round( y1 / j ) + 1 );
    end else
      begin
        if X >= 0 Then
          INC( aI, Round( ( cam2d.Global.X - X ) / i ) - 1 )
        else
          INC( aI, Round( cam2d.Global.X / i ) - 1 );
        INC( bI, Round( ( cam2d.Global.X ) / i ) + 1 );
        if Y >= 0 Then
          INC( aJ, Round( ( cam2d.Global.Y - Y ) / j ) - 1 )
        else
          INC( aJ, Round( cam2d.Global.Y / j ) - 1 );
        INC( bJ, Round( cam2d.Global.Y / j ) + 1 );
      end;

  if aI < 0 Then aI := 0;
  if aJ < 0 Then aJ := 0;
  if bI >= Tiles.Count.X Then bI := Tiles.Count.X - 1;
  if bJ >= Tiles.Count.Y Then bJ := Tiles.Count.Y - 1;

  if ( not b2dStarted ) or batch2d_Check( GL_QUADS, FX, Texture ) Then
    begin
      if FX and FX_BLEND > 0 Then
        glEnable( GL_BLEND )
      else
        glEnable( GL_ALPHA_TEST );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, Texture^.ID );

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

  tci := @FLIP_TEXCOORD[ FX and FX2D_FLIPX + FX and FX2D_FLIPY ];

  w := Tiles.Size.W;
  h := Tiles.Size.H;
  for i := aI to bI do
    for j := aJ to bJ do
      begin
        // Текстурные координаты
        tc := @Texture.FramesCoord[ Tiles.Tiles[ i, j ] ];

        glTexCoord2fv( @tc[ tci[ 0 ] ] );
        glVertex2f( x + i * w, y + j * h );

        glTexCoord2fv( @tc[ tci[ 1 ] ] );
        glVertex2f( x + i * w + w, y + j * h );

        glTexCoord2fv( @tc[ tci[ 2 ] ] );
        glVertex2f( x + i * w + w, y + j * h + h );

        glTexCoord2fv( @tc[ tci[ 3 ] ] );
        glVertex2f( x + i * w, y + j * h + h );
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
