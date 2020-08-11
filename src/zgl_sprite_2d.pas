{
 *  Copyright (c) 2012 Andrey Kemka
 *
 *  This software is provided 'as-is', without any express or
 *  implied warranty. In no event will the authors be held
 *  liable for any damages arising from the use of this software.
 *
 *  Permission is granted to anyone to use this software for any purpose,
 *  including commercial applications, and to alter it and redistribute
 *  it freely, subject to the following restrictions:
 *
 *  1. The origin of this software must not be misrepresented;
 *     you must not claim that you wrote the original software.
 *     If you use this software in a product, an acknowledgment
 *     in the product documentation would be appreciated but
 *     is not required.
 *
 *  2. Altered source versions must be plainly marked as such,
 *     and must not be misrepresented as being the original software.
 *
 *  3. This notice may not be removed or altered from any
 *     source distribution.
}
unit zgl_sprite_2d;

{$I zgl_config.cfg}

interface

uses
  zgl_types,
  zgl_fx,
  zgl_textures,
  zgl_math_2d;

procedure texture2d_Draw( Texture : zglPTexture; const TexCoord : array of zglTPoint2D; X, Y, W, H, Angle : Double; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
procedure ssprite2d_Draw( Texture : zglPTexture; X, Y, W, H, Angle : Double; Alpha : Byte = 255; FX : LongWord = FX_BLEND ); overload;
procedure asprite2d_Draw( Texture : zglPTexture; X, Y, W, H, Angle : Double; Frame : Word; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
procedure csprite2d_Draw( Texture : zglPTexture; X, Y, W, H, Angle : Double; const CutRect : zglTRect; Alpha : Byte = 255; FX : LongWord = FX_BLEND ); overload;
procedure csprite2d_Draw( const Texture : array of zglPTexture; const CutRect : array of zglTRect; X, Y, W, H, Angle : Double; Alpha : Byte = 255; FX : LongWord = FX_BLEND ); overload;

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

procedure texture2d_Draw( Texture : zglPTexture; const TexCoord : array of zglTPoint2D; X, Y, W, H, Angle : Double; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
  var
    quad : array[ 0..3 ] of zglTPoint2D;
    tci  : zglPTexCoordIndex;
    p    : zglPPoint2D;
    mode : Integer;

    x1, x2 : Double;
    y1, y2 : Double;
    cX, cY : Double;
    c, s   : Double;
    mX, mY : Double;
    mW, mH : Double;
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

  tci := @FLIP_TEXCOORD[ FX and FX2D_FLIPX + FX and FX2D_FLIPY ];

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
      if FX and FX_BLEND > 0 Then begin
        glEnable( GL_BLEND );
        if FX and FX_BLENDALPHA > 0 then glEnable ( GL_ALPHA_TEST );
      end else
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
      glTexCoord2dv( @TexCoord[ tci[ 0 ] ] );
      glVertex2dv( @quad[ 0 ] );

      glColor4ubv( @fx2dVCA2[ 0 ] );
      glTexCoord2dv( @TexCoord[ tci[ 1 ] ] );
      glVertex2dv( @quad[ 1 ] );

      glColor4ubv( @fx2dVCA3[ 0 ] );
      glTexCoord2dv( @TexCoord[ tci[ 2 ] ] );
      glVertex2dv( @quad[ 2 ] );

      glColor4ubv( @fx2dVCA3[ 0 ] );
      glTexCoord2dv( @TexCoord[ tci[ 2 ] ] );
      glVertex2dv( @quad[ 2 ] );

      glColor4ubv( @fx2dVCA4[ 0 ] );
      glTexCoord2dv( @TexCoord[ tci[ 3 ] ] );
      glVertex2dv( @quad[ 3 ] );

      glColor4ubv( @fx2dVCA1[ 0 ] );
      glTexCoord2dv( @TexCoord[ tci[ 0 ] ] );
      glVertex2dv( @quad[ 0 ] );
    end else
      begin
        glTexCoord2dv( @TexCoord[ tci[ 0 ] ] );
        glVertex2dv( @quad[ 0 ] );

        glTexCoord2dv( @TexCoord[ tci[ 1 ] ] );
        glVertex2dv( @quad[ 1 ] );

        glTexCoord2dv( @TexCoord[ tci[ 2 ] ] );
        glVertex2dv( @quad[ 2 ] );

        glTexCoord2dv( @TexCoord[ tci[ 3 ] ] );
        glVertex2dv( @quad[ 3 ] );
      end;

  if not b2dStarted Then
    begin
      glEnd();

      glDisable( GL_TEXTURE_2D );
      glDisable( GL_BLEND );
      glDisable( GL_ALPHA_TEST );
    end;
end;

procedure ssprite2d_Draw( Texture : zglPTexture; X, Y, W, H, Angle : Double; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
  var
    quad : array[ 0..3 ] of zglTPoint2D;
    p    : zglPPoint2D;
    tc   : zglPTextureCoord;
    tci  : zglPTexCoordIndex;
    mode : Integer;

    x1, x2 : Double;
    y1, y2 : Double;
    cX, cY : Double;
    c, s   : Double;
    mX, mY : Double;
    mW, mH : Double;
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

  tci := @FLIP_TEXCOORD[ FX and FX2D_FLIPX + FX and FX2D_FLIPY ];
  tc  := @Texture.FramesCoord[ 0 ];

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
      if FX and FX_BLEND > 0 Then begin
        glEnable( GL_BLEND );
        if FX and FX_BLENDALPHA > 0 then glEnable ( GL_ALPHA_TEST );
      end else
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
      glTexCoord2dv( @tc[ tci[ 0 ] ] );
      glVertex2dv( @quad[ 0 ] );

      glColor4ubv( @fx2dVCA2[ 0 ] );
      glTexCoord2dv( @tc[ tci[ 1 ] ] );
      glVertex2dv( @quad[ 1 ] );

      glColor4ubv( @fx2dVCA3[ 0 ] );
      glTexCoord2dv( @tc[ tci[ 2 ] ] );
      glVertex2dv( @quad[ 2 ] );

      glColor4ubv( @fx2dVCA3[ 0 ] );
      glTexCoord2dv( @tc[ tci[ 2 ] ] );
      glVertex2dv( @quad[ 2 ] );

      glColor4ubv( @fx2dVCA4[ 0 ] );
      glTexCoord2dv( @tc[ tci[ 3 ] ] );
      glVertex2dv( @quad[ 3 ] );

      glColor4ubv( @fx2dVCA1[ 0 ] );
      glTexCoord2dv( @tc[ tci[ 0 ] ] );
      glVertex2dv( @quad[ 0 ] );
    end else
      begin
        glTexCoord2dv( @tc[ tci[ 0 ] ] );
        glVertex2dv( @quad[ 0 ] );

        glTexCoord2dv( @tc[ tci[ 1 ] ] );
        glVertex2dv( @quad[ 1 ] );

        glTexCoord2dv( @tc[ tci[ 2 ] ] );
        glVertex2dv( @quad[ 2 ] );

        glTexCoord2dv( @tc[ tci[ 3 ] ] );
        glVertex2dv( @quad[ 3 ] );
      end;

  if not b2dStarted Then
    begin
      glEnd();

      glDisable( GL_TEXTURE_2D );
      glDisable( GL_BLEND );
      glDisable( GL_ALPHA_TEST );
    end;
end;

procedure asprite2d_Draw( Texture : zglPTexture; X, Y, W, H, Angle : Double; Frame : Word; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
  var
    quad : array[ 0..3 ] of zglTPoint2D;
    p    : zglPPoint2D;
    tc   : zglPTextureCoord;
    tci  : zglPTexCoordIndex;
    fc   : Integer;
    mode : Integer;

    x1, x2 : Double;
    y1, y2 : Double;
    cX, cY : Double;
    c, s   : Double;
    mX, mY : Double;
    mW, mH : Double;
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

  fc := Length( Texture.FramesCoord ) - 1;
  if Frame > fc Then
    DEC( Frame, ( ( Frame - 1 ) div fc ) * fc )
  else
    if Frame < 1 Then
      INC( Frame, ( abs( Frame ) div fc + 1 ) * fc );
  tci := @FLIP_TEXCOORD[ FX and FX2D_FLIPX + FX and FX2D_FLIPY ];
  tc  := @Texture.FramesCoord[ Frame ];

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
      if FX and FX_BLEND > 0 Then begin
        glEnable( GL_BLEND );
        if FX and FX_BLENDALPHA > 0 then glEnable ( GL_ALPHA_TEST );
      end else
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
      glTexCoord2dv( @tc[ tci[ 0 ] ] );
      glVertex2dv( @quad[ 0 ] );

      glColor4ubv( @fx2dVCA2[ 0 ] );
      glTexCoord2dv( @tc[ tci[ 1 ] ] );
      glVertex2dv( @quad[ 1 ] );

      glColor4ubv( @fx2dVCA3[ 0 ] );
      glTexCoord2dv( @tc[ tci[ 2 ] ] );
      glVertex2dv( @quad[ 2 ] );

      glColor4ubv( @fx2dVCA3[ 0 ] );
      glTexCoord2dv( @tc[ tci[ 2 ] ] );
      glVertex2dv( @quad[ 2 ] );

      glColor4ubv( @fx2dVCA4[ 0 ] );
      glTexCoord2dv( @tc[ tci[ 3 ] ] );
      glVertex2dv( @quad[ 3 ] );

      glColor4ubv( @fx2dVCA1[ 0 ] );
      glTexCoord2dv( @tc[ tci[ 0 ] ] );
      glVertex2dv( @quad[ 0 ] );
    end else
      begin
        glTexCoord2dv( @tc[ tci[ 0 ] ] );
        glVertex2dv( @quad[ 0 ] );

        glTexCoord2dv( @tc[ tci[ 1 ] ] );
        glVertex2dv( @quad[ 1 ] );

        glTexCoord2dv( @tc[ tci[ 2 ] ] );
        glVertex2dv( @quad[ 2 ] );

        glTexCoord2dv( @tc[ tci[ 3 ] ] );
        glVertex2dv( @quad[ 3 ] );
      end;

  if not b2dStarted Then
    begin
      glEnd();

      glDisable( GL_TEXTURE_2D );
      glDisable( GL_BLEND );
      glDisable( GL_ALPHA_TEST );
    end;
end;

procedure csprite2d_Draw( Texture : zglPTexture; X, Y, W, H, Angle : Double; const CutRect : zglTRect; Alpha : Byte = 255; FX : LongWord = FX_BLEND );
  var
    quad : array[ 0..3 ] of zglTPoint2D;
    p    : zglPPoint2D;
    mode : Integer;

    tU, tV, tX, tY, tW, tH : Double;

    x1, x2 : Double;
    y1, y2 : Double;
    cX, cY : Double;
    c, s   : Double;
    mX, mY : Double;
    mW, mH : Double;
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

  tU := 1 / ( Texture.Width  / Texture.U );
  tV := 1 / ( Texture.Height / Texture.V );
  if (CutRect.w=w) then begin
    tX := tU * (CutRect.X);
    tW := tX + tU * (CutRect.W);
  end else begin
    tX := tU * (CutRect.X+0.5);
    tW := tX + tU * (CutRect.W - 1.0);
  end;

  if (CutRect.h=h) then begin
    tY := tV * (( Texture.Height - CutRect.Y));
    tH := tY - tV * (CutRect.H);
  end else begin
    tY := tV * (( Texture.Height - CutRect.Y - 0.5));
    tH := tY - tV * (CutRect.H - 1.0);
  end;
(*

glTexCoord2f( tX + tU, tY + tV );
glVertex2dv( @quad[ 0 ] );

glColor4ubv( @fx2dVCA2[ 0 ] );
glTexCoord2f( tW - tU, tY + tV );
*)
  if FX and FX2D_FLIPX > 0 Then tU := tW - tX else tU := 0;
  if FX and FX2D_FLIPY > 0 Then tV := tH - tY else tV := 0;

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
      if FX and FX_BLEND > 0 Then begin
        glEnable( GL_BLEND );
        if FX and FX_BLENDALPHA > 0 then glEnable ( GL_ALPHA_TEST );
      end else
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
      glVertex2dv( @quad[ 0 ] );

      glColor4ubv( @fx2dVCA2[ 0 ] );
      glTexCoord2f( tW - tU, tY + tV );
      glVertex2dv( @quad[ 1 ] );

      glColor4ubv( @fx2dVCA3[ 0 ] );
      glTexCoord2f( tW - tU, tH - tV );
      glVertex2dv( @quad[ 2 ] );

      glColor4ubv( @fx2dVCA3[ 0 ] );
      glTexCoord2f( tW - tU, tH - tV );
      glVertex2dv( @quad[ 2 ] );

      glColor4ubv( @fx2dVCA4[ 0 ] );
      glTexCoord2f( tX + tU, tH - tV );
      glVertex2dv( @quad[ 3 ] );

      glColor4ubv( @fx2dVCA1[ 0 ] );
      glTexCoord2f( tX + tU, tY + tV );
      glVertex2dv( @quad[ 0 ] );
    end else
      begin
        glTexCoord2f( tX + tU, tY + tV );
        glVertex2dv( @quad[ 0 ] );

        glTexCoord2f( tW - tU, tY + tV );
        glVertex2dv( @quad[ 1 ] );

        glTexCoord2f( tW - tU, tH - tV );
        glVertex2dv( @quad[ 2 ] );

        glTexCoord2f( tX + tU, tH - tV );
        glVertex2dv( @quad[ 3 ] );
      end;

  if not b2dStarted Then
    begin
      glEnd();

      glDisable( GL_TEXTURE_2D );
      glDisable( GL_BLEND );
      glDisable( GL_ALPHA_TEST );
    end;
end;

procedure csprite2d_Draw( const Texture : array of zglPTexture;
  const CutRect : array of zglTRect;
  X, Y, W, H, Angle : Double; Alpha : Byte = 255; FX : LongWord = FX_BLEND ); overload;

var
    quad : array[ 0..3 ] of zglTPoint2D;

    vtx  : array[ 0..5 ] of zglTPoint2D;
    tex  : array [0..4] of array[ 0..5 ] of zglTPoint2D;
    clr  : array[ 0..5 ] of longword;
    hasclr : Boolean;
    idxcnt : Integer;

    p    : zglPPoint2D;
    mode : Integer;

    tc     : array [ 0..4] of record
      tU, tV, tX, tY, tW, tH : Double;
    end;

    i : integer;
    tU, tV, tX, tY, tW, tH : Double;

    x1, x2 : Double;
    y1, y2 : Double;
    cX, cY : Double;
    c, s   : Double;
    mX, mY : Double;
    mW, mH : Double;
    clrdw  : longword;
begin
  if length(Texture)=0 then Exit;
  //if not Assigned( Texture ) Then exit;

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

  for i:=0 to length(Texture)-1 do begin
    tU := 1 / ( Texture[i].Width  / Texture[i].U );
    tV := 1 / ( Texture[i].Height / Texture[i].V );
    if (CutRect[i].w=w) then begin
      tX := tU * (CutRect[i].X);
      tW := tX + tU * (CutRect[i].W);
    end else begin
      tX := tU * (CutRect[i].X+0.5);
      tW := tX + tU * (CutRect[i].W - 1.0);
    end;

    if (CutRect[i].h=h) then begin
      tY := tV * (( Texture[i].Height - CutRect[i].Y));
      tH := tY - tV * (CutRect[i].H);
    end else begin
      tY := tV * (( Texture[i].Height - CutRect[i].Y - 0.5));
      tH := tY - tV * (CutRect[i].H - 1.0);
    end;

    if FX and FX2D_FLIPX > 0 Then tU := tW - tX else tU := 0;
    if FX and FX2D_FLIPY > 0 Then tV := tH - tY else tV := 0;

    tc[i].tU:=tU;
    tc[i].tV:=tV;
    tc[i].tX:=tX;
    tc[i].tY:=tY;
    tc[i].tW:=tW;
    tc[i].tH:=tH;
  end;
(*

glTexCoord2f( tX + tU, tY + tV );
glVertex2dv( @quad[ 0 ] );

glColor4ubv( @fx2dVCA2[ 0 ] );
glTexCoord2f( tW - tU, tY + tV );
*)
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
    mode := GL_TRIANGLE_STRIP;

  if ( not b2dStarted ) or batch2d_Check( mode, FX, Texture[0] ) Then
    begin
      if FX and FX_BLEND > 0 Then begin
        glEnable( GL_BLEND );
        if FX and FX_BLENDALPHA > 0 then glEnable ( GL_ALPHA_TEST );
      end else
        glEnable( GL_ALPHA_TEST );

      //glBegin( mode );
    end;

  if FX and FX_COLOR > 0 Then
    begin
      fx2dAlpha^ := Alpha;
      //glColor4ubv( @fx2dColor[ 0 ] );
    end else
      begin
        fx2dAlphaDef^ := Alpha;
        //glColor4ubv( @fx2dColorDef[ 0 ] );
      end;

  if FX and FX2D_VCA > 0 Then
    begin
      for i:= 0 to Length(Texture)-1 do begin
        tex[i][0].X:=tc[i].tX + tc[i].tU; tex[i][0].Y:=tc[i].tY + tc[i].tV;
        tex[i][1].X:=tc[i].tW - tc[i].tU; tex[i][1].Y:=tc[i].tY + tc[i].tV;
        tex[i][2].X:=tc[i].tW - tc[i].tU; tex[i][2].Y:=tc[i].tH - tc[i].tV;

        tex[i][3].X:=tc[i].tW - tc[i].tU; tex[i][3].Y:=tc[i].tH - tc[i].tV;
        tex[i][4].X:=tc[i].tX + tc[i].tU; tex[i][4].Y:=tc[i].tH - tc[i].tV;
        tex[i][5].X:=tc[i].tX + tc[i].tU; tex[i][5].Y:=tc[i].tY + tc[i].tV;
      end;

      vtx[0]:=quad[0];
      vtx[1]:=quad[1];
      vtx[2]:=quad[2];
      vtx[3]:=quad[2];
      vtx[4]:=quad[3];
      vtx[5]:=quad[0];

      idxcnt:=6;

      clr[0]:=fx2dVCA1[ 0 ];
      clr[1]:=fx2dVCA2[ 0 ];
      clr[2]:=fx2dVCA3[ 0 ];
      clr[3]:=fx2dVCA3[ 0 ];
      clr[4]:=fx2dVCA4[ 0 ];
      clr[5]:=fx2dVCA1[ 0 ];
      hasclr := true;
    end else
      begin
        for i:= 0 to Length(Texture)-1 do begin

          tex[i][0].X:=tc[i].tX + tc[i].tU; tex[i][0].Y:=tc[i].tY + tc[i].tV;

          tex[i][1].X:=tc[i].tW - tc[i].tU; tex[i][1].Y:=tc[i].tY + tc[i].tV;

          tex[i][2].X:=tc[i].tX + tc[i].tU; tex[i][2].Y:=tc[i].tH - tc[i].tV;

          tex[i][3].X:=tc[i].tW - tc[i].tU; tex[i][3].Y:=tc[i].tH - tc[i].tV;
        end;

        vtx[0]:=quad[0];
        vtx[1]:=quad[1];
        vtx[3]:=quad[2];
        vtx[2]:=quad[3];

        idxcnt:=4;

        if FX and FX_COLOR > 0 Then
          clrdw:=LongWord(fx2dColor)
        else
          clrdw:=LongWord(fx2dColorDef);

        clr[0]:=clrdw;
        clr[1]:=clrdw;
        clr[2]:=clrdw;
        clr[3]:=clrdw;
        //fx2dAlphaDef^ := Alpha;
        //glColor4ubv( @fx2dColorDef[ 0 ] );

        hasclr := true;
      end;

  if hasclr then begin
    glEnableClientState(GL_COLOR_ARRAY);
    glColorPointer(4, GL_UNSIGNED_BYTE, 0, @clr[0]);
  end;

  for i:=0 to length(Texture)-1 do begin
    glActiveTexture(GL_TEXTURE0_ARB+i);
    glEnable( GL_TEXTURE_2D );
    glBindTexture( GL_TEXTURE_2D, Texture[i].ID );
    //glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );

    glClientActiveTexture(GL_TEXTURE0_ARB+i);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glTexCoordPointer( 2, GL_FLOAT, 0, @tex[i][0] );
  end;

  glEnableClientState(GL_VERTEX_ARRAY);
  glVertexPointer(2, GL_FLOAT, 0, @vtx[0] );

  glDrawArrays( mode, 0, idxcnt );

  glDisableClientState( GL_VERTEX_ARRAY );
  glDisableClientState( GL_TEXTURE_COORD_ARRAY );
  if hasclr then glDisableClientState( GL_COLOR_ARRAY );

  if not b2dStarted Then
    begin
      //glEnd();
      for i:=length(Texture)-1 downto 0 do begin
        glActiveTexture(GL_TEXTURE0_ARB+i);
        glDisable( GL_TEXTURE_2D );
        glDisable( GL_BLEND );
        glDisable( GL_ALPHA_TEST );
      end;
    end;
end;

end.
