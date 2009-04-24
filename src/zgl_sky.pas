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
unit zgl_sky;

{$I zgl_config.cfg}

interface

uses
  zgl_opengl_all,
  zgl_textures;

procedure skybox_Init( const Top, Bottom, Left, Right, Front, Back : zglPTexture );
procedure skybox_Draw;

implementation

var
  sb_Top    : zglPTexture;
  sb_Bottom : zglPTexture;
  sb_Left   : zglPTexture;
  sb_Right  : zglPTexture;
  sb_Front  : zglPTexture;
  sb_Back   : zglPTexture;

procedure skybox_Init;
begin
  sb_Top    := Top;
  sb_Bottom := Bottom;
  sb_Left   := Left;
  sb_Right  := Right;
  sb_Front  := Front;
  sb_Back   := Back;
end;

procedure skybox_Draw;
begin
  glEnable( GL_TEXTURE_2D );

  // Top
  glBindTexture  ( GL_TEXTURE_2D, sb_Top.ID                           );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE );

  glBegin( GL_QUADS );
  glTexCoord2f( 0,        sb_Top.V ); glVertex3f( -1, 1, -1 );
  glTexCoord2f( sb_Top.U, sb_Top.V ); glVertex3f( -1, 1,  1 );
  glTexCoord2f( sb_Top.U, 0        ); glVertex3f(  1, 1,  1 );
  glTexCoord2f( 0,        0        ); glVertex3f(  1, 1, -1 );
  glEnd;

  // Bottom
  glBindTexture  ( GL_TEXTURE_2D, sb_Bottom.ID                        );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE );

  glBegin( GL_QUADS );
  glTexCoord2f( 0,           0           ); glVertex3f( -1, -1, -1 );
  glTexCoord2f( 0,           sb_Bottom.V ); glVertex3f(  1, -1, -1 );
  glTexCoord2f( sb_Bottom.U, sb_Bottom.V ); glVertex3f(  1, -1,  1 );
  glTexCoord2f( sb_Bottom.U, 0           ); glVertex3f( -1, -1,  1 );
  glEnd;

  // Left
  glBindTexture  ( GL_TEXTURE_2D, sb_Left.ID                          );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE );

  glBegin( GL_QUADS );
  glTexCoord2f( sb_Left.U, 0         ); glVertex3f( -1, -1, -1 );
  glTexCoord2f( 0,         0         ); glVertex3f( -1, -1,  1 );
  glTexCoord2f( 0,         sb_Left.V ); glVertex3f( -1,  1,  1 );
  glTexCoord2f( sb_Left.U, sb_Left.V ); glVertex3f( -1,  1, -1 );
  glEnd;

  // Right
  glBindTexture  ( GL_TEXTURE_2D, sb_Right.ID                         );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE );

  glBegin( GL_QUADS );
  glTexCoord2f( 0,          0          ); glVertex3f( 1, -1, -1 );
  glTexCoord2f( sb_Right.U, 0          ); glVertex3f( 1, -1,  1 );
  glTexCoord2f( sb_Right.U, sb_Right.V ); glVertex3f( 1,  1,  1 );
  glTexCoord2f( 0,          sb_Right.V ); glVertex3f( 1,  1, -1 );
  glEnd;

  // Back
  glBindTexture  ( GL_TEXTURE_2D, sb_Back.ID                          );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE );

  glBegin( GL_QUADS );
  glTexCoord2f( 0,         0         ); glVertex3f( -1, -1, -1 );
  glTexCoord2f( 0,         sb_Back.V ); glVertex3f( -1,  1, -1 );
  glTexCoord2f( sb_Back.U, sb_Back.V ); glVertex3f(  1,  1, -1 );
  glTexCoord2f( sb_Back.U, 0         ); glVertex3f(  1, -1, -1 );
  glEnd;

  // Front
  glBindTexture  ( GL_TEXTURE_2D, sb_Front.ID                         );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE );

  glBegin( GL_QUADS );
  glTexCoord2f( sb_Front.U, 0          ); glVertex3f( -1, -1, 1 );
  glTexCoord2f( sb_Front.U, sb_Front.V ); glVertex3f( -1,  1, 1 );
  glTexCoord2f( 0,          sb_Front.V ); glVertex3f(  1,  1, 1 );
  glTexCoord2f( 0,          0          ); glVertex3f(  1, -1, 1 );
  glEnd;

  glDisable( GL_TEXTURE_2D );

  glClear( GL_DEPTH_BUFFER_BIT );
end;

end.
