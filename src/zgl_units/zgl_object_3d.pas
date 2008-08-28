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
unit zgl_object_3d;

{$I define.inc}

interface

uses
  GL, GLExt,
  zgl_opengl,
  zgl_global_var,
  zgl_types;
  
const
  OBJ3D_TEXTURING     = $0000001;
  OBJ3D_MTEXTURING    = $0000002;
  OBJ3D_BLEND         = $0000004;
  OBJ3D_ALPHA_TEST    = $0000008;
  OBJ3D_WIRE_FRAME    = $0000010;
  OBJ3D_CULL_FACE     = $0000020;
  OBJ3D_LIGHTING      = $0000040;
  OBJ3D_SPHERE_MAP_S  = $0000080;
  OBJ3D_SPHERE_MAP_T  = $0000100;

  MAT_DIFFUSE         = $01;
  MAT_AMBIENT         = $02;
  MAT_SPECULAR        = $03;
  MAT_SHININESS       = $04;
  MAT_EMISSION        = $05;
  
  SIDE_FRONT          = $01;
  SIDE_BACK           = $02;
  SIDE_FRONT_AND_BACK = $03;
  
procedure obj3d_Begin( Flags : DWORD ); extdecl;
procedure obj3d_End; extdecl;

procedure obj3d_Enable( Flags : DWORD ); extdecl;
procedure obj3d_Disable( Flags : DWORD ); extdecl;

procedure obj3d_SetColor( Color : DWORD; Alpha : Byte ); extdecl;
procedure obj3d_BindTexture( Texture : zglPTexture; Level : Byte ); extdecl;

procedure obj3d_SetMaterial( Material, Side : Byte; Color : DWORD; Alpha : Byte ); extdecl;

const
  AX = $01;
  AY = $02;
  AZ = $04;
procedure obj3d_Rotate( Angle : Single; Axis : Byte ); extdecl;
procedure obj3d_Scale( ScaleX, ScaleY, ScaleZ : Single ); extdecl;
procedure obj3d_Move( X, Y, Z : Single ); extdecl;
procedure obj3d_SetMatrix( Matrix : zglPMatrix4f ); extdecl;
procedure obj3d_MulMatrix( Matrix : zglPMatrix4f ); extdecl;

var
  tLastFlags : DWORD;

implementation

procedure obj3d_Begin;
begin
  obj3d_Enable( Flags );
  tLastFlags := Flags;
  
  glPushMatrix;
end;

procedure obj3d_End;
begin
  obj3d_Disable( tLastFlags );
  tLastFlags := 0;
  
  glPopMatrix;
end;

procedure obj3d_Enable;
begin
  tLastFlags := tLastFlags or Flags;
  if Flags and OBJ3D_TEXTURING > 0 Then
    glEnable( GL_TEXTURE_2D );
  if Flags and OBJ3D_MTEXTURING > 0 Then
    begin
      gl_TexCoord2f  := @gl_MTexCoord2f;
      gl_TexCoord2fv := @gl_MTexCoord2fv;
      glEnable( GL_TEXTURE_2D );
    end;
    
  if Flags and OBJ3D_BLEND > 0 Then
    glEnable( GL_BLEND )
  else
    if Flags and OBJ3D_ALPHA_TEST > 0 Then
      glEnable( GL_ALPHA_TEST );
      
  if Flags and OBJ3D_WIRE_FRAME > 0 Then
    glPolygonMode( GL_FRONT_AND_BACK, GL_LINE );
    
  if Flags and OBJ3D_CULL_FACE > 0 Then
    glEnable( GL_CULL_FACE );
    
  if Flags and OBJ3D_LIGHTING > 0 Then
    glEnable( GL_LIGHTING );

  if Flags and OBJ3D_SPHERE_MAP_S > 0 Then
    begin
      glEnable ( GL_TEXTURE_GEN_S                         );
      glTexGenf( GL_S, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP );
    end;

  if Flags and OBJ3D_SPHERE_MAP_T > 0 Then
    begin
      glEnable ( GL_TEXTURE_GEN_T                         );
      glTexGenf( GL_T, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP );
    end;
end;

procedure obj3d_Disable;
  var
    i : Integer;
begin
  tLastFlags := tLastFlags xor Flags;
  if Flags and OBJ3D_TEXTURING > 0 Then
    glDisable( GL_TEXTURE_2D );
  if Flags and OBJ3D_MTEXTURING > 0 Then
    begin
      gl_TexCoord2f  := @glTexCoord2f;
      gl_TexCoord2fv := @glTexCoord2fv;
      glDisable( GL_TEXTURE_2D );
      for i := 0 to ogl_MaxTexLevels do
        begin
          glActiveTextureARB( GL_TEXTURE0_ARB + i );
          glDisable( GL_TEXTURE_2D );
        end;
      glActiveTextureARB( GL_TEXTURE0_ARB );
    end;

  if Flags and OBJ3D_BLEND > 0 Then
    glDisable( GL_BLEND )
  else
    if Flags and OBJ3D_ALPHA_TEST > 0 Then
      glDisable( GL_ALPHA_TEST );
      
  if Flags and OBJ3D_WIRE_FRAME > 0 Then
    glPolygonMode( GL_FRONT_AND_BACK, GL_FILL );
    
  if Flags and OBJ3D_CULL_FACE > 0 Then
    glDisable( GL_CULL_FACE );
    
  if Flags and OBJ3D_LIGHTING > 0 Then
    glDisable( GL_LIGHTING );

  if Flags and OBJ3D_SPHERE_MAP_S > 0 Then
    glDisable( GL_TEXTURE_GEN_S );

  if Flags and OBJ3D_SPHERE_MAP_T > 0 Then
    glDisable( GL_TEXTURE_GEN_T );
end;

procedure obj3d_SetColor;
  var
    clr : array[ 0..3 ] of Single;
begin
  clr[ 0 ] := (   Color and $FF   )         / 255;
  clr[ 1 ] := ( ( Color and $FF00 ) shr 8 ) / 255;
  clr[ 2 ] := (   Color shr 16    )         / 255;
  clr[ 3 ] := ( 1 / 255 ) * Alpha;
  if glIsEnabled( GL_LIGHTING ) = GL_TRUE Then
    glMaterialfv( GL_FRONT_AND_BACK, GL_DIFFUSE, @clr )
  else
    glColor4fv( @clr );
end;

procedure obj3d_BindTexture;
begin
  if ( Level = 0 ) and ( tLastFlags and OBJ3D_MTEXTURING = 0 ) Then
    begin
      if ( tLastFlags and OBJ3D_TEXTURING = 0 ) Then exit;
      glBindTexture( GL_TEXTURE_2D, Texture.ID );
      ogl_LastTexture := Texture;
    end else
      begin
        if ( tLastFlags and OBJ3D_MTEXTURING = 0 ) Then exit;
        ogl_MTexActive[ Level ] := TRUE;
        ogl_MTexture  [ Level ] := Texture.ID;
        if ogl_MaxTexLevels > 0 Then
          begin
            glActiveTextureARB( GL_TEXTURE0_ARB + Level );
            glBindTexture     ( GL_TEXTURE_2D, Texture.ID );
            glEnable( GL_TEXTURE_2D );
          end;
      end;
end;

procedure obj3d_SetMaterial;
  var
    clr : array[ 0..3 ] of Single;
begin
  if Material and MAT_SHININESS > 0 Then
    begin
      glMateriali( GL_FRONT          * Byte( Side = SIDE_FRONT ) or
                   GL_BACK           * Byte( Side = SIDE_BACK ) or
                   GL_FRONT_AND_BACK * Byte( Side = SIDE_FRONT_AND_BACK ),
                   GL_SHININESS,
                   Color )
    end else
      begin
        clr[ 0 ] := (   Color and $FF   )         / 255;
        clr[ 1 ] := ( ( Color and $FF00 ) shr 8 ) / 255;
        clr[ 2 ] := (   Color shr 16    )         / 255;
        clr[ 3 ] := ( 1 / 255 ) * Alpha;
  
        glMaterialfv( GL_FRONT          * Byte( Side = SIDE_FRONT ) or
                      GL_BACK           * Byte( Side = SIDE_BACK ) or
                      GL_FRONT_AND_BACK * Byte( Side = SIDE_FRONT_AND_BACK ),

                      GL_AMBIENT  * Byte( Material = MAT_AMBIENT  ) or
                      GL_DIFFUSE  * Byte( Material = MAT_DIFFUSE  ) or
                      GL_SPECULAR * Byte( Material = MAT_SPECULAR ) or
                      GL_EMISSION * Byte( Material = MAT_EMISSION ),
                      @clr );
      end;
end;

procedure obj3d_Rotate;
begin
  glRotatef( Angle, Axis and AX, Axis and AY, Axis and AZ );
end;

procedure obj3d_Scale;
begin
  glScalef( ScaleX, ScaleY, ScaleZ );
end;

procedure obj3d_Move;
begin
  glTranslatef( X, Y, Z );
end;

procedure obj3d_SetMatrix;
begin
  glLoadMatrixf( @Matrix^ );
end;

procedure obj3d_MulMatrix;
begin
  glMultMatrixf( @Matrix^ );
end;

end.
