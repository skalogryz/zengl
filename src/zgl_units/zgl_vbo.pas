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
unit zgl_vbo;

{$I define.inc}

interface

uses
  GL, GLExt,
  zgl_const,
  zgl_types,
  zgl_global_var;
  
procedure vbo_Build( var IBuffer, VBuffer : DWORD; ICount, VCount : DWORD; Indices, Vertices, Normals, TexCoords, MultiTexCoords : Pointer; var Flags : DWORD );
procedure vbo_Free( var IBuffer, VBuffer : DWORD; ICount, VCount : DWORD );
procedure vbo_Check( var Flags : DWORD );

implementation

var
  tTexLevel : Byte;

{------------------------------------------------------------------------------}
{-------------------------------- zgl_vbo.pp ----------------------------------}
{------------------------------------------------------------------------------}
procedure vbo_Build;
  var
    i, size : DWORD;
    pn, pt  : DWORD;
    Mode    : DWORD;
begin
  if Flags and USE_MULTITEX3 > 0 Then
    begin
      Flags := Flags or USE_MULTITEX2;
      Flags := Flags or USE_MULTITEX1;
    end else
      if Flags and USE_MULTITEX2 > 0 Then
        Flags := Flags or USE_MULTITEX1;

  if Flags and BUILD_VBO_STATIC > 0 Then
    Mode := GL_STATIC_DRAW_ARB
  else
    Mode := GL_STREAM_DRAW_ARB;

  if ogl_CanVBO Then
    begin
      glGenBuffersARB( 1, @IBuffer );
      glBindBufferARB( GL_ELEMENT_ARRAY_BUFFER_ARB, IBuffer );
      if VCount < 65536 Then
        glBufferDataARB( GL_ELEMENT_ARRAY_BUFFER_ARB, ICount * 2, Indices, Mode )
      else
        glBufferDataARB( GL_ELEMENT_ARRAY_BUFFER_ARB, ICount * 4, Indices, Mode );
      glBindBufferARB( GL_ELEMENT_ARRAY_BUFFER_ARB, 0 );

      size := VCount * 12;
      if Flags and USE_NORMALS > 0 Then
        begin
          pn := size;
          INC( size, VCount * 12 );
        end;
      if Flags and USE_TEXTURE > 0 Then
        begin
          pt := size;
          INC( size, VCount * 8 );
          if ogl_MaxTexLevels > 0 Then
            begin
              tTexLevel := Byte( Flags and USE_MULTITEX1 > 0 ) +
                           Byte( Flags and USE_MULTITEX2 > 0 ) +
                           Byte( Flags and USE_MULTITEX3 > 0 );
              for i := 1 to tTexLevel do
                INC( size, VCount * 8 );
            end;
        end;

      glGenBuffersARB( 1, @VBuffer );
      glBindBufferARB( GL_ARRAY_BUFFER_ARB, VBuffer );
      glBufferDataARB( GL_ARRAY_BUFFER_ARB, size, nil, Mode );
      glBufferSubDataARB( GL_ARRAY_BUFFER_ARB, 0, VCount * 12, Vertices );
      if Flags and USE_NORMALS > 0 Then
        glBufferSubDataARB( GL_ARRAY_BUFFER_ARB, pn, VCount * 12, Normals );
      if Flags and USE_TEXTURE > 0 Then
        begin
          glBufferSubDataARB( GL_ARRAY_BUFFER_ARB, pt, VCount * 8, TexCoords );
          if ogl_MaxTexLevels > 0 Then
            glBufferSubDataARB( GL_ARRAY_BUFFER_ARB, pt + VCount * 8, tTexLevel * VCount * 8, MultiTexCoords );
        end;

      glBindBufferARB( GL_ARRAY_BUFFER_ARB, 0 );
    end;
end;

procedure vbo_Free;
begin
  if ogl_CanVBO Then
    begin
      if glIsBufferARB( VBuffer ) = GL_TRUE Then
        glDeleteBuffersARB( VCount * 12 * 2 + VCount * 8, @VBuffer );
      if glIsBufferARB( IBuffer ) = GL_TRUE Then
        if VCount < 65536 Then
          glDeleteBuffersARB( ICount * 2, @IBuffer )
        else
          glDeleteBuffersARB( ICount * 4, @IBuffer );
    end;
end;

procedure vbo_Check;
begin
  if ( ( Flags and BUILD_VBO_STATIC > 0 ) or ( Flags and BUILD_VBO_STREAM > 0 ) ) and ogl_CanVBO Then
    Flags := Flags or USE_VBO
  else
    if Flags and BUILD_VBO_STATIC > 0 Then
      Flags := Flags xor BUILD_VBO_STATIC
    else
      if Flags and BUILD_VBO_STREAM > 0 Then
        Flags := Flags xor BUILD_VBO_STREAM;
end;

{procedure vbo_Draw;
  var
    i  : Integer;
    PV : Ptr = 0;
    PN : Ptr = 0;
    PT : Ptr = 0;
Begin
  if ogl_MaxTexLevels > 0 Then
    tTexLevel := Byte( VBO.Flags and USE_MULTITEX1 > 0 ) +
                 Byte( VBO.Flags and USE_MULTITEX2 > 0 ) +
                 Byte( VBO.Flags and USE_MULTITEX3 > 0 );
  
  if ogl_CanVBO Then
    begin
      PV := 0;
      if VBO.Flags and USE_NORMALS > 0 Then PN := PV + VBO.VCount * 12;
      if VBO.Flags and USE_TEXTURE > 0 Then PT := PV + PN + VBO.VCount * 12;
      glBindBufferARB( GL_ARRAY_BUFFER_ARB, VBO.VBuffer );
      glBindBufferARB( GL_ELEMENT_ARRAY_BUFFER_ARB, VBO.IBuffer );
    end else
      begin
        PV := Ptr( @VBO.Vertices[ 0 ] );
        if VBO.Flags and USE_NORMALS > 0 Then PN := Ptr( @VBO.Normals[ 0 ] );
        if VBO.Flags and USE_TEXTURE > 0 Then PT := Ptr( @VBO.TexCoords[ 0 ] );
      end;

  if ( VBO.Flags and USE_NORMALS > 0 ) and ( glIsEnabled( GL_LIGHTING ) = GL_TRUE ) Then
    begin
      glEnableClientState( GL_NORMAL_ARRAY );
      glNormalPointer( GL_FLOAT, 0, Pointer( PN ) );
    end;
  if VBO.Flags and USE_TEXTURE > 0 Then
    begin
      glClientActiveTextureARB( GL_TEXTURE0_ARB );
      glEnableClientState( GL_TEXTURE_COORD_ARRAY );
      glTexCoordPointer( 2, GL_FLOAT, 0, Pointer( PT ) );

      if ogl_MaxTexLevels > 0 Then
        for i := 1 to tTexLevel do
          begin
            glClientActiveTextureARB( GL_TEXTURE0_ARB + i );
            glEnableClientState( GL_TEXTURE_COORD_ARRAY );
            if ogl_CanVBO Then
              glTexCoordPointer( 2, GL_FLOAT, 0, Pointer( PT + VBO.VCount * i * 8  ) )
            else
              glTexCoordPointer( 2, GL_FLOAT, 0, @VBO.MultiTexCoords[ 0 + VBO.VCount * ( i - 1 ) ] );
          end;
    end;
    
  glEnableClientState( GL_VERTEX_ARRAY );
  glVertexPointer( 3, GL_FLOAT, 0, Pointer( PV ) );

  if ogl_CanVBO Then
    begin
      if VBO.VCount < 65536 Then
        glDrawElements( GL_TRIANGLES, VBO.FCount * 3, GL_UNSIGNED_SHORT, nil )
      else
        glDrawElements( GL_TRIANGLES, VBO.FCount * 3, GL_UNSIGNED_INT, nil );
    end else
      if VBO.VCount < 65536 Then
        glDrawElements( GL_TRIANGLES, VBO.FCount * 3, GL_UNSIGNED_SHORT, VBO.Indices )
      else
        glDrawElements( GL_TRIANGLES, VBO.FCount * 3, GL_UNSIGNED_INT, VBO.Indices );

  glDisableClientState( GL_VERTEX_ARRAY );
  glDisableClientState( GL_NORMAL_ARRAY );
  glDisableClientState( GL_TEXTURE_COORD_ARRAY );
  if ogl_CanVBO Then
    begin
      glBindBufferARB( GL_ARRAY_BUFFER_ARB, 0 );
      glBindBufferARB( GL_ELEMENT_ARRAY_BUFFER_ARB, 0 );
    end;
end;}

end.
