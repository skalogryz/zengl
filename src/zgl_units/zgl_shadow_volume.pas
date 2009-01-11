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
unit zgl_shadow_volume;

{$I define.inc}

interface
uses
  GL, GLext,
  zgl_types,
  zgl_math;

procedure shadow_InitVolume( var Volume : zglPShadowVolume; const Vertices : zglPPoint3DArray; const FCount : DWORD; const Faces : zglPFaceArray );
procedure shadow_CalcVolume( const Volume : zglPShadowVolume; const Matrix : zglPMatrix4f; const Vertices : zglPPoint3DArray; const Light : zglTPoint3D; const RebuildPlanes : Boolean; const Extrude : Single );
procedure shadow_DrawVolume( const Volume : zglPShadowVolume; const zFail : Boolean );
procedure shadow_DrawShadowVolumes( const DrawVolumes : Pointer );

implementation

procedure shadow_InitVolume;
  var
    i, j : Integer;
    edgeI, edgeJ : Integer;
    edgeI1, edgeI2, edgeJ1, edgeJ2 : Integer;
    VA : array of zglTPoint3D;
  function AddIndex( v : zglTPoint3D; ID : DWORD ) : DWORD;
    var
      i : Integer;
  begin
    for i := 0 to length( VA ) - 1 do
      if ( v.X = VA[ i ].X ) and ( v.Y = VA[ i ].Y ) and ( v.Z = VA[ i ].Z ) Then
        begin
          Result := i;
          exit;
        end;
    SetLength( VA, length( VA ) + 1 );
    VA[ length( VA ) - 1 ] := v;
    Result := length( VA ) - 1;
  end;
begin
  Volume := AllocMem( SizeOf( zglTShadowVolume ) );
  Volume.FCount := FCount;
  with Volume^ do
    begin
      ICount := FCount * 3;

  	  SetLength( Caps, ICount );
  	  SetLength( Indices, ICount );
      for i := 0 to FCount - 1 do
        for j := 0 to 2 do
          Indices[ i * 3 + j ] := AddIndex( Vertices[ Faces[ i, j ] ], Faces[ i, j ] );
      SetLength( VA, 0 );

      SetLength( Planes, FCount );
      SetLength( eVertices, FCount * 3 );
      SetLength( isFacingLight, FCount );
      SetLength( neighbourIndices, FCount * 3 );
      SetLength( isSilhouetteEdge, FCount * 3 );

      for i := 0 to FCount * 3 - 1 do
        neighbourIndices[ i ] := -1;

      for i := 0 to FCount - 2 do
        for edgeI := 0 to 2 do
          begin
            if neighbourIndices[ i * 3 + edgeI ] <> -1 Then continue;

            for j := i + 1 to FCount - 1 do
              for edgeJ := 0 to 2 do
                begin
                  edgeI1 := Indices[ i * 3 + edgeI ];
                  edgeI2 := Indices[ i * 3 + ( edgeI + 1 ) mod 3 ];
                  edgeJ1 := Indices[ j * 3 + edgeJ ];
                  edgeJ2 := Indices[ j * 3 + ( edgeJ + 1 ) mod 3 ];

                  if ( ( edgeI1 = edgeJ1 ) and ( edgeI2 = edgeJ2 ) ) or ( ( edgeI1 = edgeJ2 ) and ( edgeI2 = edgeJ1 ) ) Then
                    begin
                      neighbourIndices[ i * 3 + edgeI ] := j;
                      neighbourIndices[ j * 3 + edgeJ ] := i;
                    end;
                end;
          end;
    end;
end;

procedure shadow_CalcVolume;
  var
    i, j : Integer;
    v1, v2 : zglTPoint3D;
    mLight : zglTPoint3D;
begin
  if Assigned( Matrix ) Then
    mLight := vector_MulM4f( Light, matrix4f_Inverse( Matrix^ ) );

  with Volume^ do
    begin
      if RebuildPlanes Then
        for i := 0 to FCount - 1 do
          Planes[ i ] := plane_Get( Vertices[ Indices[ i * 3 + 0 ] ],
                                    Vertices[ Indices[ i * 3 + 1 ] ],
                                    Vertices[ Indices[ i * 3 + 2 ] ] );

      FillChar( isFacingLight[ 0 ], FCount, 0 );
      for i := 0 to FCount - 1 do
        if plane_Distance( Planes[ i ], mLight ) >= 0 Then
          isFacingLight[ i ] := TRUE;

      FillChar( isSilhouetteEdge[ 0 ], FCount * 3, 0 );
      for i := 0 to FCount * 3 - 1 do
        if ( neighbourIndices[ i ] = -1 ) or ( not isFacingLight[ neighbourIndices[ i ] ] ) Then
          isSilhouetteEdge[ i ] := TRUE;

        eVCount := 0;
        for i := 0 to FCount - 1 do
          begin
            if not isFacingLight[ i ] Then continue;

            for j := 0 to 2 do
              if isSilhouetteEdge[ i * 3 + j ] Then
                begin
                  v1 := Vertices[ Indices[ i * 3 + j ] ];
    					    v2 := Vertices[ Indices[ i * 3 + ( j + 1 ) mod 3 ] ];
                  eVertices[ eVCount ] := v2; INC( eVCount );
                  eVertices[ eVCount ] := v1; INC( eVCount );
                  v1.X := ( v1.X - mLight.X ) * Extrude;
                  v1.Y := ( v1.Y - mLight.Y ) * Extrude;
                  v1.Z := ( v1.Z - mLight.Z ) * Extrude;
                  v2.X := ( v2.X - mLight.X ) * Extrude;
                  v2.Y := ( v2.Y - mLight.Y ) * Extrude;
                  v2.Z := ( v2.Z - mLight.Z ) * Extrude;
                  eVertices[ eVCount ] := v1; INC( eVCount );
                  eVertices[ eVCount ] := v2; INC( eVCount );
                end;
  		    end;

      for i := 0 to FCount * 3 - 1 do
        begin
          if isFacingLight[ i div 3 ] Then
            Caps[ i ] := Vertices[ Indices[ i ] ]
          else
            begin
              v1 := Vertices[ Indices[ i ] ];
              Caps[ i ].X := ( v1.X - mLight.X ) * Extrude;
              Caps[ i ].Y := ( v1.Y - mLight.Y ) * Extrude;
              Caps[ i ].Z := ( v1.Z - mLight.Z ) * Extrude;
            end;
        end;
    end;
end;

procedure shadow_DrawVolume;
  procedure DrawVolume;
  begin
    with Volume^ do
      begin
        glEnableClientState( GL_VERTEX_ARRAY );
        glVertexPointer( 3, GL_FLOAT, 0, @eVertices[ 0 ] );
        glDrawArrays( GL_QUADS, 0, eVCount );
        glDisableClientState( GL_VERTEX_ARRAY );

        if zFail Then
          begin
            glEnableClientState( GL_VERTEX_ARRAY );
            glVertexPointer( 3, GL_FLOAT, 0, @Caps[ 0 ] );
            glDrawArrays( GL_TRIANGLES, 0, ICount );
            glDisableClientState( GL_VERTEX_ARRAY );
          end;
      end;
  end;
begin
  if Assigned( glActiveStencilFaceEXT ) Then
    begin
      if zFail Then
        begin
          glDisable( GL_CULL_FACE );
          glEnable( GL_STENCIL_TEST_TWO_SIDE_EXT );

          glActiveStencilFaceEXT( GL_BACK );
          glStencilOp( GL_KEEP, GL_INCR_WRAP_EXT, GL_KEEP );
          glActiveStencilFaceEXT( GL_FRONT );
          glStencilOp( GL_KEEP, GL_DECR_WRAP_EXT, GL_KEEP );

          DrawVolume;

          glEnable( GL_CULL_FACE );
          glDisable( GL_STENCIL_TEST_TWO_SIDE_EXT );
        end else
          begin
            glDisable( GL_CULL_FACE );
            glEnable( GL_STENCIL_TEST_TWO_SIDE_EXT );

            glActiveStencilFaceEXT( GL_BACK );
            glStencilOp( GL_KEEP, GL_KEEP, GL_DECR_WRAP_EXT );
            glActiveStencilFaceEXT( GL_FRONT );
            glStencilOp( GL_KEEP, GL_KEEP, GL_INCR_WRAP_EXT );

            DrawVolume;

            glEnable( GL_CULL_FACE );
            glDisable( GL_STENCIL_TEST_TWO_SIDE_EXT );
          end;
    end else
      begin
        if zFail Then
          begin
            glStencilOp( GL_KEEP, GL_INCR, GL_KEEP );
            glCullFace( GL_FRONT );
            DrawVolume;

            glStencilOp( GL_KEEP, GL_DECR, GL_KEEP );
            glCullFace( GL_BACK );
            DrawVolume;
          end else
            begin
              glStencilOp( GL_KEEP, GL_KEEP, GL_INCR );
              glCullFace( GL_BACK );
              DrawVolume;

              glStencilOp( GL_KEEP, GL_KEEP, GL_DECR );
              glCullFace( GL_FRONT );
              DrawVolume;
            end;
      end;
end;

procedure shadow_DrawShadowVolumes;
begin
  glDepthFunc( GL_LESS );
  glEnable( GL_STENCIL_TEST );
  glDepthMask( GL_FALSE );

  glColorMask( GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE );
  glStencilFunc( GL_ALWAYS, 0, $FFFFFFFF );

  asm call DrawVolumes end;

  glStencilOp( GL_REPLACE, GL_REPLACE, GL_REPLACE );
  glStencilFunc( GL_NOTEQUAL, 0, $FFFFFFFF );

  glColorMask( GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE );
  glDisable( GL_DEPTH_TEST );
  glDisable( GL_CULL_FACE );

  glPushMatrix;
  glLoadIdentity;
  glMatrixMode( GL_PROJECTION );
  glPushMatrix;
  glLoadIdentity;

  glBegin( GL_QUADS );
    glVertex3f( -1, -1, 0 );
    glVertex3f(  1, -1, 0 );
    glVertex3f(  1,  1, 0 );
    glVertex3f( -1,  1, 0 );
  glEnd;

  glPopMatrix;
  glMatrixMode( GL_MODELVIEW );
  glPopMatrix;
  glDisable( GL_STENCIL_TEST );

  glDepthMask( GL_TRUE );
  glEnable( GL_DEPTH_TEST );
  glDepthFunc( GL_LEQUAL );
    
  glColor3f( 1, 1, 1 );
end;

end.
