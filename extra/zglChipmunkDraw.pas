unit zglChipmunkDraw;

// Если проект не собирается с ZenGL статически, то стоит закоментировать этот define
{$DEFINE STATIC}

interface
uses
  {$IFDEF STATIC}
  zgl_types,
  zgl_primitives_2d,
  {$ELSE}
  zglHeader,
  {$ENDIF}
  zglChipmunk;

procedure cpDrawSpace( const space : PcpSpace; const DrawCollisions : Boolean );

var
  cpColorStatic    : DWORD = $00FF00;
  cpColorActive    : DWORD = $0000FF;
  cpColorCollision : DWORD = $FF0000;

implementation

procedure cpDrawShape( obj : pointer; data : pointer );
  var
    i     : Integer;
    shape : PcpShape;
begin
  shape := obj;
  case shape.klass.cptype of
    CP_CIRCLE_SHAPE:
      with PcpCircleShape( shape )^ do
        begin
          pr2d_Circle( tc.x, tc.y, r, PDWORD( data )^, 255, 32, PR2D_SMOOTH );
          pr2d_Line( tc.x, tc.y, tc.x + shape.body.rot.x * r, tc.y + shape.body.rot.y * r, PDWORD( data )^, 255, PR2D_SMOOTH );
        end;
    CP_SEGMENT_SHAPE:
      with PcpSegmentShape( shape )^ do
        begin
          pr2d_Line( ta.x, ta.y, tb.x, tb.y, PDWORD( data )^, 255, PR2D_SMOOTH );
        end;
    CP_POLY_SHAPE:
      with PcpPolyShape( shape )^ do
        begin
          for i := 0 to numVerts - 2 do
            pr2d_Line( tverts[ i ].x, tverts[ i ].y, tverts[ i + 1 ].x, tverts[ i + 1 ].y, PDWORD( data )^, 255, PR2D_SMOOTH );
          pr2d_Line( tverts[ numVerts - 1 ].x, tverts[ numVerts - 1 ].y, tverts[ 0 ].x, tverts[ 0 ].y, PDWORD( data )^, 255, PR2D_SMOOTH );
        end;
  end;
end;

procedure cpDrawCollision( ptr : pointer; data : pointer );
  var
    i : integer;
    a : PcpArbiter;
    v : cpVect;
begin
  a := ptr;
  for i := 0 to a.numContacts - 1 do
    begin
      v := a.contacts[ i ].p;
      pr2d_Circle( v.x, v.y, 4, PDWORD( data )^, 255, 8, PR2D_SMOOTH or PR2D_FILL );
    end;
end;

procedure cpDrawSpace;
begin
  if not Assigned( space ) Then exit;

  cpSpaceHashEach( space.staticShapes, cpDrawShape, @cpColorStatic );
  cpSpaceHashEach( space.activeShapes, cpDrawShape, @cpColorActive );

  if DrawCollisions Then
    cpArrayEach( space.arbiters, cpDrawCollision, @cpColorCollision );
end;

end.
