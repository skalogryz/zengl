unit zgl_msvcrt;

interface
uses
  zgl_types;

{$IFDEF FPC}
  {$LINKLIB libmsvcrt.a}

  function pow( x : cdouble ) : cdouble; cdecl; external 'msvcrt.dll';
{$ELSE}
  function memcpy( dest : Pointer; src : Pointer; count : csize_t ) : Pointer; cdecl; external 'msvcrt.dll';
  function memset( dest : Pointer; c : Integer; count : csize_t ) : Pointer; cdecl; external 'msvcrt.dll';
  function malloc( size : csize_t ) : Pointer; cdecl; external 'msvcrt.dll';
  procedure free( memblock : Pointer ); cdecl; external 'msvcrt.dll';
{$ENDIF}

implementation

end.
