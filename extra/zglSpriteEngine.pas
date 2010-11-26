unit zglSpriteEngine;

// Если проект не собирается с ZenGL статически, то стоит закоментировать этот define
{$DEFINE STATIC}

interface

uses
  {$IFNDEF STATIC}
  zglHeader
  {$ELSE}
  zgl_textures
  {$ENDIF}
  ;

type
  zglCSprite2D  = class;
  zglCSEngine2D = class;

  zglCSEngine2D = class
  protected
    FCount : LongWord;
    FList  : array of zglCSprite2D;

    procedure SortByLayer( iLo, iHi : Integer );
    procedure SortByID( iLo, iHi : Integer );

    function  GetSprite( ID : LongWord ) : zglCSprite2D;
    procedure SetSprite( ID : LongWord; const Sprite : zglCSprite2D );
  public
    function  AddSprite : Integer; overload;
    function  AddSprite( const Texture : zglPTexture; const Layer : Integer ) : zglCSprite2D; overload;
    procedure DelSprite( const ID : Integer );
    procedure ClearAll;

    procedure Draw;
    procedure Proc;

    property Count: LongWord read FCount;
    property List[ID : LongWord]: zglCSprite2D read GetSprite write SetSprite;
  end;

  zglCSprite2D = class
  protected
  public
    ID      : Integer;
    Manager : zglCSEngine2D;
    Texture : zglPTexture;
    Destroy : Boolean;
    Layer   : Integer;
    X, Y    : Single;
    W, H    : Single;
    Angle   : Single;
    Frame   : Single;
    Alpha   : Integer;
    FxFlags : LongWord;

    constructor Create( const _Manager : zglCSEngine2D; const _ID : Integer );
    destructor  Free;

    procedure OnInit( const _Texture : zglPTexture; const _Layer : Integer ); virtual;
    procedure OnDraw; virtual;
    procedure OnProc; virtual;
    procedure OnFree; virtual;
  end;

implementation
{$IFDEF STATIC}
uses
  zgl_main,
  zgl_fx,
  zgl_sprite_2d;
{$ENDIF}

procedure zglCSEngine2D.SortByLayer( iLo, iHi : Integer );
  var
    lo, hi, mid : Integer;
    t : zglCSprite2D;
begin
  lo   := iLo;
  hi   := iHi;
  mid  := FList[ ( lo + hi ) shr 1 ].Layer;

  repeat
    while FList[ lo ].Layer < mid do INC( lo );
    while FList[ hi ].Layer > mid do DEC( hi );
    if lo <= hi then
      begin
        t           := FList[ lo ];
        FList[ lo ] := FList[ hi ];
        FList[ hi ] := t;
        INC( lo );
        DEC( hi );
      end;
  until lo > hi;

  if hi > iLo Then SortByLayer( iLo, hi );
  if lo < iHi Then SortByLayer( lo, iHi );
end;

procedure zglCSEngine2D.SortByID( iLo, iHi : Integer );
  var
    lo, hi, mid : Integer;
    t : zglCSprite2D;
begin
  lo   := iLo;
  hi   := iHi;
  mid  := FList[ ( lo + hi ) shr 1 ].ID;

  repeat
    while FList[ lo ].ID < mid do INC( lo );
    while FList[ hi ].ID > mid do DEC( hi );
    if Lo <= Hi then
      begin
        t           := FList[ lo ];
        FList[ lo ] := FList[ hi ];
        FList[ hi ] := t;
        INC( lo );
        DEC( hi );
      end;
  until lo > hi;

  if hi > iLo Then SortByID( iLo, hi );
  if lo < iHi Then SortByID( lo, iHi );
end;

function zglCSEngine2D.GetSprite( ID : LongWord ) : zglCSprite2D;
begin
  Result := FList[ ID ];
end;

procedure zglCSEngine2D.SetSprite( ID : LongWord; const Sprite : zglCSprite2D );
begin
  FList[ ID ] := Sprite;
end;

function zglCSEngine2D.AddSprite : Integer;
begin
  if FCount + 1 > length( FList ) Then
    SetLength( FList, FCount + 16384 );
  Result := FCount;
  INC( FCount );
end;

function zglCSEngine2D.AddSprite( const Texture : zglPTexture; const Layer : Integer ) : zglCSprite2D;
  var
    id : Integer;
begin
  id := AddSprite();

  FList[ id ] := zglCSprite2D.Create( Self, id );
  Result := FList[ id ];
  Result.OnInit( Texture, Layer );
end;

procedure zglCSEngine2D.DelSprite( const ID : Integer );
  var
    i : Integer;
begin
  if ( ID < 0 ) or ( ID > FCount - 1 ) or ( FCount = 0 ) Then exit;

  FList[ ID ].Free;

  for i := ID to FCount - 2 do
    begin
      FList[ i ]    := FList[ i + 1 ];
      FList[ i ].ID := i;
    end;

  DEC( FCount );
end;

procedure zglCSEngine2D.ClearAll;
  var
    i : Integer;
begin
  for i := 0 to FCount - 1 do
    FList[ i ].Free;
  SetLength( FList, 0 );
  FCount := 0;
end;

procedure zglCSEngine2D.Draw;
  var
    i, a, b, l : Integer;
    s : zglCSprite2D;
begin
  if FCount > 1 Then
    begin
      l := 0;
      for i := 0 to FCount - 1 do
        begin
          s := FList[ i ];
          if s.Layer > l Then l := s.Layer;
          if s.Layer < l Then
            begin
              SortByLayer( 0, FCount - 1 );
              // TODO: наверное сделать выбор вкл./выкл. устойчивой сортировки
              l := FList[ 0 ].Layer;
              a := 0;
              for b := 0 to FCount - 1 do
                begin
                  s := FList[ b ];
                  if ( l <> s.Layer ) Then
                    begin
                      SortByID( a, b - 1 );
                      a := b;
                      l := s.Layer;
                    end;
                  if b = FCount - 1 Then
                    SortByID( a, b );
                end;
              for a := 0 to FCount - 1 do
                FList[ a ].ID := a;
              break;
            end;
        end;
    end;

  i := 0;
  while i < FCount do
    begin
      s := FList[ i ];
      s.OnDraw();

      if s.Destroy Then
        DelSprite( s.ID )
      else
        INC( i );
    end;
end;

procedure zglCSEngine2D.Proc;
  var
    i : Integer;
    s : zglCSprite2D;
begin
  i := 0;
  while i < FCount do
    begin
      s := FList[ i ];
      s.OnProc();

      if s.Destroy Then
        DelSprite( s.ID )
      else
        INC( i );
    end;
end;

constructor zglCSprite2D.Create( const _Manager : zglCSEngine2D; const _ID : Integer );
begin
  Manager := _Manager;
  ID      := _ID;

  OnInit( nil, 0 );
end;

destructor zglCSprite2D.Free;
begin
  OnFree;
end;

procedure zglCSprite2D.OnInit( const _Texture : zglPTexture; const _Layer : Integer );
begin
  Texture := _Texture;
  Layer   := _Layer;
  X       := 0;
  Y       := 0;
  if Assigned( Texture ) Then
    begin
      W   := Texture.Width div Texture.FramesX;
      H   := Texture.Height div Texture.FramesY;
    end else
      begin
        W := 0;
        H := 0;
      end;
  Angle   := 0;
  Frame   := 1;
  Alpha   := 255;
  FxFlags := FX_BLEND;
end;

procedure zglCSprite2D.OnDraw;
begin
  asprite2d_Draw( Texture, X, Y, W, H, Angle, Round( Frame ), Alpha, FxFlags );
end;

procedure zglCSprite2D.OnProc;
begin
end;

procedure zglCSprite2D.OnFree;
begin
end;

end.
