{-------------------------------}
{-----------= ZenGL =-----------}
{-------- Sprite Engine --------}
{-------------------------------}
{ build: 2                      }
{ date:  09.08.08               }
{-------------------------------}
{ by:   Andru ( Kemka Andrey )  }
{ mail: dr.andru@gmail.com      }
{ ICQ:  496-929-849             }
{ site: http://andru.2x4.ru     }
{-------------------------------}
{                      (C) 2008 }
{-------------------------------}
unit zglSpriteEngine;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface
uses zglHeader;

type
  zglCSprite = class
  protected
  public
    ID            : WORD;
    X, Y          : Single;
    Z             : Integer;
    Width, Height : Single;
    Alpha         : Byte;
    Angle         : Single;
    FX            : DWORD;
    Animated      : Boolean;
    AnimSpeed     : Single;
    Frame         : Single;
    Texture       : zglPTexture;
    Kill          : Boolean;

    constructor Create; virtual;
    procedure Freeing; virtual;

    procedure Draw;    virtual;
    procedure Process; virtual;
end;

type
  zglCSpriteEngine = class
  protected
    procedure qSortN( iLo, iHi : WORD );
    procedure qSortI( iLo, iHi : WORD );
  public
    List     : array[ 0..4095 ] of zglCSprite;
    DrawList : array[ 0..4095 ] of WORD;
    Count    : WORD;

    InvertSort : Boolean;

    function  CreateNew : zglCSprite;
    function  AddNew : WORD;
    procedure Del( ID : WORD );

    procedure Sort;

    procedure Draw;    virtual;
    procedure Process; virtual;
end;

var
  SEngine  : zglCSpriteEngine;

implementation

{------------------------------------------------------------------------------}
{-------------------------------  S P R I T E  --------------------------------}
{------------------------------------------------------------------------------}

constructor zglCSprite.Create;
begin
  inherited;
  X         := 0;
  Y         := 0;
  Z         := 0;
  Width     := 0;
  Height    := 0;
  Alpha     := 255;
  Angle     := 0;
  Animated  := FALSE;
  AnimSpeed := 0;
  FX        := FX_BLEND;
  Frame     := 1;
  Kill      := FALSE;
  Texture   := nil;
end;

procedure zglCSprite.Freeing;
begin
end;

procedure zglCSprite.Draw;
begin
  if not Assigned( Texture ) Then exit;
  if Animated Then
    asprite2d_Draw( Texture, X, Y, Width, Height, Angle, m_Round( Frame ), Alpha, FX )
  else
    ssprite2d_Draw( Texture, X, Y, Width, Height, Angle, Alpha, FX );
end;

procedure zglCSprite.Process;
begin
  if Animated Then
    begin
      Frame := Frame + AnimSpeed;
      if Frame - 1 > Texture^.FramesX * Texture^.FramesY  Then Frame := 1;
    end;
end;

{------------------------------------------------------------------------------}
{-------------------------  S P R I T E  E N G I N E  -------------------------}
{------------------------------------------------------------------------------}

function zglCSpriteEngine.CreateNew : zglCSprite;
begin
  INC( Count );

  List[ Count - 1 ]     := zglCSprite.Create;
  List[ Count - 1 ].ID  := Count - 1;
  DrawList[ Count - 1 ] := List[ Count - 1 ].ID;
  Result                := List[ Count - 1 ];
end;

function zglCSpriteEngine.AddNew;
begin
  INC( Count );

  DrawList[ Count - 1 ] := Count - 1;
  Result                := Count - 1;
end;

procedure zglCSpriteEngine.Del;
  var
    i : WORD;
begin
  if Count = 0 Then exit;
  DEC( Count );
  i := List[ ID ].ID;
  List[ ID ].Freeing;
  List[ ID ].Destroy;
  List[ ID ] := List[ Count ];
  List[ ID ].ID := i;

  for i := 0 to Count - 1 do
    DrawList[ i ] := List[ i ].ID;
end;

procedure zglCSpriteEngine.qSortN;
  var
    Lo, Hi, Mid, T: WORD;
begin
  Lo := iLo;
  Hi := iHi;
  Mid := List[ DrawList[ ( Lo + Hi ) shr 1 ] ].Z;

  repeat
    while List[ DrawList[ Lo ] ].Z > Mid do INC( Lo );
    while List[ DrawList[ Hi ] ].Z < Mid do DEC( Hi );
    if Lo <= Hi then
      begin
        T              := DrawList[ Lo ];
        DrawList[ Lo ] := DrawList[ Hi ];
        DrawList[ Hi ] := T;
        INC( Lo );
        DEC( Hi );
      end;
  until Lo > Hi;

  if Hi - iLo > 1 Then
    if Hi > iLo Then qSortN( iLo, Hi );
  if iHi - Lo > 1 Then
    if Lo < iHi Then qSortN( Lo, iHi );
end;

procedure zglCSpriteEngine.qSortI;
  var
    Lo, Hi, Mid, T: WORD;
begin
  Lo := iLo;
  Hi := iHi;
  Mid := List[ DrawList[ ( Lo + Hi ) shr 1 ] ].Z;

  repeat
    while List[ DrawList[ Lo ] ].Z < Mid do INC( Lo );
    while List[ DrawList[ Hi ] ].Z > Mid do DEC( Hi );
    if Lo <= Hi then
      begin
        T              := DrawList[ Lo ];
        DrawList[ Lo ] := DrawList[ Hi ];
        DrawList[ Hi ] := T;
        INC( Lo );
        DEC( Hi );
      end;
  until Lo > Hi;

  if Hi - iLo > 1 Then
    if Hi > iLo Then qSortI( iLo, Hi );
  if iHi - Lo > 1 Then
    if Lo < iHi Then qSortI( Lo, iHi );
end;

procedure zglCSpriteEngine.Sort;
begin
  if Count > 2 Then
    if InvertSort Then
      qSortI( 0, Count - 1 )
    else
      qSortN( 0, Count - 1 );
end;

procedure zglCSpriteEngine.Draw;
  var
    i : WORD;
begin
  Sort;
  for i := 0 to Count - 1 do
    List[ DrawList[ i ] ].Draw;
end;

procedure zglCSpriteEngine.Process;
  var
    i : WORD;
begin
  i := 0;
  while i <= Count - 1 do
    begin
      if List[ i ].Kill Then
        begin
          Del( List[ i ].ID );
          DEC( i );
        end else
          List[ i ].Process;
      INC( i );
    end;
end;

initialization
  SEngine := zglCSpriteEngine.Create;

end.
