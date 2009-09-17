{
 * Copyright © Kemka Andrey aka Andru
 * mail: dr.andru@gmail.com
 * site: http://andru-kun.inf.ua
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
unit zgl_sengine_oop;

interface

uses
  zgl_types,
  zgl_textures;

type
  zglCSprite2D  = class;
  zglCSEngine2D = class;

  zglCSEngine2D = class
  protected
    procedure SortByLayer( iLo, iHi : Integer );
    procedure SortByID( iLo, iHi : Integer );
  public
    Count : DWORD;
    List  : array of zglCSprite2D;

    function  AddSprite : Integer; overload;
    function  AddSprite( const Texture : zglPTexture; const Layer : Integer ) : zglCSprite2D; overload;
    procedure DelSprite( const ID : Integer );
    procedure ClearAll;

    procedure Draw;
    procedure Proc;
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
    FxFlags : DWORD;

    constructor Create( const _Manager : zglCSEngine2D; const _ID : Integer );
    destructor  Free;

    procedure OnInit( const _Texture : zglPTexture; const _Layer : Integer ); virtual;
    procedure OnDraw; virtual;
    procedure OnProc; virtual;
    procedure OnFree; virtual;
  end;

implementation
uses
  zgl_main,
  zgl_fx,
  zgl_sprite_2d;

function zglCSEngine2D.AddSprite : Integer;
begin
  if Count + 1 > length( List ) Then
    SetLength( List, length( List ) + 16384 );
  Result := Count;
  INC( Count );
end;

function zglCSEngine2D.AddSprite( const Texture : zglPTexture; const Layer : Integer ) : zglCSprite2D;
  var
    ID : Integer;
begin
  ID := AddSprite;

  List[ ID ] := zglCSprite2D.Create( Self, ID );
  Result := List[ ID ];
  Result.OnInit( Texture, Layer );
end;

procedure zglCSEngine2D.DelSprite;
  var
    i : Integer;
begin
  if ( ID < 0 ) or ( ID > Count - 1 ) or ( Count = 0 ) Then exit;

  List[ ID ].Free;

  for i := ID to Count - 2 do
    begin
      List[ i ]    := List[ i + 1 ];
      List[ i ].ID := i;
    end;

  DEC( Count );
end;

procedure zglCSEngine2D.ClearAll;
  var
    i : Integer;
begin
  for i := 0 to Count - 1 do
    List[ i ].Free;
  SetLength( List, 0 );
  Count := 0;
end;

procedure zglCSEngine2D.Draw;
  var
    i, a, b, l : Integer;
    s : zglCSprite2D;
begin
  if Count > 1 Then
    begin
      l := 0;
      for i := 0 to Count - 1 do
        begin
          s := List[ i ];
          if s.Layer > l Then l := s.Layer;
          if s.Layer < l Then
            begin
              SortByLayer( 0, Count - 1 );
              // TODO: наверное сделать выбор вкл./выкл. устойчивой сортировки
              l := List[ 0 ].Layer;
              a := 0;
              for b := 0 to Count - 1 do
                begin
                  s := List[ b ];
                  if ( l <> s.Layer ) Then
                    begin
                      SortByID( a, b - 1 );
                      a := b;
                      l := s.Layer;
                    end;
                  if b = Count - 1 Then
                    SortByID( a, b );
                end;
              for a := 0 to Count - 1 do
                List[ a ].ID := a;
              break;
            end;
        end;
    end;

  i := 0;
  while i < Count do
    begin
      s := List[ i ];
      s.OnDraw;

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
  while i < Count do
    begin
      s := List[ i ];
      s.OnProc;

      if s.Destroy Then
        DelSprite( s.ID )
      else
        INC( i );
    end;
end;

procedure zglCSEngine2D.SortByLayer;
  var
    Lo, Hi, Mid : Integer;
    T : zglCSprite2D;
begin
  Lo   := iLo;
  Hi   := iHi;
  Mid  := List[ ( Lo + Hi ) shr 1 ].Layer;

  repeat
    while List[ Lo ].Layer < Mid do INC( Lo );
    while List[ Hi ].Layer > Mid do DEC( Hi );
    if Lo <= Hi then
      begin
        T          := List[ Lo ];
        List[ Lo ] := List[ Hi ];
        List[ Hi ] := T;
        INC( Lo );
        DEC( Hi );
      end;
  until Lo > Hi;

  if Hi > iLo Then SortByLayer( iLo, Hi );
  if Lo < iHi Then SortByLayer( Lo, iHi );
end;

procedure zglCSEngine2D.SortByID;
  var
    Lo, Hi, Mid : Integer;
    T : zglCSprite2D;
begin
  Lo   := iLo;
  Hi   := iHi;
  Mid  := List[ ( Lo + Hi ) shr 1 ].ID;

  repeat
    while List[ Lo ].ID < Mid do INC( Lo );
    while List[ Hi ].ID > Mid do DEC( Hi );
    if Lo <= Hi then
      begin
        T          := List[ Lo ];
        List[ Lo ] := List[ Hi ];
        List[ Hi ] := T;
        INC( Lo );
        DEC( Hi );
      end;
  until Lo > Hi;

  if Hi > iLo Then SortByID( iLo, Hi );
  if Lo < iHi Then SortByID( Lo, iHi );
end;

constructor zglCSprite2D.Create;
begin
  Manager := _Manager;
  ID      := _ID;

  OnInit( nil, 0 );
end;

destructor zglCSprite2D.Free;
begin
  OnFree;
end;

procedure zglCSprite2D.OnInit;
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
