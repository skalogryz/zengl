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
unit zgl_sengine_2d;

{$I zgl_config.cfg}

interface

uses
  zgl_types,
  zgl_textures;

type
  zglPSprite2D = ^zglTSprite2D;
  zglPSEngine2D = ^zglTSEngine2D;

  zglTSEngine2D = record
    Count : DWORD;
    List  : array of zglPSprite2D;
  end;

  zglTSprite2D = record
    ID      : Integer;
    Manager : zglPSEngine2D;
    Texture : zglPTexture;
    Layer   : Integer;
    X, Y    : Single;
    W, H    : Single;
    Angle   : Single;
    Frame   : Single;
    Alpha   : Integer;
    Flags   : DWORD;
    Data    : Pointer;

    OnInit  : procedure( const Sprite : zglPSprite2D );
    OnDraw  : procedure( const Sprite : zglPSprite2D );
    OnProc  : procedure( const Sprite : zglPSprite2D );
    OnFree  : procedure( const Sprite : zglPSprite2D );
  end;

function  sengine2d_AddSprite( const Texture : zglPTexture; const Layer : Integer; const OnInit, OnDraw, OnProc, OnFree : Pointer ) : zglPSprite2D;
procedure sengine2d_DelSprite( const ID : Integer );

procedure sengine2d_Set( const SEngine : zglPSEngine2D );
procedure sengine2d_Draw;
procedure sengine2d_Proc;
procedure sengine2d_Sort( iLo, iHi : Integer );
procedure sengine2d_SortID( iLo, iHi : Integer );

implementation
uses
  zgl_main,
  zgl_fx,
  zgl_sprite_2d;

var
  _sengine  : zglTSEngine2D;
  sengine2d : zglPSEngine2D;

function sengine2d_AddSprite;
  var
    new : zglPSprite2D;
begin
  if sengine2d.Count + 1 > length( sengine2d.List ) Then
    SetLength( sengine2d.List, length( sengine2d.List ) + 16384 );

  zgl_GetMem( Pointer( new ), SizeOf( zglTSprite2D ) );
  sengine2d.List[ sengine2d.Count ] := new;
  INC( sengine2d.Count );

  new.ID      := sengine2d.Count - 1;
  new.Manager := sengine2d;
  new.Texture := Texture;
  new.Layer   := Layer;
  new.X       := 0;
  new.Y       := 0;
  new.W       := Texture.Width div Texture.FramesX;
  new.H       := Texture.Height div Texture.FramesY;
  new.Angle   := 0;
  new.Frame   := 1;
  new.Alpha   := 255;
  new.Flags   := FX_BLEND;
  new.Data    := nil;
  new.OnInit  := OnInit;
  new.OnDraw  := OnDraw;
  new.OnProc  := OnProc;
  new.OnFree  := OnFree;
  Result      := new;
  if Assigned( Result.OnInit ) Then
    Result.OnInit( Result );
end;

procedure sengine2d_DelSprite;
  var
    i : Integer;
begin
  if ( ID < 0 ) or ( ID > sengine2d.Count - 1 ) Then exit;

  if Assigned( sengine2d.List[ ID ].OnFree ) Then
    sengine2d.List[ ID ].OnFree( sengine2d.List[ ID ] );

  FreeMemory( sengine2d.List[ ID ] );
  for i := ID to sengine2d.Count - 2 do
    begin
      sengine2d.List[ i ]    := sengine2d.List[ i + 1 ];
      sengine2d.List[ i ].ID := i;
    end;

  DEC( sengine2d.Count );
end;

procedure sengine2d_Set;
begin
  if Assigned( SEngine ) Then
    sengine2d := SEngine
  else
    sengine2d := @_sengine;
end;

procedure sengine2d_Draw;
  var
    i, a, b, l : Integer;
    s : zglPSprite2D;
begin
  if sengine2d.Count > 1 Then
    begin
      l := 0;
      for i := 0 to sengine2d.Count - 1 do
        begin
          s := sengine2d.List[ i ];
          if s.Layer > l Then l := s.Layer;
          if s.Layer < l Then
            begin
              sengine2d_Sort( 0, sengine2d.Count - 1 );
              // TODO: наверное сделать выбор вкл./выкл. устойчивой сортировки
              l := sengine2d.List[ 0 ].Layer;
              a := 0;
              for b := 0 to sengine2d.Count - 1 do
                begin
                  s := sengine2d.List[ b ];
                  if ( l <> s.Layer ) Then
                    begin
                      sengine2d_SortID( a, b - 1 );
                      a := b;
                      l := s.Layer;
                    end;
                  if b = sengine2d.Count - 1 Then
                    sengine2d_SortID( a, b );
                end;
              for a := 0 to sengine2d.Count - 1 do
                sengine2d.List[ a ].ID := a;
              break;
            end;
        end;
    end;

  for i := 0 to sengine2d.Count - 1 do
    begin
      s := sengine2d.List[ i ];
      if Assigned( s.OnDraw ) Then
        s.OnDraw( s )
      else
        asprite2d_Draw( s.Texture, s.X, s.Y, s.W, s.H, s.Angle, Round( s.Frame ), s.Alpha, s.Flags );
    end;
end;

procedure sengine2d_Proc;
  var
    i : Integer;
    s : zglPSprite2D;
begin
  for i := 0 to sengine2d.Count - 1 do
    begin
      s := sengine2d.List[ i ];
      if Assigned( s.OnProc ) Then
        s.OnProc( s )
      else;
    end;
end;

procedure sengine2d_Sort;
  var
    Lo, Hi, Mid : Integer;
    List : array of zglPSprite2D;
    T  : zglPSprite2D;
begin
  Lo   := iLo;
  Hi   := iHi;
  List := sengine2d.List;
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

  if Hi > iLo Then sengine2d_Sort( iLo, Hi );
  if Lo < iHi Then sengine2d_Sort( Lo, iHi );
end;

procedure sengine2d_SortID;
  var
    Lo, Hi, Mid : Integer;
    List : array of zglPSprite2D;
    T  : zglPSprite2D;
begin
  Lo   := iLo;
  Hi   := iHi;
  List := sengine2d.List;
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

  if Hi > iLo Then sengine2d_SortID( iLo, Hi );
  if Lo < iHi Then sengine2d_SortID( Lo, iHi );
end;

initialization
  sengine2d := @_sengine;

end.
