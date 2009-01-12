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
unit zgl_timers;

{$I define.inc}

interface
uses
  {$IFDEF LINUX}
  Unix,
  {$ENDIF}
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  {$IFDEF DARWIN}
  MacOSAll,
  {$ENDIF}
  zgl_global_var,
  zgl_types;

function  timer_Add( const OnTimer : Pointer; const Interval : DWORD ) : zglPTimer;
procedure timer_Del( Timer : zglPTimer );

function timer_GetTicks : Double;
procedure timer_Reset;

var
{$IFDEF LINUX}
  t_tmr     : TimeVal;
{$ENDIF}
{$IFDEF WIN32}
  Frequency : int64;
  Freq      : Single;
{$ENDIF}
  t_start   : Double;

implementation
uses
  zgl_main;

function timer_Add;
begin
  Result := @managerTimer.First;
  while Assigned( Result.Next ) do
    Result := Result.Next;

  zgl_GetMem( Result.Next, SizeOf( zglTTimer ) );
  Result.Next.Active   := TRUE;
  Result.Next.Interval := Interval;
  Result.Next.OnTimer  := OnTimer;
  Result.Next.Prev     := Result;
  Result := Result.Next;
  INC( managerTimer.Count );
end;

procedure timer_Del;
begin
  if not CanKillTimers Then
    begin
      INC( TimersToKill );
      aTimersToKill[ TimersToKill ] := Timer;
      exit;
    end;

  if Assigned( Timer.Prev ) Then
    Timer.Prev.Next := Timer.Next;
  if Assigned( Timer.Next ) Then
    Timer.Next.Prev := Timer.Prev;
  Freememory( Timer );
  DEC( managerTimer.Count );
end;

function timer_GetTicks;
  {$IFDEF WIN32}
  var
    T : int64;
  {$ENDIF}
  {$IFDEF DARWIN}
  var
    T : UnsignedWide;
  {$ENDIF}
begin
{$IFDEF LINUX}
  fpGetTimeOfDay( @t_tmr, nil );
  Result := t_tmr.tv_sec * 1000 + t_tmr.tv_usec / 1000 - t_start;
{$ENDIF}
{$IFDEF WIN32}
  QueryPerformanceCounter( T );
  Result := 1000 * T * Freq - t_start;
{$ENDIF}
{$IFDEF DARWIN}
  Microseconds( T );
  Result := T.int / 1000 - t_start;
{$ENDIF}
end;

procedure timer_Reset;
  var
    currTimer : zglPTimer;
begin
  currTimer := @managerTimer.First;
  while Assigned( currTimer ) do
    begin
      currTimer.LastTick := timer_GetTicks;
      currTimer := currTimer.Next;
    end;
end;

initialization
{$IFDEF WIN32}
  QueryPerformanceFrequency( Frequency );
  Freq := 1 / Frequency;
{$ENDIF}
  t_start := timer_GetTicks;

end.
