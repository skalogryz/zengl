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
unit zgl_timers;

{$I zgl_config.cfg}

interface
uses
  {$IFDEF LINUX}
  Unix
  {$ENDIF}
  {$IFDEF WIN32}
  Windows
  {$ENDIF}
  {$IFDEF DARWIN}
  MacOSAll
  {$ENDIF}
  ;

type
  zglPTimer = ^zglTTimer;
  zglTTimer = record
    Active     : Boolean;
    Interval   : DWORD;
    LastTick   : Double;
    OnTimer    : procedure;

    Prev, Next : zglPTimer;
end;

type
  zglPTimerManager = ^zglTTimerManager;
  zglTTimerManager = record
    Count : DWORD;
    First : zglTTimer;
end;

function  timer_Add( const OnTimer : Pointer; const Interval : DWORD ) : zglPTimer;
procedure timer_Del( var Timer : zglPTimer );

procedure timer_MainLoop;
function  timer_GetTicks : Double;
procedure timer_Reset;

var
  managerTimer  : zglTTimerManager;
  CanKillTimers : Boolean = TRUE;
  TimersToKill  : WORD = 0;
  aTimersToKill : array[ 0..1023 ] of zglPTimer;
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
  zgl_application,
  zgl_main;

function timer_Add;
begin
  Result := @managerTimer.First;
  while Assigned( Result.Next ) do
    Result := Result.Next;

  zgl_GetMem( Pointer( Result.Next ), SizeOf( zglTTimer ) );
  Result.Next.Active   := TRUE;
  Result.Next.Interval := Interval;
  Result.Next.OnTimer  := OnTimer;
  Result.Next.LastTick := timer_GetTicks;
  Result.Next.Prev     := Result;
  Result.Next.Next     := nil;
  Result := Result.Next;
  INC( managerTimer.Count );
end;

procedure timer_Del;
begin
  if not Assigned( Timer ) Then exit;

  if not CanKillTimers Then
    begin
      INC( TimersToKill );
      aTimersToKill[ TimersToKill ] := Timer;
      Timer := nil;
      exit;
    end;

  if Assigned( Timer.Prev ) Then
    Timer.Prev.Next := Timer.Next;
  if Assigned( Timer.Next ) Then
    Timer.Next.Prev := Timer.Prev;
  FreeMemory( Timer );
  DEC( managerTimer.Count );

  Timer := nil;
end;

procedure timer_MainLoop;
  var
    i     : Integer;
    t     : Double;
    timer : zglPTimer;
begin
  CanKillTimers := FALSE;

  timer := @managerTimer.First;
  if timer <> nil Then
    for i := 0 to managerTimer.Count do
      begin
        if timer^.Active then
          begin
            t := timer_GetTicks;
            while t >= timer^.LastTick + timer^.Interval do
              begin
                timer^.LastTick := timer^.LastTick + timer^.Interval;
                timer^.OnTimer;
                if t < timer_GetTicks - timer^.Interval Then
                  break
                else
                  t := timer_GetTicks;
              end;
          end else timer^.LastTick := timer_GetTicks;

        timer := timer^.Next;
      end;

  CanKillTimers := TRUE;
  for i := 1 to TimersToKill do
    timer_Del( aTimersToKill[ i ] );
  TimersToKill  := 0;
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
  {$Q-}
  // FIXME: почему-то overflow вылетает с флагом -Co
  Result := t_tmr.tv_sec * 1000 + t_tmr.tv_usec / 1000 - t_start;
  {$Q+}
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
  app_dt := timer_GetTicks;
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
