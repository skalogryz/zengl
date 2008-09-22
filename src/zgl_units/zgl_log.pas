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
unit zgl_log;

{$I define.inc}

interface
uses
  zgl_file,
  
  zgl_const,
  zgl_global_var,
  zgl_timers,

  zgl_math,

  Utils;

procedure log_Init;
procedure log_Close;
procedure log_Add( Message : String; Timings : Boolean = TRUE ); extdecl;
procedure log_AddC( Message : PChar; Timings : Boolean = TRUE ); extdecl;
procedure log_Flush; extdecl;
function  log_Timing : String;

var
  log      : zglTFile;
  logstart : DWORD;
  logfile  : PChar = 'log.txt';

implementation

procedure log_Init;
begin
  if ( app_Flags and APP_USE_LOG = 0 ) Then exit;
  if log <> {$IFDEF LINUX}nil{$ELSE}0{$ENDIF} Then exit;
  app_Log := TRUE;
  logstart := m_Round( timer_GetTicks );
  
  file_Open( log, logfile, FOM_CREATE );
  log_Add( '############################', FALSE );
  log_Add( '# ' + cs_ZenGL + ' #', FALSE );
  log_Add( '############################', FALSE );
  log_Add( 'Begin' );
end;

procedure log_Close;
begin
  if log <> {$IFDEF LINUX}nil{$ELSE}0{$ENDIF} Then
    file_Close( log );
end;

procedure log_Add;
  var
    str : String;
begin
  if not app_Log Then exit;
  {$IFDEF LINUX}
  writeln( Message );
  {$ENDIF}
  if Timings Then
    str := log_Timing + Message + #13#10
  else
    str := Message + #13#10;

  file_Write( log, str[ 1 ], length( str ) );

  log_Flush;
end;

procedure log_AddC( Message : PChar; Timings : Boolean = TRUE );
begin
  log_Add( Message, Timings );
end;

procedure log_Flush;
begin
  if log <> {$IFDEF LINUX}nil{$ELSE}0{$ENDIF} Then
    file_Flush( log );
end;

function log_Timing;
  var
    V : DWORD;
begin
  V := m_Round( timer_GetTicks ) - logstart;
  case V of
    0..9:               Result := '[0000000' + u_IntToStr( V ) + 'ms] ';
    10..99:             Result := '[000000'  + u_IntToStr( V ) + 'ms] ';
    100..999:           Result := '[00000'   + u_IntToStr( V ) + 'ms] ';
    1000..9999:         Result := '[0000'    + u_IntToStr( V ) + 'ms] ';
    10000..99999:       Result := '[000'     + u_IntToStr( V ) + 'ms] ';
    100000..999999:     Result := '[00'      + u_IntToStr( V ) + 'ms] ';
    1000000..9999999:   Result := '[0'       + u_IntToStr( V ) + 'ms] ';
    10000000..99999999: Result := '['        + u_IntToStr( V ) + 'ms] ';
  end;
end;

end.
