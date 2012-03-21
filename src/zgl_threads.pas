{
 *  Copyright © Kemka Andrey aka Andru
 *  mail: dr.andru@gmail.com
 *  site: http://zengl.org
 *
 *  This file is part of ZenGL.
 *
 *  ZenGL is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as
 *  published by the Free Software Foundation, either version 3 of
 *  the License, or (at your option) any later version.
 *
 *  ZenGL is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with ZenGL. If not, see http://www.gnu.org/licenses/
}
unit zgl_threads;

{$I zgl_config.cfg}

interface
uses
  {$IFDEF UNIX}
  {$IFNDEF ANDROID}
  cthreads,
  {$ENDIF}
  UnixType
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows
  {$ENDIF}
  ;

type
  {$IFNDEF ANDROID}
  zglTThreadCallback = function( Data : Pointer ) : LongInt;
  {$ELSE}
  zglTThreadCallback = function( Data : Pointer ) : Pointer; cdecl;
  {$ENDIF}

  zglPThread = ^zglTThread;
  zglTThread = record
    ID     : LongWord;
    Handle : LongWord;
  end;

  zglTMutex     = Pointer;
  zglTSemaphore = Pointer;

function  thread_Create( Callback : zglTThreadCallback; Data : Pointer ) : zglTThread;
function  thread_MutexInit : zglTMutex;
procedure thread_MutexDestroy( var Mutex : zglTMutex );
procedure thread_MutexLock( var Mutex : zglTMutex );
procedure thread_MutexUnlock( var Mutex : zglTMutex );
function  thread_SemInit : zglTSemaphore;
procedure thread_SemDestroy( var Semaphore : zglTSemaphore );
procedure thread_SemPost( var Semaphore : zglTSemaphore );
procedure thread_SemWait( var Semaphore : zglTSemaphore; Milliseconds : LongWord = $FFFFFFFF );

implementation

{$IFDEF UNIX}
const
  libpthread = {$IFNDEF ANDROID} 'libpthread' {$ELSE} 'libc' {$ENDIF};

type
  ppthread_t           = ^pthread_t;
  ppthread_attr_t      = ^pthread_attr_t;
  ppthread_mutex_t     = ^pthread_mutex_t;
  ppthread_mutexattr_t = ^pthread_mutexattr_t;
  psem_t               = ^sem_t;

function pthread_create( __thread : ppthread_t; __attr : ppthread_attr_t; __start_routine : Pointer; __arg : Pointer ) : LongInt; cdecl; external libpthread;
function pthread_mutex_init( __mutex : ppthread_mutex_t; __mutex_attr : ppthread_mutexattr_t ) : LongInt; cdecl; external libpthread;
function pthread_mutex_destroy( __mutex : ppthread_mutex_t ) : LongInt; cdecl; external libpthread;
function pthread_mutex_lock( __mutex : ppthread_mutex_t ) : LongInt; cdecl; external libpthread;
function pthread_mutex_unlock( __mutex : ppthread_mutex_t ) : LongInt; cdecl; external libpthread;
function sem_init( __sem : psem_t; __pshared : LongInt; __value : DWORD ) : LongInt; cdecl; external libpthread;
function sem_destroy ( __sem : psem_t ) : LongInt; cdecl; external libpthread;
function sem_wait( __sem : psem_t ) : LongInt; cdecl; external libpthread;
function sem_post( __sem : psem_t ) : LongInt; cdecl; external libpthread;
function sem_timedwait( __sem : psem_t; __abs_timeout : ptimespec ) : LongInt; cdecl; external libpthread;
{$ENDIF}

function thread_Create( Callback : zglTThreadCallback; Data : Pointer ) : zglTThread;
begin
{$IFNDEF ANDROID}
  {$IFDEF FPC}
  Result.Handle := BeginThread( TThreadFunc( Callback ), Data, TThreadID( Result.ID ) );
  {$ELSE}
  Result.Handle := BeginThread( nil, 0, Pointer( Callback ), Data, 0, Result.ID );
  {$ENDIF}
{$ELSE}
  Result := nil;
  pthread_create( @Result, nil, Pointer( Callback ), Data );
{$ENDIF}
end;

function thread_MutexInit : zglTMutex;
begin
{$IFDEF UNIX}
  Result := nil;
  pthread_mutex_init( @Result, nil );
{$ELSE}
  Result := Pointer( CreateMutex( nil, FALSE, nil ) );
{$ENDIF}
end;

procedure thread_MutexDestroy( var Mutex : zglTMutex );
begin
{$IFDEF UNIX}
  pthread_mutex_destroy( @Mutex );
  Mutex := nil;
{$ELSE}
  CloseHandle( LongWord( Mutex ) );
{$ENDIF}
end;

procedure thread_MutexLock( var Mutex : zglTMutex );
begin
{$IFDEF UNIX}
  pthread_mutex_lock( @Mutex );
{$ELSE}
  WaitForSingleObject( LongWord( Mutex ), INFINITE );
{$ENDIF}
end;

procedure thread_MutexUnlock( var Mutex : zglTMutex );
begin
{$IFDEF UNIX}
  pthread_mutex_unlock( @Mutex );
{$ELSE}
  ReleaseMutex( LongWord( Mutex ) );
{$ENDIF}
end;

function thread_SemInit : zglTSemaphore;
begin
{$IFDEF UNIX}
  sem_init( @Result, 0, 0 );
{$ELSE}
  Result := Pointer( CreateSemaphore( nil, 0, 1, nil ) );
{$ENDIF}
end;

procedure thread_SemDestroy( var Semaphore : zglTSemaphore );
begin
{$IFDEF UNIX}
  sem_destroy( Semaphore );
  FreeMem( Semaphore );
  Semaphore := nil;
{$ELSE}
  CloseHandle( LongWord( Semaphore ) );
  Semaphore := nil;
{$ENDIF}
end;

procedure thread_SemPost( var Semaphore : zglTSemaphore );
begin
{$IFDEF UNIX}
  sem_post( Semaphore );
{$ELSE}
  ReleaseSemaphore( LongWord( Semaphore ), 1, nil );
{$ENDIF}
end;

procedure thread_SemWait( var Semaphore : zglTSemaphore; Milliseconds : LongWord = $FFFFFFFF );
  {$IFDEF UNIX}
  var
    time : TimeSpec;
  {$ENDIF}
begin
{$IFDEF UNIX}
  time.tv_sec := Milliseconds mod 1000000;
  time.tv_nsec := ( Milliseconds mod 1000000 ) * 1000000;
  sem_timedwait( Semaphore, @time );
{$ELSE}
  WaitForSingleObject( LongWord( Semaphore ), Milliseconds );
{$ENDIF}
end;

end.

