/*
 *  Copyright © Kemka Andrey aka Andru
 *  mail: dr.andru@gmail.com
 *  site: http://andru-kun.inf.ua
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
*/
package zengl.android;

import javax.microedition.khronos.egl.EGLConfig;
import javax.microedition.khronos.opengles.GL10;

import android.content.Context;
import android.opengl.GLSurfaceView;
import android.view.*;

public class ZenGL extends GLSurfaceView
{
	private native void Main();
	private native void zglNativeSurfaceCreated( String Path );
	private native void zglNativeSurfaceChanged( int width, int height );
	private native void zglNativeDrawFrame();
	private native void zglNativeActivate( boolean Activate );
	private native Boolean zglNativeCloseQuery();
	private native void zglNativeTouch( int ID, float X, float Y, float Pressure );

	private zglCRenderer Renderer;
	private String SourceDir;

	public ZenGL( Context context, String appName, String appSourceDir )
	{
		super( context );

		System.loadLibrary( "chipmunk" );
		System.loadLibrary( "openal" );
		System.loadLibrary( "zenjpeg" );
		System.loadLibrary( appName );

		SourceDir = appSourceDir;
		Renderer = new zglCRenderer();
		setRenderer( Renderer );
	}
	
	public Boolean onCloseQuery()
	{
		return zglNativeCloseQuery();
	}

	@Override
	public void onPause()
	{
		zglNativeActivate( false );
	}

	@Override
	public void onResume()
	{
		zglNativeActivate( true );
	}

	@Override
	public boolean onTouchEvent( MotionEvent event )
	{
		int action = event.getAction();
		int actionType = action & MotionEvent.ACTION_MASK;

		switch ( actionType )
		{
			case MotionEvent.ACTION_DOWN:
			{
				int count = event.getPointerCount();
				for ( int i = 0; i < count; i++ )
				{
					int pointerID = event.getPointerId( i );
					zglNativeTouch( pointerID, event.getX( i ), event.getY( i ), event.getPressure( i ) );
				}
				break;
			}

			case MotionEvent.ACTION_UP:
			{
				int count = event.getPointerCount();
				for ( int i = 0; i < count; i++ )
				{
					int pointerID = event.getPointerId( i );
					zglNativeTouch( pointerID, event.getX( i ), event.getY( i ), 0 );
				}
				break;
			}

			case MotionEvent.ACTION_MOVE:
			{
				int count = event.getPointerCount();
				for ( int i = 0; i < count; i++ )
				{
					int pointerID = event.getPointerId( i );
					zglNativeTouch( pointerID, event.getX( i ), event.getY( i ), event.getPressure( i ) );
				}
				break;
			}

			case MotionEvent.ACTION_POINTER_DOWN:
			{
				int pointerID = ( action & MotionEvent.ACTION_POINTER_ID_MASK ) >> MotionEvent.ACTION_POINTER_ID_SHIFT;
				int pointerIndex = event.findPointerIndex( pointerID );
				zglNativeTouch( pointerID, event.getX( pointerIndex ), event.getY( pointerIndex ), event.getPressure( pointerIndex ) );
				break;
			}

			case MotionEvent.ACTION_POINTER_UP:
			{
				int pointerID = ( action & MotionEvent.ACTION_POINTER_ID_MASK ) >> MotionEvent.ACTION_POINTER_ID_SHIFT;
				int pointerIndex = event.findPointerIndex( pointerID );
				zglNativeTouch( pointerID, event.getX( pointerIndex ), event.getY( pointerIndex ), 0 );
				break;
			}
		}

		return true;
	}

	class zglCRenderer implements Renderer
	{
		public void onSurfaceCreated( GL10 gl, EGLConfig config )
		{
			zglNativeSurfaceCreated( SourceDir );
			Main();
		}

		public void onSurfaceChanged( GL10 gl, int width, int height )
		{
			zglNativeSurfaceChanged( width, height );
		}

		public void onDrawFrame( GL10 gl )
		{
			zglNativeDrawFrame();
		}
	}
}