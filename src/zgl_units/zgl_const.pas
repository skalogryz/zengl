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
unit zgl_const;

{$I define.inc}

interface

const
  // constant value
  cv_MaxCount = 1023;
  cv_pi       = 3.1415926;
  cv_pi180    = 3.1415926 / 180;

  defWidth    = 800;
  defHeight   = 600;
  defBPP      = 32;

  // constant string
  cs_ZenGL    = 'ZenGL build 28[22.09.08]';
  cv_version  = 28;

  // zgl_Reg
  SYS_LOAD             = $000001;
  SYS_DRAW             = $000002;
  SYS_EXIT             = $000003;
  TEX_FORMAT_EXTENSION = $000010;
  TEX_FORMAT_LOADER    = $000011;
  SND_FORMAT_EXTENSION = $000020;
  SND_FORMAT_LOADER    = $000021;

  // zgl_Get
  SYS_FPS         = 1;
  LOG_FILENAME    = 2;
  ZGL_VERSION     = 3;
  SCR_ADD_X       = 4;
  SCR_ADD_Y       = 5;
  DESKTOP_WIDTH   = 6;
  DESKTOP_HEIGHT  = 7;
  RESOLUTION_LIST = 8;
  MANAGER_TIMER   = 9;
  MANAGER_TEXTURE = 10;
  MANAGER_FONT    = 11;
  MANAGER_RTARGET = 12;
  MANAGER_SOUND   = 13;

  // zgl_Enable/zgl_Disable
  COLOR_BUFFER_CLEAR   = $000001;
  DEPTH_BUFFER         = $000002;
  DEPTH_BUFFER_CLEAR   = $000004;
  DEPTH_MASK           = $000008;
  CORRECT_RESOLUTION   = $000010;
  APP_USE_AUTOPAUSE    = $000020;
  APP_USE_AUTOMINIMIZE = $000040;
  APP_USE_LOG          = $000080;
  SND_CAN_PLAY         = $000100;
  SND_CAN_PLAY_FILE    = $000200;
  CROP_INVISIBLE       = $000400;

  // Screen
  REFRESH_MAXIMUM = 0;
  REFRESH_DEFAULT = 1;

  // Textures
  TEX_MIPMAP            = $000001;
  TEX_CLAMP             = $000002;
  TEX_REPEAT            = $000004;
  TEX_COMPRESS          = $000008;
  TEX_CONVERT_TO_POT    = $000010;

  TEX_GRAYSCALE         = $000020;
  TEX_INVERT            = $000040;
  TEX_USEMASK           = $000080;

  TEX_FILTER_NEAREST    = $000100;
  TEX_FILTER_LINEAR     = $000200;
  TEX_FILTER_BILINEAR   = $000400;
  TEX_FILTER_TRILINEAR  = $000800;
  TEX_FILTER_ANISOTROPY = $001000;

  TEX_RGB               = $002000;

  TEX_QUALITY_LOW       = $400000;
  TEX_QUALITY_MEDIUM    = $800000;

  // 3D
  USE_NORMALS    = $001;
  USE_TEXTURE    = $002;
  USE_MULTITEX1  = $004;
  USE_MULTITEX2  = $008;
  USE_MULTITEX3  = $010;
  BUILD_FNORMALS = $020;
  BUILD_SNORMALS = $040;
  BUILD_PLANES   = $080;
  BUILD_VBO      = $100;

implementation

end.
