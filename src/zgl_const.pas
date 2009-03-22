{
 * Copyright © Kemka Andrey aka Andru
 * mail: dr.andru@gmail.com
 * site: http://andru-kun.ru
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
unit zgl_const;

{$I zgl_config.cfg}

interface

const
  LIB_ERROR  = {$IFDEF LINUX_OR_DARWIN} nil {$ELSE} 0 {$ENDIF};
  FILE_ERROR = {$IFDEF LINUX_OR_DARWIN} nil {$ELSE} 0 {$ENDIF};

  cs_ZenGL = 'ZenGL 0.1.22';

  defWidth  = 800;
  defHeight = 600;
  defBPP    = 32;

  // zgl_Enable/zgl_Disable
  COLOR_BUFFER_CLEAR   = $000001;
  DEPTH_BUFFER         = $000002;
  DEPTH_BUFFER_CLEAR   = $000004;
  DEPTH_MASK           = $000008;
  STENCIL_BUFFER_CLEAR = $000010;
  CORRECT_RESOLUTION   = $000020;
  APP_USE_AUTOPAUSE    = $000040;
  APP_USE_AUTOMINIMIZE = $000080;
  APP_USE_LOG          = $000100;
  SND_CAN_PLAY         = $000200;
  SND_CAN_PLAY_FILE    = $000400;
  CROP_INVISIBLE       = $000800;

implementation

end.
