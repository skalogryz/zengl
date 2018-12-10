unit zgl_mouse_to_touch;

interface

uses
  zgl_mouse, zgl_touch, zgl_keyboard;

procedure RegisterMouseToTouch;
procedure UnregisterMouseToTouch;

implementation

var
  _mouse_PMove    : procedure( X, Y : Integer );
  _mouse_PPress   : procedure( Button : Byte );
  _mouse_PRelease : procedure( Button : Byte );
  _mouse_PWheel   : procedure( Axis : Byte );
  fingerID : Integer = 0;
  _key_PPress     : procedure( KeyCode : Byte );
  _key_PRelease   : procedure( KeyCode : Byte );


procedure emu_mouse_PMove   ( X, Y : Integer );
begin
  if mouse_Down(M_BLEFT) then begin
    if touch_Down(fingerID) then begin
      touchX[fingerID]:=x;
      touchY[fingerID]:=Y;
      if Assigned(touch_PMove) then touch_PMove(fingerID, X,Y);
    end;
  end;
end;

procedure emu_mouse_PPress  ( Button : Byte );
begin
  if not touch_Down(fingerID) then begin
    touchDown[fingerID]:=true;
    touchUp[fingerID]:=false;
    touchX[fingerID]:=mouseX;
    touchY[fingerID]:=mouseY;
    if Assigned(touch_PPress) then touch_PPress(fingerID);
  end;
end;

procedure emu_mouse_PRelease( Button : Byte );
begin
  if not touch_Up(fingerID) then begin
    touchUp[fingerID]:=true;
    touchDown[fingerID]:=false;
    if Assigned(touch_PRelease) then touch_PRelease(fingerID);
  end;
end;

procedure emu_mouse_PWheel  ( Axis : Byte );
begin
  // not emulated
end;

procedure UnregisterMouseToTouch;
begin
  mouse_PMove     := _mouse_PMove;
  mouse_PPress    := _mouse_PPress;
  mouse_PRelease  := _mouse_PRelease;
  mouse_PWheel    := _mouse_PWheel;
end;


procedure emu_key_PPress( KeyCode : Byte );
begin
  case KeyCode of
    K_CTRL, K_CTRL_L, K_CTRL_R: begin
      fingerID:=1;
    end;
  end;
  if Assigned(_key_PPress) then _key_PPress(KeyCode);
end;

procedure emu_key_PRelease( KeyCode : Byte );
begin
  case KeyCode of
    K_CTRL, K_CTRL_L, K_CTRL_R: begin
      fingerID:=0
    end;
  end;
  if Assigned(_key_PRelease) then _key_PRelease(KeyCode);
end;

procedure RegisterMouseToTouch;
begin
  _mouse_PMove     := mouse_PMove;
  _mouse_PPress    := mouse_PPress;
  _mouse_PRelease  := mouse_PRelease;
  _mouse_PWheel    := mouse_PWheel;
  _key_PPress     := key_PPress;
  _key_PRelease   := key_PRelease;
  key_PPress      := emu_key_PPress;
  key_PRelease    := emu_key_PRelease;
  mouse_PMove     := emu_mouse_PMove;
  mouse_PPress    := emu_mouse_PPress;
  mouse_PRelease  := emu_mouse_PRelease;
  mouse_PWheel    := emu_mouse_PWheel;
end;

end.
