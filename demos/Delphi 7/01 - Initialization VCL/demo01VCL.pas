unit demo01VCL;

{$DEFINE STATIC}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  {$IFNDEF STATIC}
  zglHeader
  {$ELSE}
  zgl_main,
  zgl_window,
  zgl_screen,
  zgl_timers
  {$ENDIF};

type
  TForm1 = class(TForm)
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  zglInited : Boolean;

implementation

{$R *.dfm}

procedure Init;
begin
  scr_SetOptions( Form1.ClientWidth, Form1.ClientHeight, REFRESH_DEFAULT, FALSE, TRUE );

  Form1.BringToFront;
end;

procedure Draw;
begin
  Application.ProcessMessages;
end;

procedure Timer;
begin
end;

procedure UpdateDT( dt : Double );
begin
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  if not zglInited Then
    begin
      zglInited := TRUE;
      {$IFNDEF STATIC}
      zglLoad( libZenGL );
      {$ENDIF}

      timer_Add( @Timer, 16 );

      zgl_Reg( SYS_LOAD, @Init );
      zgl_Reg( SYS_DRAW, @Draw );
      zgl_Reg( SYS_UPDATE, @UpdateDT );

      zgl_InitToHandle( Form1.Handle );
      Application.Terminate();
    end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if zglInited Then
    zgl_Exit();
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  if zglInited Then
    wnd_SetSize( Form1.ClientWidth, Form1.ClientHeight );
end;

end.
