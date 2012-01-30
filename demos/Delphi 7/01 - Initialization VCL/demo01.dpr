program demo01;

uses
  Forms,
  demo01VCL in 'demo01VCL.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
