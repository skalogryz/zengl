unit uMain;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls,
  {$IFDEF LCLGTK}
  GLib, GTK, GDK,
  {$ENDIF}
  {$IFDEF LCLGTK2}
  GLib2, GTK2, GDK2, GDK2x,
  {$ENDIF}
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_utils,
  zgl_primitives_2d,
  zgl_sprite_2d,
  zgl_font,
  zgl_font_gen;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonRebuildFont: TButton;
    ButtonImportSymbols: TButton;
    ButtonDefaultSymbols: TButton;
    ButtonExit: TButton;
    ButtonSaveFont: TButton;
    ButtonChooseFont: TButton;
    CheckBoxAntialiasing: TCheckBox;
    CheckBoxPack: TCheckBox;
    ComboBoxPageSize: TComboBox;
    EditCurrentPage: TEdit;
    FontDialog: TFontDialog;
    GroupBox1: TGroupBox;
    LabelPageSize: TLabel;
    LabelCurrentPage: TLabel;
    Memo1: TMemo;
    OpenDialog: TOpenDialog;
    Panel1: TPanel;
    SaveFontDialog: TSaveDialog;
    UpDownCurrentPage: TUpDown;
    procedure ButtonChooseFontClick(Sender: TObject);
    procedure ButtonDefaultSymbolsClick(Sender: TObject);
    procedure ButtonExitClick(Sender: TObject);
    procedure ButtonImportSymbolsClick(Sender: TObject);
    procedure ButtonRebuildFontClick(Sender: TObject);
    procedure ButtonSaveFontClick(Sender: TObject);
    procedure CheckBoxAntialiasingChange(Sender: TObject);
    procedure CheckBoxPackChange(Sender: TObject);
    procedure ComboBoxPageSizeChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
  public
    { public declarations }
    procedure RebuildSymbolList;
    procedure RebuildFont;
  end;

var
  Form1: TForm1;
  zglInited : Boolean;
  fontMoving : Boolean;
  fontX, fontY : Integer;
  lastX, lastY : Integer;

implementation

{$R *.lfm}

procedure Init;
  var
    i : Integer;
begin
  wnd_ShowCursor( TRUE );
  wnd_SetSize( Form1.Panel1.ClientWidth, Form1.Panel1.ClientHeight );
  scr_SetOptions( Form1.Panel1.ClientWidth, Form1.Panel1.ClientHeight, 0, FALSE, TRUE );

  fontgen_Init();
  fg_Font := font_Add();

  // English
  for i := 32 to 126 do
    begin
      fg_CharsUse[ i ] := TRUE;
      INC( fg_Font.Count.Chars );
    end;
  // Europe
  for i := 161 to 255 do
    begin
      fg_CharsUse[ i ] := TRUE;
      INC( fg_Font.Count.Chars );
    end;
  // Russian
  fg_CharsUse[ 1025 ] := TRUE; // Ё
  fg_CharsUse[ 1105 ] := TRUE; // ё
  INC( fg_Font.Count.Chars, 2 );
  for i := 1040 to 1103 do
    begin
      fg_CharsUse[ i ] := TRUE;
      INC( fg_Font.Count.Chars );
    end;
  // Ukranian
  fg_CharsUse[ 1028 ] := TRUE; // Є
  fg_CharsUse[ 1108 ] := TRUE; // є
  fg_CharsUse[ 1030 ] := TRUE; // І
  fg_CharsUse[ 1110 ] := TRUE; // і
  fg_CharsUse[ 1031 ] := TRUE; // Ї
  fg_CharsUse[ 1111 ] := TRUE; // ї
  INC( fg_Font.Count.Chars, 6 );

  Form1.RebuildFont();
  fontX := ( Form1.Panel1.Width - fg_PageSize ) div 2;
  fontY := ( Form1.Panel1.Height - fg_PageSize ) div 2;
end;

procedure Draw;
begin
  pr2d_Rect( 0, 0, Form1.Panel1.Width, Form1.Panel1.Height, $505050, 255, PR2D_FILL );
  pr2d_Rect( fontX, fontY, fg_PageSize, fg_PageSize, $000000, 255, PR2D_FILL );

  if Assigned( fg_Font.Pages ) Then
    ssprite2d_Draw( fg_Font.Pages[ Form1.UpDownCurrentPage.Position - 1 ], fontX, fontY, fg_PageSize, fg_PageSize, 0 );

  Application.ProcessMessages();
  u_Sleep( 10 );
end;

{ TForm1 }

procedure TForm1.RebuildSymbolList;
  var
    i : Integer;
    c : Word;
begin
  i := 1;
  FillChar( fg_CharsUse, 65536, 0 );
  fg_Font.Count.Chars := 0;
  while i <= length( Memo1.Lines.Text ) do
    begin
      c := font_GetCID( Memo1.Lines.Text, i, @i );
      if not fg_CharsUse[ c ] Then
        begin
          fg_CharsUse[ c ] := TRUE;
          INC( fg_Font.Count.Chars );
        end;
    end;
end;

procedure TForm1.RebuildFont;
begin
  fontgen_BuildFont( fg_Font, FontDialog.Font.Name );
  UpDownCurrentPage.Max := fg_Font.Count.Pages;
end;

procedure TForm1.ButtonChooseFontClick(Sender: TObject);
begin
  if FontDialog.Execute() Then
    begin
      fg_FontSize   := FontDialog.Font.Size;
      fg_FontBold   := fsBold in FontDialog.Font.Style;
      fg_FontItalic := fsItalic in FontDialog.Font.Style;
      RebuildFont();
    end;
end;

procedure TForm1.ButtonDefaultSymbolsClick(Sender: TObject);
begin
  RebuildSymbolList();
end;

procedure TForm1.ButtonExitClick(Sender: TObject);
begin
  zgl_Exit();
end;

procedure TForm1.ButtonImportSymbolsClick(Sender: TObject);
  var
    i : Integer;
    s : TStrings;
begin
  s := TStringList.Create;
  if OpenDialog.Execute() Then
    begin
      s.LoadFromFile( OpenDialog.FileName );
      for i := 0 to s.Count - 1 do
        Memo1.Lines.Append( s.Strings[ i ] );
    end;
end;

procedure TForm1.ButtonRebuildFontClick(Sender: TObject);
begin
  RebuildFont();
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  zgl_Exit();
end;

procedure TForm1.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not fontMoving Then
    begin
      fontMoving := TRUE;
      lastX := X;
      lastY := Y;
    end;
end;

procedure TForm1.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if fontMoving Then
    begin
      fontX := fontX + ( X - lastX );
      fontY := fontY + ( Y - lastY );
      lastX := X;
      lastY := Y;
    end;
end;

procedure TForm1.Panel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  fontMoving := FALSE;
end;

procedure TForm1.ButtonSaveFontClick(Sender: TObject);
  var
    style : String;
begin
  if fg_FontBold and fg_FontItalic Then
    style := 'BoldItalic'
  else
    if fg_FontBold Then
      style := 'Bold'
    else
      if fg_FontItalic Then
        style := 'Italic'
      else
        style := 'Regular';

  SaveFontDialog.FileName := FontDialog.Font.Name + '-' + style + '-' + IntToStr( fg_FontSize ) + 'pt';

  if SaveFontDialog.Execute() Then
    begin
      fontgen_SaveFont( fg_Font, SaveFontDialog.FileName );
    end;
end;

procedure TForm1.CheckBoxAntialiasingChange(Sender: TObject);
begin
  fg_FontAA := CheckBoxAntialiasing.Checked;
  RebuildFont();
end;

procedure TForm1.CheckBoxPackChange(Sender: TObject);
begin
  fg_FontPack := CheckBoxPack.Checked;
  RebuildFont();
end;

procedure TForm1.ComboBoxPageSizeChange(Sender: TObject);
begin
  fg_PageSize := StrToInt( ComboBoxPageSize.Items[ ComboBoxPageSize.ItemIndex ] );
  RebuildFont();
end;

procedure TForm1.FormActivate(Sender: TObject);
{$IFDEF LINUX}
  var
    widget : PGtkWidget;
    socket : PGtkWidget;
    glist  : PGlist;
{$ENDIF}
begin
  if not zglInited Then
    begin
      zglInited := TRUE;

      zgl_Disable( APP_USE_LOG );
      zgl_Enable( APP_USE_UTF8 );

      zgl_Reg( SYS_LOAD, @Init );
      zgl_Reg( SYS_DRAW, @Draw );
    {$IFDEF LINUX}
      glist  := gtk_container_children( GTK_CONTAINER( PGtkWidget( Panel1.Handle ) ) );
      widget := PGtkWidget( glist.data );
      socket := gtk_socket_new();
      gtk_container_add( GTK_CONTAINER( widget ), socket );

      gtk_widget_show( socket );
      gtk_widget_show( widget );

      gtk_widget_realize( socket );
      {$IFDEF LCLGTK}
      zgl_InitToHandle( ( PGdkWindowPrivate( widget.window ) ).xwindow );
      {$ENDIF}
      {$IFDEF LCLGTK2}
      zgl_InitToHandle( GDK_WINDOW_XID( widget.window ) );
      {$ENDIF}
    {$ENDIF}
    {$IFDEF WINDOWS}
      zgl_InitToHandle( Panel1.Handle );
    {$ENDIF}
      Application.Terminate();
    end;
end;

end.

