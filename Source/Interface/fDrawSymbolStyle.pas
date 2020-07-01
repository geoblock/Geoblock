//------------------------------------------------------------------------------

// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock

//------------------------------------------------------------------------------
{!  The dialog for choosing symbol style }

unit fDrawSymbolStyle;

interface

uses
  Winapi.Windows, 
  Winapi.Messages, 
  System.SysUtils, 
  System.Variants, 
  System.Classes, 
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms,
  Vcl.Dialogs, 
  Vcl.StdCtrls, 
  Vcl.ExtCtrls, 
  Vcl.Samples.Spin, 
  Vcl.Buttons, Grids,

  
  fInitialDialog,
  dDialogs;


type
  PSymbolOptions = ^TSymbolOptions;

  TSymbolOptions = record
    Name:   TFontName;
    Color:  TColor;
    Size:   integer;
    Symbol: string[1];
  end;


type
  TfmDrawSymbolStyle = class(TfmInitialDialog)
    GroupBoxOptions: TGroupBox;
    LabelFont:    TLabel;
    LabelSymbol:  TLabel;
    LabelColor:   TLabel;
    LabelSize:    TLabel;
    StringGridSymbols: TStringGrid;
    ComboBoxFont: TComboBox;
    PanelColor:   TPanel;
    SpeedButtonColor: TSpeedButton;
    ImageColor:   TImage;
    GroupBoxExample: TGroupBox;
    ImageExample: TImage;
    SpinEditSize: TSpinEdit;
    procedure ComboBoxFontChange(Sender: TObject);
    procedure StringGridSymbolsSelectCell(Sender: TObject;
      ACol, ARow: integer; var CanSelect: boolean);
    procedure SpinEditSizeChange(Sender: TObject);
    procedure ImageColorClick(Sender: TObject);
    procedure PanelColorEnter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
     
    procedure ExampleDraw(S: string);
  public
     
    DefaultOptions, SymbolOptions: TSymbolOptions;
  end;

var
  fmDrawSymbolStyle: TfmDrawSymbolStyle;

//========================================================================
implementation
//========================================================================


{$R *.dfm}

procedure TfmDrawSymbolStyle.ComboBoxFontChange(Sender: TObject);
var
  I, J: integer;
begin
  StringGridSymbols.Visible := False;
  StringGridSymbols.Font.Name := ComboBoxFont.Text;
  SymbolOptions.Name := ComboBoxFont.Text;
  with StringGridSymbols do
  begin
    for I := 0 to RowCount - 1 do
      for J := 0 to ColCount - 1 do
        Cells[J, I] := '' + char((I * ColCount + J + Ord('!')));
    Row := 0;
    Col := 0;
  end;
  StringGridSymbols.Visible := True;
  ImageExample.Canvas.Font.Name := ComboBoxFont.Text;
  SymbolOptions.Name := ComboBoxFont.Text;
  ExampleDraw(StringGridSymbols.Cells[0, 0]);
end;

procedure TfmDrawSymbolStyle.StringGridSymbolsSelectCell(Sender: TObject;
  ACol, ARow: integer; var CanSelect: boolean);
begin
  SymbolOptions.Symbol := StringGridSymbols.Cells[ACol, ARow];
  ExampleDraw(StringGridSymbols.Cells[ACol, ARow]);
end;

procedure TfmDrawSymbolStyle.SpinEditSizeChange(Sender: TObject);
begin
  SymbolOptions.Size := SpinEditSize.Value;
  ExampleDraw(SymbolOptions.Symbol);
end;

procedure TfmDrawSymbolStyle.ImageColorClick(Sender: TObject);
begin
  PanelColor.SetFocus;
  dmDialogs.ColorDialog.Color := ImageColor.Canvas.Brush.Color;
  if dmDialogs.ColorDialog.Execute then
  begin
    with ImageColor.Canvas do
    begin
      PanelColorEnter(Sender);
      Brush.Color := dmDialogs.ColorDialog.Color;
      FillRect(Rect(1, 1, ImageColor.Width - 1, ImageColor.Height - 1));
      PanelColorEnter(Sender);
      SymbolOptions.Color := Brush.Color;
    end;
    ImageColor.Canvas.Brush.Color := SymbolOptions.Color;
    ExampleDraw(SymbolOptions.Symbol);
  end;
end;

procedure TfmDrawSymbolStyle.PanelColorEnter(Sender: TObject);
begin
  with ImageColor.Canvas do
  begin
    DrawFocusRect(Rect(0, 0, ImageColor.Width, ImageColor.Height));
  end;
end;

procedure TfmDrawSymbolStyle.FormCreate(Sender: TObject);
var
  I, J: integer;

begin
  inherited;
  ComboBoxFont.Clear;
  ComboBoxFont.Sorted    := True;
  ComboBoxFont.Items     := Screen.Fonts;
  ComboBoxFont.ItemIndex := 0;
  with ImageColor.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    FillRect(Rect(0, 0, ImageColor.Width, ImageColor.Height));
    Brush.Style := bsSolid;
    Brush.Color := clBlack;
    FillRect(Rect(1, 1, ImageColor.Width - 1, ImageColor.Height - 1));
  end;

  with StringGridSymbols do
  begin
    Font.Name := ComboBoxFont.Items[0];
    for I := 0 to RowCount - 1 do
      for J := 0 to ColCount - 1 do
        Cells[J, I] := '' + char((I * ColCount + J + Ord('!')));
  end;
  DefaultOptions.Name   := ComboBoxFont.Items[ComboBoxFont.ItemIndex];
  DefaultOptions.Symbol := StringGridSymbols.Cells[0, 0];

  ImageExample.Canvas.Brush.Color := clBtnFace;
  ImageExample.Canvas.Font.Name   := ComboBoxFont.Items[ComboBoxFont.ItemIndex];
  ExampleDraw(StringGridSymbols.Cells[0, 0]);
end;

procedure TfmDrawSymbolStyle.ExampleDraw(S: string);
begin
  with ImageExample do
  begin
    Canvas.Font.Size  := SymbolOptions.Size;
    Canvas.Font.Color := SymbolOptions.Color;
    Canvas.FillRect(Rect(0, 0, Width, Height));
    Canvas.TextOut((Width div 2 - Canvas.TextWidth(S) div 2),
      (Height div 2 - Canvas.TextHeight(S) div 2), S);
  end;
end;

end.
