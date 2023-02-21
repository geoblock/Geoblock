//------------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//------------------------------------------------------------------------------
(* The dialog to set a line style *)

unit fDrawLineStyle;

interface

uses
  Winapi.Windows,
  System.SysUtils, 
  System.Classes, 
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs,
  Vcl.StdCtrls, 
  Vcl.ExtCtrls, 
  Vcl.Buttons, 
  Vcl.ComCtrls, 
  Vcl.Samples.Spin,

  
  fInitialDialog;

type
  PLineOptions = ^TLineOptions;

  TLineOptions = record
    Width: integer;
    Color: TColor;
    Style: TPenStyle;
  end;

type
  TfmDrawLineStyle = class(TfmInitialDialog)
    GroupBoxOptions: TGroupBox;
    LabelColor:      TLabel;
    LabelLineType:   TLabel;
    LabelLineWidth:  TLabel;
    ComboBoxLineType: TComboBox;
    PanelColor:      TPanel;
    ImageColor:      TImage;
    SpeedButtonColor: TSpeedButton;
    GroupBoxExample: TGroupBox;
    ImageExample:    TImage;
    SpinEditWidth:   TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure ComboBoxLineTypeChange(Sender: TObject);
    procedure EditWidthChange(Sender: TObject);
    procedure ImageColorClick(Sender: TObject);
    procedure ComboBoxLineTypeDrawItem(Control: TWinControl;
      Index: integer; Rect: TRect; State: TOwnerDrawState);
    procedure PanelColorEnter(Sender: TObject);
    procedure PanelColorExit(Sender: TObject);
    procedure SpinEditWidthChange(Sender: TObject);
    procedure SpinEditWidthExit(Sender: TObject);
    procedure SpinEditWidthKeyPress(Sender: TObject; var Key: char);
  public
    LineOptions: TLineOptions;
    procedure ExampleDraw;
    constructor CreateServiceLines(Width: integer; Color: TColor; Style: TPenStyle);
  end;

var
  fmDrawLineStyle: TfmDrawLineStyle;

//========================================================================
implementation
//========================================================================

uses
  dDialogs;

{$R *.DFM}

procedure TfmDrawLineStyle.FormCreate(Sender: TObject);
begin
  inherited;
  with ImageColor.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    FillRect(Rect(0, 0, ImageColor.Width, ImageColor.Height));
    Brush.Style := bsSolid;
    Brush.Color := LineOptions.Color;
    FillRect(Rect(1, 1, ImageColor.Width - 1, ImageColor.Height - 1));
  end;
  ComboBoxLineType.ItemIndex := 0;
end;

procedure TfmDrawLineStyle.ComboBoxLineTypeChange(Sender: TObject);
begin
  case ComboBoxLineType.ItemIndex of
    0: LineOptions.Style := psSolid;
    1: LineOptions.Style := psDash;
    2: LineOptions.Style := psDot;
    3: LineOptions.Style := psDashDot;
    4: LineOptions.Style := psDashDotDot;
    5: LineOptions.Style := psClear;
    6: LineOptions.Style := psInsideFrame;
  end;
  ExampleDraw;
end;

procedure TfmDrawLineStyle.EditWidthChange(Sender: TObject);
var
  LW, Code: integer;
begin
  Val(SpinEditWidth.Text, LW, Code);
  if Code = 0 then
  begin
    LineOptions.Width := LW;
    ExampleDraw;
  end;
end;

procedure TfmDrawLineStyle.ImageColorClick(Sender: TObject);
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
      LineOptions.Color := Brush.Color;
    end;
    LineOptions.Color := ImageColor.Canvas.Brush.Color;
    ExampleDraw;
  end;
end;

procedure TfmDrawLineStyle.ComboBoxLineTypeDrawItem(Control: TWinControl;
  Index: integer; Rect: TRect; State: TOwnerDrawState);
var
  Bmap: TBiTMap;
  R:    TRect;
begin
  Bmap := TBitmap.Create;
  Bmap.Width := (Control as TComboBox).Width - 2;
  Bmap.Height := Rect.Bottom - Rect.Top - 2;

  with Bmap do
  begin
    Canvas.Pen.Color    := clBlack;
    Canvas.Pen.Style    := psSolid;
    Canvas.Brush.Color  := clWhite;
    Canvas.Brush.Style  := bsSolid;
    Canvas.Brush.Bitmap := nil;
    Canvas.RectAngle(0, 0, Width, Height);

    Canvas.Pen.Color := clBlack;
    case Index of
      0: Canvas.Pen.Style := psSolid;
      1: Canvas.Pen.Style := psDash;
      2: Canvas.Pen.Style := psDot;
      3: Canvas.Pen.Style := psDashDot;
      4: Canvas.Pen.Style := psDashDotDot;
      5: Canvas.Pen.Style := psClear;
      6: Canvas.Pen.Style := psInsideFrame;
    end;
    Canvas.MoveTo(0, Height div 2);
    Canvas.LineTo(Width, Height div 2);
  end;

  with (Control as TComboBox) do
  begin
    Canvas.FillRect(Rect);
    R.Left   := Rect.Left + 1;
    R.Right  := R.Left + Bmap.Width;
    R.Top    := Rect.Top + 1;
    R.Bottom := R.Top + Bmap.Height;
    Canvas.CopyRect(R, Bmap.Canvas, Bmap.Canvas.ClipRect);
  end;
  Bmap.Free;
end;

constructor TfmDrawLineStyle.CreateServiceLines(Width: integer;
  Color: TColor; Style: TPenStyle);
begin
  Create(Application);

  LineOptions.Width := Width;
  LineOptions.Color := Color;
  LineOptions.Style := Style;

  with ImageColor.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    FillRect(Rect(0, 0, ImageColor.Width, ImageColor.Height));
    Brush.Style := bsSolid;
    Brush.Color := LineOptions.Color;
    FillRect(Rect(1, 1, ImageColor.Width - 1, ImageColor.Height - 1));
  end;
  case Canvas.Pen.Style of
    psSolid: ComboBoxLineType.ItemIndex   := 0;
    psDash: ComboBoxLineType.ItemIndex    := 1;
    psDot: ComboBoxLineType.ItemIndex     := 2;
    psDashDot: ComboBoxLineType.ItemIndex := 3;
    psDashDotDot: ComboBoxLineType.ItemIndex := 4;
    psClear: ComboBoxLineType.ItemIndex   := 5;
    psInsideFrame: ComboBoxLineType.ItemIndex := 6;
  end;
  ExampleDraw;
end;

procedure TfmDrawLineStyle.ExampleDraw;
var
  vPen: TPen;
begin
  vPen := TPen.Create;
  vPen.Color := LineOptions.Color;
  vPen.Width := LineOptions.Width;
  vPen.Style := LineOptions.Style;
  with ImageExample do
  begin
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(Rect(0, 0, Width, Height));
    Canvas.Pen.Assign(vPen);
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(Width, Height);
  end;
  vPen.Free;
end;

procedure TfmDrawLineStyle.PanelColorEnter(Sender: TObject);
begin
  with ImageColor.Canvas do
  begin
    DrawFocusRect(Rect(0, 0, ImageColor.Width, ImageColor.Height));
  end;
end;

procedure TfmDrawLineStyle.PanelColorExit(Sender: TObject);
begin
  with ImageColor.Canvas do
  begin
    DrawFocusRect(Rect(0, 0, ImageColor.Width, ImageColor.Height));
  end;
end;

procedure TfmDrawLineStyle.SpinEditWidthChange(Sender: TObject);
var
  LW, Code: integer;
begin
  Val(SpinEditWidth.Text, LW, Code);
  if Code = 0 then
  begin
    LineOptions.Width := LW;
    ExampleDraw;
  end;
end;

procedure TfmDrawLineStyle.SpinEditWidthExit(Sender: TObject);
var
  LW:   integer;
  Code: integer;
begin
  LW   := 1;
  Code := 0;
  Val(SpinEditWidth.Text, LW, Code);
  if Code <> 0 then
    SpinEditWidth.Text := '1';
end;

procedure TfmDrawLineStyle.SpinEditWidthKeyPress(Sender: TObject; var Key: char);
begin
  if ((Key < '0') or (Key > '9')) and (Key <> #8) then
  begin
    Beep;
    Key := #0;
  end;
end;

end.
