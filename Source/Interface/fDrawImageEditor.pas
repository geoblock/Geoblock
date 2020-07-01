//------------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//------------------------------------------------------------------------------
{! The Image Editor for database fields

  History :
     22/03/06 - Ilya Lysenko and Alexey Barsuk - added ability to set a size of the
                                                 texture and tiles option, fixed pathes.
     12/12/03 - Pavel Vassiliev - Fixed minor bugs
     11/12/99 - Elena Kinzerskaya - Added DBImageMouseMove
     01/08/97 - Pavel Vassiliev - Creation
}

unit fDrawImageEditor;

interface

uses
  Winapi.Windows,
  System.SysUtils, 
  System.Variants, 
  System.Classes, 
  Vcl.Graphics,
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs, 
  Vcl.StdCtrls, 
  Vcl.ExtCtrls,
  Vcl.Mask, 
  Vcl.DBCtrls, 
  Vcl.Buttons, 
  Vcl.Clipbrd, 
  Vcl.Imaging.Jpeg,

  
  fInitialDialog;

type
  TfmDrawImageEditor = class(TfmInitialDialog)
    SpeedButtonNew: TSpeedButton;
    SpeedButtonSave: TSpeedButton;
    SpeedButtonClear: TSpeedButton;
    SpeedButtonCopy: TSpeedButton;
    SpeedButtonCut: TSpeedButton;
    SpeedButtonPaste: TSpeedButton;
    SpeedButtonMSPaint: TSpeedButton;
    GroupBoxFill: TGroupBox;
    Bevel1:     TBevel;
    LabelPattern: TLabel;
    LabelExample: TLabel;
    ImageExample: TImage;
    LabelName:  TLabel;
    DBImage:    TDBImage;
    DBEditName: TDBEdit;
    cbApplyTiles: TCheckBox;
    cbxSize:    TComboBox;
    lbTextureSize: TLabel;
    procedure cbApplyTilesClick(Sender: TObject);
    procedure cbxSizeChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SpeedButtonNewClick(Sender: TObject);
    procedure SpeedButtonSaveClick(Sender: TObject);
    procedure SpeedButtonClearClick(Sender: TObject);
    procedure SpeedButtonCopyClick(Sender: TObject);
    procedure SpeedButtonCutClick(Sender: TObject);
    procedure SpeedButtonPasteClick(Sender: TObject);
    procedure SpeedButtonMSPaintClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure DBImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure DBImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
  private
     
    procedure DrawExample;
    procedure GetTextureImage;
  public
     
  end;

var
  fmDrawImageEditor: TfmDrawImageEditor;
  theTextureImage: TBitMap;
  ImageSize: integer = 32;

//========================================================================
implementation
//========================================================================

uses
  uGlobals,
  uCommon,
  dDialogs,
  dBase,
  uResStrings,
  GBGraphics;

{$R *.dfm}

//----------------------------------------------------------

function GetX(X: integer): integer;
begin
  Result := ((X) div 4);
end;

function GetY(Y: integer): integer;
begin
  Result := ((Y) div 4);
end;


procedure TfmDrawImageEditor.FormCreate(Sender: TObject);
begin
  inherited;
  SpeedButtonMSPaint.Visible :=
    FileExists(GetEnvironmentVariable(PChar('SystemRoot')) + '\System32\MSPAINT.EXE');
  theTextureImage := TBitmap.Create;
  DBImage.Cursor  := crPenCursor;
end;

procedure TfmDrawImageEditor.FormDestroy(Sender: TObject);
begin
  inherited;
//  theTextureImage.Free;
end;

procedure TfmDrawImageEditor.SpeedButtonMSPaintClick(Sender: TObject);
begin
  WinExec(PAnsiChar(GetEnvironmentVariable(PAnsiChar('SystemRoot')) +
    '\System32\MSPAINT.EXE'),
    SW_SHOWDEFAULT);
end;

procedure TfmDrawImageEditor.GetTextureImage;
var
  Picture: TPicture;
begin
  Picture := TPicture.Create;
  Picture.Bitmap.Assign(theTextureImage);
  if (Picture.Width > ImageSize) or (Picture.Height > ImageSize) then
  begin
    Picture.Bitmap.Canvas.StretchDraw(rect(0, 0, ImageSize, ImageSize),
      Picture.Bitmap);
    Picture.BitMap.Width  := ImageSize;
    Picture.Bitmap.Height := ImageSize;
  end;
  DBImage.Picture.Bitmap.Assign(Picture.BitMap);
  Picture.Free;
  DrawExample;
end;

procedure TfmDrawImageEditor.SpeedButtonNewClick(Sender: TObject);
begin
  if dmDialogs.OpenPictureDialog.Execute then
  begin
    theTextureImage.LoadFromFile(dmDialogs.OpenPictureDialog.FileName);
    GetTextureImage;
  end;
end;

procedure TfmDrawImageEditor.SpeedButtonSaveClick(Sender: TObject);
begin
  if dmDialogs.SavePictureDialog.Execute then
  begin
    DBImage.Picture.SaveToFile(dmDialogs.SavePictureDialog.FileName);
  end;
end;

procedure TfmDrawImageEditor.SpeedButtonClearClick(Sender: TObject);
var
  Bitmap: TBitmap;
begin
  if MessageDlg(LoadResString(@rsClear) + '?', mtConfirmation,
    [mbYes, mbNo], 0) = mrYes then
  begin
    dmBase.DataSource.DataSet.FieldByName(DBImage.DataField).Clear;
    Bitmap := GetEmptyBitmap(ImageSize, ImageSize);
    with ImageExample do
    begin
      Canvas.Brush.Bitmap := nil;
      Canvas.Brush.Bitmap := BitMap;
      Canvas.Rectangle(0, 0, Width, Height);
      Canvas.Brush.Bitmap := nil;
    end;
    Bitmap.Free;
  end;
end;

procedure TfmDrawImageEditor.SpeedButtonCopyClick(Sender: TObject);
begin
  if not DBImage.Picture.Bitmap.Empty then
  begin
    DBImage.CopyToClipBoard;
  end
  else
  begin
    Clipboard.Clear;
  end;
end;

procedure TfmDrawImageEditor.SpeedButtonCutClick(Sender: TObject);
begin
  if not DBImage.Picture.Bitmap.Empty then
  begin
    DBImage.CopyToClipBoard;
  end
  else
  begin
    Clipboard.Clear;
  end;
  SpeedButtonClearClick(Sender);
end;

procedure TfmDrawImageEditor.SpeedButtonPasteClick(Sender: TObject);
var
  BitMap: TBitMap;
begin
  if Clipboard.HasFormat(CF_BITMAP) then
  begin
    BitMap := TBitMap.Create;
    BitMap.Assign(Clipboard);
    if (Bitmap.Width > ImageSize) or (Bitmap.Height > ImageSize) then
    begin
      BitMap.Canvas.StretchDraw(rect(0, 0, ImageSize, ImageSize), Bitmap);
      BitMap.Width  := ImageSize;
      BitMap.Height := ImageSize;
    end;
    DBImage.Picture.Bitmap.Assign(BitMap);
    DrawExample;
  end;
end;

procedure TfmDrawImageEditor.FormActivate(Sender: TObject);
begin
  DrawExample;
  cbxSize.ItemIndex := 0;
end;

procedure TfmDrawImageEditor.DrawExample;
var
  R: TRect;
begin
  try
    R := Rect(0, 0, ImageExample.Width - 1, ImageExample.Height - 1);
    ImageExample.Canvas.Brush.Color := clWhite;
    ImageExample.Canvas.FillRect(R);
    if (cbApplyTiles.Checked) then
    begin
      DrawTile(R, ImageExample.Canvas, DBImage.Picture.Bitmap);
    end
    else
    begin
      ImageExample.Picture.Bitmap.Canvas.StretchDraw(
        rect(0, 0, ImageExample.Width, ImageExample.Height), DBImage.Picture.Bitmap);
    end;
  except
  end;
end;

procedure TfmDrawImageEditor.cbApplyTilesClick(Sender: TObject);
begin
  inherited;
  GetTextureImage;
end;

procedure TfmDrawImageEditor.cbxSizeChange(Sender: TObject);
begin
  inherited;
  ImageSize := StrToInt(cbxSize.Text);
  GetTextureImage;
end;

procedure TfmDrawImageEditor.DBImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  Bitmap: TBitmap;
const
  Colors: array[boolean] of TColor = (clBlack, clWhite);
begin
  if DBImage.Picture.Bitmap.Empty then
  begin
    Bitmap := GetEmptyBitmap(ImageSize, ImageSize);
  end
  else
  begin
    Bitmap := GetEmptyBitmap(ImageSize, ImageSize);
  end;
  try
    Bitmap.Canvas.StretchDraw(Rect(0, 0, ImageSize, ImageSize),
      DBImage.Picture.Bitmap);
    Bitmap.Canvas.Pixels[GetX(X), GetY(Y)] :=
      Colors[Bitmap.Canvas.Pixels[GetX(X), GetY(Y)] = clBlack];
    DBImage.Picture.Assign(Bitmap);
    DrawExample;
  except
  end;
  Bitmap.Free;
end;

procedure TfmDrawImageEditor.DBImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
var
  Bitmap: TBitmap;
const
  Colors: array[boolean] of TColor = (clBlack, clWhite);
begin
  if (Shift * [ssLeft, ssRight] = []) then
  begin
    exit;
  end;
  {if DBImage.Picture.Bitmap.Empty then
    Bitmap:=GetEmptyBitmap(ImageSize,ImageSize);
  else}
  Bitmap := GetEmptyBitmap(ImageSize, ImageSize);
  try
    Bitmap.Canvas.StretchDraw(Rect(0, 0, ImageSize, ImageSize),
      DBImage.Picture.Bitmap);
    Bitmap.Canvas.Pixels[GetX(X), GetY(Y)] := Colors[ssRight in Shift];
    DBImage.Picture.Assign(Bitmap);
    DrawExample;
  except
  end;
  Bitmap.Free;
end;

end.
