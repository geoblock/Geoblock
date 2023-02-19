//-----------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//----------------------------------------------------------------------------
{
  The TerraContours to draw contours of values in TerraScene

}
unit fTerraContours;

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
  Vcl.StdCtrls //GR32_Image,
  //PColor
  ;

type
  TfmGSContours = class(TForm)
    btnSaveContours: TButton;
    procedure FormPaint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
    procedure DrawIsolines;
  end;

var
  fmGSContours: TfmGSContours;
//ContoursObj: TPColor;
implementation

uses
  uTerraLoader,

  GLS.Isolines;

{$R *.dfm}

procedure TfmGSContours.DrawIsolines;
var
  bmp:    TBitmap;
  i, j:   integer;
  cLines: array of TGLIsoline;
  x, y:   integer;
begin
  { bmp := TBitmap.Create;
   bmp.Width := matrixSize;
   bmp.Height := matrixSize;
   bmp.Canvas.Pen.Width := 5;
   bmp.Canvas.Pen.Color := clRed;

   SetLength(cLines, theContours.LineList.Count);

   for i := 0 to (theContours.LineList.Count - 1) do
   begin
      cLines[i] := theContours.LineList[i];

      x := round(cLines[i].Line[0].x);
      y := round(cLines[i].Line[0].y);
      bmp.Canvas.MoveTo(x, y);

      for j := 1 to cLines[i].NP - 1 do
      begin
         x := round(cLines[i].Line[j].x);
         y := round(cLines[i].Line[j].y);
         bmp.Canvas.LineTo(x, y);
      end;
   end;

   SetLength(cLines, 0);
   bmp.SaveToFile('D:\cont.bmp');
   bmp.Free;
   FreeAndNil(theContours); }

  //++++++++++++++++ PCOLOR ++++++++++++++++++++++

   {ContoursObj := TPColor.Create(Self);
   ContoursObj.Parent := Self;

   ContoursObj.ImageMode := imNormal;
   ContoursObj.Coordinates.Automatic := true;
   ContoursObj.TypeImage := dtInterp;

   ContoursObj.SetSizeGraph(Self.Width, Self.Height, 0, round(Tri.Max.x - Tri.Min.x), 0, round(Tri.Max.y - Tri.Min.y));
   ContoursObj.DrawData(@GSDepths, 0, round(Tri.Max.x - Tri.Min.x), 0, round(Tri.Max.y - Tri.Min.y));}

end;

procedure TfmGSContours.FormDestroy(Sender: TObject);
begin
   {SetLength(GSDepths, 0, 0);
   FreeAndNil(ContoursObj); }
end;

procedure TfmGSContours.FormPaint(Sender: TObject);
begin
  {ContoursObj.RePaintImage;
  ContoursObj.Width := Self.Width;
  ContoursObj.Height := Self.Height; }
end;

procedure TfmGSContours.FormShow(Sender: TObject);
begin
  //DrawIsolines;
end;

end.
