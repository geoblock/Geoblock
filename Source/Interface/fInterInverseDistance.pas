//-----------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//-----------------------------------------------------------------------------
(* Inverse distance interpolation dialog *)

unit fInterInverseDistance;

interface

uses
  System.SysUtils, 
  System.Classes, 
  System.IniFiles,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs,
  Vcl.StdCtrls, 
  Vcl.ExtCtrls, 
  Vcl.Samples.Spin, 
  Vcl.ComCtrls,

  
  fInitialDialog,
  cInterpol;

type
  TfmInterInverseDistance = class(TfmInitialDialog)
    GroupBoxMode:  TGroupBox;
    LabelPower:    TLabel;
    SpinEditPower: TSpinEdit;
    ButtonReset:   TButton;
    RadioGroupSearch: TRadioGroup;
    CheckBoxUseTriangulation: TCheckBox;
    SpinEditLocalPoints: TSpinEdit;
    SpinEditGlobalPoints: TSpinEdit;
    procedure RadioGroupSearchClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ButtonResetClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    IniFile: TIniFile;
    procedure ActionExampleExecute(Sender: TObject);
    procedure ReadIniFile;
    procedure WriteIniFile;
  public
    function InitParams: boolean;
  end;

var
  fmInterInverseDistance: TfmInterInverseDistance;

//=========================================================================
implementation
//=========================================================================

{$R *.DFM}

procedure TfmInterInverseDistance.FormActivate(Sender: TObject);
begin
  inherited;
  ActionExampleExecute(Self);
end;

procedure TfmInterInverseDistance.ActionExampleExecute(Sender: TObject);
begin
  (*
    with ImageExample do
    begin
      //Axes
      Canvas.Brush.Color:=clBtnFace;
      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(Rect(0,0,Width,Height));
      //XZBack
        Canvas.Brush.Color:=clRed;
        Canvas.Pen.Color:=clWhite;
        Canvas.FillRect(Rect(Width div 4,0,Width,Height-Height div 4));
      //YZLeft
        Canvas.Brush.Color:=clGreen;
        Canvas.Pen.Color:=clWhite;
        Canvas.Polygon([Point(0, Height div 4),
                        Point(Width div 4, 0),
                        Point(Width div 4,Height-Height div 4),
                        Point(0, Height)]);
      //XYBottom
        Canvas.Brush.Color:=clBlue;
        Canvas.Pen.Color:=clWhite;
        Canvas.Polygon([Point(0, Height ),
                        Point(Width div 4, Height-Height div 4),
                        Point(Width, Height-Height div 4),
                        Point(Width-Width div 4, Height)]);
      //XYTop
        Canvas.Brush.Color:=clBlue;
        Canvas.Pen.Color:=clWhite;
        Canvas.Polygon([Point(0, Height div 4 ),
                        Point(Width div 4, 0),
                        Point(Width, 0),
                        Point(Width-Width div 4, Height div 4)]);
      //YZRight
        Canvas.Brush.Color:=clGreen;
        Canvas.Pen.Color:=clWhite;
        Canvas.Polygon([Point(Width-Width div 4, Height div 4),
                        Point(Width, 0),
                        Point(Width,Height-Height div 4),
                        Point(Width-Width div 4, Height)]);
      //XZFront
        Canvas.Brush.Color:=clRed;
        Canvas.Pen.Color:=clWhite;
        Canvas.Polygon([Point(1, Height div 4),
                        Point(Width-Width div 4, Height div 4),
                        Point(Width-Width div 4, Height),
                        Point(1, Height)]);
    end;
  *)
end;

function TfmInterInverseDistance.InitParams: boolean;
begin
  InvDistPars.Power := 1;
  InvDistPars.NP    := 20;
  InvDistPars.RegionNP := 10;
  InvDistPars.SearchMode := 0;
  InvDistPars.UseTriangulation := False;
end;

procedure TfmInterInverseDistance.ButtonResetClick(Sender: TObject);
begin
  SpinEditPower.Value := 1;
  RadioGroupSearch.ItemIndex := 0;
  SpinEditGlobalPoints.Value := 20;
  SpinEditLocalPoints.Value := 10;
  CheckBoxUseTriangulation.Checked := False;
end;

procedure TfmInterInverseDistance.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      RadioGroupSearch.ItemIndex := ReadInteger(Name, RadioGroupSearch.Name, 0);
      SpinEditPower.Value := ReadInteger(Name, SpinEditPower.Name, 1);
      SpinEditGlobalPoints.Value := ReadInteger(Name, SpinEditGlobalPoints.Name, 20);
      SpinEditLocalPoints.Value := ReadInteger(Name, SpinEditLocalPoints.Name, 10);
      CheckBoxUseTriangulation.Checked :=
        ReadBool(Name, CheckBoxUseTriangulation.Name, False);
    finally
      IniFile.Free;
    end;
end;

procedure TfmInterInverseDistance.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteInteger(Name, RadioGroupSearch.Name, RadioGroupSearch.ItemIndex);
      WriteInteger(Name, SpinEditPower.Name, SpinEditPower.Value);
      WriteInteger(Name, SpinEditGlobalPoints.Name, SpinEditGlobalPoints.Value);
      WriteInteger(Name, SpinEditLocalPoints.Name, SpinEditLocalPoints.Value);
      WriteBool(Name, CheckBoxUseTriangulation.Name, CheckBoxUseTriangulation.Checked);
    finally
      IniFile.Free;
    end;
end;

procedure TfmInterInverseDistance.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
  RadioGroupSearchClick(Self);
end;

procedure TfmInterInverseDistance.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;

procedure TfmInterInverseDistance.ButtonOKClick(Sender: TObject);
begin
  InvDistPars.Power := SpinEditPower.Value;
  InvDistPars.RegionNP := SpinEditLocalPoints.Value;
  InvDistPars.NP := SpinEditGlobalPoints.Value;
  InvDistPars.SearchMode := RadioGroupSearch.ItemIndex;
  InvDistPars.UseTriangulation := CheckBoxUseTriangulation.Checked;
end;


procedure TfmInterInverseDistance.RadioGroupSearchClick(Sender: TObject);
begin
  inherited;
  case RadioGroupSearch.ItemIndex of
    0:
    begin
      SpinEditGlobalPoints.Enabled := False;
      SpinEditLocalPoints.Enabled  := False;
    end;
    1:
    begin
      SpinEditGlobalPoints.Enabled := True;
      SpinEditLocalPoints.Enabled  := False;
    end;
    2:
    begin
      SpinEditGlobalPoints.Enabled := False;
      SpinEditLocalPoints.Enabled  := True;
    end;
  end;
end;

end.
