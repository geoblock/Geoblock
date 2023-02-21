//----------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//----------------------------------------------------------------------------
(* The dialog to select parameters for making isoline contours *)

unit fPerformContours;

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

  
  fInitialDialog;

type
  TfmPerformContours = class(TfmInitialDialog)
    GroupBoxMark: TGroupBox;
    LabelBetweenMarks: TLabel;
    LabelMarkEvery: TLabel;
    CheckBoxPlaceMarks: TCheckBox;
    PanelFont: TPanel;
    RadioGroupShowAs: TRadioGroup;
    StaticTextOutline: TStaticText;
    SpinEditOutline: TSpinEdit;
    SpinEditMarkEvery: TSpinEdit;
    SpinEditExpandMarks: TSpinEdit;
    procedure ButtonOKClick(Sender: TObject);
    procedure CheckBoxPlaceMarksClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PanelFontClick(Sender: TObject);
  public
     
  private
    procedure ReadIniFile;
    procedure WriteIniFile;
    procedure EnableMarks;
  end;

var
  fmPerformContours: TfmPerformContours;

//======================================================================
implementation
//======================================================================

uses
  fGeoblock,
  dDialogs,
  cGlobals;

{$R *.DFM}

procedure TfmPerformContours.PanelFontClick(Sender: TObject);
begin
  with dmDialogs do
    if FontDialog.Execute then
    begin
      PanelFont.Font.Assign(FontDialog.Font);
    end;
end;

procedure TfmPerformContours.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      SpinEditOutline.Value   := ReadInteger(Name, SpinEditOutline.Name, 1);
      RadioGroupShowAs.ItemIndex := ReadInteger(Name, RadioGroupShowAs.Name, 0);
      PanelFont.Font.Color    := ReadInteger(Name, PanelFont.Name, clBtnFace);
      PanelFont.Font.Size     := ReadInteger(Name, PanelFont.Name + 'Size', 10);
      CheckBoxPlaceMarks.Checked := ReadBool(Name, CheckBoxPlaceMarks.Name, False);
      SpinEditExpandMarks.Value := ReadInteger(Name, SpinEditExpandMarks.Name, 1);
      SpinEditMarkEvery.Value := ReadInteger(Name, SpinEditMarkEvery.Name, 1);
    finally
      IniFile.Free;
    end;
end;

procedure TfmPerformContours.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteInteger(Name, SpinEditOutline.Name, SpinEditOutline.Value);
      WriteInteger(Name, RadioGroupShowAs.Name, RadioGroupShowAs.ItemIndex);
      WriteInteger(Name, PanelFont.Name, PanelFont.Font.Color);
      WriteInteger(Name, PanelFont.Name + 'Size', PanelFont.Font.Size);
      WriteBool(Name, CheckBoxPlaceMarks.Name, CheckBoxPlaceMarks.Checked);
      WriteInteger(Name, SpinEditExpandMarks.Name, SpinEditExpandMarks.Value);
      WriteInteger(Name, SpinEditMarkEvery.Name, SpinEditMarkEvery.Value);
    finally
      IniFile.Free;
    end;
end;

procedure TfmPerformContours.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
  EnableMarks;
end;

procedure TfmPerformContours.EnableMarks;
begin
  if CheckBoxPlaceMarks.Checked then
  begin
    LabelBetweenMarks.Enabled   := True;
    SpinEditExpandMarks.Enabled := True;
    LabelMarkEvery.Enabled      := True;
    SpinEditMarkEvery.Enabled   := True;
  end
  else
  begin
    LabelBetweenMarks.Enabled   := False;
    SpinEditExpandMarks.Enabled := False;
    LabelMarkEvery.Enabled      := False;
    SpinEditMarkEvery.Enabled   := False;
  end;
end;

procedure TfmPerformContours.ButtonOKClick(Sender: TObject);
begin
  inherited;
  {  // Barsuk & Lysenko
  with TfmGBContours.Create(Self) do
  try
   ShowModal;
  finally
    Free;
  end;
  }
end;

procedure TfmPerformContours.CheckBoxPlaceMarksClick(Sender: TObject);
begin
  EnableMarks;
end;

procedure TfmPerformContours.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;

end.
