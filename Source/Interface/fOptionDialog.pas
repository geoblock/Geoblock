//----------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//----------------------------------------------------------------------------
{! The dialog for display options }

unit fOptionDialog;

interface

uses
  System.SysUtils, 
  System.Classes, 
  System.IniFiles,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs,
  Vcl.ExtCtrls, 
  Vcl.StdCtrls, 
  Vcl.CheckLst, 
  Vcl.Buttons, 
  Vcl.Samples.Spin,
  //DB
  Data.DB,

  //Geoblock
  fInitialDialog,
  cGlobals,
  uModels;

type
  TfmOptionDialog = class(TfmInitialDialog)
    GroupBoxAttribute: TGroupBox;
    LabelNumeric:      TLabel;
    ListBoxNumericAttributes: TListBox;
    SpeedButtonLegend: TSpeedButton;
    SpeedButtonFont:   TSpeedButton;
    RadioGroupSize:    TRadioGroup;
    SpinEditFactor:    TSpinEdit;
    ListBoxTextAttributes: TListBox;
    CheckBoxTextAttributes: TCheckBox;
    RadioGroupDetails: TRadioGroup;
    CheckBoxAsCylinder: TCheckBox;
    CheckBoxAsSphere:  TCheckBox;
    GroupBoxModel:     TGroupBox;
    StaticTextModel:   TStaticText;
    procedure SpeedButtonLegendClick(Sender: TObject);
    procedure SpeedButtonFontClick(Sender: TObject);
    procedure CheckBoxTextAttributesClick(Sender: TObject);
    procedure RadioGroupDetailsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListBoxNumericAttributesClick(Sender: TObject);
    procedure ListBoxTextAttributesClick(Sender: TObject);
    procedure RadioGroupSizeClick(Sender: TObject);
  protected
    procedure AssignFromModel(Source: TGBModel); virtual;
    procedure AssignToModel(Dest: TGBModel);
  private
    FFont: TFont;
    procedure SetFont(const Value: TFont); virtual;
    property Font: TFont Read FFont Write SetFont;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure ReadIniFile; override;
    procedure WriteIniFile; override;
  end;

var
  fmOptionDialog: TfmOptionDialog;

//==========================================================================
implementation
//==========================================================================

uses
  fMapWindow,
  dDialogs;

{$R *.DFM}

procedure TfmOptionDialog.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
  ListBoxTextAttributes.Enabled := CheckBoxTextAttributes.Checked;
  SpeedButtonFont.Enabled := CheckBoxTextAttributes.Checked;
  case RadioGroupDetails.ItemIndex of
    0:
    begin
      CheckBoxAsSphere.Enabled   := True;
      CheckBoxAsCylinder.Enabled := False;
      CheckBoxAsCylinder.Checked := False;
    end;
    1:
    begin
      CheckBoxAsSphere.Enabled   := False;
      CheckBoxAsSphere.Checked   := False;
      CheckBoxAsCylinder.Enabled := True;
    end;
    2:
    begin
      CheckBoxAsSphere.Enabled   := False;
      CheckBoxAsCylinder.Enabled := False;
      CheckBoxAsSphere.Checked   := False;
      CheckBoxAsCylinder.Checked := False;
    end;
  end;
end;


procedure TfmOptionDialog.SpeedButtonFontClick(Sender: TObject);
begin
  with dmDialogs do
  begin
    FontDialog.Font.Assign(FFont);
    if FontDialog.Execute then
    begin
      FFont.Assign(FontDialog.Font);
      ListBoxTextAttributesClick(Sender);
    end;
  end;
end;

procedure TfmOptionDialog.AssignFromModel(Source: TGBModel);
var
  I: integer;
begin
  StaticTextModel.Caption := Source.ModelName;
  StaticTextModel.Hint    := StaticTextModel.Caption;
  ListBoxNumericAttributes.Items.BeginUpdate;
  ListBoxTextAttributes.Items.BeginUpdate;
  try
    ListBoxNumericAttributes.Items.Clear;
    ListBoxTextAttributes.Items.Clear;
    for I := 0 to Source.Attributes.AttributeCount - 1 do
    begin
      //Numeric attributes
      case Source.Attributes[I].AttributeType of
        atReal, atInteger:
          ListBoxNumericAttributes.Items.Add(Source.Attributes[I].AttributeName);
      end;
      //Text attributes
      case Source.Attributes[I].AttributeType of
        atReal, atInteger, atText:
        begin
          ListBoxTextAttributes.Items.Add(Source.Attributes[I].AttributeName);
        end;
      end;
    end;
    if Source.DrawMode = dmPoint then
      RadioGroupDetails.ItemIndex := 0
    else if Source.DrawMode = dmLine then
      RadioGroupDetails.ItemIndex := 1
    else if Source.DrawMode = dmFill then
      RadioGroupDetails.ItemIndex := 2;

    CheckBoxTextAttributes.Checked := Source.TextAttributesChecked;
    CheckBoxAsSphere.Checked := Source.AsSphere;
    CheckBoxAsCylinder.Checked := Source.AsCylinder;
    RadioGroupSize.ItemIndex := Source.SizeMode;
    SpinEditFactor.Value := Source.SizeFactor;

    ListBoxNumericAttributes.ItemIndex :=
      ListBoxNumericAttributes.Items.IndexOf(Source.ActiveAttribute.AttributeName);
    ListBoxTextAttributes.ItemIndex    := Source.AttribTextNo;

    FFont := Source.Canvas3D.Font;
  finally
    ListBoxNumericAttributes.Items.EndUpdate;
    ListBoxTextAttributes.Items.EndUpdate;
  end;
end;

procedure TfmOptionDialog.AssignToModel(Dest: TGBModel);
begin
  Dest.BeginUpdate;
  if ListBoxNumericAttributes.ItemIndex <> -1 then
    if ListBoxTextAttributes.ItemIndex <> -1 then
      try
        Dest.Canvas3D.Font := FFont;
        Dest.TextAttributesChecked := CheckBoxTextAttributes.Checked;
        Dest.AttribTextNo := ListBoxTextAttributes.ItemIndex;

        Dest.ActiveAttributeNo :=
          Dest.Attributes.AttributeByName(
          ListBoxNumericAttributes.Items[ListBoxNumericAttributes.ItemIndex]).Index;

        Dest.AsSphere   := CheckBoxAsSphere.Checked;
        Dest.AsCylinder := CheckBoxAsCylinder.Checked;
        Dest.SizeMode   := RadioGroupSize.ItemIndex;
        Dest.SizeFactor := SpinEditFactor.Value;
        Dest.State      := Dest.State - [msOpenGLListValid];

      finally
        Dest.EndUpdate;
      end;
end;


procedure TfmOptionDialog.SpeedButtonLegendClick(Sender: TObject);
var
  SelectedField: string;
begin
  SelectedField := ListBoxNumericAttributes.Items[ListBoxNumericAttributes.ItemIndex];
  fmMapWindow.Model.Attributes.AttributeByName(SelectedField).LegendDialog;
  fmMapWindow.Model.ActiveAttributeNo :=
    fmMapWindow.Model.Attributes.AttributeByName(SelectedField).Index;
  fmMapWindow.Model.Update;
end;


procedure TfmOptionDialog.Assign(Source: TPersistent);
begin
  if Source is TGBModel then
    AssignFromModel(TGBModel(Source))
  else
    inherited;
end;

procedure TfmOptionDialog.AssignTo(Dest: TPersistent);
begin
  if Dest is TGBModel then
    AssignToModel(TGBModel(Dest))
  else
    inherited;
end;

procedure TfmOptionDialog.RadioGroupDetailsClick(Sender: TObject);
begin
  case RadioGroupDetails.ItemIndex of
    0:
    begin  // Points
      CheckBoxAsSphere.Enabled   := True;
      CheckBoxAsCylinder.Enabled := False;
      CheckBoxAsCylinder.Checked := False;
      fmMapWindow.Model.DrawMode := dmPoint;
    end;
    1:
    begin  //Lines
      CheckBoxAsSphere.Enabled   := False;
      CheckBoxAsCylinder.Enabled := True;
      CheckBoxAsSphere.Checked   := False;
      fmMapWindow.Model.DrawMode := dmLine;
    end;
    2:
    begin //Fills
      CheckBoxAsSphere.Enabled   := False;
      CheckBoxAsCylinder.Enabled := False;
      CheckBoxAsSphere.Checked   := False;
      CheckBoxAsCylinder.Checked := False;
      fmMapWindow.Model.DrawMode := dmFill;
    end;
  end;
  if ListBoxNumericAttributes.ItemIndex <> -1 then
    ListBoxNumericAttributesClick(Sender);
end;


procedure TfmOptionDialog.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      Top  := ReadInteger(Name, 'Top', 100);
      Left := ReadInteger(Name, 'Left', 200);
    finally
      IniFile.Free;
    end;
end;

procedure TfmOptionDialog.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteInteger(Name, 'Top', Top);
      WriteInteger(Name, 'Left', Left);
    finally
      IniFile.Free;
    end;
end;

procedure TfmOptionDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;

procedure TfmOptionDialog.ListBoxNumericAttributesClick(Sender: TObject);
begin
  AssignTo(fmMapWindow.Model);
  fmMapWindow.Repaint;
end;

procedure TfmOptionDialog.ListBoxTextAttributesClick(Sender: TObject);
begin
  AssignTo(fmMapWindow.Model);
  fmMapWindow.Repaint;
end;

procedure TfmOptionDialog.CheckBoxTextAttributesClick(Sender: TObject);
begin
  ListBoxTextAttributes.Enabled := CheckBoxTextAttributes.Checked;
  SpeedButtonFont.Enabled := CheckBoxTextAttributes.Checked;
  AssignTo(fmMapWindow.Model);
  fmMapWindow.Repaint;
end;

procedure TfmOptionDialog.RadioGroupSizeClick(Sender: TObject);
begin
  if ListBoxNumericAttributes.ItemIndex <> -1 then
    ListBoxNumericAttributesClick(Sender);
end;

procedure TfmOptionDialog.SetFont(const Value: TFont);
begin
  FFont := Value;
end;

end.
