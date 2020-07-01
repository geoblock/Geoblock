//----------------------------------------------------------------------------
// The unit of Geoblock, http://sourceforge.net/projects/geoblock
//---------------------------------------------------------------------------
{ The dialog for variogram parameters }

unit fAnalyseVariograms;

interface

uses
  System.SysUtils, 
  System.Variants,
  System.Classes,
  System.IniFiles,
  System.Math,
  System.ImageList,
  Vcl.ImgList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Buttons,
  Vcl.ToolWin,
  Vcl.ExtCtrls,
  Vcl.Samples.Spin,
  //DB
  Bde.DBTables,
  Data.DB,

  fMethodDialog,
  dBase,
  uInterpol,
  uVariograms;

type
  TfmAnalyseVariograms = class(TfmMethodDialog)
    ComboBoxVariogramType: TComboBox;
    LabelVariogramType: TLabel;
    GroupBoxLagData: TGroupBox;
    LabelLagsNumber: TLabel;
    LabelLagSeparationDistance: TLabel;
    LabelLagTolerance: TLabel;
    SpinEditLagsNumber: TSpinEdit;
    EditLagSeparationDistance: TEdit;
    EditLagTolerance: TEdit;
    GroupBoxDirection: TGroupBox;
    SpinEditDirectionsNumber: TSpinEdit;
    LabelDirectionsNumber: TLabel;
    ListBoxDirections: TListBox;
    CheckBoxStandardized: TCheckBox;
    GroupBoxAzimuth: TGroupBox;
    LabelAzimuth: TLabel;
    LabelAzimuthTolerance: TLabel;
    LabelAzimuthBandwidth: TLabel;
    EditAzimuthBandwidth: TEdit;
    EditAzimuthTolerance: TEdit;
    EditAzimuth:  TEdit;
    GroupBoxDip:  TGroupBox;
    LabelDip:     TLabel;
    LabelDipTolerance: TLabel;
    LabelDipBandwidth: TLabel;
    EditDip:      TEdit;
    EditDipTolerance: TEdit;
    EditDipBandwidth: TEdit;
    ButtonReset:  TButton;
    procedure ButtonResetClick(Sender: TObject);
    procedure ListBoxDirectionsClick(Sender: TObject);
    procedure SpinEditDirectionsNumberChange(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure DirectionPropertyChange(Sender: TObject);
    procedure ListBoxInputNamesClick(Sender: TObject);
    procedure ToolButtonInputClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    DirCountOld: integer;
    Mode3D:      boolean;
    DefaultAzimuthBandwidth, DefaultDipBandwidth: double;
    procedure SetDirectionData(p: PVariogramDirection);
    procedure MakeVariogram;
  public
    procedure ReadIniFile;
    procedure WriteIniFile;
  end;

var
  fmAnalyseVariograms: TfmAnalyseVariograms;

implementation

uses
  uGlobals, 
  uCommon, 
  uFileCreator,
  uProfuns, 
  gnuGettext;

{$R *.dfm}

procedure TfmAnalyseVariograms.SetDirectionData(p: PVariogramDirection);
begin
  with p^ do
  begin
    azimuth := 0;
    azimuth_tolerance := 90;
    azimuth_bandwidth := DefaultAzimuthBandwidth;
    dip     := 0;
    dip_tolerance := 90;
    dip_bandwidth := DefaultDipBandwidth;
  end;
end;

procedure TfmAnalyseVariograms.FormCreate(Sender: TObject);
var
  i: integer;
  p: PVariogramDirection;
begin
  inherited;
  ReadIniFile;
  SpinEditLagsNumber.MaxValue := MAXLAG;
  SpinEditDirectionsNumber.MaxValue := MAXDIR;
  DirCountOld := SpinEditDirectionsNumber.Value;
  ListBoxDirections.Items.Clear;
  for i := 0 to SpinEditDirectionsNumber.Value - 1 do
  begin
    New(p);
    SetDirectionData(p);
    ListBoxDirections.AddItem(_('Direction') + ' ' + IntToStr(i + 1), TObject(p));
  end;
  DefaultAzimuthBandwidth := 50;
  DefaultDipBandwidth     := 50;
  ListBoxDirections.ItemIndex := 0;
  ListBoxDirectionsClick(ListBoxDirections);
  ButtonResetClick(ButtonReset);
end;

procedure TfmAnalyseVariograms.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;

procedure TfmAnalyseVariograms.ButtonResetClick(Sender: TObject);
var
  i: integer;
  p: PVariogramDirection;
begin
  for i := 0 to SpinEditDirectionsNumber.Value - 1 do
  begin
    p := PVariogramDirection(ListBoxDirections.Items.Objects[i]);
    p^.azimuth_bandwidth := DefaultAzimuthBandwidth;
    p^.dip_bandwidth := DefaultDipBandwidth;
  end;
  ListBoxDirectionsClick(self);
  ComboBoxVariogramType.ItemIndex := 0;
  EditLagSeparationDistance.Text :=
    IntToStr(Trunc(Max(DefaultAzimuthBandwidth, DefaultDipBandwidth) /
    SpinEditLagsNumber.Value));
  EditLagTolerance.Text := IntToStr(
    Trunc(StrToFloat(EditLagSeparationDistance.Text) / 2));
end;

procedure TfmAnalyseVariograms.DirectionPropertyChange(Sender: TObject);
var
  ed: TEdit;
  i:  integer;
  p:  PVariogramDirection;
begin
  ed := Sender as TEdit;
  i  := ListBoxDirections.ItemIndex;
  p  := PVariogramDirection(ListBoxDirections.Items.Objects[i]);
  if ed = EditAzimuth then
    p^.azimuth := StrToFloat(ed.Text);
  if ed = EditAzimuthTolerance then
    p^.azimuth_tolerance := StrToFloat(ed.Text);
  if ed = EditAzimuthBandwidth then
    p^.azimuth_bandwidth := StrToFloat(ed.Text);
  if ed = EditDip then
    p^.dip := StrToFloat(ed.Text);
  if ed = EditDipTolerance then
    p^.dip_tolerance := StrToFloat(ed.Text);
  if ed = EditDipBandwidth then
    p^.dip_bandwidth := StrToFloat(ed.Text);
end;

procedure TfmAnalyseVariograms.ButtonOKClick(Sender: TObject);
begin
  MakeVariogram;
end;

procedure TfmAnalyseVariograms.ListBoxDirectionsClick(Sender: TObject);
var
  i: integer;
  d: TVariogramDirection;
begin
  i := ListBoxDirections.ItemIndex;
  d := PVariogramDirection(ListBoxDirections.Items.Objects[i])^;
  EditAzimuth.Text := FloatToStr(d.azimuth);
  EditAzimuthTolerance.Text := FloatToStr(d.azimuth_tolerance);
  EditAzimuthBandwidth.Text := FloatToStr(d.azimuth_bandwidth);
  EditDip.Text := FloatToStr(d.dip);
  EditDipTolerance.Text := FloatToStr(d.dip_tolerance);
  EditDipBandwidth.Text := FloatToStr(d.dip_bandwidth);
end;

procedure TfmAnalyseVariograms.ListBoxInputNamesClick(Sender: TObject);
var
  xmin, xmax, ymin, ymax, zmin, zmax: double;
begin
  inherited;
  ListBoxRealAttributeClick(self);
  with ListBoxRealAttribute do
    if Items.Count > 0 then
    begin
      ItemIndex   := 0;
      Selected[0] := True;
    end
    else
      ItemIndex := -1;
  GetMinMaxXYZ(InModelName, xmin, xmax, ymin, ymax, zmin, zmax);
  DefaultAzimuthBandwidth := Trunc(Max(xmax - xmin, ymax - ymin)) + 1;
  DefaultDipBandwidth     := Trunc(zmax - zmin) + 1;
end;

procedure TfmAnalyseVariograms.ToolButtonInputClick(Sender: TObject);
begin
  inherited;
  ListBoxInputNamesClick(Self);
  PanelOutPath.Caption := ExpandPath(dirExpVar);
  OutModelName := PanelOutPath.Caption + EditOutName.Text;
  OutModelType := mtAll;
  if Sender = ToolButtonPoints2D then
  begin
    EditDip.Enabled := False;
    EditDipTolerance.Enabled := False;
    EditDipBandwidth.Enabled := False;
    Mode3D := False;
  end
  else
  begin
    EditDip.Enabled := True;
    EditDipTolerance.Enabled := True;
    EditDipBandwidth.Enabled := True;
    Mode3D := True;
  end;
end;

procedure TfmAnalyseVariograms.SpinEditDirectionsNumberChange(Sender: TObject);
var
  DirCount, i: integer;
  p: PVariogramDirection;
begin
  DirCount := (Sender as TSpinEdit).Value;
  if DirCount > DirCountOld then
    for i := DirCountOld to DirCount - 1 do
    begin
      New(p);
      SetDirectionData(p);
      ListBoxDirections.AddItem(_('Direction') + ' ' + IntToStr(i + 1), TObject(p));
    end
  else if DirCount < DirCountOld then
    for i := DirCount to DirCountOld do
      ListBoxDirections.Items.Delete(i);
  if ListBoxDirections.ItemIndex < 0 then
    ListBoxDirections.ItemIndex := 0;
  ListBoxDirectionsClick(self);
  DirCountOld := DirCount;
end;

procedure TfmAnalyseVariograms.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      ComboBoxVariogramType.ItemIndex :=
        ReadInteger(Name, ComboBoxVariogramType.Name, 0);
      SpinEditLagsNumber.Value     := ReadInteger(Name, SpinEditLagsNumber.Name, 2);
      SpinEditDirectionsNumber.Value     := ReadInteger(Name, SpinEditDirectionsNumber.Name, 1);
      ComboBoxVariogramType.ItemIndex  := ReadInteger(Name, ComboBoxVariogramType.Name, -1);
      CheckBoxStandardized.Checked := ReadBool(Name, CheckBoxStandardized.Name, False);
    finally
      IniFile.Free;
    end;
end;

procedure TfmAnalyseVariograms.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteInteger(Name, ComboBoxVariogramType.Name, ComboBoxVariogramType.ItemIndex);
      WriteInteger(Name, SpinEditLagsNumber.Name, SpinEditLagsNumber.Value);
      WriteInteger(Name, SpinEditDirectionsNumber.Name, SpinEditDirectionsNumber.Value);
      WriteInteger(Name, ComboBoxVariogramType.Name, ComboBoxVariogramType.ItemIndex);
      WriteBool(Name, CheckBoxStandardized.Name, CheckBoxStandardized.Checked);
    finally
      IniFile.Free;
    end;
end;

procedure TfmAnalyseVariograms.MakeVariogram;
var
  TablePoints, TableVariogram, TableVariogramData: TTable;
  TableVariogramName, TableVariogramDataName: TFileName;
  PointsArr:    TCoordinateArray;
  Variogram:    TExpVariogram;
  i, j, id, il: integer;
begin
  TablePoints := TTable.Create(Self);
  TablePoints.TableName := InModelName;
  with TablePoints do
  begin
    Open;
    SetLength(PointsArr, RecordCount);
    First;
    j := 0;
    for i := 0 to RecordCount - 1 do
    begin
      if not FieldByName(ActiveAttribute).IsNull then
      begin
        PointsArr[j].X := FieldByName(fldX).AsFloat;
        PointsArr[j].Y := FieldByName(fldY).AsFloat;
        if InModelType = mtPoints3D then
          PointsArr[j].Z := FieldByName(fldZ).AsFloat
        else
          PointsArr[j].Z := 0;
        PointsArr[j].Value := FieldByName(ActiveAttribute).AsFloat;
        Inc(j);
      end;
      Next;
    end;
    SetLength(PointsArr, j);
    Close;
  end; // of with

  TablePoints.Free;

  /// ATTENTION! You should change the case operator body
  /// every time you have changed ComboBoxVariogramType items !!!
  with Variogram do
  begin
    case ComboBoxVariogramType.ItemIndex of
      0: var_type := vtSemivariogram;
      1: var_type := vtCovariance;
      2: var_type := vtCorrelogram;
      3: var_type := vtGeneralRelativeSemivariogram;
      4: var_type := vtPairwiseRelativeSemivariogram;
      5: var_type := vtLogSemivariogram;
      6: var_type := vtSemimadogram;
    end;
    nlag := SpinEditLagsNumber.Value;
    lag_distance := StrToFloat(EditLagSeparationDistance.Text);
    lag_tolerance := StrToFloat(EditLagTolerance.Text);
    ndir := SpinEditDirectionsNumber.Value;
    for i := 0 to ndir - 1 do
      directions[i + 1] := PVariogramDirection(ListBoxDirections.Items.Objects[i])^;

    standardized := CheckBoxStandardized.Checked;
    cross := 0;
    cut   := 0;
  end;

  gamv3(PointsArr, Variogram, ProgressBar);

  TableVariogramName     := PanelOutPath.Caption + EditOutName.Text;
  TableVariogramDataName := TableVariogramName + '_DATA';
  SetLength(PointsArr, 0);
  CreateExpVariogramTables(TableVariogramName);
  TableVariogram     := TTable.Create(Self);
  TableVariogram.TableName := TableVariogramName;
  TableVariogramData := TTable.Create(Self);
  TableVariogramData.TableName := TableVariogramDataName;

  with Variogram do
  begin
    TableVariogram.Open;
    TableVariogramData.Open;
    j := 0;
    for id := 1 to Variogram.ndir do
    begin
      with TableVariogram do
      begin
        Append;
        FieldByName(fldID).Value      := id;
        FieldByName(fldVarType).Value := var_type;
        FieldByName(fldDim3D).Value   := Mode3D;
        FieldByName(fldATTRIBUTE).Value := ActiveAttribute;
        FieldByName(fldAzimuthAngle).Value :=
          RoundTo(directions[id].azimuth, Precision);
        FieldByName(fldAzimuthTolerance).Value :=
          RoundTo(directions[id].azimuth_tolerance, Precision);
        FieldByName(fldAzimuthBandwidth).Value :=
          RoundTo(directions[id].azimuth_bandwidth, Precision);
        FieldByName(fldDipAngle).Value :=
          RoundTo(directions[id].dip, Precision);
        FieldByName(fldDipTolerance).Value :=
          RoundTo(directions[id].dip_tolerance, Precision);
        FieldByName(fldDipBandwidth).Value :=
          RoundTo(directions[id].dip_bandwidth, Precision);
        FieldByName(fldLagsN).Value   := nlag;
        FieldByName(fldLagDistance).Value := RoundTo(lag_distance, Precision);
        FieldByName(fldLagTolerance).Value := RoundTo(lag_tolerance, Precision);
        Post;
      end;
      with TableVariogramData do
        for il := 1 to Variogram.nlag + 2 do
        begin
          Append;
          FieldByName(fldID).Value    := j + 1;
          FieldByName(fldVarID).Value := DataV[j].direction;
          FieldByName(fldAvgSeparationDistance).Value :=
            RoundTo(DataV[j].distance, Precision);
          FieldByName(fldG).Value :=
            RoundTo(Variogram.DataV[j].Value, 2 * Precision);
          FieldByName(fldLagPairsNum).Value := DataV[j].npair;
          FieldByName(fldMeanHead).Value := RoundTo(DataV[j].head_mean, Precision);
          FieldByName(fldMeanTail).Value := RoundTo(DataV[j].tail_mean, Precision);
          FieldByName(fldVarianceHead).Value :=
            RoundTo(DataV[j].head_variance, 2 * Precision);
          FieldByName(fldVarianceTail).Value :=
            RoundTo(DataV[j].tail_variance, 2 * Precision);
          Post;
          Inc(j);
        end;
    end;

    TableVariogram.Close;
    TableVariogramData.Close;
  end; // of with

  SetLength(Variogram.DataV, 0);
  TableVariogram.Free;
  TableVariogramData.Free;
end;

end.
