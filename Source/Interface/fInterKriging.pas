//-----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//-----------------------------------------------------------------------------
{! The dialog for Kriging options }

unit fInterKriging;

interface

uses
  Winapi.Windows,
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
  Vcl.ToolWin,
  VclTee.TeeProcs, 
  VclTee.TeEngine, 
  VclTee.Chart,
  VclTee.DBChart, 
  VclTee.Series,
  VclTee.TeeGDIPlus,
  //DB
  Data.DB, 
  Bde.DBTables,
  
  fInitialDialog,
  uInterpol,
  uVariograms;

type
  TfmInterKriging = class(TfmInitialDialog)
    GroupBoxVariogram: TGroupBox;
    TreeViewVariogram: TTreeView;
    Query:      TQuery;
    QueryData:  TQuery;
    Table:      TTable;
    ButtonNewModel: TButton;
    LabelMaxLag: TLabel;
    EditMax:    TEdit;
    GroupBoxsearchOptions: TGroupBox;
    DBChart:    TDBChart;
    Series2:    TLineSeries;
    Panel1:     TPanel;
    GroupBoxType: TGroupBox;
    RadioButtonSimple: TRadioButton;
    RadioButtonOrdinary: TRadioButton;
    GroupBoxDrift: TGroupBox;
    EditMinPoints: TEdit;
    EditMaxPoints: TEdit;
    LabelPointsNumber: TLabel;
    LabelMin:   TLabel;
    LabelMax:   TLabel;
    GroupBoxSearchType: TGroupBox;
    RadioButtonNormalSearch: TRadioButton;
    RadioButtonOctantSearch: TRadioButton;
    EditPointsPerOctant: TEdit;
    LabelSearchRadius: TLabel;
    EditRadius: TEdit;
    GroupBox1:  TGroupBox;
    CheckBoxLinearX: TCheckBox;
    CheckBoxLinearY: TCheckBox;
    CheckBoxLinearZ: TCheckBox;
    GroupBox2:  TGroupBox;
    CheckBoxQuadraticX: TCheckBox;
    CheckBoxQuadraticY: TCheckBox;
    CheckBoxQuadraticZ: TCheckBox;
    GroupBox3:  TGroupBox;
    CheckBoxQuadraticXY: TCheckBox;
    CheckBoxQuadraticXZ: TCheckBox;
    CheckBoxQuadraticYZ: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EditMaxKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure ButtonNewModelClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure EditMaxChange(Sender: TObject);
    procedure TreeViewVariogramClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    VariogramModelPath: string;
    CurrentTable: string;
    CurrentID:    integer;
    CurrentAttribute: string;
    MinH, MaxH:   double;
    procedure ShowVariogramModels;
    procedure SelectData(tblname: string; vid: integer; attribute: string);
    procedure CalcModel;
  public
    procedure ReadIniFile; override;
    procedure WriteIniFile; override;
  end;

var
  fmInterKriging: TfmInterKriging;

//=========================================================================
implementation
//=========================================================================

{$R *.DFM}

uses
  System.StrUtils,
  System.Math,
  // GB
  uCommon,
  uGlobals,
  gnuGettext,
  fMethodVarioModeller;

type
  TVariogramLocalParams = record
    var_id:    integer;
    attribute: string;
  end;
  PVariogramLocalParams = ^TVariogramLocalParams;

procedure TfmInterKriging.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
  VariogramModelPath := ExpandPath(dirFitVar);
  Table.DatabaseName := VariogramModelPath;
  Query.DatabaseName := VariogramModelPath;
  QueryData.DatabaseName := VariogramModelPath;
  DBChart.Series[0].DataSource := nil;
  CurrentID := -1;
  CurrentTable := '';
  CurrentAttribute := '';
  MinH := 0;
  MaxH := 100;
  ShowVariogramModels;
end;

procedure TfmInterKriging.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;

procedure TfmInterKriging.ShowVariogramModels;
var
  sr:     TSearchRec;
  FileAttrs: integer;
  FilePath, tblname: string;
  r, root, t, q: TTreeNode;
  i, j:   integer;
  tblptr: PString;
  p:      PVariogramLocalParams;
  mf:     string;
begin
  // Filling up Experimental models tree
  TreeViewVariogram.Items.Clear;
  FileAttrs := faAnyFile;
  FilePath := VariogramModelPath + TableMask;
  r := TreeViewVariogram.Items.AddChild(nil, 'Variograms');
  if FindFirst(FilePath, FileAttrs, sr) = 0 then
    repeat
      with Query, TreeViewVariogram.Items do
      begin
        SQL.Clear;
        tblname := LeftStr(sr.Name, Pos('.', sr.Name) - 1);
        SQL.Add('SELECT ' + fldVarID + ', ' + fldATTRIBUTE + ' FROM ' + tblname);
        SQL.Add('GROUP BY ' + fldVarID + ', ' + fldATTRIBUTE);
        Open;
        New(tblptr);
        tblptr^ := tblname;
        root    := TreeViewVariogram.Items.AddChildObject(r, tblname, tblptr);
        First;
        i := 1;
        while not EOF do
        begin
          New(p);
          p^.var_id := FieldByName(fldVarID).Value;
          p^.Attribute := FieldByName(fldATTRIBUTE).Value;
          t := AddChildObject(root, _('Variogram') + ' ' + IntToStr(i), p);
          j := 1;
          with Table do
          begin
            TableName := tblname;
            Filter    := fldAttribute + '=''' + p^.Attribute +
              ''' AND ' + fldVarID + '=' + IntToStr(p^.var_id);
            Filtered  := True;
            Open;
            First;
            while not EOF do
            begin
              q := AddChild(t, _('Structure') + ' ' + IntToStr(j));
              case FieldByName(fldModelFunctionID).Value of
                1: mf := 'Spherical';
                2: mf := 'Exponential';
                3: mf := 'Gaussian';
                4: mf := 'Power';
              end;
              AddChild(q, _('Model function') + ': ' + mf);
              AddChild(q, _('Nugget') + ': ' + FloatToStr(FieldByName(fldNugget).Value));
              AddChild(q, _('Contribution') + ': ' + FloatToStr(
                FieldByName(fldContribution).Value));
              AddChild(q, _('Range') + ': ' + FloatToStr(FieldByName(fldRange).Value));
              AddChild(q, _('Anis1') + ': ' + FloatToStr(FieldByName(fldAnis1).Value));
              AddChild(q, _('Anis2') + ': ' + FloatToStr(FieldByName(fldAnis2).Value));
              AddChild(q, _('Azimuth') + ': ' + FloatToStr(
                FieldByName(fldAzimuthAngle).Value));
              AddChild(q, _('Dip') + ': ' + FloatToStr(FieldByName(fldDipAngle).Value));
              AddChild(q, _('Plunge') + ': ' + FloatToStr(
                FieldByName(fldPlungeAngle).Value));
              Next;
              Inc(j);
            end;
            Close;
            Filtered := False;
          end;
          Next;
          Inc(i);
        end;
        Close;
      end;
    until FindNext(sr) <> 0;
  FindClose(sr);
  if (r.GetFirstChild <> nil) and (r.GetFirstChild.GetFirstChild <> nil) then
    r.GetFirstChild.GetFirstChild.Selected := True;
  TreeViewVariogramClick(TreeViewVariogram);
end;

//-----------------------------------------------------------------------------

procedure TfmInterKriging.ButtonNewModelClick(Sender: TObject);
begin
  with TfmMethodVarioModeller.Create(Self) do
    try
      if ShowModal = mrOk then
        ShowVariogramModels;
    finally
      Free;
    end;
end;

procedure TfmInterKriging.ButtonOKClick(Sender: TObject);
var
  i: integer;
begin
  inherited;
  with KrigingParams do
  begin
    if RadioButtonSimple.Checked then
      k_type := 0;
    if RadioButtonOrdinary.Checked then
      k_type := 1;
    if RadioButtonNormalSearch.Checked then
      search_type := NORMAL_SEARCH;
    if RadioButtonOctantSearch.Checked then
    begin
      search_type := OCTANT_SEARCH;
      points_per_octant := StrToInt(EditPointsPerOctant.Text);
    end;
    min_points := StrToInt(EditMinPoints.Text);
    max_points := StrToInt(EditMaxPoints.Text);
    max_radius := StrToFloat(EditRadius.Text);
    if CheckBoxLinearX.Checked then
      drift[1] := 1
    else
      drift[1] := 0;
    if CheckBoxLinearY.Checked then
      drift[2] := 1
    else
      drift[2] := 0;
    if CheckBoxLinearZ.Checked then
      drift[3] := 1
    else
      drift[3] := 0;
    if CheckBoxQuadraticX.Checked then
      drift[4] := 1
    else
      drift[4] := 0;
    if CheckBoxQuadraticY.Checked then
      drift[5] := 1
    else
      drift[5] := 0;
    if CheckBoxQuadraticZ.Checked then
      drift[6] := 1
    else
      drift[6] := 0;
    if CheckBoxQuadraticXY.Checked then
      drift[7] := 1
    else
      drift[7] := 0;
    if CheckBoxQuadraticXZ.Checked then
      drift[8] := 1
    else
      drift[8] := 0;
    if CheckBoxQuadraticYZ.Checked then
      drift[9] := 1
    else
      drift[9] := 0;
    model.nst_count := QueryData.RecordCount;
    QueryData.First;
    for i := 1 to model.nst_count do
      with QueryData, model.structures[i] do
      begin
        model_type := FieldByName(fldModelFunctionID).Value;
        nugget := FieldByName(fldNugget).Value;
        contribution := FieldByName(fldContribution).Value;
        range  := FieldByName(fldRange).Value;
        anis1  := FieldByName(fldAnis1).Value;
        anis2  := FieldByName(fldAnis2).Value;
        azimuth := FieldByName(fldAzimuthAngle).Value;
        dip    := FieldByName(fldDipAngle).Value;
        plunge := FieldByName(fldPlungeAngle).Value;
        Next;
      end;
  end;
end;

//-----------------------------------------------------------------------------

procedure TfmInterKriging.CalcModel;
var
  i: integer;
  m: TVariogramModel;
  h, delta: double;
begin
  m.nst_count := QueryData.RecordCount;
  for i := 1 to m.nst_count do
    with QueryData, m.structures[i] do
    begin
      model_type := FieldByName(fldModelFunctionID).Value;
      nugget := FieldByName(fldNugget).Value;
      contribution := FieldByName(fldContribution).Value;
      range  := FieldByName(fldRange).Value;
      anis1  := FieldByName(fldAnis1).Value;
      anis2  := FieldByName(fldAnis2).Value;
      azimuth := FieldByName(fldAzimuthAngle).Value;
      dip    := FieldByName(fldDipAngle).Value;
      plunge := FieldByName(fldPlungeAngle).Value;
    end;

  DBChart.Series[0].Clear;
  h     := MinH;
  delta := Abs(MaxH - MinH) / 100;
  with DBChart do
    while h <= MaxH do
    begin
      Series[0].AddXY(h, CalcVariogramModelValue(m, h));
      h := h + delta;
    end;
end;

//-----------------------------------------------------------------------------

procedure TfmInterKriging.SelectData(tblname: string; vid: integer; attribute: string);
begin
  if tblname <> '' then
  begin
    QueryData.Close;
    with QueryData.SQL do
    begin
      Clear;
      Add('SELECT * FROM "' + tblname + '"');
      Add('WHERE (' + fldVarID + '=' + IntToStr(vid) + ')');
      Add('AND (' + fldATTRIBUTE + '= "' + attribute + '")');
    end;
    QueryData.Open;
    CalcModel;
  end;
end;

//-----------------------------------------------------------------------------

procedure TfmInterKriging.TreeViewVariogramClick(Sender: TObject);
var
  id: integer;
  attribute, tblname: string;
  p:  PVariogramLocalParams;
begin
  tblname := CurrentTable;
  id      := CurrentID;
  attribute := CurrentAttribute;
  with TreeViewVariogram do
    if Selected <> nil then
      case Selected.Level of
        2:
        begin
          p  := PVariogramLocalParams(Selected.Data);
          id := p^.var_id;
          attribute := p^.Attribute;
          tblname := PString(Selected.Parent.Data)^;
        end;
        3:
        begin
          p  := PVariogramLocalParams(Selected.Parent.Data);
          id := p^.var_id;
          CurrentAttribute := p^.Attribute;
          tblname := PString(Selected.Parent.Parent.Data)^;
        end;
        4:
        begin
          p  := PVariogramLocalParams(Selected.Parent.Parent.Data);
          id := p^.var_id;
          CurrentAttribute := p^.Attribute;
          tblname := PString(Selected.Parent.Parent.Parent.Data)^;
        end;
      end;
  if (id <> CurrentID) or (tblname <> CurrentTable) or
    (CurrentAttribute <> attribute) then
    SelectData(tblname, id, attribute);
  CurrentTable := tblname;
  CurrentID    := id;
  CurrentAttribute := attribute;
  DBChart.Title.Text.Clear;
  if CurrentID <> -1 then
    DBChart.Title.Text.Add(CurrentTable + ' - ' + _('Variogram') +
      ' ' + IntToStr(CurrentID))
  else
    DBChart.Title.Text.Add(_('No data'));
end;

//-----------------------------------------------------------------------------

procedure TfmInterKriging.EditMaxKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    EditMaxChange(Sender);

end;

//-----------------------------------------------------------------------------

procedure TfmInterKriging.EditMaxChange(Sender: TObject);
var
  ed: TEdit;
begin
  ed := Sender as TEdit;
  if ed = EditMax then
    MaxH := StrToFloat(ed.Text);
  CalcModel;
end;

//-----------------------------------------------------------------------------

procedure TfmInterKriging.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      RadioButtonSimple.Checked   := ReadBool(Name, RadioButtonSimple.Name, False);
      RadioButtonOrdinary.Checked := ReadBool(Name, RadioButtonOrdinary.Name, True);
      RadioButtonNormalSearch.Checked :=
        ReadBool(Name, RadioButtonNormalSearch.Name, True);
      RadioButtonOctantSearch.Checked :=
        ReadBool(Name, RadioButtonOctantSearch.Name, False);

      CheckBoxLinearX.Checked     := ReadBool(Name, CheckBoxLinearX.Name, False);
      CheckBoxLinearY.Checked     := ReadBool(Name, CheckBoxLinearY.Name, False);
      CheckBoxLinearZ.Checked     := ReadBool(Name, CheckBoxLinearZ.Name, False);
      CheckBoxQuadraticX.Checked  := ReadBool(Name, CheckBoxQuadraticX.Name, False);
      CheckBoxQuadraticY.Checked  := ReadBool(Name, CheckBoxQuadraticY.Name, False);
      CheckBoxQuadraticZ.Checked  := ReadBool(Name, CheckBoxQuadraticZ.Name, False);
      CheckBoxQuadraticXY.Checked := ReadBool(Name, CheckBoxQuadraticXY.Name, False);
      CheckBoxQuadraticXZ.Checked := ReadBool(Name, CheckBoxQuadraticXZ.Name, False);
      CheckBoxQuadraticYZ.Checked := ReadBool(Name, CheckBoxQuadraticYZ.Name, False);

      EditMinPoints.Text := ReadString(Name, EditMinPoints.Name, '5');
      EditMaxPoints.Text := ReadString(Name, EditMaxPoints.Name, '20');
      EditPointsPerOctant.Text := ReadString(Name, EditPointsPerOctant.Name, '5');
      EditRadius.Text := ReadString(Name, EditRadius.Name, '100');
      EditMax.Text := ReadString(Name, EditMax.Name, '100');
    finally
      IniFile.Free;
    end;
end;

//-----------------------------------------------------------------------------

procedure TfmInterKriging.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteBool(Name, RadioButtonSimple.Name, RadioButtonSimple.Checked);
      WriteBool(Name, RadioButtonOrdinary.Name, RadioButtonOrdinary.Checked);
      WriteBool(Name, RadioButtonNormalSearch.Name, RadioButtonNormalSearch.Checked);
      WriteBool(Name, RadioButtonOctantSearch.Name, RadioButtonOctantSearch.Checked);

      WriteBool(Name, CheckBoxLinearX.Name, CheckBoxLinearX.Checked);
      WriteBool(Name, CheckBoxLinearY.Name, CheckBoxLinearY.Checked);
      WriteBool(Name, CheckBoxLinearZ.Name, CheckBoxLinearZ.Checked);
      WriteBool(Name, CheckBoxQuadraticX.Name, CheckBoxQuadraticX.Checked);
      WriteBool(Name, CheckBoxQuadraticY.Name, CheckBoxQuadraticY.Checked);
      WriteBool(Name, CheckBoxQuadraticZ.Name, CheckBoxQuadraticZ.Checked);
      WriteBool(Name, CheckBoxQuadraticXY.Name, CheckBoxQuadraticXY.Checked);
      WriteBool(Name, CheckBoxQuadraticXZ.Name, CheckBoxQuadraticXZ.Checked);
      WriteBool(Name, CheckBoxQuadraticYZ.Name, CheckBoxQuadraticYZ.Checked);

      WriteString(Name, EditMinPoints.Name, EditMinPoints.Text);
      WriteString(Name, EditMaxPoints.Name, EditMaxPoints.Text);
      WriteString(Name, EditPointsPerOctant.Name, EditPointsPerOctant.Text);
      WriteString(Name, EditRadius.Name, EditRadius.Text);
      WriteString(Name, EditMax.Name, EditMax.Text);
    finally
      IniFile.Free;
    end;
end;

end.
