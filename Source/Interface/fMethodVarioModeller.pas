//----------------------------------------------------------------------------
// The unit of Geoblock, http://sourceforge.net/projects/geoblock
 //---------------------------------------------------------------------------
{ The unit for fitting the variogram models}

unit fMethodVarioModeller;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.IniFiles,
  System.StrUtils,
  System.Math,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.Samples.Spin,

  VclTee.TeeProcs,
  VclTee.TeEngine,
  VclTee.Chart,
  VclTee.DbChart,
  VclTee.Series,
  VclTee.TeeGDIPlus,

  //DB
  Data.DB,
  Bde.DBTables,

  fInitialDialog,
  fMethodDialog,
  fAnalyseVariograms,
  fViewVariogram,

  GBGeometry,
  cInterpol,
  uVariograms;

type
  TfmMethodVarioModeller = class(TfmInitialDialog)
    LabelExpVar: TLabel;
    GroupBoxVariogramModel: TGroupBox;
    ComboBoxModelFunction: TComboBox;
    LabelModelFunction: TLabel;
    EditNugget: TEdit;
    EditContribution: TEdit;
    EditRange: TEdit;
    EditAzimuth: TEdit;
    EditDip: TEdit;
    EditPlunge: TEdit;
    EditAnis1: TEdit;
    EditAnis2: TEdit;
    LabelNugget: TLabel;
    LabelContribution: TLabel;
    LabelAnis1: TLabel;
    LabelAnis2: TLabel;
    LabelAzimuth: TLabel;
    LabelDip: TLabel;
    LabelPlunge: TLabel;
    LabelRange: TLabel;
    DBChart: TDBChart;
    GroupBoxExpVariogram: TGroupBox;
    GroupBoxNestedStructures: TGroupBox;
    ListBoxNestedStructures: TListBox;
    LabelNestedStructuesNumber: TLabel;
    SpinEditNestedStructuresNumber: TSpinEdit;
    TreeViewVariogram: TTreeView;
    Query:   TQuery;
    Series2: TLineSeries;
    Series1: TLineSeries;
    ButtonNewVariogram: TButton;
    ButtonAutoFit: TButton;
    procedure ButtonAutoFitClick(Sender: TObject);
    procedure ModelPropertyKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure ButtonNewVariogramClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ModelPropertyChange(Sender: TObject);
    procedure SpinEditNestedStructuresNumberChange(Sender: TObject);
    procedure ListBoxNestedStructuresClick(Sender: TObject);
    procedure TreeViewVariogramClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    ExpVariogramPath: string;
    CurrentTable: string;
    CurrentID:    integer;
    CurrentDim3D: boolean;
    CurrentAttribute: string;
    NstCountOld:  integer;
    procedure UpdateTree;
    procedure SetModelData(p: PVariogramModelStructure);
    procedure SelectData(tblname: string; vid: integer);
    procedure CalcModel;
    function GoalFunction(m: TVariogramModel): double;
  public
    procedure ReadIniFile; override;
    procedure WriteIniFile; override;
  end;

var
  fmMethodVarioModeller: TfmMethodVarioModeller;

//=========================================================================
implementation
//=========================================================================

uses
  cGlobals,
  uCommon,
  cProfuns,
  uFileCreator,
  gnuGettext;

{$R *.dfm}

type
  PString = ^string;

  TVariogramParams = record
    id:    integer;
    Dim3D: boolean;
    Attribute: string;
  end;
  PVariogramParams = ^TVariogramParams;

//-----------------------------------------------------------------------------

procedure TfmMethodVarioModeller.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      SpinEditNestedStructuresNumber.Value :=
        ReadInteger(Name, SpinEditNestedStructuresNumber.Name, 0);
      ComboBoxModelFunction.ItemIndex :=
        ReadInteger(Name, ComboBoxModelFunction.Name, 0);
      EditNugget.Text := ReadString(Name, EditNugget.Name, EditNugget.Name);
    finally
      IniFile.Free;
    end;
end;


procedure TfmMethodVarioModeller.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteString(Name, EditNugget.Name, EditNugget.Text);
      WriteInteger(Name, ComboBoxModelFunction.Name, ComboBoxModelFunction.ItemIndex);
      WriteInteger(Name, SpinEditNestedStructuresNumber.Name,
        SpinEditNestedStructuresNumber.Value);
    finally
      IniFile.Free;
    end;

end;

procedure TfmMethodVarioModeller.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;

procedure TfmMethodVarioModeller.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
  ExpVariogramPath := ExpandPath(dirExpVar);
  UpdateTree;
  with DBChart.Series[0] do
  begin
    DataSource := Query;
    XValues.ValueSource := fldAvgSeparationDistance;
    YValues.ValueSource := fldG;
  end;
  DBChart.Series[1].DataSource := nil;
  DbChart.Series[1].Title := _('Model');
  ComboBoxModelFunction.ItemIndex := 0;
  SpinEditNestedStructuresNumber.MaxValue := 4;    //MAXNST
  SpinEditNestedStructuresNumber.Value := 1;
  NstCountOld := 0;
  // Setting up variogram model parameters
  ListBoxNestedStructures.Items.Clear;
  SpinEditNestedStructuresNumberChange(SpinEditNestedStructuresNumber);
end;

//-----------------------------------------------------------------------------

function TfmMethodVarioModeller.GoalFunction(m: TVariogramModel): double;
var
  a, b, i: integer;
  x, y, s: double;
begin
  a := DBChart.Series[0].FirstValueIndex;
  b := DBChart.Series[0].LastValueIndex;
  s := 0;
  if a <> -1 then
    with DBChart do
      for i := a to b do
      begin
        x := Series[0].XValue[i];
        y := Series[0].YValue[i];
        s := s + sqr(CalcVariogramModelValue(m, x) - y);
      end;
  Result := s;
end;

//-----------------------------------------------------------------------------

procedure TfmMethodVarioModeller.ButtonAutoFitClick(Sender: TObject);
const
  delta = 0.0001;
  alpha = 0.01;
var
  m:    TVariogramModel;
  n1, n2, c1, c2, a1, a2, d_n, d_c, d_a: extended;
  n, j, a, b: integer;
  old_g, g: double;
  f, ft, ftf, neg_ftf, r: TMatrix2D;
  y, p: TVectorD;

begin
  m.nst_count := 1;
  with ListBoxNestedStructures do
    m.structures[1] := PVariogramModelStructure(Items.Objects[ItemIndex])^;

  a := DBChart.Series[0].FirstValueIndex;
  b := DBChart.Series[0].LastValueIndex;

  if a <> -1 then
  begin
    with m.structures[1] do
    begin
      if model_type <> 4 then
      begin
        nugget := 0;
        contribution := 1;
        range  := DBChart.Series[0].YValues[b] / 2;
        n      := 0;
        g      := 1e10;
        repeat
          old_g  := g;
          n1     := GoalFunction(m);
          nugget := nugget + delta;
          n2     := GoalFunction(m);
          nugget := nugget - delta;
          c1     := n1;
          contribution := contribution + delta;
          c2     := GoalFunction(m);
          contribution := contribution - delta;
          a1     := n1;
          range  := range + delta;
          a2     := GoalFunction(m);
          range  := range - delta;
          d_n    := (n2 - n1) / delta;
          d_c    := (c2 - c1) / delta;
          d_a    := (a2 - a1) / delta;
          nugget := nugget - alpha * d_n;
          contribution := contribution - alpha * d_c;
          range  := range - alpha * d_a;
          Inc(n);
          g := n1;
        until (n > 10000) or (abs(g - old_g) < delta) or ((g - old_g) > 1000);
      end
      else
      begin
        if (range <= 0) or (range > 2) then
          range := 1;
        CreateMatrix(f, b - a + 1, 2);
        SetLength(y, b - a + 2);
        with DBChart.Series[0] do
          for j := 1 to b - a + 1 do
          begin
            f[j, 1] := 1;
            f[j, 2] := Power(XValues[a + j - 1], range);
            y[j]    := YValues[a + j - 1];
          end;
        ft  := TransposeMatrix(f, b - a + 1, 2);
        ftf := MulMatrices(ft, f, 2, b - a + 1, 2);
        FreeMatrix(f);
        neg_ftf := InverseMatrix(ftf, 2);
        r := MulMatrices(neg_ftf, ft, 2, 2, b - a + 1);
        FreeMatrix(ftf);
        FreeMatrix(neg_ftf);
        p := MulMatrixVector(r, y, 2, b - a + 1);
        FreeMatrix(r);
        nugget := p[1];
        contribution := p[2];
        SetLength(p, 0);
        SetLength(y, 0);
      end;
      EditNugget.Text := FloatToStr(RoundTo(nugget, Precision));
      EditContribution.Text := FloattoStr(RoundTo(contribution, Precision));
      EditRange.Text  := FloatToStr(RoundTo(range, Precision));
    end;

    with ListBoxNestedStructures do
      PVariogramModelStructure(Items.Objects[ItemIndex])^ := m.structures[1];
    CalcModel;
  end;
end;

//-----------------------------------------------------------------------------

procedure TfmMethodVarioModeller.ButtonNewVariogramClick(Sender: TObject);
var
  dm: integer;
begin
  with TfmAnalyseVariograms.Create(Self) do
    try
      if ShowModal = mrOk then
      begin
        Hide;
        dm := ToolBarShowAs.Tag;
        with TfmViewVariogram.Create(self) do
          try
            VariogramFileName := OutModelName;
            DisplayMode := dm;  // 1 = table view, 2 = graph view
            ShowModal;
          finally
            Free;
          end;
        UpdateTree;
      end;
    finally
      Free;
    end;
end;

//-----------------------------------------------------------------------------

procedure TfmMethodVarioModeller.ButtonOKClick(Sender: TObject);
var
  tblname: string;
  TableModel: TTable;
  i: integer;
  p: PVariogramModelStructure;
begin
  if (CurrentTable <> '') and (CurrentID > 0) then
  begin
    tblname := ExpandPath(dirFitVar) + CurrentTable;
    CreateModelVariogramTables(tblname);
    TableModel := TTable.Create(self);
    TableModel.TableName := tblname;
    with TableModel, ListBoxNestedStructures do
    begin
      Open;
      for i := 0 to SpinEditNestedStructuresNumber.Value - 1 do
      begin
        Append;
        p := PVariogramModelStructure(ListBoxNestedStructures.Items.Objects[i]);
        FieldByName(fldID).Value := i + 1;
        FieldByName(fldVarID).Value := CurrentID;
        FieldByName(fldDim3D).Value := CurrentDim3D;
        FieldByName(fldATTRIBUTE).Value := CurrentAttribute;
        FieldByName(fldModelFunctionID).Value := p^.model_type;
        FieldByName(fldNugget).Value := p^.nugget;
        FieldByName(fldContribution).Value := p^.contribution;
        FieldByName(fldRange).Value := p^.range;
        FieldByName(fldAnis1).Value := p^.anis1;
        FieldByName(fldAnis2).Value := p^.anis2;
        FieldByName(fldAzimuthAngle).Value := p^.azimuth;
        FieldByName(fldDipAngle).Value := p^.dip;
        FieldByName(fldPlungeAngle).Value := p^.plunge;
        Post;
      end;
      Close;
    end;
  end;
end;

//-----------------------------------------------------------------------------

procedure TfmMethodVarioModeller.UpdateTree;
var
  sr:     TSearchRec;
  FileAttrs: integer;
  FilePath, tblname: string;
  TableVar: TTable;
  r, root, t: TTreeNode;
  p:      PVariogramParams;
  i:      integer;
  tblptr: PString;
begin
  TableVar := TTable.Create(self);
  TreeViewVariogram.Items.Clear;
  FileAttrs := faAnyFile;
  FilePath := ExpVariogramPath + TableMask;
  r := TreeViewVariogram.Items.AddChild(nil, _('Variograms'));
  if FindFirst(FilePath, FileAttrs, sr) = 0 then
  begin
    repeat
      TableVar.TableName := ExpVariogramPath + sr.Name;
      with TableVar, TreeViewVariogram.Items do
      begin
        Open;
        if FindField(fldVarType) <> nil then
        begin
          tblname := LeftStr(sr.Name, Pos('.', sr.Name) - 1);
          New(tblptr);
          tblptr^ := tblname;
          root    := TreeViewVariogram.Items.AddChildObject(r, tblname, tblptr);
          First;
          i := 1;
          while not EOF do
          begin
            New(p);
            p^.id := FieldByName(fldID).Value;
            p^.Dim3D := FieldByName(fldDim3d).Value;
            p^.Attribute := FieldByName(fldATTRIBUTE).Value;
            t := AddChildObject(root, _('Variogram') + ' ' + IntToStr(i), p);
            AddChild(t, _('Type') + ': ' +
              VariogramNames[FieldByName(fldVarType).AsInteger]);
            AddChild(t, _('Attribute') + ': ' + FieldByName(fldATTRIBUTE).Value);
            if FieldByName(fldDim3d).Value then
              AddChild(t, _('Dimension') + ': 3D')
            else
              AddChild(t, _('Dimension') + ': 2D');
            AddChild(t, _('Azimuth') + ': ' +
              FloatToStr(FieldByName(fldAzimuthAngle).Value));
            AddChild(t, _('Azimuth tolerance') + ': ' +
              FloatToStr(FieldByName(fldAzimuthTolerance).Value));
            AddChild(t, _('Arimuth bandwidth') + ': ' +
              FloatToStr(FieldByName(fldAzimuthBandwidth).Value));
            AddChild(t, _('Dip') + ': ' + FloatToStr(FieldByName(fldDipAngle).Value));
            AddChild(t, _('Dip tolerance') + ': ' +
              FloatToStr(FieldByName(fldDipTolerance).Value));
            AddChild(t, _('Dip bandwidth') + ': ' +
              FloatToStr(FieldByName(fldDipBandwidth).Value));
            AddChild(t, _('Lags') + ': ' + FloatToStr(FieldByName(fldLagsN).Value));
            AddChild(t, _('Distance') + ': ' +
              FloatToStr(FieldByName(fldLagDistance).Value));
            AddChild(t, _('Tolerance') + ': ' +
              FloatToStr(FieldByName(fldLagTolerance).Value));
            Next;
            Inc(i);
          end;
        end;
        Close;
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
  TableVar.Free;
  CurrentID    := -1;
  CurrentTable := '';
  if (r.GetFirstChild <> nil) and (r.GetFirstChild.GetFirstChild <> nil) then
    r.GetFirstChild.GetFirstChild.Selected := True;
  TreeViewVariogramClick(self);
end;

//-----------------------------------------------------------------------------

procedure TfmMethodVarioModeller.SetModelData(p: PVariogramModelStructure);
begin
  with p^ do
  begin
    model_type := 1;  // spherical model
    nugget := 0;
    contribution := 0;
    range  := 1;
    anis1  := 1;
    anis2  := 1;
    azimuth := 0;
    dip    := 0;
    plunge := 0;
  end;
end;

//-----------------------------------------------------------------------------

procedure TfmMethodVarioModeller.SelectData(tblname: string; vid: integer);
begin
  if tblname <> '' then
  begin
    DBChart.Series[0].Title := tblname + ': ' + IntToStr(vid);
    Query.Close;
    with Query.SQL do
    begin
      Clear;
      Add('SELECT * FROM "' + ExpVariogramPath + tblname + '_DATA' + '"');
      Add('WHERE ' + fldVarID + '=' + IntToStr(vid));
    end;
    Query.Open;
  end;
end;

//-----------------------------------------------------------------------------

procedure TfmMethodVarioModeller.CalcModel;
var
  i, a, b: integer;
  m: TVariogramModel;
  x: double;
begin
  m.nst_count := SpinEditNestedStructuresNumber.Value;
  for i := 1 to m.nst_count do
    m.structures[i] :=
      PVariogramModelStructure(ListBoxNestedStructures.Items.Objects[i - 1])^;
  DBChart.Series[1].Clear;
  a := DBChart.Series[0].FirstValueIndex;
  b := DBChart.Series[0].LastValueIndex;
  if a <> -1 then
    with DBChart do
      for i := a to b do
      begin
        x := Series[0].XValue[i];
        Series[1].AddXY(x, CalcVariogramModelValue(m, x));
      end;
end;

//-----------------------------------------------------------------------------

procedure TfmMethodVarioModeller.ModelPropertyKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    ModelPropertyChange(Sender);
end;


//-----------------------------------------------------------------------------

procedure TfmMethodVarioModeller.ModelPropertyChange(Sender: TObject);
var
  ed: TEdit;
  i:  integer;
  p:  PVariogramModelStructure;
begin
  i := ListBoxNestedStructures.ItemIndex;
  p := PVariogramModelStructure(ListBoxNestedStructures.Items.Objects[i]);
  if Sender is TComboBox then
  begin
    p^.model_type := (Sender as TComboBox).ItemIndex + 1;
    CalcModel;
  end
  else
  begin
    ed := Sender as TEdit;
    if ed = EditAnis1 then
      p^.anis1 := StrToFloat(ed.Text)
    else if ed = EditAnis2 then
      p^.anis2 := StrToFloat(ed.Text)
    else if ed = EditAzimuth then
      p^.azimuth := StrToFloat(ed.Text)
    else if ed = EditDip then
      p^.dip := StrToFloat(ed.Text)
    else if ed = EditPlunge then
      p^.plunge := StrToFloat(ed.Text)
    else
    begin
      if ed = EditNugget then
        p^.nugget := StrToFloat(ed.Text);
      if ed = EditContribution then
        p^.contribution := StrToFloat(ed.Text);
      if ed = EditRange then
        p^.range := StrToFloat(ed.Text);
      CalcModel;
    end;
  end;
end;

//-----------------------------------------------------------------------------

procedure TfmMethodVarioModeller.SpinEditNestedStructuresNumberChange(Sender: TObject);
var
  NstCount, i: integer;
  p: PVariogramModelStructure;
begin
  NstCount := SpinEditNestedStructuresNumber.Value;
  with ListBoxNestedStructures do
  begin
    if NstCount > NstCountOld then
      for i := NstCountOld to NstCount - 1 do
      begin
        New(p);
        SetModelData(p);
        AddItem(_('Structure') + ' ' + IntToStr(i + 1), TObject(p));
      end
    else if NstCount < NstCountOld then
      for i := NstCount to NstCountOld do
        Items.Delete(i);
    if ItemIndex < 0 then
      ItemIndex := 0;
  end;
  ListBoxNestedStructuresClick(self);
  CalcModel;
  NstCountOld := NstCount;
  ButtonAutoFit.Enabled := SpinEditNestedStructuresNumber.Value = 1;
end;

//-----------------------------------------------------------------------------

procedure TfmMethodVarioModeller.TreeViewVariogramClick(Sender: TObject);
var
  id: integer;
  tblName: string;
  p: PVariogramParams;
begin
  tblname := CurrentTable;
  id      := CurrentID;
  with TreeViewVariogram do
    if Selected <> nil then
      case Selected.Level of
        1: tblName := PString(Selected.Data)^;
        2:
        begin
          p  := PVariogramParams(Selected.Data);
          id := p^.id;
          CurrentDim3D := p^.Dim3D;
          CurrentAttribute := p^.Attribute;
          tblname := PString(Selected.Parent.Data)^;
          if (id <> CurrentID) or (tblname <> CurrentTable) then
            SelectData(tblname, id);
        end;
        3:
        begin
          p  := PVariogramParams(Selected.Parent.Data);
          id := p^.id;
          CurrentDim3D := p^.Dim3D;
          CurrentAttribute := p^.Attribute;
          tblname := PString(Selected.Parent.Parent.Data)^;
        end;
      end
    else
      DBChart.Series[0].Title := _('No data');
  ;
  CurrentTable := tblname;
  CurrentID    := id;
end;

//-----------------------------------------------------------------------------

procedure TfmMethodVarioModeller.ListBoxNestedStructuresClick(Sender: TObject);
var
  I: integer;
  d: TVariogramModelStructure;
begin
  I := ListBoxNestedStructures.ItemIndex;
  if I = 0 then
    EditNugget.Enabled := True
  else
    EditNugget.Enabled := False;
  d := PVariogramModelStructure(ListBoxNestedStructures.Items.Objects[i])^;
  ComboBoxModelFunction.ItemIndex := d.model_type - 1;
  if I = 0 then
    EditNugget.Text := FloatToStr(d.nugget);
  EditContribution.Text := FloatToStr(d.contribution);
  EditRange.Text   := FloatToStr(d.range);
  EditAnis1.Text   := FloatToStr(d.anis1);
  EditAnis2.Text   := FloatToStr(d.anis2);
  EditAzimuth.Text := FloatToStr(d.azimuth);
  EditDip.Text     := FloatToStr(d.dip);
  EditPlunge.Text  := FloatToStr(d.plunge);
end;

end.
