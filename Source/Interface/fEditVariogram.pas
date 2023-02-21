//----------------------------------------------------------------------------
// The unit of Geoblock, http://sourceforge.net/projects/geoblock
//---------------------------------------------------------------------------
 (* Fitting the variogram models *)

unit fEditVariogram;

interface

uses
  Winapi.Windows, 
  Winapi.Messages, 
  System.SysUtils, 
  System.Variants, 
  System.Classes, 
  System.StrUtils, 
  System.Math,
  System.IniFiles,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms,
  Vcl.Dialogs, 
  Vcl.StdCtrls, 
  Vcl.ExtCtrls, 
  Vcl.Samples.Spin, 
  Vcl.ComCtrls,
  VclTee.TeeGDIPlus,
//  VirtualTrees,
  TeeProcs,
  TeEngine,
  Chart,
  DbChart,
  Series,
  //DB
  Data.DB,
  Bde.DBTables,
  
  fInitialDialog,
  cInterpol,
  uVariograms;

type
  TfmEditVariogram = class(TfmInitialDialog)
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
    procedure ModelPropertyKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure ButtonNewVariogramClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ModelPropertyChange(Sender: TObject);
    procedure SpinEditNestedStructuresNumberChange(Sender: TObject);
    procedure ListBoxNestedStructuresClick(Sender: TObject);
    procedure TreeViewVariogramClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  end;

var
  fmEditVariogram: TfmEditVariogram;

implementation

uses
  uCommon, 
  cGlobals, 
  gnuGettext,
  fAnalyseVariograms,
  fViewVariogram;

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

procedure TfmEditVariogram.FormCreate(Sender: TObject);
begin
  inherited;
  ExpVariogramPath := ExpandPath(dirExpVar);
  UpdateTree;
  with DBChart.Series[0] do
  begin
    DataSource := Query;
    XValues.ValueSource := fldAvgSeparationDistance;
    YValues.ValueSource := fldValue;
  end;
  DBChart.Series[1].DataSource := nil;
  DbChart.Series[1].Title := 'Model';
  ComboBoxModelFunction.ItemIndex := 0;
  SpinEditNestedStructuresNumber.MaxValue := 4;    //MAXNST
  SpinEditNestedStructuresNumber.Value := 1;
  NstCountOld := 0;
  // Setting up variogram model parameters
  ListBoxNestedStructures.Items.Clear;
  SpinEditNestedStructuresNumberChange(SpinEditNestedStructuresNumber);
end;

//-----------------------------------------------------------------------------

procedure TfmEditVariogram.ButtonNewVariogramClick(Sender: TObject);
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

procedure TfmEditVariogram.ButtonOKClick(Sender: TObject);
var
  tblname: string;
  TableModel: TTable;
  i: integer;
  p: PVariogramModelStructure;
begin
  if (CurrentTable <> '') and (CurrentID > 0) then
  begin
    tblname := ExpandPath(dirFitVar) + CurrentTable;
    ///CreateModelVariogramFiles(tblname);
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

procedure TfmEditVariogram.UpdateTree;
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
  r := TreeViewVariogram.Items.AddChild(nil, 'Variograms');
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
  r.GetFirstChild.GetFirstChild.Selected := True;
  TreeViewVariogramClick(self);
end;

//-----------------------------------------------------------------------------

procedure TfmEditVariogram.SetModelData(p: PVariogramModelStructure);
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

procedure TfmEditVariogram.SelectData(tblname: string; vid: integer);
begin
  DBChart.Series[0].Title := tblname + ': ' + IntToStr(vid);
  if tblname <> '' then
  begin
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

procedure TfmEditVariogram.CalcModel;
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

procedure TfmEditVariogram.ModelPropertyKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    ModelPropertyChange(Sender);
end;

//-----------------------------------------------------------------------------

procedure TfmEditVariogram.ModelPropertyChange(Sender: TObject);
var
  ed: TEdit;
  i:  integer;
  p:  PVariogramModelStructure;
begin
  i := ListBoxNestedStructures.ItemIndex;
  p := PVariogramModelStructure(ListBoxNestedStructures.Items.Objects[i]);
  if Sender is TComboBox then
    p^.model_type := (Sender as TComboBox).ItemIndex + 1
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

procedure TfmEditVariogram.SpinEditNestedStructuresNumberChange(Sender: TObject);
var
  NstCount, i: integer;
  p: PVariogramModelStructure;
begin
  NstCount := (Sender as TSpinEdit).Value;
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
end;

//-----------------------------------------------------------------------------

procedure TfmEditVariogram.TreeViewVariogramClick(Sender: TObject);
var
  id: integer;
  tblname: string;
  p: PVariogramParams;
begin
  tblname := CurrentTable;
  id      := CurrentID;
  with TreeViewVariogram do
    case Selected.Level of
      1: tblname := PString(Selected.Data)^;
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
    end;
  CurrentTable := tblname;
  CurrentID    := id;
end;

//-----------------------------------------------------------------------------

procedure TfmEditVariogram.ListBoxNestedStructuresClick(Sender: TObject);
var
  i: integer;
  d: TVariogramModelStructure;
begin
  i := ListBoxNestedStructures.ItemIndex;
  d := PVariogramModelStructure(ListBoxNestedStructures.Items.Objects[i])^;
  ComboBoxModelFunction.ItemIndex := d.model_type - 1;
  EditNugget.Text := FloatToStr(d.nugget);
  EditContribution.Text := FloatToStr(d.contribution);
  EditRange.Text := FloatToStr(d.range);
  EditAnis1.Text := FloatToStr(d.anis1);
  EditAnis2.Text := FloatToStr(d.anis2);
  EditAzimuth.Text := FloatToStr(d.azimuth);
  EditDip.Text := FloatToStr(d.dip);
  EditPlunge.Text := FloatToStr(d.plunge);
end;

end.
