//----------------------------------------------------------------------------
// The unit of Geoblock, http://sourceforge.net/projects/geoblock
//---------------------------------------------------------------------------
{ Experimental variograms viewer }

unit fViewVariogram;

interface

uses
  System.SysUtils, 
  System.Variants, 
  System.Classes,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.ComCtrls, 
  Vcl.Samples.Spin,
  Vcl.Dialogs,
  Vcl.StdCtrls, 
  Vcl.ExtCtrls, 
  Vcl.Grids,
  VclTee.TeeProcs, 
  VclTee.TeEngine, 
  VclTee.Chart, 
  Vcl.DBGrids, 
  VclTee.Series,
  VclTee.DbChart, 
  VclTee.TeeGDIPlus,

  //DB
  Bde.DBTables,
  Data.DB,

  fInitialDialog,
  cGlobals, 
  uVariograms,
  gnuGettext;

type
  TfmViewVariogram = class(TfmInitialDialog)
    GroupBoxVariogram: TGroupBox;
    TreeViewVariogram: TTreeView;
    PageControlViewMode: TPageControl;
    TabSheetTable: TTabSheet;
    TabSheetGraph: TTabSheet;
    DBChart:    TDBChart;
    DBGrid:     TDBGrid;
    Query:      TQuery;
    DataSource: TDataSource;
    Series1:    TLineSeries;
    procedure TreeViewVariogramClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    CurrentID: integer;
    procedure SelectData(vid: integer);
    procedure UpdateTree;
  public
    VariogramFileName: TFileName;
    DisplayMode: integer;
  end;

var
  fmViewVariogram: TfmViewVariogram;

//=================================================================
implementation
//=================================================================

{$R *.dfm}

procedure TfmViewVariogram.FormShow(Sender: TObject);
begin
  UpdateTree;
  case DisplayMode of
    1: PageControlViewMode.ActivePage := TabSheetTable;
    2: PageControlViewMode.ActivePage := TabSheetGraph;
  end;
  with DBChart.Series[0] do
  begin
    DataSource := Query;
    XValues.ValueSource := fldAvgSeparationDistance;
    YValues.ValueSource := fldG;
  end;
end;

procedure TfmViewVariogram.UpdateTree;
var
  TableVar: TTable;
  root, t: TTreeNode;
  p: PInteger;
  i: integer;
begin
  TreeViewVariogram.Items.Clear;
  TableVar := TTable.Create(self);
  TableVar.TableName := VariogramFileName;
  root     := TreeViewVariogram.Items.AddChild(nil, ExtractFileName(VariogramFileName));
  with TableVar, TreeViewVariogram.Items do
  begin
    Open;
    First;
    i := 1;
    while not EOF do
    begin
      New(p);
      p^ := FieldByName(fldID).Value;
      t  := AddChildObject(root, _('Variogram') + ' ' + IntToStr(i), p);
      AddChild(t, _('Type') + ': ' + VariogramNames[FieldByName(fldVarType).AsInteger]);
      AddChild(t, _('Attribute') + ': ' + FieldByName(fldATTRIBUTE).Value);
      if FieldByName(fldDim3d).Value then
        AddChild(t, _('Dimension') + ': 3D')
      else
        AddChild(t, _('Dimension') + ': 2D');
      AddChild(t, _('Azimuth') + ': ' + FloatToStr(FieldByName(fldAzimuthAngle).Value));
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
      AddChild(t, _('Distance') + ': ' + FloatToStr(FieldByName(fldLagDistance).Value));
      AddChild(t, _('Tolerance') + ': ' +
        FloatToStr(FieldByName(fldLagTolerance).Value));
      Next;
      Inc(i);
    end;
    Close;
  end;
  TableVar.Free;
  root.GetFirstChild.Selected := True;
  CurrentID := -1;
  TreeViewVariogramClick(self);
end;

procedure TfmViewVariogram.SelectData(vid: integer);
begin
  Query.Close;
  with Query.SQL do
  begin
    Clear;
    Add('SELECT * FROM "' + VariogramFileName + '_DATA' + '"');
    Add('WHERE ' + fldVarID + '=' + IntToStr(vid));
  end;
  Query.Open;
end;

procedure TfmViewVariogram.TreeViewVariogramClick(Sender: TObject);
var
  id: integer;
begin
  with TreeViewVariogram do
  begin
    if Selected.Level = 1 then
      id := PInteger(Selected.Data)^
    else if Selected.Level = 2 then
      id := PInteger(Selected.Parent.Data)^
    else
      id := CurrentID;
    if id <> CurrentID then
      SelectData(id);
    CurrentID := id;
  end;
end;

end.
