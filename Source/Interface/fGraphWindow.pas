//-----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//-----------------------------------------------------------------------------
{! The form to plot data with the help of TTeeChart components}

unit fGraphWindow;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.IniFiles,
  System.ImageList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Menus,
  Vcl.StdCtrls,
  Vcl.CheckLst,
  Vcl.ExtCtrls,
  Vcl.ImgList,
  Vcl.Buttons,
  Vcl.ComCtrls,
  Vcl.ToolWin,
  //DB
  Data.DB,
  Bde.DBTables,
  //TeeChart
  VclTee.TeEngine,
  VclTee.TeeFunci,
  VclTee.Series,
  VclTee.TeeProcs,
  VclTee.Chart,
  VclTee.DbChart,
  VclTee.TeeGDIPlus,

  
  uResStrings,
  uGlobals,
  fInitialForm;

type
  TGraphType = (gtPoints, gtLines, gtRectangles);
  TCurveType = (ctFrequency, ctCumulative, ctResidue);

type
  TfmGraphWindow = class(TfmInitialForm)
    ToolBar:    TToolBar;
    ToolButtonSeparator: TToolButton;
    ToolButtonClose: TToolButton;
    ToolButtonPrint: TToolButton;
    ToolButton1: TToolButton;
    CheckBoxLegend: TCheckBox;
    CheckBoxView3D: TCheckBox;
    ToolButtonProfilogram: TToolButton;
    ToolButtonHistogram: TToolButton;
    SpeedButtonGraphType: TSpeedButton;
    SpeedButtonCurveType: TSpeedButton;
    ImageList:  TImageList;
    Panel:      TPanel;
    LabelAxesX: TLabel;
    LabelAxesY: TLabel;
    CheckListBoxYFields: TCheckListBox;
    ComboBoxXField: TComboBox;
    DBChart:    TDBChart;
    Series1:    TPointSeries;
    TeeFunction1: THighTeeFunction;
    TableGraph: TTable;
    DropdownMenuGraph: TPopupMenu;
    MenuItemLines: TMenuItem;
    MenuItemPoints: TMenuItem;
    MenuItemRectangles: TMenuItem;
    DropdownMenuCurve: TPopupMenu;
    MenuItemFrequency: TMenuItem;
    MenuItemCumulative: TMenuItem;
    MenuItemResidue: TMenuItem;
    Series2:    TAreaSeries;
    procedure FormCreate(Sender: TObject);
    procedure ComboBoxXFieldChange(Sender: TObject);
    procedure ToolButtonCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ToolButtonPrintClick(Sender: TObject);
    procedure SpeedButtonGraphTypeClick(Sender: TObject);
    procedure SpeedButtonCurveTypeClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure MenuItemGraphTypeClick(Sender: TObject);
    procedure CheckBoxLegendClick(Sender: TObject);
    procedure CheckBoxView3DClick(Sender: TObject);
    procedure ToolButtonProfilogramClick(Sender: TObject);
    procedure ToolButtonHistogramClick(Sender: TObject);
    procedure CheckListBoxYFieldsClick(Sender: TObject);
  private
    FGraphType: TGraphType;
    FCurveType: TCurveType;
    procedure GetWorkFields;
    procedure ReadIniFile;
    procedure WriteIniFile;
  public
    ModelType: integer;
    procedure CopyToClipBoard;
    procedure OpenTable(ATableName: string);
    procedure SetSeries;
    property GraphType: TGraphType Read FGraphType Write FGraphType;
    property CurveType: TCurveType Read FCurveType Write FCurveType;
    procedure UnCheckedItems(Menu: TMenu);

  end;

var
  fmGraphWindow: TfmGraphWindow;

//=========================================================================
implementation
//=========================================================================

uses
  fGeoblock;

{$R *.dfm}

procedure TfmGraphWindow.ComboBoxXFieldChange(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    SetSeries;
    DBChart.Update;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfmGraphWindow.ToolButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmGraphWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  if fmGeoblock.MDIChildCount = 1 then
  begin
    fmGeoblock.EnableFileItems(False);
    fmGeoblock.EnableMapItems(mtAll, False);
  end;
  WriteIniFile;
  inherited;
end;

procedure TfmGraphWindow.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
end;

procedure TfmGraphWindow.ToolButtonPrintClick(Sender: TObject);
begin
  if MessageDlg(LoadResString(@rsPrint) + ' ?', mtConfirmation,
    [mbYes, mbNo], 0) = mrYes then
    DBChart.PrintLandscape;
end;

procedure TfmGraphWindow.SpeedButtonGraphTypeClick(Sender: TObject);
var
  P: TPoint;
begin
  with SpeedButtonGraphType do
  begin
    P := BoundsRect.TopLeft;
    P := Point(P.x, P.y + Height + 2);
  end;
  P := ClientToScreen(P);
  DropdownMenuGraph.Popup(P.x, P.y);
end;

procedure TfmGraphWindow.SpeedButtonCurveTypeClick(Sender: TObject);
var
  P: TPoint;
begin
  with SpeedButtonCurveType do
  begin
    P := BoundsRect.TopLeft;
    P := Point(P.x, P.y + Height + 2);
  end;
  P := ClientToScreen(P);
  DropdownMenuCurve.Popup(P.x, P.y);
end;

procedure TfmGraphWindow.FormActivate(Sender: TObject);
begin
  fmGeoblock.EditCopy.Enabled  := True;
  fmGeoblock.EditPaste.Enabled := True;
end;

procedure TfmGraphWindow.FormDeactivate(Sender: TObject);
begin
  fmGeoblock.EditCopy.Enabled  := False;
  fmGeoblock.EditPaste.Enabled := False;
end;

procedure TfmGraphWindow.CopyToClipboard;
begin
  DBChart.CopyToClipBoardMetafile(True);
end;

procedure TfmGraphWindow.GetWorkFields;
var
  I: integer;
begin
  for I := 0 to TableGraph.FieldCount - 1 do
  begin
    if (TableGraph.Fields[I].FieldName = fldID) or
      (TableGraph.Fields[I].FieldName = fldFROM) or
      (TableGraph.Fields[I].FieldName = fldTO) then
      Continue;
    if (TableGraph.Fields[I].DataType = ftInteger) or
      (TableGraph.Fields[I].DataType = ftFloat) or
      (TableGraph.Fields[I].DataType = ftSmallint) or
      (TableGraph.Fields[I].DataType = ftWord) then
    begin
      CheckListBoxYFields.Items.Add(TableGraph.Fields[I].FieldName);
      ComboBoxXField.Items.Add(TableGraph.Fields[I].FieldName);
    end;
  end;
  CheckListBoxYFields.Checked[0] := True;
  ComboBoxXField.ItemIndex := 0;
  SetSeries;
end;

procedure TfmGraphWindow.OpenTable(ATableName: string);
begin
  DBChart.Legend.LegendStyle := lsSeries;
  Screen.Cursor := crHourGlass;
  try
    TableGraph.TableName := ATableName;
    Caption := TableGraph.TableName;
    DBChart.Title.Text.Add(ATableName);
    TableGraph.Open;
    GetWorkFields;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfmGraphWindow.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      Top    := ReadInteger('Graph Window', 'Top', Top);
      Left   := ReadInteger('Graph Window', 'Left', Left);
      Height := ReadInteger('Graph Window', 'Height', Height);
      Width  := ReadInteger('Graph Window', 'Width', Width);
      // DBChart.Title.Text := ReadInteger();
    finally
      IniFile.Free;
    end;
end;

procedure TfmGraphWindow.SetSeries;
var
  I, N: integer;
  ChartSeries: TChartSeries;
begin
  ChartSeries := nil;
  N := DBChart.SeriesCount - 1;
  for I := 0 to N do
    DBChart.Series[0].Free;
  for I := 0 to CheckListBoxYFields.Items.Count - 1 do
  begin
    if not CheckListBoxYFields.Checked[I] then
      Continue;
    case GraphType of
      gtLines:
        ChartSeries := TLineSeries.Create(Self);
      gtRectangles:
        ChartSeries := TBarSeries.Create(Self);
      gtPoints:
      begin
        ChartSeries := TPointSeries.Create(Self);
        (ChartSeries as TPointSeries).Pointer.Style := psCircle;
      end;
    end;
    ChartSeries.Marks.Visible := False;
    with ChartSeries do
    begin
      ParentChart := DBChart;
      DataSource  := TableGraph;

      if ComboBoxXField.ItemIndex > 0 then
        XValues.ValueSource := ComboBoxXField.Text;

      YValues.ValueSource := CheckListBoxYFields.Items.Strings[I];
      Title := CheckListBoxYFields.Items.Strings[I];
    end;
  end;

  // ChartSeries.XValues.ValueSource := CheckListBoxXFields.Items[CheckListBoxXFields.ItemIndex];

  if ComboBoxXField.ItemIndex = 0 then
    DBChart.BottomAxis.Title.Caption := LoadResString(@rsRecordNumber)
  else if ChartSeries <> nil then
    DBChart.BottomAxis.Title.Caption := ChartSeries.XValues.ValueSource;
  DBChart.LeftAxis.Title.Caption := ChartSeries.YValues.ValueSource;
end;

procedure TfmGraphWindow.MenuItemGraphTypeClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  case TMenuItem(Sender).GetParentMenu.Tag of
    0:
      GraphType := TGraphType(TMenuItem(Sender).Tag);
    1:
      CurveType := TCurveType(TMenuItem(Sender).Tag);
  end;
  UnCheckedItems(TMenuItem(Sender).GetParentMenu);
  TMenuItem(Sender).Checked := True;
  SetSeries;
  DBChart.Update;
  Screen.Cursor := crDefault;
end;

procedure TfmGraphWindow.CheckBoxLegendClick(Sender: TObject);
begin
  if CheckBoxLegend.Checked then
    DBChart.Legend.Visible := True
  else
    DBChart.Legend.Visible := False;
  DBChart.UpDate;
end;

procedure TfmGraphWindow.CheckBoxView3DClick(Sender: TObject);
begin
  if CheckBoxView3D.Checked then
    DBChart.View3D := True
  else
    DBChart.View3D := False;
  DBChart.UpDate;
end;

procedure TfmGraphWindow.ToolButtonProfilogramClick(Sender: TObject);
begin
  CheckListBoxYFields.Enabled := True;
end;

procedure TfmGraphWindow.ToolButtonHistogramClick(Sender: TObject);
begin
  CheckListBoxYFields.Enabled := False;
end;

procedure TfmGraphWindow.CheckListBoxYFieldsClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    SetSeries;
    DBChart.Update;
  except
    CheckListBoxYFields.Checked[0] := True;
  end;
  Screen.Cursor := crDefault;
end;

procedure TfmGraphWindow.UnCheckedItems(Menu: TMenu);
var
  I: integer;
begin
  for I := 0 to Menu.Items.Count - 1 do
    Menu.Items[I].Checked := False;

end;

procedure TfmGraphWindow.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteInteger('Graph Window', 'Top', Top);
      WriteInteger('Graph Window', 'Left', Left);
      WriteInteger('Graph Window', 'Height', Height);
      WriteInteger('Graph Window', 'Width', Width);
    finally
      IniFile.Free;
    end;
end;

end.
