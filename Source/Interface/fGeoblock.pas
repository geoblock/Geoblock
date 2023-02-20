 //----------------------------------------------------------------------------
 // The unit of Geoblock, http://sourceforge.net/projects/geoblock
 // Copyright (c) 1997-2018 Getos Ltd.
 // The contents of this file are subject to the Mozilla Public License
 // Version 2.0 (the "License"); you may not use this file except in compliance
 // with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/

 // Software distributed under the License is distributed on an "AS IS" basis,
 // WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 // the specific language governing rights and limitations under the License.
 // The initial developer of the original code is Getos Ltd., Copyright (c) 1997.
 // Portions created by contributors are documented in a history log
 // and Copyright (c) of these contributors. All Rights Reserved.
 //---------------------------------------------------------------------------
 {! The Main form with menus, panels and toolbars }

unit fGeoblock;

interface

uses
  Winapi.Windows, 
  System.IniFiles,
  System.SysUtils, 
  System.Classes, 
  System.Actions,
  System.Win.Registry,
  Vcl.Forms, 
  Vcl.Controls, 
  Vcl.StdCtrls,
  Vcl.Dialogs, 
  Vcl.Buttons,
  Vcl.ComCtrls, 
  Vcl.ExtCtrls, 
  Vcl.ToolWin, 
  Vcl.ActnList, 
  Vcl.ActnCtrls,
  Vcl.Samples.Spin, 
  Vcl.StdActns, 
  Vcl.BandActn, 
  Vcl.ActnMan, 
  Vcl.ActnMenus,
  Vcl.XPStyleActnCtrls, 
  Vcl.StdStyleActnCtrls,
  Vcl.ImgList,
  Vcl.CustomizeDlg,
  Vcl.ActnColorMaps,
  Vcl.Graphics, 
  Vcl.HtmlHelpViewer,

  fInitialForm,
  dbase,
  dDialogs;

/// The Main form
type
  TfmGeoblock = class(TfmInitialForm)
    StatusBar:    TStatusBar;
    ControlBarTop: TControlBar;
    ControlBarBottom: TControlBar;
    ControlBarLeft: TControlBar;
    ControlBarRight: TControlBar;
    PanelModelPalette: TPanel;
    PageControlModels: TPageControl;
    TabSheetHoles: TTabSheet;
    TabSheetPoints2D: TTabSheet;
    TabSheetPoints3D: TTabSheet;
    TabSheetPolygons: TTabSheet;
    TabSheetTins: TTabSheet;
    TabSheetSolids: TTabSheet;
    TabSheetGrids2D: TTabSheet;
    ToolBarGrids2D: TToolBar;
    TabSheetGrids3D: TTabSheet;
    TabSheetMeshes2D: TTabSheet;
    TabSheetMeshes3D: TTabSheet;
    ToolButton2:  TToolButton;
    TabSheetDrawings: TTabSheet;
    ToolBarGrids3D: TToolBar;
    StaticTextGrid3DRow: TStaticText;
    SpinEditGrid3DRow: TSpinEdit;
    StaticTextGrid3DColumn: TStaticText;
    SpinEditGrid3DColumn: TSpinEdit;
    StaticTextGrid3DLayer: TStaticText;
    SpinEditGrid3DLayer: TSpinEdit;
    StaticTextGrid2DRow: TStaticText;
    StaticTextGrid2DColumn: TStaticText;
    SpinEditGrid2DColumn: TSpinEdit;
    ActionManager: TActionManager;
    EditCopy:     TEditCopy;
    EditPaste:    TEditPaste;
    EditSelectAll: TEditSelectAll;
    EditUndo:     TEditUndo;
    EditDelete:   TEditDelete;
    WindowClose:  TWindowClose;
    WindowCascade: TWindowCascade;
    WindowTileHorizontal: TWindowTileHorizontal;
    WindowTileVertical: TWindowTileVertical;
    WindowMinimizeAll: TWindowMinimizeAll;
    WindowArrange: TWindowArrange;
    StyleText:    TAction;
    StyleSymbol:  TAction;
    StyleLine:    TAction;
    StyleFill:    TAction;
    CalculatorGeology: TAction;
    CalculatorGeometry: TAction;
    ActionToolBarStandard: TActionToolBar;
    ActionToolBarEdit: TActionToolBar;
    ActionToolBarMap: TActionToolBar;
    ActionToolBarView: TActionToolBar;
    ActionToolBarAnalyse: TActionToolBar;
    EditCut:      TEditCut;
    ToolsStandardStyle: TAction;
    ToolsXPStyle: TAction;
    ToolsCustomize: TCustomizeActionBars;
    HelpGlossary: TAction;
    HelpContents: TAction;
    HelpAbout:    TAction;
    ObserveTop:   TAction;
    ObserveBottom: TAction;
    ObserveBack:  TAction;
    ObserveLeft:  TAction;
    ObserveRight: TAction;
    ObserveFront: TAction;
    Observe3D:    TAction;
    ObserveRotate: TAction;
    ObservePerspective: TAction;
    ActionToolBarObserve: TActionToolBar;
    WindowRedraw: TAction;
    ToolsConfiguration: TAction;
    ToolsUnitsConverter: TAction;
    CalculatorMining: TAction;
    CalculatorSurvey: TAction;
    acAnalyseProblemBook: TAction;
    acAnalyseVolumeCalculation: TAction;
    acAnalyseReserveCalculation: TAction;
    acAnalyseBaseStatistics: TAction;
    acAnalyseFactorAnalysis: TAction;
    acAnalyseVariograms: TAction;
    CompositingSampleContacts: TAction;
    CompositingCenters: TAction;
    CompositingOreIntervals: TAction;
    CompositingOreSorting: TAction;
    CompositingInsideHorizons: TAction;
    CompositingLinearReserves: TAction;
    FileOpenProject: TAction;
    FileOpenModel: TAction;
    FileOpenReport: TAction;
    FileOpenImage: TAction;
    FileOpenText: TAction;
    FileSave:     TAction;
    FileSaveAs:   TAction;
    FileSaveProjectAs: TAction;
    FileClose:    TAction;
    FileCloseAll: TAction;
    FileImport:   TAction;
    FileExport:   TAction;
    MethodConversion: TAction;
    FilePrint:    TAction;
    FilePrintPreview: TAction;
    FileExit: TFileExit;
    EditFind:     TAction;
    DrawText:     TAction;
    DrawSymbol:   TAction;
    DrawPolyline: TAction;
    DrawPolygon:  TAction;
    DrawEllipse:  TAction;
    DrawRectangle: TAction;
    DrawSolid:    TAction;
    DrawReshape:  TAction;
    DrawAddNode:  TAction;
    DrawDepth:    TAction;
    MapOptions:   TAction;
    MapLegend:    TAction;
    MapMaterial:  TAction;
    MapLighting:  TAction;
    MapBackColor: TAction;
    ViewProjectManager: TAction;
    ViewScale:    TAction;
    aZoomInOut: TAction;
    ViewSelect:   TAction;
    ViewScroll:   TAction;
    ViewPan:      TAction;
    ViewPlaneXY:  TAction;
    ViewPlaneXZ:  TAction;
    ViewPlaneYZ:  TAction;
    ViewVolumeXYZ: TAction;
    ViewStatusline: TAction;
    aZoomToProject: TAction;
    aZoomToComplete: TAction;
    aZoomToModel: TAction;
    aZoomToRectangle: TAction;
    DrillholesOptions: TAction;
    DrillholesSelectDrillhole: TAction;
    DrillholesSelectContact: TAction;
    DrillholesSelectSegment: TAction;
    DrillholesCreateDrillhole: TAction;
    DrillholesCreateSegment: TAction;
    Points2DOptions: TAction;
    Points2DSelectPoint: TAction;
    Points2DCreatePoint: TAction;
    Points3DOptions: TAction;
    Points3DSelectPoint: TAction;
    Points3DCreatePoint: TAction;
    PolygonsOptions: TAction;
    PolygonsSelectPolygon: TAction;
    PolygonsSelectVertex: TAction;
    PolygonsCreatePolygon: TAction;
    PolygonsSelectType: TAction;
    PolygonsLinkToSolid: TAction;
    TinOptions:   TAction;
    TinVertex:    TAction;
    TinSelectEdge: TAction;
    TinSelectTriangle: TAction;
    TinCreateVertex: TAction;
    TinCreateTriangle: TAction;
    TinSwapDiagonals: TAction;
    SolidOptions: TAction;
    SolidCreate:  TAction;
    SolidSelect:  TAction;
    Grid2DOptions: TAction;
    Grid2DSelectCell: TAction;
    Grid2DSelectRow: TAction;
    Grid2DSelectCol: TAction;
    Grid2DCreateCell: TAction;
    Grid3DOptions: TAction;
    Grid3DSelectCell: TAction;
    Grid3DSelectRow: TAction;
    Grid3DSelectCol: TAction;
    Grid3DSelectLay: TAction;
    Grid3DCreateCell: TAction;
    Mesh2DOptions: TAction;
    Mesh2DSelectNode: TAction;
    Mesh2DSelectCell: TAction;
    Mesh2DCreateCell: TAction;
    Mesh3DOptions: TAction;
    Mesh3DSelectNode: TAction;
    Mesh3DSelectCell: TAction;
    Mesh3DCreateNode: TAction;
    Mesh3DCreateCell: TAction;
    ShowSection:  TAction;
    ShowContours: TAction;
    ShowIsosurface: TAction;
    ShowVectors:  TAction;
    ShowFilm:     TAction;
    MethodGridGeneration: TAction;
    MethodTriangulation: TAction;
    MethodInterpolation: TAction;
    MethodBlockEvaluation: TAction;
    MethodPitOptimization: TAction;
    MethodSetOperations: TAction;
    ActionToolBarPoints2D: TActionToolBar;
    ActionToolBarDrillholes: TActionToolBar;
    ActionToolBarPoints3D: TActionToolBar;
    ActionToolBarPolygons: TActionToolBar;
    SpinEditGrid2DRow: TSpinEdit;
    ActionToolBarGrid2D: TActionToolBar;
    ActionToolBarGrid3D: TActionToolBar;
    ActionToolBarTin: TActionToolBar;
    ActionToolBarSolids: TActionToolBar;
    ActionToolBarMesh2D: TActionToolBar;
    ActionToolBarMesh3D: TActionToolBar;
    ActionToolBarDrawing: TActionToolBar;
    ActionToolBarMethod: TActionToolBar;
    MethodTransformation: TAction;
    XPColorMap:   TXPColorMap;
    MethodPrediction: TAction;
    CustomizeDlg: TCustomizeDlg;
    ActionMainMenuBar: TActionMainMenuBar;
    Action1:      TAction;
    FileDataBase: TAction;
    TabSheetPolylines: TTabSheet;
    ActionToolBarPolylines: TActionToolBar;
    ViewTableWindow: TAction;
    MethodVarModeling: TAction;
    ViewMapWindow: TAction;
    OctreeConstruction: TAction;
    MethodSimulation: TAction;
    procedure ViewTableWindowExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure ToolsStandardStyleExecute(Sender: TObject);
    procedure ToolsXPStyleExecute(Sender: TObject);
    procedure ObserveTopExecute(Sender: TObject);
    procedure ObserveBottomExecute(Sender: TObject);
    procedure ObserveBackExecute(Sender: TObject);
    procedure ObserveRightExecute(Sender: TObject);
    procedure ObserveLeftExecute(Sender: TObject);
    procedure ObserveFrontExecute(Sender: TObject);
    procedure Observe3DExecute(Sender: TObject);
    procedure ObserveRotateExecute(Sender: TObject);
    procedure ObservePerspectiveExecute(Sender: TObject);
    procedure WindowRedrawExecute(Sender: TObject);
    procedure StyleTextExecute(Sender: TObject);
    procedure StyleSymbolExecute(Sender: TObject);
    procedure StyleLineExecute(Sender: TObject);
    procedure StyleFillExecute(Sender: TObject);
    procedure CalculatorGeologyExecute(Sender: TObject);
    procedure CalculatorGeometryExecute(Sender: TObject);
    procedure CalculatorMiningExecute(Sender: TObject);
    procedure CalculatorSurveyExecute(Sender: TObject);
    procedure acAnalyseProblemBookExecute(Sender: TObject);
    procedure acAnalyseVolumeCalculationExecute(Sender: TObject);
    procedure acAnalyseReserveCalculationExecute(Sender: TObject);
    procedure acAnalyseBaseStatisticsExecute(Sender: TObject);
    procedure acAnalyseVariogramsExecute(Sender: TObject);
    procedure CompositingSampleContactsExecute(Sender: TObject);
    procedure CompositingCentersExecute(Sender: TObject);
    procedure CompositingOreSortingExecute(Sender: TObject);
    procedure CompositingInsideHorizonsExecute(Sender: TObject);
    procedure CompositingOreIntervalsExecute(Sender: TObject);
    procedure CompositingLinearReservesExecute(Sender: TObject);
    procedure FileNewExecute(Sender: TObject);
    procedure FileDataBaseExecute(Sender: TObject);
    procedure FileOpenProjectExecute(Sender: TObject);
    procedure FileOpenModelExecute(Sender: TObject);
    procedure FileOpenReportExecute(Sender: TObject);
    procedure FileOpenImageExecute(Sender: TObject);
    procedure FileOpenTextExecute(Sender: TObject);
    procedure FileSaveAsExecute(Sender: TObject);
    procedure FileSaveProjectAsExecute(Sender: TObject);
    procedure FileCloseAllExecute(Sender: TObject);
    procedure FileCloseExecute(Sender: TObject);
    procedure FileImportExecute(Sender: TObject);
    procedure FileExportExecute(Sender: TObject);
    procedure FilePrintExecute(Sender: TObject);
    procedure FilePrintPreviewExecute(Sender: TObject);
    procedure EditCopyExecute(Sender: TObject);
    procedure EditPasteExecute(Sender: TObject);
    procedure EditSelectAllExecute(Sender: TObject);
    procedure EditFindExecute(Sender: TObject);
    procedure MapSceneryExecute(Sender: TObject);
    procedure MapOptionsExecute(Sender: TObject);
    procedure MapLegendExecute(Sender: TObject);
    procedure MapMaterialExecute(Sender: TObject);
    procedure MapLightingExecute(Sender: TObject);
    procedure MapBackColorExecute(Sender: TObject);
    procedure DrawDepthExecute(Sender: TObject);
    procedure DrawAddNodeExecute(Sender: TObject);
    procedure DrawReshapeExecute(Sender: TObject);
    procedure DrawSolidExecute(Sender: TObject);
    procedure DrawEllipseExecute(Sender: TObject);
    procedure DrawPolygonExecute(Sender: TObject);
    procedure DrawRectangleExecute(Sender: TObject);
    procedure DrawPolylineExecute(Sender: TObject);
    procedure DrawSymbolExecute(Sender: TObject);
    procedure DrawTextExecute(Sender: TObject);
    procedure ViewProjectManagerExecute(Sender: TObject);
    procedure ViewScaleExecute(Sender: TObject);
    procedure ViewSelectExecute(Sender: TObject);
    procedure ViewScrollExecute(Sender: TObject);
    procedure ViewPanExecute(Sender: TObject);
    procedure ViewVolumeXYZExecute(Sender: TObject);
    procedure ViewPlaneXYExecute(Sender: TObject);
    procedure ViewPlaneXZExecute(Sender: TObject);
    procedure ViewPlaneYZExecute(Sender: TObject);
    procedure ViewStatuslineExecute(Sender: TObject);
    procedure aZoomInOutExecute(Sender: TObject);
    procedure aZoomToProjectExecute(Sender: TObject);
    procedure aZoomToCompleteExecute(Sender: TObject);
    procedure aZoomToModelExecute(Sender: TObject);
    procedure aZoomToRectangleExecute(Sender: TObject);
    procedure PolygonsSelectTypeExecute(Sender: TObject);
    procedure PolygonsLinkToSolidExecute(Sender: TObject);
    procedure MethodGridGenerationExecute(Sender: TObject);
    procedure MethodTriangulationExecute(Sender: TObject);
    procedure MethodInterpolationExecute(Sender: TObject);
    procedure MethodBlockEvaluationExecute(Sender: TObject);
    procedure MethodPitOptimizationExecute(Sender: TObject);
    procedure MethodSetOperationsExecute(Sender: TObject);
    procedure MethodConversionExecute(Sender: TObject);
    procedure MethodPredictionExecute(Sender: TObject);
    procedure MethodTransformationExecute(Sender: TObject);
    procedure MethodVarModelingExecute(Sender: TObject);
    procedure ShowSectionExecute(Sender: TObject);
    procedure ShowContoursExecute(Sender: TObject);
    procedure ShowIsosurfaceExecute(Sender: TObject);
    procedure ShowVectorsExecute(Sender: TObject);
    procedure ShowFilmExecute(Sender: TObject);
    procedure Points2DSelectPointExecute(Sender: TObject);
    procedure Points2DCreatePointExecute(Sender: TObject);
    procedure Points3DSelectPointExecute(Sender: TObject);
    procedure Grid2DSelectCellExecute(Sender: TObject);
    procedure DrillholesCreateSegmentExecute(Sender: TObject);
    procedure DrillholesSelectDrillholeExecute(Sender: TObject);
    procedure DrillholesSelectContactExecute(Sender: TObject);
    procedure DrillholesSelectSegmentExecute(Sender: TObject);
    procedure DrillholesCreateDrillholeExecute(Sender: TObject);
    procedure Grid2DSelectRowExecute(Sender: TObject);
    procedure Grid2DSelectColExecute(Sender: TObject);
    procedure ToolsConfigurationExecute(Sender: TObject);
    procedure ToolsUnitsConverterExecute(Sender: TObject);
    procedure HelpContentsExecute(Sender: TObject);
    procedure HelpGlossaryExecute(Sender: TObject);
    procedure HelpAboutExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OctreeConstructionExecute(Sender: TObject);
    procedure MethodSimulationExecute(Sender: TObject);
  public
    FPopupXY: TPoint;
    procedure ShowTable(const AFileName: TFileName; AModelType: integer);
    procedure ShowMap(const AFileName: TFileName; AModelType: integer);
    procedure ShowGraph(const AFileName: TFileName; AModelType: integer);
    procedure EnableFileItems(AEnabled: boolean);
    procedure EnableMapItems(AModelType: integer; AEnabled: boolean);
    procedure DrawEnableItems(AEnabled: boolean);
    procedure ReadIniFile; override;
    procedure WriteIniFile; override;
  private
    procedure ShowStatusText(Sender: TObject);
    procedure DefaultLayout;
  end;

var
  fmGeoblock: TfmGeoblock;

//==========================================================================
implementation
//==========================================================================

uses
  cGlobals,
  uCommon,
  cResStrings,
  cProfuns,
  gnuGettext,
  fAnalyseProblems,
  fAnalyseReserves,
  fAnalyseVariograms,
  fComposeContacts,
  fComposeCenters,
  fComposeOreSorts,
  fComposeOreIntervals,
  fComposeByHorizons,
  fComposeLinearReserves,
  fDrawSymbolStyle,
  fDrawLineStyle,
  fDrawFillStyle,
  fDrawObjectDepth,
  fGraphWindow,
  fFileOpenText,
  fFileOpenModel,
  fFileImageRegistration,
  fFileImport,
  fFileExport,
  fHelpAbout,
  fMethodDialog,
  fMethodGridGeneration,
  fMethodTriangulation,
  fMethodInterpolation,
  fMethodPrediction,
  fMethodEvaluation,
  fMethodPitOptimization,
  fMethodConversion,
  fMethodTransformation,
  fMethodSetOperations,
  fMethodVarioModeller,
  fMethodSimulation,
  fMethodOctree,
  fViewProjectManager,
  fViewScale,
  fViewVariogram,
  fMapWindow,
  fPerformIsosurfaces,
  fPerformVectors,
  fPerformContours,

  fTableWindow,
  fToolsConfiguration,
  fToolsUnitsConverter,
  fToolsGeologyCalculator,
  fToolsGeometryCalculator,
  fToolsMiningCalculator,
  //fFileBrowser,
  //fFileDataBrowser,
  fToolsSurveyCalculator,
  //fViewProjectManager,
  fMapLegend,
  fMapScenery,
  fFileDataBrowser;

{$R *.DFM}

//==================== Interface =======================

procedure TfmGeoblock.DefaultLayout;
begin
//  fmGeoblock.Left := 1; fmGeoblock.Top := 1;
//  fmGeoblock.Width := Screen.Width - 100;
//  fmGeoblock.Height := Screen.Height - 30;
  fmFileDataBrowser.Left := fmGeoblock.Left + 1;
  fmFileDataBrowser.Top := fmGeoblock.Top + 120;
  fmFileDataBrowser.Height := fmGeoblock.Height - 150;
//  fmViewProjectManager.Position := poDesigned;
  fmViewProjectManager.Left := fmGeoblock.Width + 30;
  fmViewProjectManager.Top := fmGeoblock.Top + 120;

{
  fmMapWindow.Top := 25;
  fmMapScenery.Left := fmFileDataBrowser.Width + 1;
  fmMapScenery.Top := 50;
  fmMapScenery.Left := fmFileDataBrowser.Width + 10;
  fmTableWindow.Top := 75;
  fmTableWindow.Left := fmFileDataBrowser.Width + 20;
  fmGraphWindow.Top := 100;
  fmGraphWindow.Left := fmFileDataBrowser.Width + 30;
}
end;

procedure TfmGeoblock.FormCreate(Sender: TObject);
begin
  ReadIniFile;
  Application.OnHint      := ShowStatusText;
//  PanelModelPalette.Width := fmMain.Width - PanelModelPalette.Left - 10;
end;

procedure TfmGeoblock.FormShow(Sender: TObject);
begin
  DefaultLayout;
  fmFileDataBrowser.Show;
  fmViewProjectManager.Show;
end;

procedure TfmGeoblock.ShowTable(const AFileName: TFileName; AModelType: integer);
var
  FileName : TFileName;
begin
  fmTableWindow := TfmTableWindow.Create(Self);
  fmTableWindow.ModelType := AModelType;
  fmTableWindow.OpenTable(AFileName);
  case AModelType of
    mtPolygons:
    begin
      FileName := ChangeModelTable(DirPolygonPoly, DirPolygonVertex, AFileName);
      fmTableWindow := TfmTableWindow.Create(Self);
      fmTableWindow.OpenTable(FileName);
    end;
    mtTins:   //Open vertices
    begin
      FileName := ChangeModelTable(DirTinFaces, DirTinVertices, AFileName);
      fmTableWindow := TfmTableWindow.Create(Self);
      fmTableWindow.OpenTable(FileName);
    end;
    mtGrids2D:
    begin
      fmFileOpenText := TfmFileOpenText.Create(Self);
      fmFileOpenText.TextFileName := AFileName + '.par';
      fmFileOpenText.Caption := ExtractFileName(fmFileOpenText.TextFileName);
      fmFileOpenText.LoadFile;
      fmFileOpenText.Show;
    end;
    mtGrids3D:
    begin
      fmFileOpenText := TfmFileOpenText.Create(Self);
      fmFileOpenText.TextFileName := AFileName + '.par';
      fmFileOpenText.Caption := ExtractFileName(fmFileOpenText.TextFileName);
      fmFileOpenText.LoadFile;
      fmFileOpenText.Show;
    end;
    mtMeshes2D:
    begin
      FileName := ChangeModelTable(DirMesh2DVertices, DirMesh2DFaces, AFileName);
      fmTableWindow := TfmTableWindow.Create(Self);
      fmTableWindow.OpenTable(FileName);
    end;
    mtMeshes3D:
    begin
      FileName := ChangeModelTable(DirMesh3DNode, DirMesh3DElement, AFileName);
      fmTableWindow := TfmTableWindow.Create(Self);
      fmTableWindow.OpenTable(FileName);
    end;
  end;

  WindowTileHorizontal.Execute;
end;


procedure TfmGeoblock.ShowMap(const AFileName: TFileName; AModelType: integer);
begin
  Screen.Cursor := crHourGlass;
  try
    if fmMapWindow = nil then
    begin
      fmMapWindow := TfmMapWindow.Create(Self);
      fmMapWindow.InitMapWin(AFileName, AModelType);
      ViewVolumeXYZ.Checked    := True;
    end
    else
      fmMapWindow.OpenNewModel(AFileName, AModelType);
  finally
    Screen.Cursor := crDefault;
    EnableMapItems(AModelType, True);
    fmMapWindow.SetFocus;
  end;
end;

procedure TfmGeoblock.ShowGraph(const AFileName: TFileName; AModelType: integer);
begin
  fmGraphWindow := TfmGraphWindow.Create(Self);
  fmGraphWindow.ModelType := AModelType;
  fmGraphWindow.OpenTable(AFileName);
end;

procedure TfmGeoblock.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  I: integer;
begin
  // Protection from unexpected reloading Windows System
  if MDIChildCount <> 0 then
  begin
    if MessageDlg(LoadResString(@rsShutdown) + ' ' + LoadResString(@rsGeoblock) +
      '?', mtConfirmation, mbOKCancel, 0) = mrOk then
    begin
      for I := MDIChildCount - 1 downto 0 do
        MDIChildren[I].Close;
      CanClose := True;
    end
    else
      CanClose := False;
  end;
end;

procedure TfmGeoblock.CompositingSampleContactsExecute(Sender: TObject);
begin
  with TfmComposeContacts.Create(Self) do
    try
      if ShowModal = mrOk then
      begin
        Hide;
        case ToolBarShowAs.Tag of
          0: ShowMap(TableOutput.TableName, OutModelType);
          1: ShowTable(TableOutput.TableName, OutModelType);
          2: ShowGraph(TableOutput.TableName, OutModelType);
        end;
      end;
    finally
      Free;
    end;
end;

procedure TfmGeoblock.CompositingCentersExecute(Sender: TObject);
begin
  with TfmComposeCenters.Create(Self) do
    try
      if ShowModal = mrOk then
      begin
        Hide;
        case ToolBarShowAs.Tag of
          0: ShowMap(OutModelName, OutModelType);
          1: ShowTable(OutModelName, OutModelType);
          2: ShowGraph(OutModelName, OutModelType);
        end;
      end;
    finally
      Free;
    end;
end;

procedure TfmGeoblock.CompositingOreSortingExecute(Sender: TObject);
begin
  with TfmComposeOreSorts.Create(Self) do
    try
      if ShowModal = mrOk then
      begin
        Hide;
        case ToolBarShowAs.Tag of
          0: ShowMap(OutModelName, OutModelType);
          1: ShowTable(OutModelName, OutModelType);
          2: ShowGraph(OutModelName, OutModelType);
        end;
      end;
    finally
      Free;
    end;
end;

procedure TfmGeoblock.CompositingInsideHorizonsExecute(Sender: TObject);
begin
  with TfmComposeByHorizons.Create(Self) do
    try
      if ShowModal = mrOk then

      begin
        Hide;
        case ToolBarShowAs.Tag of
          0: ShowMap(OutModelName, OutModelType);
          1: ShowTable(OutModelName, OutModelType);
          2: ShowGraph(OutModelName, OutModelType);
        end;
      end;
    finally
      Free;
    end;
end;

procedure TfmGeoblock.CompositingOreIntervalsExecute(Sender: TObject);
begin
  with TfmComposeOreIntervals.Create(Self) do
    try
      if ShowModal = mrOk then

      begin
        Hide;
        case ToolBarShowAs.Tag of
          0: ShowMap(OutModelName, OutModelType);
          1: ShowTable(OutModelName, OutModelType);
          2: ShowGraph(OutModelName, OutModelType);
        end;
      end;
    finally
      Free;
    end;
end;

procedure TfmGeoblock.CompositingLinearReservesExecute(Sender: TObject);
begin
  with TfmComposeLinearReserves.Create(Self) do
    try
      if ShowModal = mrOk then

      begin
        Hide;
        case ToolBarShowAs.Tag of
          0: ShowMap(OutModelName, OutModelType);
          1: ShowTable(OutModelName, OutModelType);
          2: ShowGraph(OutModelName, OutModelType);
        end;
      end;
    finally
      Free;
    end;
end;


procedure TfmGeoblock.EnableFileItems(AEnabled: boolean);
begin
  //File Menu
  FileSave.Enabled     := AEnabled;
  FileSaveAs.Enabled   := AEnabled;
  FileSaveProjectAs.Enabled := AEnabled;
  FileClose.Enabled    := AEnabled;
  FileCloseAll.Enabled := AEnabled;

  FileExport.Enabled    := AEnabled;
  FilePrint.Enabled     := AEnabled;
  FilePrintPreview.Enabled := AEnabled;
  //Edit
  EditSelectAll.Enabled := AEnabled;
  EditFind.Enabled      := AEnabled;
  //Method
  MethodConversion.Enabled := AEnabled;
  //Analyse
  acAnalyseBaseStatistics.Enabled := AEnabled;
end;


//________________ File __________________\\

procedure TfmGeoblock.FileNewExecute(Sender: TObject);
begin
  with TfmFileNew.Create(Self) do
  try
  if ShowModal = mrOk then
    with dBase.dmBase do
    case ToolBarShowAs.Tag of
      0: ShowMap(TableOutput.TableName, OutModelType);
      1: ShowTable(TableOutput.TableName, OutModelType);
      2: ShowGraph(TableOutput.TableName, OutModelType);
    end;
  finally
    Free;
  end;
end;

procedure TfmGeoblock.FileOpenProjectExecute(Sender: TObject);
begin
  with dmDialogs do
  begin
    OpenDialogProject.FileName   := ExpandPath(DirProject) + 'Geoblock' + PrjExt;
    OpenDialogProject.InitialDir := ExpandPath(DirProject);
    if OpenDialogProject.Execute then
    begin
      if OpenDialogProject.FilterIndex = 2 then
      begin
        fmViewProjectManager.LoadFromFile(OpenDialogProject.FileName);
        aZoomToComplete.Execute;
      end
      else
        with TfmFileOpenText.Create(Self) do
        begin
          TextFileName := OpenDialogProject.FileName;
          LoadFile;
          ShowModal;
          EnableFileItems(True);
        end;
    end;
  end;
end;

procedure TfmGeoblock.FileDataBaseExecute(Sender: TObject);
begin
  fmFileDataBrowser.Show;
end;


procedure TfmGeoblock.FileOpenModelExecute(Sender: TObject);
var
  I: integer;
begin
  with TfmFileOpenModel.Create(Self) do
    try
      if ShowModal = mrOk then
      begin
        for I := 0 to Files.Count - 1 do
          case ToolBarShowAs.Tag of
            0: ShowMap(Files[I], ModelType);
            1: ShowTable(Files[I], ModelType);
            2: ShowGraph(Files[I], ModelType);
          end;
        EnableFileItems(True);
        aZoomToComplete.Execute;
      end;
    finally
      Free;
    end;
  fmViewProjectManager.EnableButtons;
end;

procedure TfmGeoblock.FileOpenReportExecute(Sender: TObject);
begin
  with dmDialogs do
  begin
    OpenDialog.InitialDir := DataBasePath + DirReport;
    OpenDialog.Filter     := '.rpt';
    if OpenDialog.Execute then
    begin

    end;
  end;
end;

procedure TfmGeoblock.FileOpenImageExecute(Sender: TObject);
begin
  dmDialogs.OpenPictureDialog.InitialDir := ExpandPath(DirPicture);
  if dmDialogs.OpenPictureDialog.Execute then
  begin
    with TfmFileImageRegistration.Create(Self) do
      try
        //      Image := Image.LoadFromFile(FileName);
      finally
        Free;
      end;
    {
    FileName := dmDialogs.OpenPictureDialog.FileName;
    Picture := TPicture.Create;
    Picture.LoadFromFile(FileName);
    if (Picture.Width < 256) or (Picture.Height < 256) then begin
      Picture.BitMap.Width:=256;
      Picture.Bitmap.Height:=256;
    end;
    GLImage.Picture.Assign(Picture);
    Picture.Free;
    ShowMap(FileName, PageControlDataset.ActivePage.Tag);
    }
  end;
end;

procedure TfmGeoblock.FileOpenTextExecute(Sender: TObject);
begin
  with dmDialogs do
  begin
    OpenDialogText.InitialDir  := ExpandPath(DirFiles);
    OpenDialogText.FilterIndex := 4; //*.*
    if OpenDialogText.Execute then
    begin
      with TfmFileOpenText.Create(Self) do
        try
          TextFileName := OpenDialogText.FileName;
          LoadFile;
          if ShowModal = mrOk then
            EnableFileItems(True);
        finally
          Free;
        end;
    end;
  end;
end;

procedure TfmGeoblock.FileSaveProjectAsExecute(Sender: TObject);
begin
  with dmDialogs do
  begin
    SaveDialogText.FileName   := ExpandPath(DirProject) + 'Geoblock' + PrjExt;
    SaveDialogText.InitialDir := ExpandPath(DirProject);
    SaveDialogText.Title      := LoadResString(@rsSaveProjectAs);
    SaveDialogText.FilterIndex := 2; //*.prj
    if SaveDialogText.Execute then
      fmViewProjectManager.SaveToFile(SaveDialogText.FileName);
  end;
end;

procedure TfmGeoblock.FileSaveAsExecute(Sender: TObject);
var
  FileName: TFileName;
begin
  with dmDialogs do
  begin
    if (ActiveMDIChild is TfmTableWindow) then
    begin
      FileName := (ActiveMDIChild as TfmTableWindow).TableMaster.TableName;
      SaveDialog.FileName := FileName;
      if SaveDialog.Execute then
      begin
        if SaveDialog.FileName <> FileName then
        begin
          (ActiveMDIChild as
            TfmTableWindow).SaveAs(SaveDialog.FileName);
        end
        else
          (ActiveMDIChild as TfmTableWindow).TableMaster.Close;
      end;
    end;
    if (ActiveMDIChild is TfmMapWindow) then
    begin
      SavePictureDialog.InitialDir := ExpandPath(DirPicture);
      if SavePictureDialog.Execute then
      begin
        (ActiveMDIChild as
          TfmMapWindow).SaveAs(SavePictureDialog.FileName);
      end;
    end;
    if (ActiveMDIChild is TfmGraphWindow) then
    begin
      //Save as Graph...
    end;
    if (ActiveMDIChild is TfmFileOpenText) then
      (ActiveMDIChild as TfmFileOpenText).SaveTextFileAs;
  end;
end;


procedure TfmGeoblock.FileCloseAllExecute(Sender: TObject);
var
  I: integer;
begin
  for I := 0 to 1000 do
  begin
    FileCloseExecute(nil);
  end;
  EnableFileItems(False);
  EnableMapItems(mtUnknown, False);
end;

procedure TfmGeoblock.FileCloseExecute(Sender: TObject);
begin
  with dmBase do
  begin
    if (ActiveMDIChild is TfmTableWindow) then
    begin
      (ActiveMDIChild as TfmTableWindow).TableMaster.Close;
      (ActiveMDIChild as TfmTableWindow).Close;
    end;
    if (ActiveMDIChild is TfmMapWindow) then
    begin
      (ActiveMDIChild as TfmMapWindow).TableMap.Close;
      (ActiveMDIChild as TfmMapWindow).Close;
    end;
    if (ActiveMDIChild is TfmGraphWindow) then
    begin
      (ActiveMDIChild as TfmGraphWindow).TableGraph.Close;
      (ActiveMDIChild as TfmGraphWindow).Close;
    end;
    Application.ProcessMessages;
  end;
end;

procedure TfmGeoblock.FileImportExecute(Sender: TObject);
begin
  with TfmFileImport.Create(Self) do
    try
      if ChooseModelType then
      begin
        if ShowModal = mrOk then
          case ToolBarShowAs.Tag of
            0: ShowMap(OutModelName, OutModelType);
            1: ShowTable(OutModelName, OutModelType);
            2: ShowGraph(OutModelName, OutModelType);
          end;
      end;
    finally
      Free;
    end;
end;

procedure TfmGeoblock.FileExportExecute(Sender: TObject);
begin
  if (ActiveMDIChild is TfmMapWindow) then
    fmMapWindow.DoExportToMIF
  else
    with (ActiveMDIChild as TfmTableWindow) do
    begin
      with TfmFileExport.Create(Self) do
      begin
        try
          SourceFile := TableMaster.TableName;
          ShowModal;
        finally
          Free;
        end;
      end;
    end;
end;

procedure TfmGeoblock.FilePrintExecute(Sender: TObject);
begin
  if dmDialogs.PrintDialog.Execute then
    if (ActiveMDIChild is TfmTableWindow) then
    with (ActiveMDIChild as TfmTableWindow) do
    try
      // Change to FastReport
      // dmBase.RvTableConnection.Table:=TableMaster;
      // dmBase.RvProject.ProjectFile:=ExePath+'HELP\RU\Rave\TableReports.rav';
      // dmBase.RvProject.Open;
      // dmBase.GenerateReport;
    finally
      // dmBase.RvProject.Close;
    end;
end;

procedure TfmGeoblock.FilePrintPreviewExecute(Sender: TObject);
begin
  if (ActiveMDIChild is TfmTableWindow) then
    with (ActiveMDIChild as TfmTableWindow) do
    begin
    // Change to FastReport
(*    with TfmQRListing.CreateSimpleReport(Self, TableMaster.TableName) do
      begin
        QuickRep.Preview;
        Free;
      end;
      with TfmQRReserve.CreateReport(Self, TableMaster.TableName) do
      begin
        QuickRep.Preview;
        Free;
      end;
*)
    end;
end;

//______________________Edit__________________________\\

procedure TfmGeoblock.EditCopyExecute(Sender: TObject);
begin
  if (ActiveMDIChild is TfmTableWindow) then
    TfmTableWindow(ActiveMDIChild).CopyToClipboard(ActiveMDIChild)
  else if (ActiveMDIChild is TfmFileOpenText) then
    TfmFileOpenText(ActiveMDIChild).CopyToClipboard
  else if (ActiveMDIChild is TfmGraphWindow) then
    TfmGraphWindow(ActiveMDIChild).CopyToClipboard;
end;

procedure TfmGeoblock.EditPasteExecute(Sender: TObject);
begin
  if (ActiveMDIChild is TfmTableWindow) then
    TfmTableWindow(ActiveMDIChild).PasteFromClipboard(ActiveMDIChild)
  else if (ActiveMDIChild is TfmFileOpenText) then
    TfmFileOpenText(ActiveMDIChild).PasteFromClipboard;
end;

procedure TfmGeoblock.EditSelectAllExecute(Sender: TObject);
begin
  if (ActiveMDIChild is TfmTableWindow) then
    TfmTableWindow(ActiveMDIChild).SelectAll(ActiveMDIChild);
end;

procedure TfmGeoblock.EditFindExecute(Sender: TObject);
begin
  if (ActiveMDIChild is TfmTableWindow) then
    TfmTableWindow(ActiveMDIChild).EditTableFindExecute(ActiveMDIChild);
  if (ActiveMDIChild is TfmMapWindow) then
    with (ActiveMDIChild as TfmMapWindow) do
    begin
      fmTableWindow.EditTableFindExecute(Self);
    end;
end;



//____________________________ Methods _____________________________\\

procedure TfmGeoblock.MethodGridGenerationExecute(Sender: TObject);
begin
  with TfmMethodGridGeneration.Create(Self) do
    try
      if ShowModal = mrOk then
      begin
        Hide;
        case ToolBarShowAs.Tag of
          0: ShowMap(OutModelName, OutModelType);
          1: ShowTable(OutModelName, OutModelType);
          2: ShowGraph(OutModelName, OutModelType);
        end;
      end;
    finally
      Free;
    end;
end;

procedure TfmGeoblock.MethodTriangulationExecute(Sender: TObject);
begin
  with TfmMethodTriangulation.Create(Self) do
    try
      if ShowModal = mrOk then
      begin
        Hide;
        case ToolBarShowAs.Tag of
          0: ShowMap(OutModelName, OutModelType);
          1: ShowTable(OutModelName, OutModelType);
          2: ShowGraph(OutModelName, OutModelType);
        end;
      end;
    finally
      Free;
    end;
end;

procedure TfmGeoblock.MethodVarModelingExecute(Sender: TObject);
begin
  with TfmMethodVarioModeller.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfmGeoblock.MethodInterpolationExecute(Sender: TObject);
begin
  with TfmMethodInterpolation.Create(Self) do
    try
      if ShowModal = mrOk then
      begin
        Hide;
        case ToolBarShowAs.Tag of
          0: ShowMap(OutModelName, OutModelType);
          1: ShowTable(OutModelName, OutModelType);
          2: ShowGraph(OutModelName, OutModelType);
        end;
      end;
    finally
      Free;
    end;
end;

procedure TfmGeoblock.MethodPredictionExecute(Sender: TObject);
begin
  with TfmMethodPrediction.Create(Self) do
    try
      if ShowModal = mrOk then
      begin
        Hide;
        case ToolBarShowAs.Tag of
          0: ShowMap(OutModelName, OutModelType);
          1: ShowTable(OutModelName, OutModelType);
          2: ShowGraph(OutModelName, OutModelType);
        end;
      end;
    finally
      Free;
    end;
end;


procedure TfmGeoblock.MethodBlockEvaluationExecute(Sender: TObject);
begin
  with TfmMethodEvaluation.Create(Self) do
    try
      if ShowModal = mrOk then
      begin
        Hide;
        case ToolBarShowAs.Tag of
          0: ShowMap(OutModelName, OutModelType);
          1: ShowTable(OutModelName, OutModelType);
          2: ShowGraph(OutModelName, OutModelType);
        end;
      end;
    finally
      Free;
    end;
end;

procedure TfmGeoblock.MethodPitOptimizationExecute(Sender: TObject);
begin
  { Dialog for Floating Cone, Lerch-Grossman and other open pit optimizations... }
  with TfmMethodPitOptimization.Create(Self) do
    try
      if ShowModal = mrOk then
      begin
        Hide;
        case ToolBarShowAs.Tag of
          0: ShowMap(OutModelName, OutModelType);
          1: ShowTable(OutModelName, OutModelType);
          2: ShowGraph(OutModelName, OutModelType);
        end;
      end;
    finally
      Free;
    end;
end;


procedure TfmGeoblock.MethodConversionExecute(Sender: TObject);
begin
  with TfmMethodConversion.Create(Self) do
    try
      //Next must be taken from DB browser
      //InModelName := fmMapWindow.Model.ModelName;
      //InModelType := fmMapWindow.Model.ModelType;
      //Then show only one toolbar button for input model of conversion
      if ShowModal = mrOk then
      begin
        Hide;
        case ToolBarShowAs.Tag of
          0: ShowMap(OutModelName, OutModelType);
          1: ShowTable(OutModelName, OutModelType);
          2: ShowGraph(OutModelName, OutModelType);
        end;
      end;
    finally
      Free;
    end;
end;


procedure TfmGeoblock.MethodTransformationExecute(Sender: TObject);
begin
  with TfmMethodTransformation.Create(Self) do
    try
      if fmMapWindow <> nil then
      begin
        InModelType := fmMapWindow.Model.ModelType;
        InModelName := fmMapWindow.Caption;
        ListBoxInputNames.ItemIndex := ListBoxInputNames.Items.IndexOf(InModelName);
      end;

      if ShowModal = mrOk then

      begin
        Hide;
        case ToolBarShowAs.Tag of
          0: ShowMap(OutModelName, OutModelType);
          1: ShowTable(OutModelName, OutModelType);
          2: ShowGraph(OutModelName, OutModelType);
        end;
      end;
    finally
      Free;
    end;
end;


procedure TfmGeoblock.MethodSetOperationsExecute(Sender: TObject);
begin
  with TfmMethodSetOperations.Create(nil) do
    try
      if ShowModal = mrOk then

      begin
        Hide;
        case ToolBarShowAs.Tag of
          0: ShowMap(OutModelName, OutModelType);
          1: ShowTable(OutModelName, OutModelType);
          2: ShowGraph(OutModelName, OutModelType);
        end;
      end;
    finally
      Free;
    end;
end;

procedure TfmGeoblock.MethodSimulationExecute(Sender: TObject);
begin
  with TfmFileNew.Create(Self) do
  try
  if ShowModal = mrOk then
    with dBase.dmBase do
    case ToolBarShowAs.Tag of
      0: ShowMap(TableOutput.TableName, OutModelType);
      1: ShowTable(TableOutput.TableName, OutModelType);
      2: ShowGraph(TableOutput.TableName, OutModelType);
    end;
  finally
    Free;
  end;
end;

procedure TfmGeoblock.OctreeConstructionExecute(Sender: TObject);
begin
  with TfmMethodOctree.Create(nil) do
    try
      if ShowModal = mrOk then
      begin
        Hide;
        case ToolBarShowAs.Tag of
          0: ShowMap(OutModelName, OutModelType);
          1: ShowTable(OutModelName, OutModelType);
          2: ShowGraph(OutModelName, OutModelType);
        end;
      end;
    finally
      Free;
    end;
end;


//_____________________ Map __________________________\\

procedure TfmGeoblock.EnableMapItems(AModelType: integer; AEnabled: boolean);
begin
  //File Items
  FileExport.Enabled := AEnabled;

  //Map Items
  MapOptions.Enabled   := AEnabled;
  MapLegend.Enabled    := AEnabled;
  MapMaterial.Enabled  := AEnabled;
  MapLighting.Enabled  := AEnabled;
  MapBackColor.Enabled := AEnabled;

  //View Items
  ControlBarRight.Visible := AEnabled; // and ViewVolumeXYZ.Checked;

  ViewVolumeXYZ.Enabled := AEnabled;
  ViewPlaneXY.Enabled := AEnabled;
  ViewPlaneXZ.Enabled := AEnabled;
  ViewPlaneYZ.Enabled := AEnabled;
  ViewScale.Enabled := AEnabled;
  ViewSelect.Enabled := AEnabled;
  ViewScroll.Enabled := AEnabled;
  ViewPan.Enabled := AEnabled;

  aZoomInOut.Enabled      := AEnabled;
  aZoomToRectangle.Enabled := AEnabled;
  aZoomToProject.Enabled  := AEnabled;
  aZoomToModel.Enabled    := AEnabled;
  aZoomToComplete.Enabled := AEnabled;


  //Draw Items
  //ToolBarDraw.Visible := AEnabled;

  DrawText.Enabled      := AEnabled;
  DrawSymbol.Enabled    := AEnabled;
  DrawPolygon.Enabled   := AEnabled;
  DrawPolyline.Enabled  := AEnabled;
  DrawRectangle.Enabled := AEnabled;
  DrawEllipse.Enabled   := AEnabled;
  DrawSolid.Enabled     := AEnabled;
  DrawAddNode.Enabled   := AEnabled;
  DrawReshape.Enabled   := AEnabled;
  DrawDepth.Enabled     := AEnabled;

  acAnalyseBaseStatistics.Enabled := AEnabled;

  case AModelType of
    mtDholes, mtAll:
    begin
      PageControlModels.ActivePage := TabSheetHoles;
      DrillholesOptions.Enabled    := AEnabled;
      DrillholesSelectDrillhole.Enabled := AEnabled;
      DrillholesSelectContact.Enabled := AEnabled;
      DrillholesSelectSegment.Enabled := AEnabled;
      DrillholesCreatedrillhole.Enabled := AEnabled;
      DrillholesCreateSegment.Enabled := AEnabled;
      //        ShowVectors.Enabled:=AEnabled;
    end;
    mtPoints2D:
    begin
      PageControlModels.ActivePage := TabSheetPoints2D;
      Points2DOptions.Enabled := AEnabled;
      Points2DSelectPoint.Enabled := AEnabled;
      Points2DCreatePoint.Enabled := AEnabled;
      ShowVectors.Enabled := AEnabled;
    end;
    mtPoints3D:
    begin
      PageControlModels.ActivePage := TabSheetPoints3D;
      Points3DOptions.Enabled := AEnabled;
      Points3DSelectPoint.Enabled := AEnabled;
      Points3DCreatePoint.Enabled := AEnabled;
      ShowVectors.Enabled := AEnabled;
    end;
    mtPolygons:
    begin
      PageControlModels.ActivePage := TabSheetPolygons;
      PolygonsOptions.Enabled      := AEnabled;
      PolygonsSelectPolygon.Enabled := AEnabled;
      PolygonsSelectVertex.Enabled := AEnabled;
      PolygonsCreatePolygon.Enabled := AEnabled;
      PolygonsLinkToSolid.Enabled  := AEnabled;
      try
        PolygonsSelectTypeExecute(nil);
      except
      end;
    end;
    mtTins:
    begin
      PageControlModels.ActivePage := TabSheetTins;
      //ViewGeoScene.Enabled := AEnabled;
      TinOptions.Enabled   := AEnabled;
      ShowContours.Enabled := AEnabled;
      ShowSection.Enabled  := AEnabled;
      ShowVectors.Enabled  := AEnabled;
    end;
    mtSolids:
    begin
      SolidOptions.Enabled := AEnabled;
      ShowSection.Enabled  := AEnabled;
    end;
    mtGrids2D:
    begin
      PageControlModels.ActivePage := TabSheetGrids2D;
      Grid2DOptions.Enabled    := AEnabled;
      Grid2DSelectCell.Enabled := AEnabled;
      Grid2DSelectRow.Enabled  := AEnabled;
      Grid2DSelectCol.Enabled  := AEnabled;
      ShowContours.Enabled     := AEnabled;
      ShowVectors.Enabled      := AEnabled;
    end;
    mtGrids3D:
    begin
      PageControlModels.ActivePage := TabSheetGrids3D;
      Grid3DOptions.Enabled   := AEnabled;
      Grid3DSelectRow.Enabled := AEnabled;
      Grid3DSelectCol.Enabled := AEnabled;
      Grid3DSelectLay.Enabled := AEnabled;
      ShowIsosurface.Enabled  := AEnabled;
      ShowVectors.Enabled     := AEnabled;
      ShowSection.Enabled     := AEnabled;
    end;
    mtMeshes2D:
    begin
      PageControlModels.ActivePage := TabSheetMeshes2D;
      Mesh2DOptions.Enabled := AEnabled;
      ShowContours.Enabled  := AEnabled;
      ShowVectors.Enabled   := AEnabled;
    end;
    mtMeshes3D:
    begin
      PageControlModels.ActivePage := TabSheetMeshes3D;
      Mesh3DOptions.Enabled  := AEnabled;
      ShowContours.Enabled   := AEnabled;
      ShowIsosurface.Enabled := AEnabled;
      ShowVectors.Enabled    := AEnabled;
      ShowSection.Enabled    := AEnabled;
    end;
  end;
end;

procedure TfmGeoblock.MapSceneryExecute(Sender: TObject);
begin
  if fmMapScenery = nil then
    fmMapScenery := TfmMapScenery.Create(self);
  fmMapScenery.Show;
end;

procedure TfmGeoblock.MapOptionsExecute(Sender: TObject);
begin
  with fmMapWindow do
  begin
    Model.SelectOptions;
    FormPaint(fmMapWindow);
  end;
end;

procedure TfmGeoblock.MapLegendExecute(Sender: TObject);
begin
  with fmMapWindow do
  begin
    Model.ActiveAttribute.LegendDialog;
    FormPaint(fmMapWindow);
  end;
end;

procedure TfmGeoblock.MapMaterialExecute(Sender: TObject);
begin
  if (ActiveMDIChild is TfmTableWindow) then
  begin
    with TfmDrawFillStyle.Create(nil) do
      try
        ShowModal;
      finally
        Free;
      end;
  end;
end;

procedure TfmGeoblock.MapLightingExecute(Sender: TObject);
begin
  fmMapWindow.LightDlg;
end;

procedure TfmGeoblock.MapBackColorExecute(Sender: TObject);
begin
  fmMapWindow.BackColorDlg;
  //fmMapScenery.ActionBackgroundExecute(Self);
end;

//________________________ Draw ____________________\\

procedure TfmGeoblock.DrawEnableItems(AEnabled: boolean);
begin
  DrawText.Enabled      := AEnabled;
  DrawSymbol.Enabled    := AEnabled;
  DrawPolyline.Enabled  := AEnabled;
  DrawPolygon.Enabled   := AEnabled;
  DrawEllipse.Enabled   := AEnabled;
  DrawReshape.Enabled   := AEnabled;
  DrawRectangle.Enabled := AEnabled;
  DrawAddNode.Enabled   := AEnabled;
end;

procedure TfmGeoblock.DrawDepthExecute(Sender: TObject);
begin
  with TfmDrawObjectDepth.Create(nil) do
    try
      ShowModal;
    finally
      Free;
    end;
end;


procedure TfmGeoblock.DrawAddNodeExecute(Sender: TObject);
begin
  DrawAddNode.Checked := True;
end;

procedure TfmGeoblock.DrawReshapeExecute(Sender: TObject);
begin
  DrawReshape.Checked := True;
end;

procedure TfmGeoblock.DrawSolidExecute(Sender: TObject);
begin
  DrawSolid.Checked := True;
end;

procedure TfmGeoblock.DrawEllipseExecute(Sender: TObject);
begin
  DrawEllipse.Checked := True;
end;

procedure TfmGeoblock.DrawPolygonExecute(Sender: TObject);
begin
  DrawPolygon.Checked := True;
end;

procedure TfmGeoblock.DrawRectangleExecute(Sender: TObject);
begin
  DrawRectangle.Checked := True;
end;

procedure TfmGeoblock.DrawPolylineExecute(Sender: TObject);
begin
  DrawPolyline.Checked := True;
end;

procedure TfmGeoblock.DrawSymbolExecute(Sender: TObject);
begin
  DrawSymbol.Checked := True;
end;

procedure TfmGeoblock.DrawTextExecute(Sender: TObject);
begin
  DrawText.Checked := True;
end;

//_________________________ View ______________________________\\


procedure TfmGeoblock.ViewProjectManagerExecute(Sender: TObject);
begin
  with fmViewProjectManager do
    try //save previous parameters
      if (fmMapWindow <> nil) and (fmMapWindow.ModelList <> nil) then
        with fmMapWindow do
          ModelList[ModelIndex].Assign(Model);
    except
    end;
  fmViewProjectManager.Show;
end;

{
procedure TfmMain.ViewGeosceneExecute(Sender: TObject);
begin
 with TfmGeoScene.Create(nil) do
    try
      ShowModal;
    finally
      Free;
    end;
end;
}
procedure TfmGeoblock.ViewScaleExecute(Sender: TObject);
begin
  fmMapWindow.ViewScale;
end;

procedure TfmGeoblock.ViewSelectExecute(Sender: TObject);
begin
  fmMapWindow.GBCanvas.Cursor := crSelectCursor;
end;

procedure TfmGeoblock.ViewScrollExecute(Sender: TObject);
begin
  fmMapWindow.GBCanvas.Cursor := crScrollCursor;
end;

procedure TfmGeoblock.ViewPanExecute(Sender: TObject);
begin
  fmMapWindow.GBCanvas.Cursor := crPanCursor;
end;

procedure TfmGeoblock.ViewVolumeXYZExecute(Sender: TObject);
begin
  ControlBarRight.Visible := True;
  Observe3D.Checked := True;
  ViewPan.Enabled   := True;
  fmMapWindow.ViewXYZ;
end;

procedure TfmGeoblock.ViewPlaneXYExecute(Sender: TObject);
begin
  ControlBarRight.Visible := False;
  if ViewPan.Checked then
    ViewSelect.Checked := True;
  ViewPan.Enabled := False;
  fmMapWindow.ViewXY;
end;

procedure TfmGeoblock.ViewPlaneXZExecute(Sender: TObject);
begin
  ControlBarRight.Visible := False;
  if ViewPan.Checked then
    ViewSelect.Checked := True;
  ViewPan.Enabled := False;
  fmMapWindow.ViewXZ;
end;

procedure TfmGeoblock.ViewPlaneYZExecute(Sender: TObject);
begin
  ControlBarRight.Visible := False;
  if ViewPan.Checked then
    ViewSelect.Checked := True;
  ViewPan.Enabled := False;
  fmMapWindow.ViewYZ;
end;

procedure TfmGeoblock.ViewStatuslineExecute(Sender: TObject);
begin
  ViewStatusLine.Checked := not ViewStatusLine.Checked;
  StatusBar.Visible      := ViewStatusLine.Checked;
end;

procedure TfmGeoblock.ViewTableWindowExecute(Sender: TObject);
begin

end;

//_______________________ Zoom ______________________\\

procedure TfmGeoblock.aZoomInOutExecute(Sender: TObject);
begin
  fmMapWindow.GBCanvas.Cursor := crZoomCursor;
end;

procedure TfmGeoblock.aZoomToProjectExecute(Sender: TObject);
begin
  aZoomToProject.Checked := True;
  fmMapWindow.ViewDefault;
end;

procedure TfmGeoblock.aZoomToCompleteExecute(Sender: TObject);
begin
  aZoomToComplete.Checked := True;
  fmMapWindow.ViewZoomToAll;
end;

procedure TfmGeoblock.aZoomToModelExecute(Sender: TObject);
begin
  fmMapWindow.ViewZoomToModel;
end;

procedure TfmGeoblock.aZoomToRectangleExecute(Sender: TObject);
begin

end;

//___________________________ Display ______________________________\\

{ Drillholes }
procedure TfmGeoblock.DrillholesCreateSegmentExecute(Sender: TObject);
begin
  //TODO: fmMapWindow.CreateSegment;
end;

procedure TfmGeoblock.DrillholesSelectDrillholeExecute(Sender: TObject);
begin
  //TODO: fmMapWindow.SelectDhole;
end;

procedure TfmGeoblock.DrillholesSelectContactExecute(Sender: TObject);
begin
  //TODO: fmMapWindow.SelectContact
end;

procedure TfmGeoblock.DrillholesSelectSegmentExecute(Sender: TObject);
begin
  //TODO: fmMapWindow.SelectSegment
end;

procedure TfmGeoblock.DrillholesCreateDrillholeExecute(Sender: TObject);
begin
  //TODO: fmMapWindow.CreateHole
end;


{ Points 2D }
procedure TfmGeoblock.Points2DSelectPointExecute(Sender: TObject);
begin
  //TODO: fmMapWindow.SelectPoint2D;
end;

procedure TfmGeoblock.Points2DCreatePointExecute(Sender: TObject);
begin
  //TODO: fmMapWindow.CreatePoint2D
end;

{ Points 3D }
procedure TfmGeoblock.Points3DSelectPointExecute(Sender: TObject);
begin
  //TODO: fmMapWindow.SelectPoint3D
end;

procedure TfmGeoblock.PolygonsSelectTypeExecute(Sender: TObject);
begin
  //  ComboBoxPolygonType.Items.Clear;
  (*
    GetFieldValues(ExpandPath(DirMaterial)+tblPolyMat+TableExt, fldNAME,
      ComboBoxPolygonType.Items.Append);
    with ComboBoxPolygonType, Items do
    begin
      ItemIndex := ReductionToRange(0,0,Items.Count-1);
    end;
   *)
end;

procedure TfmGeoblock.PolygonsLinkToSolidExecute(Sender: TObject);
begin

end;

{ Grid2D }
procedure TfmGeoblock.Grid2DSelectCellExecute(Sender: TObject);
begin
  fmMapWindow.Grid2DSelectCell;
end;

procedure TfmGeoblock.Grid2DSelectRowExecute(Sender: TObject);
begin
  fmMapWindow.Grid2DSelectRow;
end;

procedure TfmGeoblock.Grid2DSelectColExecute(Sender: TObject);
begin
  fmMapWindow.Grid2DSelectCol;
end;


//_____________________ Show ________________________\\
//                                                   //
procedure TfmGeoblock.ShowSectionExecute(Sender: TObject);
begin
  Screen.Cursor := crSelectCursor;
  try
    fmMapWindow.PerformCrossSection;
  finally
    Screen.Cursor := crDefault;
    fmMapWindow.SetFocus;
  end;
end;

procedure TfmGeoblock.ShowContoursExecute(Sender: TObject);
begin
  with TfmPerformContours.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfmGeoblock.ShowIsosurfaceExecute(Sender: TObject);
begin
  with TfmPerformIsosurfaces.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfmGeoblock.ShowVectorsExecute(Sender: TObject);
begin
  with TfmPerformVectors.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfmGeoblock.ShowFilmExecute(Sender: TObject);
begin
  //fmMapWindow.PerformFilmsDlg;
end;

procedure TfmGeoblock.ShowStatusText(Sender: TObject);
begin
  StatusBar.SimpleText     := Application.Hint;
  StatusBar.Panels[0].Text := Application.Hint;
end;


//________________________ Observe _______________________________\\

procedure TfmGeoblock.ObserveTopExecute(Sender: TObject);
begin
  ObserveTop.Checked := True;
  fmMapWindow.ViewTop;
end;

procedure TfmGeoblock.ObserveBottomExecute(Sender: TObject);
begin
  ObserveBottom.Checked := True;
  fmMapWindow.ViewBottom;
end;

procedure TfmGeoblock.ObserveRightExecute(Sender: TObject);
begin
  ObserveRight.Checked := True;
  fmMapWindow.ViewRight;
end;

procedure TfmGeoblock.ObserveLeftExecute(Sender: TObject);
begin
  ObserveLeft.Checked := True;
  fmMapWindow.ViewLeft;
end;

procedure TfmGeoblock.ObserveBackExecute(Sender: TObject);
begin
  ObserveBack.Checked := True;
  fmMapWindow.ViewBack;
end;

procedure TfmGeoblock.ObserveFrontExecute(Sender: TObject);
begin
  ObserveFront.Checked := True;
  fmMapWindow.ViewFront;
end;

procedure TfmGeoblock.Observe3DExecute(Sender: TObject);
begin
  Observe3D.Checked := True;
  fmMapWindow.View3D;
end;

procedure TfmGeoblock.ObserveRotateExecute(Sender: TObject);
begin
  fmMapWindow.ViewRotate;
end;

procedure TfmGeoblock.ObservePerspectiveExecute(Sender: TObject);
begin
  ObservePerspective.Checked := not ObservePerspective.Checked;
  // Change perspective in MapWindow using OpenGL..
end;

//_______________________Style___________________________\\
procedure TfmGeoblock.StyleTextExecute(Sender: TObject);
begin
  if dmDialogs.FontDialog.Execute then
  begin

  end;
end;

procedure TfmGeoblock.StyleSymbolExecute(Sender: TObject);
begin
  with TfmDrawSymbolStyle.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfmGeoblock.StyleLineExecute(Sender: TObject);
begin
  with TfmDrawLineStyle.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfmGeoblock.StyleFillExecute(Sender: TObject);
begin
  with TfmDrawFillStyle.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;


//_____________________ Analyse _______________________\\
procedure TfmGeoblock.acAnalyseProblemBookExecute(Sender: TObject);
begin
  with TfmAnalyseProblems.Create(nil) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfmGeoblock.acAnalyseVolumeCalculationExecute(Sender: TObject);
begin
  // Now only interactive calculation of a picked map object at MapWindow
end;

procedure TfmGeoblock.acAnalyseReserveCalculationExecute(Sender: TObject);
begin
  with TfmAnalyseReserves.Create(Self) do
    try
      if ShowModal = mrOk then

      begin
        ShowTable(OutModelName, mtUnknown);
      end;
    finally
      Free;
    end;
end;

procedure TfmGeoblock.acAnalyseBaseStatisticsExecute(Sender: TObject);
var
  FileName: TFileName;
begin
  if (ActiveMDIChild is TfmTableWindow) then
    FileName := (ActiveMDIChild as TfmTableWindow).TableMaster.TableName
  else if (ActiveMDIChild is TfmMapWindow) then
    FileName := (ActiveMDIChild as TfmMapWindow).Model.ModelName;

  with TfmGraphWindow.Create(Self) do
    try
      OpenTable(FileName);
      Show;
    except
      Free;
    end;
end;

procedure TfmGeoblock.acAnalyseVariogramsExecute(Sender: TObject);
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
      end;
    finally
      Free;
    end;
end;

//_____________________ Tools _______________________\\

procedure TfmGeoblock.ToolsConfigurationExecute(Sender: TObject);
begin
  with TfmToolsConfiguration.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfmGeoblock.ToolsStandardStyleExecute(Sender: TObject);
begin
  ActionManager.Style := StandardStyle;
end;


procedure TfmGeoblock.ToolsXPStyleExecute(Sender: TObject);
begin
  ActionManager.Style := XPStyle;
  ActionMainMenuBar.Shadows := True;
end;

procedure TfmGeoblock.ToolsUnitsConverterExecute(Sender: TObject);
begin
  with TfmToolsUnitsConverter.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

//_______________________ Calculator ________________________\\
procedure TfmGeoblock.CalculatorGeologyExecute(Sender: TObject);
begin
  with TfmToolsGeologyCalculator.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfmGeoblock.CalculatorGeometryExecute(Sender: TObject);
begin
  with TfmToolsGeometryCalculator.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfmGeoblock.CalculatorMiningExecute(Sender: TObject);
begin
  with TfmToolsMiningCalculator.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfmGeoblock.CalculatorSurveyExecute(Sender: TObject);
begin
  with TfmToolsSurveyCalculator.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;


//________________________Window_________________________\\
procedure TfmGeoblock.WindowRedrawExecute(Sender: TObject);
begin
  if fmMapWindow <> nil then
    fmMapWindow.Repaint;
  if fmTableWindow <> nil then
    fmTableWindow.Repaint;
  if fmGraphWindow <> nil then
    fmGraphWindow.Repaint;
end;

//_______________________ Help ________________________\\

procedure TfmGeoblock.HelpContentsExecute(Sender: TObject);
begin
  Application.HelpShowTableOfContents;
end;

procedure TfmGeoblock.HelpGlossaryExecute(Sender: TObject);
begin
  Application.HelpContext(HelpGlossary.HelpContext);
end;

procedure TfmGeoblock.HelpAboutExecute(Sender: TObject);
begin
  with TfmHelpAbout.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TfmGeoblock.ReadIniFile;
var
  StyleID: integer;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      Top  := ReadInteger(Name, 'Top', 100);
      Left := ReadInteger(Name, 'Left', 200);
      if ReadBool(Name, 'InitMax', False) then
        WindowState := wsMaximized
      else
        WindowState := wsNormal;
      StyleID := ReadInteger(Name, 'StyleID', 0);
      if StyleID = 0 then
      begin
        ActionManager.Style := StandardStyle;
        ToolsStandardStyle.Checked := True;
      end
      else
      begin
        ActionManager.Style  := XPStyle;
        ToolsXPStyle.Checked := True;
      end
    finally
      IniFile.Free;
    end;
end;

procedure TfmGeoblock.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteInteger(Name, 'Top', Top);
      WriteInteger(Name, 'Left', Left);
      WriteBool(Name, 'InitMax', WindowState = wsMaximized);

      if ActionManager.Style = StandardStyle then
        WriteInteger(Name, 'StyleID', 0)
      else
        WriteInteger(Name, 'StyleID', 1);
    finally
      IniFile.Free;
    end;
end;

procedure TfmGeoblock.FormClose(Sender: TObject; var Action: TCloseAction);
var
  FileName: TFileName;

begin
  try
    FileName := ExpandPath(DirProject);
    fmViewProjectManager.SaveToFile(FileName + 'Geoblock.prj');
  except
  end;
  WriteIniFile;
  inherited;
end;

procedure TfmGeoblock.FormDestroy(Sender: TObject);
begin
  Screen.OnActiveFormChange := nil;
end;

end.
