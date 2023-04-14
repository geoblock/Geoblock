//----------------------------------------------------------------------------
// The unit of Geoblock Project, http://sourceforge.net/projects/geoblock
//
// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
// the specific language governing rights and limitations under the License.
//
// The initial developer of the original code is Getos Ltd., Copyright (c) 1997.
// Portions created by contributors are documented in an accompanying history log
// and Copyright (c) of these contributors, 1997-2023. All Rights Reserved.
//---------------------------------------------------------------------------


program Geoblock;

{.$R 'Source\Interface\Gb.res'}

uses
  Forms,
  gnuGettext in 'Source\Code\gnuGettext.pas',
  dBase in 'Source\Interface\dBase.pas' {dmBase: TDataModule},
  dDialogs in 'Source\Interface\dDialogs.pas' {dmDialogs: TDataModule},
  fInitialDialog in 'Source\Interface\fInitialDialog.pas' {fmInitialDialog},
  fMethodDialog in 'Source\Interface\fMethodDialog.pas' {fmMethodDialog},
  fMethodDualDialog in 'Source\Interface\fMethodDualDialog.pas' {fmMethodDualDialog},
  fOptionDialog in 'Source\Interface\fOptionDialog.pas' {fmOptionDialog},
  fFileOpenDialog in 'Source\Interface\fFileOpenDialog.pas' {fmFileOpenDialog},
  fPageDialog in 'Source\Interface\fPageDialog.pas' {fmPageDialog},
  fPageTreeDialog in 'Source\Interface\fPageTreeDialog.pas' {fmPageTreeDialog},
  fFileEditGridPars in 'Source\Interface\fFileEditGridPars.pas' {fmFileEditGridPars},
  fGeoblock in 'Source\Interface\fGeoblock.pas' {fmGeoblock},
  fMethodEvaluation in 'Source\Interface\fMethodEvaluation.pas' {fmMethodEvaluation},
  fHelpAbout in 'Source\Interface\fHelpAbout.pas' {fmHelpAbout},
  fDrawFillStyle in 'Source\Interface\fDrawFillStyle.pas' {fmDrawFillStyle},
  fDisplayGrid3DOptions in 'Source\Interface\fDisplayGrid3DOptions.pas' {fmDisplayGrid3DOptions},
  fComposeLinearReserves in 'Source\Interface\fComposeLinearReserves.pas' {fmComposeLinearReserves},
  fDisplayTinOptions in 'Source\Interface\fDisplayTinOptions.pas' {fmDisplayTinOptions},
  fDisplayPoints2DOptions in 'Source\Interface\fDisplayPoints2DOptions.pas' {fmDisplayPoints2DOptions},
  fDisplayPoints3DOptions in 'Source\Interface\fDisplayPoints3DOptions.pas' {fmDisplayPoints3DOptions},
  fDisplayGrid2DOptions in 'Source\Interface\fDisplayGrid2DOptions.pas' {fmDisplayGrid2DOptions},
  fDisplayMesh3DOptions in 'Source\Interface\fDisplayMesh3DOptions.pas' {fmDisplayMesh3DOptions},
  fDisplayMesh2DOptions in 'Source\Interface\fDisplayMesh2DOptions.pas' {fmDisplayMesh2DOptions},
  fComposeByHorizons in 'Source\Interface\fComposeByHorizons.pas' {fmComposeByHorizons},
  fDisplayHolesOptions in 'Source\Interface\fDisplayHolesOptions.pas' {fmDisplayHolesOptions},
  fDisplayPolygonsOptions in 'Source\Interface\fDisplayPolygonsOptions.pas' {fmDisplayPolygonsOptions},
  fPerformVectors in 'Source\Interface\fPerformVectors.pas' {fmPerformVectors},
  fPerformContours in 'Source\Interface\fPerformContours.pas' {fmPerformContours},
  fPerformIsosurfaces in 'Source\Interface\fPerformIsosurfaces.pas' {fmPerformIsosurfaces},
  fComposeOreIntervals in 'Source\Interface\fComposeOreIntervals.pas' {fmComposeOreIntervals},
  fDrawLineStyle in 'Source\Interface\fDrawLineStyle.pas' {fmDrawLineStyle},
  fMethodGridGeneration in 'Source\Interface\fMethodGridGeneration.pas' {fmMethodGridGeneration},
  fToolsUnitsConverter in 'Source\Interface\fToolsUnitsConverter.pas' {fmToolsUnitsConverter},
  fMethodTriangulation in 'Source\Interface\fMethodTriangulation.pas' {fmMethodTriangulation},
  fReserveCutOptions in 'Source\Interface\fReserveCutOptions.pas' {fmReserveCutOptions},
  fDrawObjectDepth in 'Source\Interface\fDrawObjectDepth.pas' {fmDrawObjectDepth},
  fEditCalcField in 'Source\Interface\fEditCalcField.pas' {fmEditCalcField},
  fEditDeleteField in 'Source\Interface\fEditDeleteField.pas' {fmEditDeleteField},
  fEditMemoField in 'Source\Interface\fEditMemoField.pas' {fmEditMemoField},
  fEditLookupField in 'Source\Interface\fEditLookupField.pas' {fmEditLookupField},
  fMethodInterpolation in 'Source\Interface\fMethodInterpolation.pas' {fmMethodInterpolation},
  fInterKriging in 'Source\Interface\fInterKriging.pas' {fmInterKriging},
  fInterPolynomRegression in 'Source\Interface\fInterPolynomRegression.pas' {fmInterPolynomialRegression},
  fDisplaySolidOptions in 'Source\Interface\fDisplaySolidOptions.pas' {fmDisplaySolidsOptions},
  fMapLight in 'Source\Interface\fMapLight.pas' {fmMapLighting},
  fAnalyseReserves in 'Source\Interface\fAnalyseReserves.pas' {fmAnalyseReserves},
  fFileImageRegistration in 'Source\Interface\fFileImageRegistration.pas' {fmFileImageRegistration},
  fRecordEditor in 'Source\Interface\fRecordEditor.pas' {fmRecordEditor},
  fMethodSetOperations in 'Source\Interface\fMethodSetOperations.pas' {fmMethodSetOperations},
  fComposeCenters in 'Source\Interface\fComposeCenters.pas' {fmComposeCenters},
  fViewRotate in 'Source\Interface\fViewRotate.pas' {fmViewRotate},
  fEditRenField in 'Source\Interface\fEditRenField.pas' {fmEditRenField},
  fViewScale in 'Source\Interface\fViewScale.pas' {fmViewScale},
  fEditAddField in 'Source\Interface\fEditAddField.pas' {fmEditAddField},
  fFileExport in 'Source\Interface\fFileExport.pas' {fmFileExport},
  fFileEditWhittlePars in 'Source\Interface\fFileEditWhittlePars.pas' {fmFileEditWhittlePars},
  fInfoDialog in 'Source\Interface\fInfoDialog.pas' {fmInfoDialog},
  fFileImport in 'Source\Interface\fFileImport.pas' {fmFileImport},
  fEditQuery in 'Source\Interface\fEditQuery.pas' {fmEditQuery},
  fEditGetStatist in 'Source\Interface\fEditGetStatist.pas' {fmEditGetStatist},
  fToolsSurveyCalculator in 'Source\Interface\fToolsSurveyCalculator.pas' {fmToolsSurveyCalculator},
  fAnalyseVariograms in 'Source\Interface\fAnalyseVariograms.pas' {fmAnalyseVariograms},
  fMapLegend in 'Source\Interface\fMapLegend.pas' {fmMapLegend},
  fEditFileXML in 'Source\Interface\fEditFileXML.pas' {fmEditFileXML},
  fToolsGeometryCalculator in 'Source\Interface\fToolsGeometryCalculator.pas' {fmToolsGeometryCalculator},
  fToolsGeologyCalculator in 'Source\Interface\fToolsGeologyCalculator.pas' {fmToolsGeologyCalculator},
  fComposeContacts in 'Source\Interface\fComposeContacts.pas' {fmComposeContacts},
  fToolsConfiguration in 'Source\Interface\fToolsConfiguration.pas' {fmToolsConfiguration},
  fToolsMiningCalculator in 'Source\Interface\fToolsMiningCalculator.pas' {fmToolsMiningCalculator},
  fViewProjectManager in 'Source\Interface\fViewProjectManager.pas' {fmViewProjectManager},
  fStartup in 'Source\Interface\fStartup.pas' {fmStartup},
  iReinit in 'Source\Interface\iReinit.pas',
  fMethodConversion in 'Source\Interface\fMethodConversion.pas' {fmMethodConversion},
  fMethodTransformation in 'Source\Interface\fMethodTransformation.pas' {fmMethodTransformation},
  fMapWindow in 'Source\Interface\fMapWindow.pas' {fmMapWindow},
  fTableWindow in 'Source\Interface\fTableWindow.pas' {fmTableWindow},
  fGraphWindow in 'Source\Interface\fGraphWindow.pas' {fmGraphWindow},
  fDrawImageEditor in 'Source\Interface\fDrawImageEditor.pas' {fmDrawImageEditor},
  fDrawSymbolStyle in 'Source\Interface\fDrawSymbolStyle.pas' {fmDrawSymbolStyle},
  fFileOpenText in 'Source\Interface\fFileOpenText.pas' {fmFileOpenText},
  fAnalyseProblems in 'Source\Interface\fAnalyseProblems.pas' {fmAnalyseProblems},
  fComposeOreSorts in 'Source\Interface\fComposeOreSorts.pas' {fmComposeOreSorts},
  fMethodVarioModeller in 'Source\Interface\fMethodVarioModeller.pas' {fmMethodVarioModeller},
  fViewVariogram in 'Source\Interface\fViewVariogram.pas' {fmViewVariogram},
  fMapScenery in 'Source\Interface\fMapScenery.pas' {fmMapScenery},
  fFileDataBrowser in 'Source\Interface\fFileDataBrowser.pas' {fmFileDataBrowser},
  fTerraContours in 'Source\Interface\fTerraContours.pas' {fmGSContours},
  fTerraScene in 'Source\Interface\fTerraScene.pas' {fmGeoScene},
  fTerraSplash in 'Source\Interface\fTerraSplash.pas' {fmGS_Splash},
  fFileOpenModel in 'Source\Interface\fFileOpenModel.pas' {fmFileOpenModel},
  fMethodPitOptimization in 'Source\Interface\fMethodPitOptimization.pas' {fmMethodPitOptimization},
  fMethodPrediction in 'Source\Interface\fMethodPrediction.pas',
  fEditRecord in 'Source\Interface\fEditRecord.pas' {fmInitialDialog1},
  fEditVariogram in 'Source\Interface\fEditVariogram.pas' {fmEditVariogram},
  fInterLinear in 'Source\Interface\fInterLinear.pas' {fmInterLinear},
  fViewHorizon in 'Source\Interface\fViewHorizon.pas' {fmViewHorizon},
  fViewDataVisualizer in 'Source\Interface\fViewDataVisualizer.pas' {fmInitialForm1},
  cResStrings in 'Source\Code\cResStrings.pas',
  cGlobals in 'Source\Code\cGlobals.pas',
  uCommon in 'Source\Code\uCommon.pas',
  cInterpol in 'Source\Code\cInterpol.pas',
  uPluginMng in 'Source\Code\uPluginMng.pas',
  uPluginReg in 'Source\Code\uPluginReg.pas',
  cProfuns in 'Source\Code\cProfuns.pas',
  cSorting in 'Source\Code\cSorting.pas',
  uVariograms in 'Source\Code\uVariograms.pas',
  uDelaunay2D in 'Source\Code\uDelaunay2D.pas',
  uIOPoly in 'Source\Code\uIOPoly.pas',
  uDrawVor in 'Source\Code\uDrawVor.pas',
  uInverseDistance in 'Source\Code\uInverseDistance.pas',
  cSuperblock in 'Source\Code\cSuperblock.pas',
  uLinearByTin in 'Source\Code\uLinearByTin.pas',
  uNaturalNeighbors in 'Source\Code\uNaturalNeighbors.pas',
  uKriging in 'Source\Code\uKriging.pas',
  uClosestPointInt in 'Source\Code\uClosestPointInt.pas',
  uPolynomialRegression in 'Source\Code\uPolynomialRegression.pas',
  uDelaunay3D in 'Source\Code\uDelaunay3D.pas',
  uObjects3D in 'Source\Code\uObjects3D.pas',
  uTerraLayers in 'Source\Code\uTerraLayers.pas',
  uTerraModel in 'Source\Code\uTerraModel.pas',
  uTerraObjects in 'Source\Code\uTerraObjects.pas',
  uTerraSound in 'Source\Code\uTerraSound.pas',
  uModels in 'Source\Code\uModels.pas',
  uOptimizeLG in 'Source\Code\uOptimizeLG.pas',
  uFileCreator in 'Source\Code\uFileCreator.pas',
  uTerraLoader in 'Source\Code\uTerraLoader.pas',
  uWhittle in 'Source\Code\uWhittle.pas',
  cDiscoMetric in 'Source\Code\cDiscoMetric.pas',
  cDiscoCore in 'Source\Code\cDiscoCore.pas',
  cDiscoPoly in 'Source\Code\cDiscoPoly.pas',
  uTerraBalloon in 'Source\Code\uTerraBalloon.pas',
  iToolsGBA in 'Source\Interface\iToolsGBA.pas',
  fTerraSceneVR in 'Source\Interface\fTerraSceneVR.pas' {fmSceneVR},
  uOptimizePF in 'Source\Code\uOptimizePF.pas',
  fMethodOctree in 'Source\Interface\fMethodOctree.pas' {fmMethodOctree},
  fMethodSimulation in 'Source\Interface\fMethodSimulation.pas' {fmFileNew},
  fInitialForm in 'Source\Interface\fInitialForm.pas' {fmInitialForm};

{$R *.RES}
{$SetPEFlags $20}  // Allows up to 4GB address space with FastMM

begin
//  TStyleManager.TrySetStyle('Aqua Light Slate');
//    LogoShow;
  InitGeneralRegistry;
  InitCursors;
  InitLanguage;

  Application.Title := 'Geoblock';
  Application.CreateForm(TfmGeoblock, fmGeoblock);
  Application.CreateForm(TfmViewProjectManager, fmViewProjectManager);
  Application.CreateForm(TdmDialogs, dmDialogs);
  Application.CreateForm(TdmBase, dmBase);
  Application.CreateForm(TfmFileNew, fmFileNew);
  Application.CreateForm(TfmInitialForm, fmInitialForm);
  Application.CreateForm(TfmFileDataBrowser, fmFileDataBrowser);
  fmGeoblock.Visible := True;
//   LogoClose;
  Application.Run;
end.
