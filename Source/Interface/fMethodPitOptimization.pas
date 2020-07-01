//----------------------------------------------------------------------------
// The unit of Geoblock, http://sourceforge.net/projects/geoblock
//---------------------------------------------------------------------------

unit fMethodPitOptimization;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.IniFiles,
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

  fMethodDialog,
  uGlobals,
  uInterpol,
  uOptimizePF,
  uOptimizeLG;

type
  TfmMethodPitOptimization = class(TfmMethodDialog)
    RadioGroupMethod: TRadioGroup;
    procedure RadioGroupMethodClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonOKClick(Sender: TObject);
    procedure ListBoxInputNamesClick(Sender: TObject);
  private
    S1, S2: String;
    procedure Optimize;
    procedure ReadIniFile;
    procedure WriteIniFile;
  public
  end;

var
  fmMethodPitOptimization: TfmMethodPitOptimization;

//=========================================================================
implementation
//=========================================================================

{$R *.dfm}

const
  // The methods of optimizations - itemIndexes for RadioGroupMethods.Items
  opFloatingCone = 0;       //FC
  opLerchsGrossmann = 1;    //LG
  opMaxPseudoFlow = 2;      //PF
  opGeneticAlgorithm = 3;   //GA
  opAntColony = 4;          //AC
  opParticleSwarm = 5;      //PS

var
  aInBlocks, aOutBlocks: TCoordinateArray;
  aParams : TInvDistPars;


//----------------------------
{ TfmMethodPitOptimization }
//----------------------------

procedure TfmMethodPitOptimization.Optimize;
begin
  case RadioGroupMethod.ItemIndex of
    opFloatingCone: ;      //OptimizeFC();
    opLerchsGrossmann: OptimizeLG(aInBlocks, aOutBlocks, aParams, ProgressBar);
    opMaxPseudoFlow: OptimizePF(aInBlocks, aOutBlocks, aParams, ProgressBar);
    opGeneticAlgorithm:;   //OptimizeGA();
    opAntColony:;          //OptimizeAC();
    opParticleSwarm:;      //OptimizePS();
  end;
end;

procedure TfmMethodPitOptimization.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
  ToolButtonGrids3D.Click;
  ListBoxInputNamesClick(Self);
  RadioGroupMethodClick(Self);
end;

procedure TfmMethodPitOptimization.ListBoxInputNamesClick(Sender: TObject);
begin
  inherited;
  ListBoxRealAttributeClick(Self);
  S1 := EditOutName.Text;
  S2 := EditOutName.Text;
end;

procedure TfmMethodPitOptimization.RadioGroupMethodClick(Sender: TObject);
begin
  S2 := S1;
  case RadioGroupMethod.ItemIndex of
    opFloatingCone: S2 := S2 + '_' + 'FC';
    opLerchsGrossmann: S2 := S2 + '_' + 'LG';
    opMaxPseudoFlow: S2 := S2 + '_' + 'PF';
    opGeneticAlgorithm: S2 := S2 + '_' + 'GA';
    opAntColony: S2 := S2 + '_' + 'AC';
    opParticleSwarm: S2 := S2 + '_' + 'PS';
  end;
  EditOutName.Text := S2;
end;

procedure TfmMethodPitOptimization.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
//      EditScaleDoubleOre.AsDouble := ReadFloat(Name, LabelValuableOre.Caption, 80);
    finally
      IniFile.Free;
    end;
end;

procedure TfmMethodPitOptimization.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
//      WriteFloat(Name, LabelValuableOre.Caption, EditScaleDoubleOre.AsDouble);
    finally
      IniFile.Free;
    end;
end;

procedure TfmMethodPitOptimization.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;

procedure TfmMethodPitOptimization.ButtonOKClick(Sender: TObject);
begin
  inherited;
  Optimize;
end;

end.
