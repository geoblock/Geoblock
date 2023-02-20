//------------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//------------------------------------------------------------------------------
{
  The dialog for evaluation of block values
}

unit fMethodEvaluation;

interface

uses
  System.SysUtils, 
  System.Classes,
  System.IniFiles,
  System.ImageList,
  Vcl.ImgList,
  Vcl.Graphics,
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs,
  Vcl.StdCtrls, 
  Vcl.ExtCtrls, 
  Vcl.ComCtrls, 
  Vcl.Buttons, 
  Vcl.ToolWin, 
  Vcl.Menus,
  //DB
  Data.DB, 
  Bde.DBTables,
  
  fMethodDialog,
  GBEditRange, 
  GBEditValue, 
  dBase;

type
  TfmMethodEvaluation = class(TfmMethodDialog)
    GroupBoxRecovery: TGroupBox;
    RadioButtonConstant: TRadioButton;
    RadioButtonRecoveryField: TRadioButton;
    EditScaleDoubleRecovery: TGBEditScaleDouble;
    GroupBoxMiningCosts: TGroupBox;
    LabelValuableOre: TLabel;
    LabelWasteRocks: TLabel;
    EditScaleDoubleOre: TGBEditScaleDouble;
    EditScaleDoubleWaste: TGBEditScaleDouble;
    GroupBoxDensity: TGroupBox;
    RadioButtonDensityConstant: TRadioButton;
    RadioButtonDensityField: TRadioButton;
    EditScaleDoubleDensityConst: TGBEditScaleDouble;
    GroupBoxComponent: TGroupBox;
    LabelCutoff: TLabel;
    LabelPrice: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    GBEditValuePrice: TGBEditValue;
    MainMenu1: TMainMenu;
    GBEditScaleDoubleCutoff: TGBEditScaleDouble;
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure Evaluate;
    procedure ReadIniFile;
    procedure WriteIniFile;
  public
     
  end;

var
  fmMethodEvaluation: TfmMethodEvaluation;

//========================================================================
implementation
//========================================================================

uses
  uCommon,
  cGlobals;

{$R *.DFM}


procedure TfmMethodEvaluation.Evaluate;
var
  Query: TQuery;
  I:     integer;
  XO, YO, ZO, DX, DY, DZ: double;
  NX, NY, NZ: integer;
  Volume, Density, Grade, BlockMass, OreMass, WasteMass, MarketPrice,
  Recovery, Metal: double;
  Income, Expense, OreCost, WasteCost, Value: double;

begin
  with dmBase do
    try
      if ActiveAttribute <> '' then
        if ReadParFile(TableInput.TableName, XO, YO, ZO, DX, DY, DZ, NX, NY, NZ)
        then
        begin
          TableInput.Open;
          Query := TQuery.Create(Self);
          try
            if TableInput.FindField('VALUE_' + ActiveAttribute) <> nil then
            begin
              Query.Close;
              Query.SQL.Clear;
              Query.SQL.Add('ALTER TABLE "' + TableInput.TableName +
                '"' + ' DROP "' + TableInput.TableName +
                '"."VALUE_' + ActiveAttribute + '"');
              TableInput.Close;
              Query.ExecSQL;
            end;

            TableInput.Close;
            Query.Close;
            Query.SQL.Clear;
            Query.SQL.Add('ALTER TABLE "' + TableInput.TableName +
              '"' + ' ADD "' + TableInput.TableName + '"."VALUE_' +
              ActiveAttribute + '" MONEY');
            Query.ExecSQL;
          finally
            Query.Free;
          end;
          TableInput.Open;
          ProgressBar.Min := 1;
          ProgressBar.Max := TableInput.RecordCount;
          ProgressBar.Position := 0;
          Recovery    := EditScaleDoubleRecovery.Value;
          Volume      := DX * DY * DZ;
          Density     := EditScaleDoubleDensityConst.Value;
          MarketPrice := GBEditValuePrice.AsDouble;
          OreCost     := EditScaleDoubleOre.Value;
          WasteCost   := EditScaleDoubleWaste.Value;
          for I := 1 to TableInput.RecordCount do
          begin
            if I mod 100 = 0 then
              ProgressBar.Position := I;
            Grade := TableInput.FieldByName(ActiveAttribute).AsFloat;
            TableInput.Edit;

            BlockMass := Volume * Density;

            if TableInput[ActiveAttribute] < GBEditScaleDoubleCutoff.AsDouble then
              OreMass := 0
            else
              OreMass := BlockMass;

            WasteMass := BlockMass - OreMass;
            Metal     := (Grade * 0.01) * BlockMass * (Recovery * 0.01);
            Income    := Metal * MarketPrice;
            Expense   := OreMass * OreCost + WasteMass * WasteCost;
            Value     := Income - Expense;
            TableInput[fldG + '_' + ActiveAttribute] := Value;
            TableInput.Post;
            TableInput.Next;
          end;
        end;
    finally
      TableInput.Close;
    end;
end;

procedure TfmMethodEvaluation.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
  ToolButtonGrids3D.Click;
end;

procedure TfmMethodEvaluation.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      EditScaleDoubleOre.AsDouble := ReadFloat(Name, LabelValuableOre.Caption, 80);
      EditScaleDoubleWaste.AsDouble := ReadFloat(Name, LabelWasteRocks.Caption, 20);
      GBEditScaleDoubleCutoff.AsDouble := ReadFloat(Name, LabelCutoff.Caption, 12);
      GBEditValuePrice.AsDouble := ReadFloat(Name, LabelPrice.Caption, 500);
    finally
      IniFile.Free;
    end;
end;

procedure TfmMethodEvaluation.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteFloat(Name, LabelValuableOre.Caption, EditScaleDoubleOre.AsDouble);
      WriteFloat(Name, LabelWasteRocks.Caption, EditScaleDoubleWaste.AsDouble);
      WriteFloat(Name, LabelCutoff.Caption, GBEditScaleDoubleCutoff.AsDouble);
      WriteFloat(Name, LabelPrice.Caption, GBEditValuePrice.AsDouble);
    finally
      IniFile.Free;
    end;
end;

procedure TfmMethodEvaluation.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;

procedure TfmMethodEvaluation.ButtonOKClick(Sender: TObject);
begin
  inherited;
  Evaluate;
end;

end.
