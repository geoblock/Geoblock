// ----------------------------------------------------------------------------
// Periodic Table
// The Plugin for Geoblock project http://sourceforge.net/projects/geoblock
// ----------------------------------------------------------------------------
{ ! The Main Form for Periodic Table plugin }

unit fMendeleev;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Win.Registry,
  System.SysUtils,
  System.Classes,
  System.IniFiles,
  System.UITypes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Mask,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Buttons,
  Vcl.ComCtrls,
  Data.Db,
  Bde.DBTables,
  Vcl.DBCtrls;

type
  TfmMendeleev = class(TForm)
    PanelPeriodicTable: TPanel;
    Panel1: TPanel;
    LabelName: TLabel;
    LabelAtomicNumber: TLabel;
    LabelAtomicWeight: TLabel;
    LabelState: TLabel;
    LabelMeltingPoint: TLabel;
    LabelBoilingPoint: TLabel;
    LabelDensity: TLabel;
    TableElements: TTable;
    DBRichEditDescription: TDBRichEdit;
    DataSourceElements: TDataSource;
    DBEditWeight: TDBEdit;
    DBEditState: TDBEdit;
    DBEditMeltingPoint: TDBEdit;
    DBEditBoilingPoint: TDBEdit;
    DBEditDensity: TDBEdit;
    DBEditName: TDBEdit;
    DBEditAtomicNumber: TDBEdit;
    LabelDescription: TLabel;
    DBListBoxMainMinerals: TDBListBox;
    LabelMainMinerals: TLabel;
    LabelValence: TLabel;
    DBEditValence: TDBEdit;
    PanelTable: TPanel;
    SpeedButtonH: TSpeedButton;
    LabelIA: TLabel;
    LabelIIA: TLabel;
    LabelIIIB: TLabel;
    LabelIIIA: TLabel;
    LabelVIIIA: TLabel;
    LabelVIIA: TLabel;
    LabelLanthanide: TLabel;
    LabelActinide: TLabel;
    LabelIVB: TLabel;
    LabelVB: TLabel;
    Label1VIB: TLabel;
    LabelVIIB: TLabel;
    LabelIIB: TLabel;
    Label1IB: TLabel;
    LabelIVA: TLabel;
    LabelVA: TLabel;
    LabelVIA: TLabel;
    SpeedButtonLi: TSpeedButton;
    SpeedButtonNa: TSpeedButton;
    SpeedButtonK: TSpeedButton;
    SpeedButtonRb: TSpeedButton;
    SpeedButtonCs: TSpeedButton;
    SpeedButtonFr: TSpeedButton;
    SpeedButtonBe: TSpeedButton;
    SpeedButtonMg: TSpeedButton;
    SpeedButtonCa: TSpeedButton;
    SpeedButtonSr: TSpeedButton;
    SpeedButtonBa: TSpeedButton;
    SpeedButtonRa: TSpeedButton;
    SpeedButtonSc: TSpeedButton;
    SpeedButtonY: TSpeedButton;
    SpeedButtonLa: TSpeedButton;
    SpeedButtonAc: TSpeedButton;
    SpeedButtonTi: TSpeedButton;
    SpeedButtonZr: TSpeedButton;
    SpeedButtonHf: TSpeedButton;
    SpeedButtonDb: TSpeedButton;
    SpeedButtonV: TSpeedButton;
    SpeedButtonNb: TSpeedButton;
    SpeedButtonTa: TSpeedButton;
    SpeedButtonJl: TSpeedButton;
    SpeedButtonCr: TSpeedButton;
    SpeedButtonMo: TSpeedButton;
    SpeedButtonW: TSpeedButton;
    SpeedButtonMn: TSpeedButton;
    SpeedButtonTc: TSpeedButton;
    SpeedButtonRe: TSpeedButton;
    SpeedButtonFe: TSpeedButton;
    SpeedButtonRu: TSpeedButton;
    SpeedButtonOs: TSpeedButton;
    SpeedButtonCo: TSpeedButton;
    SpeedButtonRh: TSpeedButton;
    SpeedButtonIr: TSpeedButton;
    SpeedButtonNi: TSpeedButton;
    SpeedButtonPd: TSpeedButton;
    SpeedButtonPt: TSpeedButton;
    SpeedButtonCu: TSpeedButton;
    SpeedButtonAg: TSpeedButton;
    SpeedButtonAu: TSpeedButton;
    SpeedButtonZn: TSpeedButton;
    SpeedButtonCd: TSpeedButton;
    SpeedButtonHg: TSpeedButton;
    SpeedButtonB: TSpeedButton;
    SpeedButtonAl: TSpeedButton;
    SpeedButtonGa: TSpeedButton;
    SpeedButtonIn: TSpeedButton;
    SpeedButtonTl: TSpeedButton;
    SpeedButtonC: TSpeedButton;
    SpeedButtonSi: TSpeedButton;
    SpeedButtonGe: TSpeedButton;
    SpeedButtonSn: TSpeedButton;
    SpeedButtonPb: TSpeedButton;
    SpeedButtonN: TSpeedButton;
    SpeedButtonP: TSpeedButton;
    SpeedButtonAs: TSpeedButton;
    SpeedButtonSb: TSpeedButton;
    SpeedButtonBi: TSpeedButton;
    SpeedButtonO: TSpeedButton;
    SpeedButtonS: TSpeedButton;
    SpeedButtonSe: TSpeedButton;
    SpeedButtonTe: TSpeedButton;
    SpeedButtonPo: TSpeedButton;
    SpeedButtonF: TSpeedButton;
    SpeedButtonCl: TSpeedButton;
    SpeedButtonBr: TSpeedButton;
    SpeedButtonI: TSpeedButton;
    SpeedButtonAt: TSpeedButton;
    SpeedButtonHe: TSpeedButton;
    SpeedButtonNe: TSpeedButton;
    SpeedButtonAr: TSpeedButton;
    SpeedButtonKr: TSpeedButton;
    SpeedButtonXe: TSpeedButton;
    SpeedButtonRn: TSpeedButton;
    SpeedButtonCe: TSpeedButton;
    SpeedButtonTh: TSpeedButton;
    SpeedButtonPr: TSpeedButton;
    SpeedButtonPa: TSpeedButton;
    SpeedButtonNd: TSpeedButton;
    SpeedButtonU: TSpeedButton;
    SpeedButtonPm: TSpeedButton;
    SpeedButtonNp: TSpeedButton;
    SpeedButtonSm: TSpeedButton;
    SpeedButtonPu: TSpeedButton;
    SpeedButtonEu: TSpeedButton;
    SpeedButtonAm: TSpeedButton;
    SpeedButtonGd: TSpeedButton;
    SpeedButtonCm: TSpeedButton;
    SpeedButtonTb: TSpeedButton;
    SpeedButtonBk: TSpeedButton;
    SpeedButtonDy: TSpeedButton;
    SpeedButtonCf: TSpeedButton;
    SpeedButtonHo: TSpeedButton;
    SpeedButtonEs: TSpeedButton;
    SpeedButtonEr: TSpeedButton;
    SpeedButtonFm: TSpeedButton;
    SpeedButtonTm: TSpeedButton;
    SpeedButtonMd: TSpeedButton;
    SpeedButtonYb: TSpeedButton;
    SpeedButtonNo: TSpeedButton;
    SpeedButtonLu: TSpeedButton;
    SpeedButtonLr: TSpeedButton;
    SpeedButtonRf: TSpeedButton;
    SpeedButtonBh: TSpeedButton;
    SpeedButtonHn: TSpeedButton;
    SpeedButtonMt: TSpeedButton;
    SpeedButtonUn: TSpeedButton;
    GroupBoxVIII: TGroupBox;
    PanelMendeleevTable: TPanel;
    PageControlLegend: TPageControl;
    TabSheetChemistry: TTabSheet;
    LabelAlkaliMetals: TLabel;
    LabelRareEarthMetals: TLabel;
    LabelTransitionMetals: TLabel;
    LabelAlkaliEarthMetals: TLabel;
    LabelOtherMetals: TLabel;
    LabelOtherNonmetals: TLabel;
    LabelHalogens: TLabel;
    LabelNobleGases: TLabel;
    TabSheetMetallurgy: TTabSheet;
    LabelFerrousMetals: TLabel;
    LabelNonferrousMetals: TLabel;
    LabelRareElements: TLabel;
    LabelPreciousMetals: TLabel;
    LabelRadioactiveElements: TLabel;
    LabelNonmetallicElements: TLabel;
    PanelBottom: TPanel;
    ButtonCancel: TButton;
    ButtonOK: TButton;
    ButtonHelp: TButton;
    ButtonPrint: TButton;
    ButtonView: TButton;
    TableMainMinerals: TTable;
    DataSourceMainMinerals: TDataSource;
    procedure FormCreate(Sender: TObject);
    procedure SpeedButtonElementClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure PageControlLegendChange(Sender: TObject);
    procedure ButtonPrintClick(Sender: TObject);
    procedure ButtonViewClick(Sender: TObject);
  private
    RegIni: TRegistryIniFile;
    procedure UpdateMainMinerals;
  public
    procedure SetLanguage;
  end;

var
  fmMendeleev: TfmMendeleev;

  // ===========================================================================
implementation

// ===========================================================================

uses
  cGlobals,
  GNUgettext;

{$R *.DFM}

procedure TfmMendeleev.SetLanguage;
begin
  RegIni := TRegistryIniFile.Create(GeneralSection);
  LangID := RegIni.ReadInteger(GeneralSection, 'Language', 9); // Default 9
  AppPath := RegIni.ReadString(GeneralSection, 'AppPath', AppPath);

  if LangID <> LANG_ENGLISH then
  begin
    TextDomain('geology');
    BindTextDomain('geology', AppPath + 'Locale' + PathDelim);
  end;
  case LangID of
    LANG_ENGLISH:
      begin
        UseLanguage('en');
        Application.HelpFile := UpperCase(AppPath + 'Help' + PathDelim + 'en' +
          PathDelim + 'Geoblock.chm');
        TableElements.TableName :=
          UpperCase(AppPath + 'Data' + PathDelim + 'Reference' + PathDelim +
          'en' + PathDelim + 'Elements');
        TableMainMinerals.TableName :=
          UpperCase(AppPath + 'Data' + PathDelim + 'Reference' + PathDelim +
          'en' + PathDelim + 'MainMinerals');
      end;
    LANG_RUSSIAN:
      begin
        UseLanguage('ru');
        Application.HelpFile := UpperCase(AppPath + 'Help' + PathDelim + 'ru' +
          PathDelim + 'Geoblock.chm');
        TableElements.TableName :=
          UpperCase(AppPath + 'Data' + PathDelim + 'Reference' + PathDelim +
          'ru' + PathDelim + 'Elements');
        TableMainMinerals.TableName :=
          UpperCase(AppPath + 'Data' + PathDelim + 'Reference' + PathDelim +
          'ru' + PathDelim + 'MainMinerals');
      end;
  end;
  TP_GlobalIgnoreClass(TTable);
  TP_GlobalIgnoreClass(TFields);
  TP_GlobalIgnoreClass(TDBEdit);
  TP_GlobalIgnoreClass(TDBRichEdit);

  DBEditState.DataField := 'STATE';
  DBRichEditDescription.DataField := 'DESCRIPTION';
  DBEditName.DataField := 'NAME';
  DBListBoxMainMinerals.DataField := 'MINERAL';

  TranslateComponent(Self);

  RegIni.Free;
end;

procedure TfmMendeleev.FormCreate(Sender: TObject);
begin
  SetLanguage;
  TableElements.Open;
  TableMainMinerals.Open;
end;

procedure TfmMendeleev.FormActivate(Sender: TObject);
begin
  SpeedButtonFe.Down := True;
  SpeedButtonFe.Click;
  TableElements.RecNo := TComponent(Sender).Tag;
  UpdateMainMinerals;
end;

procedure TfmMendeleev.SpeedButtonElementClick(Sender: TObject);
begin
  TableElements.RecNo := TComponent(Sender).Tag;
  UpdateMainMinerals;
end;

procedure TfmMendeleev.UpdateMainMinerals;
var
  S: String;
begin
  try
    DBListBoxMainMinerals.Items.BeginUpdate;
    try
      DBListBoxMainMinerals.Items.Clear;
      TableMainMinerals.First;
      while not TableMainMinerals.EOF do
      begin
        if TableMainMinerals.FieldByName(fldELEMENT)
          .AsString = TableElements.FieldByName(fldSYMBOL).AsString then
        begin
          S := TableMainMinerals.FieldByName
            (DBListBoxMainMinerals.DataField).AsString;
          DBListBoxMainMinerals.Items.Add(S);
        end;
        TableMainMinerals.Next;
      end;
    finally
      DBListBoxMainMinerals.Items.EndUpdate;
    end;
  except
    DBListBoxMainMinerals.Items.Add('');
  end;
end;

procedure TfmMendeleev.PageControlLegendChange(Sender: TObject);
begin
  if PageControlLegend.ActivePage = TabSheetMetallurgy then
  begin
    // Ferrous Metals
    SpeedButtonTi.Font.Color := LabelFerrousMetals.Font.Color;
    SpeedButtonV.Font.Color := LabelFerrousMetals.Font.Color;
    SpeedButtonFe.Font.Color := LabelFerrousMetals.Font.Color;
    SpeedButtonMn.Font.Color := LabelFerrousMetals.Font.Color;
    SpeedButtonCr.Font.Color := LabelFerrousMetals.Font.Color;
    // NonFerrous Metals
    SpeedButtonNi.Font.Color := LabelNonferrousMetals.Font.Color;
    SpeedButtonCu.Font.Color := LabelNonferrousMetals.Font.Color;
    SpeedButtonZn.Font.Color := LabelNonferrousMetals.Font.Color;
    SpeedButtonAl.Font.Color := LabelNonferrousMetals.Font.Color;
    // Precious Metals
    SpeedButtonAu.Font.Color := LabelPreciousMetals.Font.Color;
    SpeedButtonAg.Font.Color := LabelPreciousMetals.Font.Color;
    SpeedButtonPt.Font.Color := LabelPreciousMetals.Font.Color;
    // RareElements
    SpeedButtonZr.Font.Color := LabelRareElements.Font.Color;
    // RadioactiveElements
    SpeedButtonRa.Font.Color := LabelRadioactiveElements.Font.Color;
    SpeedButtonU.Font.Color := LabelRadioactiveElements.Font.Color;
    // NonmetallicElements
    SpeedButtonSi.Font.Color := LabelNonmetallicElements.Font.Color;
    SpeedButtonP.Font.Color := LabelNonmetallicElements.Font.Color;
    SpeedButtonS.Font.Color := LabelNonmetallicElements.Font.Color;
  end;
  if PageControlLegend.ActivePage = TabSheetChemistry then
  begin
    // TransitionMetals
    SpeedButtonTi.Font.Color := LabelTransitionMetals.Font.Color;
    SpeedButtonV.Font.Color := LabelTransitionMetals.Font.Color;
    SpeedButtonFe.Font.Color := LabelTransitionMetals.Font.Color;
    SpeedButtonMn.Font.Color := LabelTransitionMetals.Font.Color;
    SpeedButtonCr.Font.Color := LabelTransitionMetals.Font.Color;
    SpeedButtonNi.Font.Color := LabelTransitionMetals.Font.Color;
    SpeedButtonCu.Font.Color := LabelTransitionMetals.Font.Color;
    SpeedButtonZn.Font.Color := LabelTransitionMetals.Font.Color;
    SpeedButtonAl.Font.Color := LabelOtherMetals.Font.Color;
    SpeedButtonAu.Font.Color := LabelTransitionMetals.Font.Color;
    SpeedButtonAg.Font.Color := LabelTransitionMetals.Font.Color;
    SpeedButtonPt.Font.Color := LabelTransitionMetals.Font.Color;
    SpeedButtonZr.Font.Color := LabelTransitionMetals.Font.Color;
    // RareEarthMetals
    SpeedButtonRa.Font.Color := LabelRareEarthMetals.Font.Color;
    SpeedButtonU.Font.Color := LabelRareEarthMetals.Font.Color;
    // OtherNonmetals
    SpeedButtonSi.Font.Color := LabelOtherNonmetals.Font.Color;
    SpeedButtonP.Font.Color := LabelOtherNonmetals.Font.Color;
    SpeedButtonS.Font.Color := LabelOtherNonmetals.Font.Color;
  end;
end;

procedure TfmMendeleev.ButtonPrintClick(Sender: TObject);
begin
  if MessageDlg(ButtonPrint.Caption + '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes
  then
    Print;
end;

procedure TfmMendeleev.ButtonViewClick(Sender: TObject);
begin
  /// Show atomic structures in GeoScene viewer
end;

end.
