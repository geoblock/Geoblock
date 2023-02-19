//----------------------------------------------------------------------------
// Main form of Geochronology plugin for Geoblock
//----------------------------------------------------------------------------
//
{! The Main form of Geochronology plugin for Geoblock }

unit fGeochronology;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.IniFiles,
  System.Win.Registry,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.DBCtrls,
  Vcl.Grids,
  Vcl.DBGrids,
  Data.DB,
  Data.FMTBcd,
  Data.SqlExpr,
  Bde.DBTables,

  Vcl.ToolWin,
  Vcl.ActnMan,
  Vcl.ActnCtrls,
  Vcl.ActnMenus,
  {GR32_Image,}
  fInitialDialog;

type
  TfmMainForm = class(TfmInitialDialog)
    GroupBoxTimeScales: TGroupBox;
    GroupBoxHistoryTree: TGroupBox;
    TreeView: TTreeView;
    Splitter1: TSplitter;
    GroupBoxLegend: TGroupBox;
    DBGrid: TDBGrid;
    GroupBoxDescription: TGroupBox;
    DBRichEditDescription: TDBRichEdit;
    DataSourceAge: TDataSource;
    TableAge: TTable;
    DBGridEpoch: TDBGrid;
    DataSourceEpoch: TDataSource;
    TableEpoch: TTable;
    PanelTimeScale: TPanel;
    PanelUniverseScale: TPanel;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    TrackBarScale: TTrackBar;
    procedure FormCreate(Sender: TObject);
  private
  public
    RegIni: TRegistryIniFile;
    procedure SetLanguage;
  end;

var
  fmMainForm: TfmMainForm;

//===============================================================
implementation
//===============================================================

uses
  cGlobals,
  uCommon,
  cProfuns,
  gnuGettext;

{$R *.dfm}

procedure TfmMainForm.SetLanguage;
var
  FileName: TFileName;

begin
  RegIni := TRegistryIniFile.Create(GeneralSection);
  LangID := RegIni.ReadInteger(GeneralSection, 'LangID', 9);//Default 9
  PathApp := RegIni.ReadString(GeneralSection,'PathApp', PathApp);

  if LangID <> LANG_ENGLISH then
  begin
    TextDomain('geology');
    BindTextDomain ('geology', PathApp + 'Locale'+ PathDelim);
  end;
  case LangID of
    LANG_ENGLISH:
    begin
      UseLanguage('en');
      Application.HelpFile := UpperCase(PathApp+'Help'+PathDelim+'en'+PathDelim+'Geoblock.chm');
      FileName := UpperCase(PathApp +'Data'+PathDelim +'Reference'+ PathDelim +'en'+ PathDelim + 'Geochronology.eng');
      TableAge.TableName := UpperCase(PathApp +'Data'+PathDelim +'Reference'+ PathDelim +'en'+ PathDelim + 'age.db');
      TableEpoch.TableName := UpperCase(PathApp +'Data'+PathDelim +'Reference'+ PathDelim +'en'+ PathDelim + 'epoch.db');
    end;
    LANG_RUSSIAN:
    begin
      UseLanguage('ru');
      Application.HelpFile := UpperCase(PathApp+'Help'+PathDelim+'ru'+PathDelim+'Geoblock.chm');
      FileName := UpperCase(PathApp +'Data'+PathDelim +'Reference'+ PathDelim +'ru'+ PathDelim + 'Geochronology.rus');
      TableAge.TableName := UpperCase(PathApp +'Data'+PathDelim +'Reference'+ PathDelim +'ru'+ PathDelim + 'age.db');
      TableEpoch.TableName := UpperCase(PathApp +'Data'+PathDelim +'Reference'+ PathDelim +'ru'+ PathDelim + 'epoch.db');
    end;
  end;

  TreeView.LoadFromFile(FileName);
  TreeView.Items[0].Expand(False);
  TranslateComponent(Self);

  RegIni.Free;
end;

procedure TfmMainForm.FormCreate(Sender: TObject);
begin
  SetLanguage;
  TableAge.Open;
  TableEpoch.Open;
end;


end.
