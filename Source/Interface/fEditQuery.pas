//------------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//------------------------------------------------------------------------------
(*  The form for SQL queries *)

unit fEditQuery;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Variants, 
  System.Classes,
  Vcl.Graphics, 
  Vcl.ComCtrls, 
  Vcl.ToolWin,
  Bde.DBTables,
  Data.DB,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Controls,


  fInitialDialog,
  dBase,
  dDialogs;

type
  TfmEditQuery = class(TfmInitialDialog)
    ToolBar:      TToolBar;
    ToolButton1:  TToolButton;
    ToolButtonOpen: TToolButton;
    ToolButtonSave: TToolButton;
    GroupBoxQuery: TGroupBox;
    MemoSql:      TMemo;
    GroupBoxOutput: TGroupBox;
    Splitter1:    TSplitter;
    PanelLeft:    TPanel;
    PanelOutputPath: TPanel;
    PanelRight:   TPanel;
    EditOutputName: TEdit; //TGBFile;
    procedure MemoSqlChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ToolButtonOpenClick(Sender: TObject);
    procedure ToolButtonSaveClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure EditOutputNameChange(Sender: TObject);
    procedure EditOutputNameKeyUp(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure GBFileOutputChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FileName, FileExt : TFileName;
  public
    GBFileOutput : TFileName;
  end;

var
  fmEditQuery: TfmEditQuery;

//================================================================
implementation
//================================================================

uses
  cGlobals,
  uCommon,
  fTableWindow,
  cProfuns,
  cResStrings;

{$R *.dfm}

procedure TfmEditQuery.MemoSqlChange(Sender: TObject);
begin
  ButtonOK.Enabled := True;
end;

procedure TfmEditQuery.FormActivate(Sender: TObject);
begin
  ButtonOK.Enabled := False;
end;

procedure TfmEditQuery.ToolButtonOpenClick(Sender: TObject);
begin
  if dmDialogs.OpenDialogSQL.Execute then
    MemoSql.Lines.LoadFromFile(dmDialogs.OpenDialogSQL.FileName);
end;

procedure TfmEditQuery.ToolButtonSaveClick(Sender: TObject);
begin
  if dmDialogs.SaveDialogSQL.Execute then
    MemoSql.Lines.SaveToFile(dmDialogs.SaveDialogSQL.FileName);
end;

procedure TfmEditQuery.ButtonOKClick(Sender: TObject);
var
  OldCursor: TCursor;
begin
  OldCursor := Screen.Cursor;
  with dmBase do
    try
      Screen.Cursor := crHourGlass;
      Query.Close;
      Query.SQL.Text := MemoSql.Text;
      try
        Query.Open;
        with TBatchMove.Create(nil) do
          try
            Source := Query;
            Destination := TableOutput;
            Mode := batCopy;
            Execute;
          finally
            Free;
          end;
        with TfmTableWindow.Create(Application) do
          OpenTable(TableOutput.TableName);
      except
        on E: Exception do
        begin
          MessageDlg(LoadResString(@rsErrorWithSQL) + ':' + E.Message +
            ' ' + Query.Text,
            mtError, [mbOK], 0);
          ModalResult := mrNone;
          Abort;
        end;
      end;
    finally
      Screen.Cursor := OldCursor;
    end;
end;

procedure TfmEditQuery.EditOutputNameChange(Sender: TObject);
begin
  GBFileOutput := EditOutputName.Text; //GBFileOutput.FileNameExt
end;

procedure TfmEditQuery.EditOutputNameKeyUp(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      GBFileOutput := NameOnly(EditOutputName.Text);
  end;
end;

procedure TfmEditQuery.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case Key of
    VK_F2: ToolButtonSaveClick(nil);
    VK_F3: ToolButtonOpenClick(nil);
  end;
end;

procedure TfmEditQuery.GBFileOutputChange(Sender: TObject);
begin
  with PanelOutputPath do
  begin
    Caption := ExtractFilePath(GBFileOutput); // GBFileOutput.FilePath;
    Hint    := Caption;
  end;
  EditOutputName.Text := ExtractFileExt(GBFileOutput);// Extract FileNameExt;
  dmBase.TableOutput.TableName := GBFileOutput; //.FileFullName;
  dmDialogs.SaveDialogSQL.FileName := GBFileOutput; //.FileName;
  dmDialogs.OpenDialogSQL.FileName := GBFileOutput; //.FileName;
end;

procedure TfmEditQuery.FormCreate(Sender: TObject);
begin
  inherited;
  dmDialogs.SaveDialogSQL.InitialDir := ExpandPath(DirDataSQLs);
  dmDialogs.OpenDialogSQL.InitialDir := ExpandPath(DirDataSQLs);
  GBFileOutputChange(Self);
end;

end.
