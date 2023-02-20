//------------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//------------------------------------------------------------------------------
{!  The form to show lookup fields
}

unit fEditLookupField;

interface

uses
  Winapi.Windows, 
  Winapi.Messages,
  System.SysUtils, 
  System.Classes, 
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs,
  Vcl.StdCtrls, 
  Vcl.ExtCtrls, 
  Vcl.Buttons,
  Data.DB, 
  Bde.DBTables,

  
  fInitialDialog,
  dBase,
  dDialogs;

type
  TfmEditLookupField = class(TfmInitialDialog)
    GroupBoxLookupField: TGroupBox;
    ListBoxLookup: TListBox;
    GroupBoxLinkField: TGroupBox;
    ListBoxLink: TListBox;
    GroupBoxViewField: TGroupBox;
    ListBoxView: TListBox;
    GroupBoxLookupTable: TGroupBox;
    Splitter1: TSplitter;
    PanelLeft: TPanel;
    PanelLookupPath: TPanel;
    PanelRight: TPanel;
    EditLookupName: TEdit;
    Panel2: TPanel;
    SpeedButtonLookupName: TSpeedButton;
    Table:  TTable;
    TableLookup: TTable;
    TableLookupee: TIntegerField;
    procedure FormCreate(Sender: TObject);
    procedure SpeedButtonLookupNameClick(Sender: TObject);
    procedure GBFileLookupChange(Sender: TObject);
  private
    FileName, FileExt : TFileName;
  public
     
    GBFileLookup : TFileName; //Earlier TGBFile;
    procedure UpdateLookupFields;
    procedure UpdateFields;
  end;

var
  fmEditLookupField: TfmEditLookupField;

//==========================================================================
implementation
//==========================================================================

uses
  uCommon,
  cGlobals;

{$R *.DFM}


procedure TfmEditLookupField.FormCreate(Sender: TObject);
begin
  inherited;
  TableLookup.DatabaseName := ExpandPath(DirDataReference);
  EditLookupName.Text      := TableLookup.TableName;
end;


procedure DataSetReadFields(DataSet: TDataSet);
var
  OldActive: boolean;
  List:      TStringList;
begin
  OldActive := DataSet.Active;
  try
    if OldActive then
      DataSet.Active := False;
    DataSet.Fields.Clear;
    List := TStringList.Create;
    try
      DataSet.GetFieldNames(List);
    finally
      List.Free;
    end;
  finally
    DataSet.Active := OldActive;
  end;
end;

procedure TfmEditLookupField.UpdateFields;
begin
  DataSetReadFields(Table);
  Table.GetFieldNames(ListBoxLookup.Items);
end;

procedure TfmEditLookupField.UpdateLookupFields;
begin
  try
    DataSetReadFields(TableLookup);
    TableLookup.GetFieldNames(ListBoxLink.Items);
    ListBoxLink.ItemIndex := 0;
    TableLookup.GetFieldNames(ListBoxView.Items);
    ListBoxLink.ItemIndex := 0;
    ButtonOK.Enabled      := True;
  except
    ButtonOK.Enabled := False;
  end;
end;

procedure TfmEditLookupField.SpeedButtonLookupNameClick(Sender: TObject);
begin
  if dmDialogs.OpenDialog.Execute then  //GBFileLookup.
  begin
    ButtonOK.Enabled := True;
    UpdateLookupFields;
  end;
end;

procedure TfmEditLookupField.GBFileLookupChange(Sender: TObject);
begin
  with PanelLookupPath do
  begin
    Caption := GBFileLookup; //.FilePath;
    Hint    := GBFileLookup; //.FilePath;
  end;
  if ExtractFileExt(GBFileLookup) = '' then
    FileExt := TableExt;
  EditLookupName.Text := ExtractFileExt(GBFileLookup);
  TableLookup.Close;
  TableLookup.TableName := GBFileLookup; //.FileFullName;
  UpdateLookupFields;
end;

end.
