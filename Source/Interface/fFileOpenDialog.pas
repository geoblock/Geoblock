//-----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//-----------------------------------------------------------------------------
{
  The Dialog to open data files
}


unit fFileOpenDialog;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IniFiles,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  Vcl.ImgList,
  Vcl.ToolWin,
  Data.DB,


  fInitialForm,
  fInitialDialog,
  DBTables,
  dDialogs;

type
  TfmFileOpenDialog = class(TfmInitialDialog)
    PanelLeft:     TPanel;
    PageControl:   TPageControl;
    PanelDirectory: TPanel;
    SpeedButton1:  TSpeedButton;
    SpeedButton2:  TSpeedButton;
    SpeedButtonBrowse: TSpeedButton;
    SpeedButtonDelete: TSpeedButton;
    ListView:      TListView;
    PanelInputPath: TPanel;
    LabelPath:     TLabel;
    GroupBoxOutput: TGroupBox;
    ToolBarShowAs: TToolBar;
    ToolButton3:   TToolButton;
    ToolButtonMap: TToolButton;
    ToolButtonTable: TToolButton;
    ToolButtonGraph: TToolButton;
    EditOutName:   TEdit;
    procedure PageControlChange(Sender: TObject);
    procedure SpeedButtonDeleteClick(Sender: TObject);
    procedure EditFileNameKeyPress(Sender: TObject; var Key: char);
    procedure SpeedButtonBrowseClick(Sender: TObject);
    procedure ListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure FormActivate(Sender: TObject);
    procedure ListViewDblClick(Sender: TObject);
    procedure ToolButtonShowAsClick(Sender: TObject);
    procedure EditFileNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FFilePath: string;
    FFiles:    TStrings;
    FFileName: TFileName;
    procedure SetFilePath(const Value: string);
    function GetFileName: string;
    procedure SetFileName(const Value: string);
     
  protected
    function GetModelType: integer; virtual;
    function GetPathFromTag(ATabSheetTag: integer): string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReadFileList(APathAndMask: string);
    property ModelType: integer read GetModelType;
    property FileName: string read GetFileName write SetFileName;
    property FilePath: string read FFilePath write SetFilePath;
    property Files: TStrings read FFiles;
    procedure ReadIniFile; override;
    procedure WriteIniFile; override;
  end;

var
  fmFileOpenDialog: TfmFileOpenDialog;

//========================================================================
implementation
//========================================================================

uses
  uResStrings,
  uGlobals,
  uFileCreator,
  uCommon;

{$R *.DFM}

constructor TfmFileOpenDialog.Create(AOwner: TComponent);
begin
  inherited;
  FFiles := TStringList.Create;
  PageControl.Images := ImageListInterface;
  ToolBarShowAs.Images := ImageListInterface;
end;


procedure TfmFileOpenDialog.ReadFileList(APathAndMask: string);
var
  Res:     integer;
  Str:     string[20];
  SearchRec: TSearchRec;
  NewItem: TListItem;

begin
  ListView.Visible := False;
  if ListView.Items.Count <> 0 then
    ListView.Items.Clear;
  Res := FindFirst(APathAndMask, not faAnyFile, SearchRec);
  while Res = 0 do
  begin
    if ExtractFileExt(Searchrec.Name) <> '.' then
    begin
      NewItem := ListView.Items.Add;
      NewItem.Caption := ChangeFileExt(SearchRec.Name, '');
      System.Str((SearchRec.Size / 1024.0): 0: 2, Str);
      NewItem.SubItems.Add(Str + ' Kb');
      NewItem.SubItems.Add(DateToStr(FileDateToDateTime(SearchRec.Time)));
    end;
    Res := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
  ListView.Visible := True;
end;

function TfmFileOpenDialog.GetPathFromTag(ATabSheetTag: integer): string;
begin
  Result := '';
end;

procedure TfmFileOpenDialog.PageControlChange(Sender: TObject);
begin
  FilePath := GetPathFromTag(PageControl.ActivePage.Tag);
  ReadFileList(FilePath + TableMask);
end;

procedure TfmFileOpenDialog.SpeedButtonDeleteClick(Sender: TObject);
begin
  if MessageDlg(LoadResString(@rsDelete) + ' ' + EditOutName.Text +
    '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    uFileCreator.DeleteFiles(FilePath + EditOutName.Text,
      PageControl.ActivePage.Tag);
    ReadFileList(FilePath + TableMask);
  end;
end;

procedure TfmFileOpenDialog.EditFileNameKeyPress(Sender: TObject; var Key: char);
var
  S: string;
begin
  S := '' + Key;
  if System.Pos(S, '();,.?!%^&@#~`''"=+<>|[]{}* \/') <> 0 then
  begin
    Beep;
    Key := #0;
  end;
end;

procedure TfmFileOpenDialog.SpeedButtonBrowseClick(Sender: TObject);
var
  TheFileName: string;
  ListItem:    TListItem;
begin
  dmDialogs.OpenDialog.FilterIndex := 1;
  dmDialogs.OpenDialog.InitialDir  := FilePath;
  if dmDialogs.OpenDialog.Execute then
  begin
    TheFileName := dmDialogs.OpenDialog.FileName;
    FilePath    := ExtractFilePath(TheFileName);
    ReadFileList(FilePath + TableMask);
    TheFileName := ChangeFileExt(ExtractFileName(TheFileName), '');
    ListItem    := ListView.FindCaption(0, TheFileName, True, True, False);
    ListView.Selected := ListItem;
    EditOutName.Text := TheFileName;
  end;
end;

procedure TfmFileOpenDialog.ListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  I: integer;
  SelItem: TListItem;
  S: string;
begin
  if csDestroying in ComponentState then
    Exit;
  Files.BeginUpdate;
  try
    Files.Clear;
    S := '';
    SelItem := ListView.Selected;
    if ListView.SelCount = 1 then
    begin
      S := SelItem.Caption;
      Files.Add(FilePath + SelItem.Caption);
    end
    else
      for I := 1 to ListView.SelCount do
      begin
        S := Format('%s "%s"', [S, SelItem.Caption]);
        Files.Add(FilePath + SelItem.Caption);
        SelItem := ListView.GetNextItem(SelItem, sdAll, [isSelected]);
      end;
    SpeedButtonDelete.Enabled := S <> '';
    ButtonOK.Enabled := S <> '';
    EditOutName.Text := S;
  finally
    Files.EndUpdate;
  end;
end;

procedure TfmFileOpenDialog.ListViewDblClick(Sender: TObject);
begin
  if Assigned(ListView.Selected) then
  begin
    EditOutName.Text := ListView.Selected.Caption;
    SpeedButtonDelete.Enabled := True;
    ModalResult      := mrOk;
  end
  else
    ButtonOK.Enabled := False;
end;

procedure TfmFileOpenDialog.FormActivate(Sender: TObject);
begin
  PageControlChange(Self);
end;

procedure TfmFileOpenDialog.EditFileNameChange(Sender: TObject);
begin
  ButtonOK.Enabled := EditOutName.Text <> '';
end;


procedure TfmFileOpenDialog.SetFilePath(const Value: string);
begin
  if CompareText(FFilePath, Value) <> 0 then
  begin
    FFilePath := Value;
    PanelInputPath.Caption := FFilePath;
    PanelInputPath.Hint := FFilePath;
  end;
end;

function TfmFileOpenDialog.GetFileName: string;
begin
  try
    Result := FFiles[0];
  except
    Result := '';
  end;
end;


procedure TfmFileOpenDialog.SetFileName(const Value: string);
begin
end;

function TfmFileOpenDialog.GetModelType: integer;
begin
  Result := mtUnknown;
end;

procedure TfmFileOpenDialog.ToolButtonShowAsClick(Sender: TObject);
begin
  ToolBarShowAs.Tag := (Sender as TToolButton).Tag;
end;

procedure TfmFileOpenDialog.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      ToolButtonMap.Down := ReadBool(Name, ToolButtonMap.Name, True);
      if ToolButtonMap.Down then
        ToolButtonMap.Click;
      ToolButtonTable.Down := ReadBool(Name, ToolButtonTable.Name, False);
      if ToolButtonTable.Down then
        ToolButtonTable.Click;
      ToolButtonGraph.Down := ReadBool(Name, ToolButtonGraph.Name, False);
      if ToolButtonGraph.Down then
        ToolButtonGraph.Click;

      PageControl.TabIndex := ReadInteger(Name, PageControl.Name, 1);
    finally
      IniFile.Free;
    end;
end;

procedure TfmFileOpenDialog.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteInteger(Name, PageControl.Name, PageControl.ActivePageIndex);
      WriteBool(Name, ToolButtonMap.Name, ToolButtonMap.Down);
      WriteBool(Name, ToolButtonTable.Name, ToolButtonTable.Down);
      WriteBool(Name, ToolButtonGraph.Name, ToolButtonGraph.Down);
    finally
      IniFile.Free;
    end;
end;

destructor TfmFileOpenDialog.Destroy;
begin
  FFiles.Free;
  inherited;
end;

procedure TfmFileOpenDialog.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
end;

procedure TfmFileOpenDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;


end.
