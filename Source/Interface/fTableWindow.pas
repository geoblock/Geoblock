// -----------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
// -----------------------------------------------------------------------------
(* The Window to browse data tables in dbgrids *)

unit fTableWindow;

interface

uses
  Winapi.Windows,
  Winapi.ShellApi,
  System.SysUtils, 
  System.Classes, 
  System.Variants,
  System.IniFiles,
  System.Actions,
  System.Math,
  System.ImageList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.DBCtrls,
  Vcl.Grids,
  Vcl.DBGrids,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.Dbcgrids,
  Vcl.ActnList,
  Vcl.ImgList,
  Vcl.ComCtrls,
  Vcl.ToolWin,
  Vcl.Menus,
  Vcl.Clipbrd,
  Bde.DBTables,
  Data.DB,

  fInitialForm,
  dDialogs,
  dBase;

type
  TfmTableWindow = class(TfmInitialForm)
    DataSourceMaster: TDataSource;
    TableMaster: TTable;
    Query: TQuery;
    DBGridMaster: TDBGrid;
    ActionList: TActionList;
    ControlBar: TControlBar;
    ToolBar: TToolBar;
    ToolButtonEditFieldRename: TToolButton;
    ToolButtonEditFieldInsert: TToolButton;
    ToolButtonEditFieldDelete: TToolButton;
    ToolButtonEditFieldCalculate: TToolButton;
    ToolButtonEditRecordSelect: TToolButton;
    EditFieldInsert: TAction;
    EditFieldDelete: TAction;
    EditFieldRename: TAction;
    EditFieldCalculate: TAction;
    EditRecordSelect: TAction;
    ToolButtonEditTableAdd: TToolButton;
    EditTableClear: TAction;
    EditTableAdd: TAction;
    ToolButtonEditTableClear: TToolButton;
    ToolButtonEditTableStatistic: TToolButton;
    StaticText: TStaticText;
    ToolButton2: TToolButton;
    ToolButton1: TToolButton;
    ToolButtonEditFieldLookup: TToolButton;
    EditFieldLookup: TAction;
    DBImage: TDBImage;
    PanelLookup: TPanel;
    DBLookupComboBox: TDBLookupComboBox;
    LabelField: TLabel;
    StaticTextFieldName: TStaticText;
    EditTableStatistic: TAction;
    ToolButtonEditTableUpdateStructure: TToolButton;
    EditTableUpdateStructure: TAction;
    ToolButtonEditTableFind: TToolButton;
    EditTableFind: TAction;
    EditCopyFieldNames: TAction;
    ToolButtonEditCopyFieldNames: TToolButton;
    SpeedButtonAllowDeleteRecords: TSpeedButton;
    DBNavigator: TDBNavigator;
    PopupMenu: TPopupMenu;
    ilTablebtn: TImageList;
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DelAllRecords;
    procedure FormCreate(Sender: TObject);
    procedure EditFieldDeleteExecute(Sender: TObject);
    procedure EditFieldInsertExecute(Sender: TObject);
    procedure EditFieldRenameExecute(Sender: TObject);
    procedure EditFieldCalculateExecute(Sender: TObject);
    procedure EditQueryExecute(Sender: TObject);
    procedure EditTableClearExecute(Sender: TObject);
    procedure EditTableAddExecute(Sender: TObject);
    procedure TableMasterAfterScroll(DataSet: TDataSet);
    procedure EditFieldLookupExecute(Sender: TObject);
    procedure DBGridMasterTitleClick(Column: TColumn);
    procedure DBGridMasterDblClick(Sender: TObject);
    procedure DBGridMasterDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure DBGridMasterKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DBLookupComboBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditTableStatisticExecute(Sender: TObject);
    procedure DBGridMasterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditTableUpdateStructureExecute(Sender: TObject);
    procedure ToolButtonEditFieldCalculateDragOver(Sender, Source: TObject;
      X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ToolButtonEditFieldCalculateDragDrop(Sender, Source: TObject;
      X, Y: Integer);
    procedure EditTableFindExecute(Sender: TObject);
    procedure TableMasterBeforeClose(DataSet: TDataSet);
    procedure EditCopyFieldNamesExecute(Sender: TObject);
    procedure SpeedButtonAllowDeleteRecordsClick(Sender: TObject);
  private
    LookupField: TField;
    procedure ReadIniFile;
    procedure WriteIniFile;
  public
    ModelType: Integer;
    procedure OpenTable(ATableName: TFileName);
    procedure CopyToClipboard(Sender: TObject);
    procedure PasteFromClipboard(Sender: TObject);
    procedure SelectAll(Sender: TObject);
    procedure SaveAs(ATableName: TFileName);
  end;

var
  FmTableWindow: TfmTableWindow;

procedure CopyBDEDatasetToTable(Dataset: TBDEDataset; Table: TTable);

//=========================================================================
implementation
//=========================================================================

uses
  cGlobals,
  cResStrings,
  cProfuns,
  uCommon,
  uFileCreator,
  fGeoblock,
  fEditMemoField,
  fEditAddField,
  fEditDeleteField,
  fEditCalcField,
  fEditRenField,
  fEditQuery,
  fEditGetStatist,
  fEditLookupField,
  fDrawImageEditor,
  fDrawFillStyle;

{$R *.DFM}

var
  LastX, LastY: Integer;
  PrevClick: Longword = 0;

procedure TfmTableWindow.FormCreate(Sender: TObject);
begin
  inherited;
  dmBase.TableLookup.TableName := ExpandPath(DirDataReference) + tblMaterial;
  DBGridMaster.DefaultDrawing := False;
  ReadIniFile;
end;

procedure TfmTableWindow.OpenTable(ATableName: TFileName);
begin
  TableMaster.TableName := ATableName;
  TableMaster.Open;
  Caption := TableMaster.TableName;
  Screen.Cursor := CrDefault;
end;

procedure TfmTableWindow.DBGridMasterDblClick(Sender: TObject);
begin
  { if (LastX<10) and (LastY<20) then
    begin
    SelectAll(Self);
    exit;
    end;{ }
  if (DBGridMaster.SelectedField.DataType = ftMemo) or
    (DBGridMaster.SelectedField.DataType = ftFmtMemo) then
  begin
    TableMaster.Edit;
    fmEditMemoField := TfmEditMemoField.Create(Application);
    fmEditMemoField.DBMemo.DataSource := DataSourceMaster;
    fmEditMemoField.DBMemo.DataField := DBGridMaster.SelectedField.FieldName;
    fmEditMemoField.Caption := DBGridMaster.SelectedField.FieldName;
    if fmEditMemoField.Showmodal = MrOk then
      TableMaster.Post
    else
      TableMaster.Cancel;
    fmEditMemoField.Free;
  end;
  if (DBGridMaster.SelectedField.DataType = FtGraphic) then
  begin
    if (DBGridMaster.SelectedField.FieldName = fldTEXTURE) or
      (DBGridMaster.SelectedField.FieldName = fldPATTERN) then
    begin
      fmDrawFillStyle := TfmDrawFillStyle.Create(Self);
      try
        fmDrawFillStyle.Caption := DBGridMaster.SelectedField.FieldName;
          // Self.Table.Edit;
        if fmDrawFillStyle.ShowModal = MrOk then
          // Self.Table.Post
        else
          // Self.Table.Cancel;
        finally
          FreeAndNil(fmDrawFillStyle);
        end;
      end
    else
    begin
      fmDrawImageEditor := TfmDrawImageEditor.Create(Self);
      try
        fmDrawImageEditor.Caption := DBGridMaster.SelectedField.FieldName;
        fmDrawImageEditor.DBImage.DataSource := Self.DataSourceMaster;
        fmDrawImageEditor.DBImage.DataField :=
          DBGridMaster.SelectedField.FieldName;
        fmDrawImageEditor.DBEditName.DataSource := Self.DataSourceMaster;
        fmDrawImageEditor.DBEditName.DataField := fldNAME;
        Self.TableMaster.Edit;
        if fmDrawImageEditor.Showmodal = MrOk then
          Self.TableMaster.Post
        else
          Self.TableMaster.Cancel;
      finally
        FreeAndNil(FmDrawImageEditor);
      end;
    end;
  end;
end;

procedure TfmTableWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if fmGeoblock.MDIChildCount = 1 then
    fmGeoblock.EnableFileItems(False);
  TableMaster.Close;
  Action := CaFree;
  WriteIniFile;
  inherited;
end;

procedure TfmTableWindow.EditTableStatisticExecute(Sender: TObject);
begin
  with TfmEditGetStatist.Create(Self) do
  begin
    TableName := TableMaster.TableName;
    Show;
  end;
end;

procedure TfmTableWindow.EditFieldInsertExecute(Sender: TObject);
begin
  fmEditAddField := TfmEditAddField.Create(Self);
  try
    fmEditAddField.ShowModal;
    if fmEditAddField.ModalResult = MrOk then
    begin
      Query.Close;
      Query.SQL.Clear;
      Query.SQL.Add('ALTER TABLE "' + TableMaster.TableName + '" ADD "' +
        TableMaster.TableName + '".' + FmEditAddField.SQLString);
      TableMaster.Close;
      try
        Query.ExecSQL;
      finally
        TableMaster.Open;
      end;
    end;
  finally
    fmEditAddField.Free;
  end;
end;

procedure TfmTableWindow.EditFieldDeleteExecute(Sender: TObject);
var
  I: Integer;
  Separator: Char;
begin
  if TableMaster.FieldDefs.Count > 1 then
  begin
    fmEditDeleteField := TfmEditDeleteField.Create(Self);
    try
      for I := 0 to TableMaster.FieldDefs.Count - 1 do
      begin
        fmEditDeleteField.CheckListBoxFields.Items.Add
          (TableMaster.FieldDefs.Items[I].Name);
      end;
      fmEditDeleteField.CheckListBoxFields.ItemIndex := 0;
      if fmEditDeleteField.ShowModal = MrOk then
      begin
        if MessageDlg(LoadResString(@rsDelete) + '?', MtConfirmation,
          [MbYes, MbNo], 0) = MrYes then
        begin
          Query.Close;
          Query.SQL.Clear;
          Query.SQL.Add('ALTER TABLE "' + TableMaster.TableName + '"');
          Separator := ' ';
          for I := 0 to FmEditDeleteField.CheckListBoxFields.Items.Count - 1 do
          begin
            if FmEditDeleteField.CheckListBoxFields.Checked[I] then
            begin
              Query.SQL.Add(Separator + 'DROP DBT."' +
                FmEditDeleteField.CheckListBoxFields.Items[I] + '"');
              Separator := ',';
            end;
          end;
          if Separator <> ' ' then
          begin
            TableMaster.Close;
            try
              Query.ExecSQL;
            except
              TableMaster.Open;
              MessageDlg(LoadResString(@rsNotEqualStructures), MtError, [MbOK], 0);
            end;
            TableMaster.Open;
          end;
        end;
      end;
    finally
      fmEditDeleteField.Free;
    end;
  end;
end;

procedure TfmTableWindow.EditFieldCalculateExecute(Sender: TObject);
var
  I: Integer;
begin
  fmEditCalcField := TfmEditCalcField.Create(Self);
  // Add only integer and real fields to listboxes
  for I := 0 to TableMaster.FieldDefs.Count - 1 do
  begin
    if (TableMaster.Fields[I].DataType in [ftSmallint, ftInteger, ftLargeInt,
      ftWord, ftFloat, ftCurrency, ftAutoInc]) then
    begin
      fmEditCalcField.lbAvailableFields.Items.Add(TableMaster.FieldDefs.Items[I].Name);
      fmEditCalcField.lbResultFields.Items.Add(TableMaster.FieldDefs.Items[I].Name);
    end;
  end;
  // Highlight activate ItemIndex
  if fmEditCalcField.lbResultFields.Items.Count > 0 then
    fmEditCalcField.lbResultFields.ItemIndex := fmEditCalcField.lbResultFields.Items.Count - 1
  else
    fmEditCalcField.lbResultFields.ItemIndex := -1;

  fmEditCalcField.ButtonOK.Enabled :=
    (fmEditCalcField.lbResultFields.Items.Count - 1 <> 0) and
    (fmEditCalcField.EditFormula.Text <> '');

  fmEditCalcField.ShowModal;

  if fmEditCalcField.ModalResult = MrOk then
  begin
    Query.Close;
    Query.SQL.Clear;
    if FmEditCalcField.cbIncRecord.Checked then
    begin
      // Increment every record with formular value
    end
    else
      Query.SQL.Add('UPDATE "' + TableMaster.TableName + '" SET "' +
        TableMaster.TableName + '"."' +
        fmEditCalcField.lbResultFields.Items[fmEditCalcField.lbResultFields.ItemIndex] +
        '" = ' + fmEditCalcField.EditFormula.Text);
    TableMaster.Close;
    try
      Query.ExecSQL;
    except
    end;
    TableMaster.Open;
  end;
  FmEditCalcField.Free;
end;

procedure TfmTableWindow.DelAllRecords;
begin
  if MessageDlg(LoadResString(@rsDeleteAllRecords) + '?', MtConfirmation,
    [MbYes, MbNo], 0) = MrYes then
  begin
    Query.Close;
    Query.SQL.Clear;
    Query.SQL.Add('DELETE FROM "' + TableMaster.TableName + '"');
    TableMaster.Close;
    try
      Query.ExecSQL;
    except
      TableMaster.Open;
      MessageDlg(LoadResString(@rsErrorWithTable), MtError, [MbOK], 0);
    end;
    TableMaster.Open;
  end;
end;

procedure TfmTableWindow.EditFieldRenameExecute(Sender: TObject);
var
  I: Integer;
  OldFieldName, NewFieldName: string;
  Comma: Char;
  OldCursor: TCursor;
  Query: TQuery;
begin
  fmEditRenField := TfmEditRenField.Create(Self);
  for I := 0 to TableMaster.FieldDefs.Count - 1 do
    fmEditRenField.ListBoxFieldName.Items.Add
      (TableMaster.FieldDefs.Items[I].Name);
  fmEditRenField.ListBoxFieldName.ItemIndex := 0;

  if fmEditRenField.Execute then
  begin
    OldCursor := Screen.Cursor;
    OldFieldName := fmEditRenField.ListBoxFieldName.Items
      [fmEditRenField.ListBoxFieldName.ItemIndex];
    NewFieldName := FmEditRenField.EditNewName.Text;
    try
      Screen.Cursor := CrHourGlass;
      Query := TQuery.Create(Self);
      try
        Comma := ' ';
        Query.Sql.Add('SELECT ');
        for I := 0 to TableMaster.Fields.Count - 1 do
        begin
          Query.Sql[0] := Query.Sql[0] + Format('%s T."%S"',
            [Comma, TableMaster.Fields[I].FieldName]);
          if CompareText(TableMaster.Fields[I].FieldName, OldFieldName) = 0 then
            Query.Sql[0] := Query.Sql[0] + Format(' as T."%S"', [NewFieldName]);
          Comma := ',';
        end;
        Query.Sql.Add(Format('FROM "%s" T', [TableMaster.TableName]));
        try
          TableMaster.BatchMove(Query, BatCopy);
        except
          ModalResult := MrNone;
          Abort;
        end;
      finally
        Query.Free;
      end;
    finally
      Screen.Cursor := OldCursor;
    end;
  end;
  fmEditRenField.Free;
end;

// Dataset Copy Routine

procedure CopyBDEDatasetToTable(Dataset: TBDEDataset; Table: TTable);
begin
  with TBatchMove.Create(Application) do
    try
      Source := Dataset;
      Destination := Table;
      Mode := BatCopy;
      Execute;
    finally
      Free;
    end;
end;

{ \Dataset Copy Routine }

procedure TfmTableWindow.EditQueryExecute(Sender: TObject);
var
  I: Integer;
  Comma: string[1];
begin
  with TfmEditQuery.Create(Self) do
    try
      Comma := ' ';
      MemoSql.Lines[0] := 'SELECT ';
      for I := 0 to TableMaster.Fields.Count - 1 do
      begin
        MemoSql.Lines[0] := MemoSql.Lines[0] + Format('%s T."%S"',
          [Comma, TableMaster.Fields[I].FieldName]);
        Comma := ',';
      end;
      MemoSql.Lines[1] := Format('FROM "%s" T', [TableMaster.TableName]);
      GBFileOutput := ExtractFilePath(TableMaster.TableName);
      if ShowModal = MrOk then
      begin
        // Select Records
      end;
    finally
      Free
    end;
end;

procedure TfmTableWindow.EditTableClearExecute(Sender: TObject);
begin
  if MessageDlg(LoadResString(@rsClear) + '?', MtConfirmation, [MbYes, MbNo],
    0) = MrYes then
  begin
    TableMaster.EmptyTable;
  end;
end;

procedure TfmTableWindow.EditTableAddExecute(Sender: TObject);
var
  ImportFile: file of Char;
  Str: String;
  I, J: Integer;
  FieldsNum: Integer;
  Ch: Char;
  TableSource: TTable;
  TableDest: TTable;
  ErrorCount: Integer;
  MaxID: Integer;
begin
  dmDialogs.OpenDialog.InitialDir := ExtractFilePath(TableMaster.TableName);
  if dmDialogs.OpenDialog.Execute then
  begin
    if dmDialogs.OpenDialog.FilterIndex = 3 then
    begin
      if StrLower(PChar(ExtractFileExt(dmDialogs.OpenDialog.FileName))) = TableExt
      then
        dmDialogs.OpenDialog.FilterIndex := 1
      else if StrLower(PChar(ExtractFileExt(dmDialogs.OpenDialog.FileName))) = TextExt
      then
        dmDialogs.OpenDialog.FilterIndex := 2;
    end;
    case dmDialogs.OpenDialog.FilterIndex of
      1:
        begin
          if Pos(DirPolygonPoly, TableMaster.TableName) < 1 then
          begin
            if IsEqualStructure(TableMaster.TableName,
              dmDialogs.OpenDialog.FileName) then
            begin
              TableSource := TTable.Create(Self);
              try
                TableSource.TableName := dmDialogs.OpenDialog.FileName;
                TableSource.Open;
                ErrorCount := 0;
                for I := 1 to TableSource.RecordCount do
                begin
                  TableMaster.Append;
                  for J := 0 to TableSource.FieldCount - 1 do
                  begin
                    try
                      TableMaster.Fields[J].Value := TableSource.Fields
                        [J].Value;
                    except
                      Inc(ErrorCount);
                    end;
                  end;
                  TableMaster.Post;
                  TableSource.Next;
                end;
                if ErrorCount <> 0 then
                  ShowMessage(Format(' %d errors', [ErrorCount]));
              finally
                TableSource.Free;
              end;
            end
            else
              ShowMessage(LoadResString(@rsNotEqualStructures));
          end
          else
          begin // POLYGONS
            if IsEqualStructure(TableMaster.TableName,
              dmDialogs.OpenDialog.FileName) and
              IsEqualStructure(TableMaster.TableName,
              dmDialogs.OpenDialog.FileName) then
            begin
              TableSource := TTable.Create(Self);
              TableDest := TTable.Create(Self);
              try
                TableSource.TableName := dmDialogs.OpenDialog.FileName;
                TableSource.Open;
                ErrorCount := 0;
                TableMaster.RecNo := 1;
                MaxID := 0;
                try
                  MaxID := TableMaster.FieldByName(FldID).AsInteger;
                except
                end;
                for I := 1 to TableMaster.RecordCount do
                begin
                  TableMaster.RecNo := I;
                  if MaxID < TableMaster.FieldByName(FldID).AsInteger then
                    MaxID := TableMaster.FieldByName(FldID).AsInteger;
                end;
                for I := 1 to TableSource.RecordCount do
                begin
                  TableMaster.Append;
                  for J := 0 to TableMaster.FieldCount - 1 do
                  begin
                    try
                      if CompareText(TableMaster.Fields[J].FieldName, FldID) = 0
                      then
                        TableMaster.Fields[J].Value := TableSource.Fields[J].Value + MaxID
                      else
                        TableMaster.Fields[J].Value :=
                          TableSource.Fields[J].Value;
                    except
                      Inc(ErrorCount);
                    end;
                  end;
                  TableMaster.Post;
                  TableSource.Next;
                end;
                TableSource.Close;
                TableSource.TableName := ChangeModelTable(DirPolygonPoly,
                  DirPolygonVertex, TableSource.TableName);
                TableDest.TableName := TableMaster.TableName;
                TableDest.TableName := ChangeModelTable(DirPolygonPoly,
                  DirPolygonVertex, TableDest.TableName);
                TableSource.Open;
                TableDest.Open;
                for I := 1 to TableSource.RecordCount do
                begin
                  TableDest.Append;
                  for J := 0 to TableSource.FieldCount - 1 do
                  begin
                    try
                      if CompareText(TableDest.Fields[J].FieldName,
                        FldID_POLY) = 0 then
                        TableDest.Fields[J].Value := TableSource.Fields[J]
                          .Value + MaxID
                      else
                        TableDest.Fields[J].Value := TableSource.Fields
                          [J].Value;
                    except
                      Inc(ErrorCount);
                    end;
                  end;
                  TableDest.Post;
                  TableSource.Next;
                end;
                if ErrorCount <> 0 then
                  ShowMessage(Format(' %d errors', [ErrorCount]));
              finally
                TableSource.Free;
                TableDest.Free;
              end;
            end
            else
              ShowMessage(LoadResString(@rsNotEqualStructures));
          end;
        end;
      2:
        begin
          AssignFile(ImportFile, dmDialogs.OpenDialog.FileName);
          Reset(ImportFile);
          try
            Str := '';
            I := 0;
            FieldsNum := TableMaster.FieldDefs.Count;
            TableMaster.Insert;
            repeat
              Read(ImportFile, Ch);
              if (Ch <> #10) then
                if (Ch <> ',') and (Ch <> #13) then
                  Str := Str + Ch
                else
                begin
                  try
                    TableMaster.Fields[I].AsString := Str;
                  except
                  end;
                  Str := '';
                  I := (I + 1) mod FieldsNum;
                  if (I = 0) or (Ch = #13) then
                  begin
                    TableMaster.Post;
                    TableMaster.Next;
                    TableMaster.Insert;
                    I := 0;
                  end;
                end;
            until EOF(ImportFile);
            TableMaster.Post;
            TableMaster.Delete;
          finally
            CloseFile(ImportFile);
          end;
        end;
      3:
        ;
    end;
    // Add records from a selected table with the same structure
  end;
end;

procedure TfmTableWindow.FormActivate(Sender: TObject);
begin
  fmGeoblock.EditCopy.Enabled := True;
  fmGeoblock.EditPaste.Enabled := True;
  DragAcceptFiles(Handle, True);
end;

procedure TfmTableWindow.FormDeactivate(Sender: TObject);
begin
  fmGeoblock.EditCopy.Enabled := False;
  fmGeoblock.EditPaste.Enabled := False;
end;

procedure TfmTableWindow.CopyToClipboard(Sender: TObject);
var
  I, J: Integer;
  Str: string;
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  try
    StringList.BeginUpdate;
    try
      DBGridMaster.Refresh;
      if DBGridMaster.SelectedRows.Count = 0 then
        StringList.Add(DBGridMaster.Fields[DBGridMaster.SelectedIndex].AsString)
      else
        for I := 0 to DBGridMaster.SelectedRows.Count - 1 do
        begin
          with DBGridMaster do
          begin
            Str := '';
            DataSource.DataSet.GotoBookmark(TBookMark(SelectedRows.Items[I]));
            for J := 0 to FieldCount - 1 do
            begin
              try
                Str := Str + Fields[J].AsString;
              except
              end;
              if J <> FieldCount - 1 then
                Str := Str + #9;
            end;
            StringList.Add(Str);
          end;
        end;
    finally
      StringList.EndUpdate;
    end;
    Clipboard.AsText := StringList.Text;
  finally
    StringList.Free;
  end;
end;

procedure TfmTableWindow.PasteFromClipboard(Sender: TObject);

{ sub }
  function GetWord(var Str: string): string;

  // +[Char]<+
  // S0     |   S1  |     S2       Se
  // -+>(")-X-------+->(")-+-+-(#9)->
  // |     |              | ^
  // |     |              | |
  // |     +--------------+ |
  // +--------X-------+-----+
  // |   S3  |
  // +[Char]<+
  var
    I: Integer;
    State: Integer;
    Ch: Char;
  const
    EndState = -1;
  begin
    Result := '';
    State := 0;
    I := 0;
    while State <> EndState do
    begin
      Inc(I);
      Ch := Str[I];
      case State of
        0:
          if Ch = '"' then
            State := 1
          else if Ch = #9 then
            State := EndState
          else
          begin
            Result := Result + Ch;
            State := 3;
          end;
        1:
          if Ch = '"' then
            State := 2
          else
            Result := Result + Ch;
        2:
          if Ch = #9 then
            State := EndState
          else
          begin
            Result := Result + Ch;
            State := 1;
          end;
        3:
          if Ch = #9 then
            State := EndState
          else
            Result := Result + Ch;
      end;
    end;
    Delete(Str, 1, I);
  end;

var
  I, J: Integer;
  Str: string;
  StringList: TStringList;
  FieldNo: Integer;
begin
  StringList := TStringList.Create;
  DBGridMaster.DataSource.DataSet.DisableControls;
  try
    try
      StringList.Text := Clipboard.AsText;
      FieldNo := DBGridMaster.SelectedIndex;
      for I := 0 to StringList.Count - 1 do
      begin
        with DBGridMaster do
        begin
          if DataSource.DataSet.EOF then
            DataSource.DataSet.Append
          else
            DataSource.DataSet.Edit;
          Str := StringList[I] + #9;
          for J := FieldNo to FieldCount - 1 do
          begin
            try
              Fields[J].AsString := GetWord(Str);
            except
            end;
            if Str = '' then
              Break;
          end;
          try
            DataSource.DataSet.Post;
          except
          end;
          DataSource.DataSet.Next;
        end;
      end;
    finally
      StringList.Free;
    end;
  finally
    DBGridMaster.DataSource.DataSet.EnableControls;
  end;
end;

procedure TfmTableWindow.TableMasterAfterScroll(DataSet: TDataSet);
begin
  StaticText.Caption := IntToStr(DataSet.RecNo) + ' : ' +
    IntToStr(DataSet.RecordCount);
end;

procedure TfmTableWindow.EditFieldLookupExecute(Sender: TObject);
begin
  { TODO -oVas -cTable Window : Check how the procedure works }
  FmEditLookupField := TfmEditLookupField.Create(Self);
  with FmEditLookupField do
    try
      Table.TableName := Self.TableMaster.TableName;
      UpdateFields;
      ListBoxLookup.ItemIndex := Max(Min(0, ListBoxLookup.Items.Count - 1),
        ListBoxLookup.Items.IndexOf(DBGridMaster.SelectedField.FieldName));
      GBFileLookup := dmBase.TableLookup.TableName;
      LookupField.Free;
      if ShowModal = MrOk then
      begin
        dmBase.TableLookup.Close;
        dmBase.TableLookup.TableName := GBFileLookup;
        try
          dmBase.TableLookup.Open;
        except
        end;
        with DBLookupComboBox do
        begin
          DataField := ListBoxLookup.Items[ListBoxLookup.ItemIndex];
          StaticTextFieldName.Caption := DataField;
          KeyField := ListBoxLink.Items[ListBoxLink.ItemIndex];
          ListField := ListBoxView.Items[ListBoxView.ItemIndex];
        end;
        PanelLookup.Visible := True;
      end
      else
        with DBLookupComboBox do
        begin
          DataField := '';
          KeyField := '';
          ListField := '';
          PanelLookup.Visible := False;
        end;
    finally
      Free;
    end;
end;

procedure TfmTableWindow.DBGridMasterTitleClick(Column: TColumn);
var
  I, OldRecNo: Longint;
  MaxWidth: Longint;
  TextSize: TSize;
begin
  if GetTickCount - PrevClick < 399 then
  begin
    TableMaster.DisableControls;
    try
      OldRecNo := TableMaster.RecNo;
      MaxWidth := 0;
      for I := 1 to TableMaster.RecordCount do
      begin
        TableMaster.RecNo := I;
        TextSize := DBGridMaster.Canvas.TextExtent
          (TableMaster.FieldByName(Column.FieldName).AsString);
        if MaxWidth < TextSize.cx then
          MaxWidth := TextSize.cx;
      end;
      Column.Width := Min(Width - 30, MaxWidth + 10);
      TableMaster.RecNo := OldRecNo;
    finally
      TableMaster.EnableControls;
    end;
  end;
  PrevClick := GetTickCount;
end;

procedure TfmTableWindow.DBGridMasterDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  NewRect: TRect;
begin
  DBGridMaster.DefaultDrawColumnCell(Rect, DataCol, Column, State);
  if Column.Field.DataType = ftGraphic then
    try
      with DBGridMaster do
      begin
        DBImage.DataField := Column.FieldName;
        NewRect := Rect;
        InflateRect(NewRect, -2, -2);
        Canvas.Brush.Bitmap := DBImage.Picture.Bitmap;
        try
          Canvas.FillRect(NewRect);
        finally
          Canvas.Brush.Bitmap := nil;
        end;
        DBImage.DataField := '';
      end;
    except
    end;
end;

procedure TfmTableWindow.DBGridMasterKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_DELETE:
      begin
        if Shift <> [SsAlt] then
          Exit;
        TableMaster.Edit;
        DBGridMaster.SelectedField.Value := Null;
        TableMaster.Post;
      end;
  end;
end;

procedure TfmTableWindow.DBLookupComboBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      if SsShift in Shift then
        TableMaster.Prior
      else
        TableMaster.Next;
  end;
end;

procedure TfmTableWindow.FormShow(Sender: TObject);
begin
  ToolBar.Width := ControlBar.Width - DBNavigator.Width - 10;
end;

procedure TfmTableWindow.SelectAll(Sender: TObject);
var
  I: Integer;
begin
  TableMaster.DisableControls;
  try
    for I := 1 to TableMaster.RecordCount do
    begin
      TableMaster.RecNo := I;
      DBGridMaster.SelectedRows.CurrentRowSelected := True;
    end;
  finally
    TableMaster.EnableControls;
  end;
end;

procedure TfmTableWindow.DBGridMasterMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  LastX := X;
  LastY := Y;
end;

procedure TfmTableWindow.EditTableUpdateStructureExecute(Sender: TObject);
var
  I: Integer;
  Comma: string[1];
  Query: TQuery;
  OldCursor: TCursor;
begin
  // Update the structure for new columns positions with SQL
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := CrHourGlass;
    Query := TQuery.Create(Self);
    try
      Comma := ' ';
      Query.Sql.Add('SELECT ');
      for I := 0 to TableMaster.Fields.Count - 1 do
      begin
        Query.Sql[0] := Query.Sql[0] + Format('%s T."%S"',
          [Comma, TableMaster.Fields[I].FieldName]);
        Comma := ',';
      end;
      Query.Sql.Add(Format('FROM "%s" T', [TableMaster.TableName]));
      try
        Query.Open;
        with TBatchMove.Create(nil) do
          try
            Source := Query;
            Destination := TableMaster;
            Mode := BatCopy;
            Execute;
          finally
            Free;
          end;
      except
        ModalResult := MrNone;
        Abort;
      end;
    finally
      Query.Free;
    end;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure TfmTableWindow.ToolButtonEditFieldCalculateDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Source is TCustomEdit;
end;

procedure TfmTableWindow.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      Top := ReadInteger('Table Window', 'Top', Top);
      Left := ReadInteger('Table Window', 'Left', Left);
      Height := ReadInteger('Table Window', 'Height', Height);
      Width := ReadInteger('Table Window', 'Width', Width);
    finally
      IniFile.Free;
    end;
end;

procedure TfmTableWindow.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteInteger('Table Window', 'Top', Top);
      WriteInteger('Table Window', 'Left', Left);
      WriteInteger('Table Window', 'Height', Height);
      WriteInteger('Table Window', 'Width', Width);
    finally
      IniFile.Free;
    end;
end;

procedure TfmTableWindow.ToolButtonEditFieldCalculateDragDrop(Sender,
  Source: TObject; X, Y: Integer);
var
  Expressions: TStrings;
  I: Integer;
begin
  Expressions := TStringList.Create;
  try
    Expressions.Text := TCustomEdit(Source).SelText;
    if Trim(Expressions.Text) <> '' then
      try
        TableMaster.Close;
        for I := 0 to Expressions.Count - 1 do
        begin
          if Trim(Expressions[I]) = '' then
            Continue;
          Query.Close;
          Query.SQL.Clear;
          Query.SQL.Add('UPDATE "' + TableMaster.TableName + '" SET ' +
            Expressions[I]);
          try
            Query.ExecSQL;
          except
          end;
        end;
      finally
        TableMaster.Open;
      end;
  finally
    Expressions.Free;
  end;
end;

procedure TfmTableWindow.SaveAs(ATableName: TFileName);
var
  NewName: string;
begin
  NewName := ATableName;
  CopyFiles(ChangeFileExt(TableMaster.TableName, '.*'), NewName, ModelType, False);
  TableMaster.Close;
  TableMaster.TableName := ATableName;
  Caption := ATableName;
  TableMaster.Open;
end;

procedure TfmTableWindow.EditTableFindExecute(Sender: TObject);
var
  RecNo: Integer;
begin
  RecNo := TableMaster.RecNo;
  TableMaster.Filter := InputBox(LoadResString(@rsSearch),
    LoadResString(@rsFilter) + ': ' + LoadResString(@rsField) + '=(>,<,<>)' +
    LoadResString(@rsValue), TableMaster.Filter);
  TableMaster.Filtered := Trim(TableMaster.Filter) <> '';
  TableMaster.RecNo := RecNo;
end;

procedure TfmTableWindow.TableMasterBeforeClose(DataSet: TDataSet);
begin
  if DataSet.State = DsEdit then
    DataSet.Post;
end;

procedure TfmTableWindow.EditCopyFieldNamesExecute(Sender: TObject);
var
  I: Integer;
  Str: string;
begin
  DBGridMaster.Refresh;
  with DBGridMaster do
  begin
    Str := '';
    for I := 0 to FieldCount - 1 do
    begin
      Str := Str + Fields[I].FieldName;
      if I <> FieldCount - 1 then
        Str := Str + #9;
    end;
  end;
  Clipboard.AsText := Str;
end;

procedure TfmTableWindow.SpeedButtonAllowDeleteRecordsClick(Sender: TObject);
begin
  if SpeedButtonAllowDeleteRecords.Down then
    DBNavigator.VisibleButtons := [NbFirst, NbPrior, NbNext, NbLast, NbInsert,
      NbDelete, NbEdit, NbPost, NbCancel, NbRefresh]
  else
    DBNavigator.VisibleButtons := [NbFirst, NbPrior, NbNext, NbLast, NbInsert,
      NbEdit, NbPost, NbCancel, NbRefresh];
end;

end.
