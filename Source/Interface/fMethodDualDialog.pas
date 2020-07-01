//----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//----------------------------------------------------------------------------

unit fMethodDualDialog;

(* Double dialog for methods *)

interface

uses
  System.SysUtils, 
  System.Classes,
  System.IniFiles,
  System.Math,
  System.ImageList,
  Vcl.ImgList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ToolWin,
  Vcl.Buttons,
  Vcl.ExtCtrls,

  //DB
  Data.DB,
  Bde.DBTables,

  dBase,
  dDialogs,
  fMethodDialog;

type
  TfmMethodDualDialog = class(TfmMethodDialog)
    GroupBoxInputB:   TGroupBox;
    PanelInputPathB:  TPanel;
    PanelInputButtonsB: TPanel;
    ToolBarInputB:    TToolBar;
    ToolButton5:      TToolButton;
    ToolButtonHolesB: TToolButton;
    ToolButtonPoints2DB: TToolButton;
    ToolButtonPoints3DB: TToolButton;
    ToolButtonPolygonsB: TToolButton;
    ToolButtonTinsB:  TToolButton;
    ToolButtonSolidsB: TToolButton;
    ToolButtonGrids2DB: TToolButton;
    ToolButtonGrids3DB: TToolButton;
    ToolButtonMeshes2DB: TToolButton;
    ToolButtonMeshes3DB: TToolButton;
    ToolButton16:     TToolButton;
    GroupBoxModelB:   TGroupBox;
    ListBoxInputNamesB: TListBox;
    GroupBoxRealAttributeB: TGroupBox;
    ListBoxRealAttributeB: TListBox;
    ToolBarBRight:    TToolBar;
    SpeedButtonInputInfoB: TSpeedButton;
    SpeedButtonInputBrowseB: TSpeedButton;
    procedure ToolButtonInputBClick(Sender: TObject);
    procedure ListBoxInputNamesBClick(Sender: TObject);
    procedure ListBoxRealAttributeBClick(Sender: TObject);
    procedure ListBoxInputNamesClick(Sender: TObject);
    procedure ToolButtonInputClick(Sender: TObject);
    procedure SpeedButtonInputBInfoClick(Sender: TObject);
    procedure SpeedButtonInputBrowseBClick(Sender: TObject);
    procedure SpeedButtonInputInfoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FInModelNameB: string;
    FInModelTypeB: integer;
    function GetInModelTypeB: integer;
    function GetInModelNameB: string;
    procedure SetInModelTypeB(const Value: integer);
    procedure SetInModelNameB(const Value: string);
  public
    ActiveAttributeB: string;
    procedure UpdateInputNamesB(APathAndMask: string);
    procedure UpdateRealAttributesB(const ATableName: TFileName;
      const AModelType: integer);
    property InModelNameB: string Read GetInModelNameB Write SetInModelNameB;
    property InModelTypeB: integer Read GetInModelTypeB Write SetInModelTypeB;

    procedure ReadIniFile;
    procedure WriteIniFile;
  end;

var
  fmMethodDualDialog: TfmMethodDualDialog;

//==========================================================
implementation
//==========================================================

uses
  uGlobals,
  uCommon,
  fEditGetStatist;

{$R *.DFM}

procedure TfmMethodDualDialog.ToolButtonInputClick(Sender: TObject);
begin
  inherited;
  ListBoxInputNamesClick(Self);
end;

procedure TfmMethodDualDialog.ToolButtonInputBClick(Sender: TObject);
begin
  ToolBarInputB.Tag := (Sender as TToolButton).Tag;
  InModelTypeB      := ToolBarInputB.Tag;
  PanelInputPathB.Caption := GetPathFromTag(ToolBarInputB.Tag);
  PanelInputPathB.Hint := PanelInputPathB.Caption;

  UpdateInputNamesB(PanelInputPathB.Caption + TableMask);
  ListBoxInputNamesB.ItemIndex := 0;
  ListBoxInputNamesBClick(Self);
end;

procedure TfmMethodDualDialog.ListBoxInputNamesClick(Sender: TObject);
begin
  inherited;
  ListBoxRealAttributeClick(Self);
  ButtonOK.Enabled := (InModelName <> '') and (InModelNameB <> '');
end;

procedure TfmMethodDualDialog.ListBoxInputNamesBClick(Sender: TObject);
begin
  if ListBoxInputNamesB.Items.Count > 0 then
  begin
    InModelNameB := PanelInputPathB.Caption +
      ListBoxInputNamesB.Items[ListBoxInputNamesB.ItemIndex];
    UpdateRealAttributesB(InModelNameB, InModelTypeB);
  end
  else
  begin
    InModelNameB     := '';
    EditOutName.Text := '';
    ListBoxInputNamesB.ItemIndex := 0;
    ListBoxRealAttributeB.Items.Clear;
  end;
  dmBase.TableInputB.TableName := InModelNameB;
  ButtonOK.Enabled := (InModelName <> '') and (InModelNameB <> '');
end;


procedure TfmMethodDualDialog.UpdateRealAttributesB(const ATableName: TFileName;
  const AModelType: integer);
var
  I:      integer;
  ATable: TTable;
begin
  ListBoxRealAttributeB.Items.BeginUpdate;
  try
    ListBoxRealAttributeB.Items.Clear;
    try
      ATable := TTable.Create(Self);
      ATable.TableName := InModelNameB;
      ATable.Open;
      for I := 0 to ATable.FieldCount - 1 do
      begin
        if IsRealAttribute(ATable.Fields[I]) then
          ListBoxRealAttributeB.Items.Add(ATable.Fields[I].FieldName);
      end;
      case AModelType of
        mtPoints2D, mtTins, mtGrids2D, mtMeshes2D:
        begin
          ListBoxRealAttributeB.Items.Add(fldZ);
        end;
      end;
    except
    end;
    ATable.Close;
  finally
    ATable.Free;
    ListBoxRealAttributeB.Items.EndUpdate;
  end;
  try
    CheckItemIndex(ListBoxRealAttributeB);
  except
  end;
  ButtonOK.Enabled := ListBoxRealAttributeB.Items.Count - 1 <> 0;
end;

procedure TfmMethodDualDialog.ListBoxRealAttributeBClick(Sender: TObject);
begin
  if ListBoxRealAttributeB.Items.Count > 0 then
    ActiveAttributeB := ListBoxRealAttributeB.Items[ListBoxRealAttributeB.ItemIndex]
  else
    ActiveAttributeB := '';
end;

procedure TfmMethodDualDialog.UpdateInputNamesB(APathAndMask: string);
var
  SearchRec: TSearchRec;
begin
  ListBoxInputNamesB.Items.BeginUpdate;
  try
    ListBoxInputNamesB.Items.Clear;
    try
      if FindFirst(APathAndMask, faAnyFile and not (faDirectory or faVolumeID),
        SearchRec) = 0 then
        repeat
          ListBoxInputNamesB.Items.Add(ChangeFileExt(SearchRec.Name, ''));
        until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  finally
    ListBoxInputNamesB.Items.EndUpdate;
  end;
end;

procedure TfmMethodDualDialog.SpeedButtonInputBInfoClick(Sender: TObject);
begin
  if InModelNameB <> '' then
  begin
    fmEditGetStatist := TfmEditGetStatist.Create(Self);
    fmEditGetStatist.TableName := InModelNameB;
    fmEditGetStatist.ShowModal;
    fmEditGetStatist.Free;
  end;
end;

procedure TfmMethodDualDialog.SpeedButtonInputInfoClick(Sender: TObject);
begin
  inherited;   // to activate the button
end;

procedure TfmMethodDualDialog.SpeedButtonInputBrowseBClick(Sender: TObject);
begin
  with dmDialogs do
  begin
    OpenDialog.InitialDir := PanelInputPathB.Caption;
    if OpenDialog.Execute then
    begin
      InModelNameB := OpenDialog.FileName;
      PanelInputPathB.Caption := ExtractFilePath(OpenDialog.FileName);
      PanelInputPathB.Hint := PanelInputPath.Caption;
      UpdateInputNamesB(PanelInputPathB.Caption + TableMask);

      ListBoxInputNamesB.ItemIndex := 0;
      ListBoxInputNamesBClick(Self);
    end;
  end;
end;

procedure TfmMethodDualDialog.FormCreate(Sender: TObject);
var
  I: integer;
begin
  inherited;
  ReadIniFile;
  for I := 1 to ToolBarInputB.ButtonCount - 1 do //Except two separators 0 and 11
  begin
    if ToolBarInputB.Buttons[I].Visible and ToolBarInputB.Buttons[I].Enabled then
    begin
      if ToolBarInputB.Buttons[I].Tag = ToolBarInputB.Tag then
      begin
        ToolBarInputB.Buttons[I].Down := True;
        ToolBarInputB.Buttons[I].Click;
        ToolBarInputB.Tag := ToolBarInputB.Buttons[I].Tag;
        Break;
      end;
    end;
  end;
end;


function TfmMethodDualDialog.GetInModelTypeB: integer;
begin
  try
    Result := FInModelTypeB;
  except
    Result := mtDHoles;
  end;
end;

function TfmMethodDualDialog.GetInModelNameB: string;
begin
  try
    Result := FInModelNameB;
  except
    Result := '';
  end;
end;

procedure TfmMethodDualDialog.SetInModelTypeB(const Value: integer);
begin
  if CompareValue(FInModelTypeB, Value) <> 0 then
  begin
    FInModelTypeB := Value;
  end;
end;

procedure TfmMethodDualDialog.SetInModelNameB(const Value: string);
begin
  if CompareText(FInModelNameB, Value) <> 0 then
  begin
    FInModelNameB := Value;
    PanelInputPathB.Caption := ExtractFilePath(FInModelNameB);
    PanelInputPathB.Hint := FInModelNameB;
  end;
end;

procedure TfmMethodDualDialog.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      ToolBarInputB.Tag := ReadInteger(Name, ToolBarInputB.Name, 0);
    finally
      IniFile.Free;
    end;
end;

procedure TfmMethodDualDialog.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteInteger(Name, ToolBarInputB.Name, ToolBarInputB.Tag);
    finally
      IniFile.Free;
    end;
end;


procedure TfmMethodDualDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;

end.
