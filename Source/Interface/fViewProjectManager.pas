//------------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//---------------------------------------------------------------------------
(* The Project Manager for MapWindow datasets *)

unit fViewProjectManager;

interface

uses
  System.SysUtils, 
  System.Variants, 
  System.Classes,
  System.Math,
  System.ImageList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.CheckLst,
  Vcl.Buttons,
  Vcl.ImgList,
  Vcl.ToolWin,

  fInitialDialog,
  fGeoblock,
  fMapWindow,
  dBase;

type
  TfmViewProjectManager = class(TfmInitialDialog)
    GroupBoxPacks: TGroupBox;
    LabelList:   TLabel;
    ImageSelect: TImage;
    ScrollBox:   TScrollBox;
    CheckListBoxTable: TCheckListBox;
    CheckListBoxGraph: TCheckListBox;
    lwModels: TListView;
    CheckListBoxVisible: TCheckListBox;
    CheckListBoxSelect: TCheckListBox;
    ImageList:   TImageList;
    ToolBar:     TToolBar;
    ToolButtonOpen: TToolButton;
    ToolButtonRemove: TToolButton;
    ToolButton1: TToolButton;
    procedure ToolButtonOpenClick(Sender: TObject);
    procedure ToolButtonRemoveClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure lwModelsSelectItem(Sender: TObject; Item: TListItem;
      Selected: boolean);
    procedure lwModelsMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonOKClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure CheckListBoxVisibleClick(Sender: TObject);
    procedure SpeedButtonAddClick(Sender: TObject);
    procedure SpeedButtonOptionsClick(Sender: TObject);
  private
    FRemoved: boolean;
  public
    procedure Clear;
    procedure EnableButtons;
    procedure LoadFromFile(const FileName: TFileName);
    procedure SaveToFile(const FileName: TFileName);
    procedure AddToManager;
  end;

var
  fmViewProjectManager : TfmViewProjectManager;

//===========================================================================
implementation
//===========================================================================

uses
  cGlobals,
  uCommon,
  cProfuns,
  cResStrings;

{$R *.dfm}

type
  TProjectLineType =
    (pltUnknown,
    pltComment,
    pltMemo,
    pltVersion,
    pltDholes,
    pltdPoints2D,
    pltdPoints3D,
    pltdPolygons,
    pltdTin,
    pltdSolids,
    pltdGrid2D,
    pltdGrid3D,
    pltdMesh2D,
    pltdMesh3D,
    pltdDrawings);

const
  LineTypeStrings: array[TProjectLineType] of AnsiString =
    ('UNKNOWN',
    'COMMENT',
    'MEMO',
    'VERSION',
    'DHOLES',
    'POINTS2D',
    'POINTS3D',
    'POLYGONS',
    'TIN',
    'SOLIDS',
    'GRID2D',
    'GRID3D',
    'MESH2D',
    'MESH3D',
    'DRAWINGS');


procedure TfmViewProjectManager.SpeedButtonAddClick(Sender: TObject);
begin
  fmGeoblock.FileOpenModel.Execute;
end;

procedure TfmViewProjectManager.SpeedButtonOptionsClick(Sender: TObject);
begin
  fmGeoblock.MapOptions.Execute;
end;


procedure TfmViewProjectManager.FormActivate(Sender: TObject);
begin
  inherited;
  ToolBar.Images := ImageListInterface;
  EnableButtons;
end;

procedure TfmViewProjectManager.lwModelsSelectItem(Sender: TObject;
  Item: TListItem; Selected: boolean);
begin
  if Selected then
    if fmMapWindow <> nil then
      fmMapWindow.ChangeModel(Item.Index);
end;

procedure TfmViewProjectManager.lwModelsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
var
  ListItem: TListItem;
begin
  ListItem := lwModels.GetItemAt(X, Y);
  if (Assigned(fmMapWindow)) and (Assigned(ListItem)) then
    lwModels.Hint := fmMapWindow.ModelList[ListItem.Index].ModelName;
end;

procedure TfmViewProjectManager.FormPaint(Sender: TObject);
begin
  FRemoved := False;
end;

procedure TfmViewProjectManager.AddToManager;

const
  CommonHeight = 16;

var
  Hgt:      integer;
  ListItem: TListItem;

begin
  with fmMapWindow do
  begin
    ScrollBox.VertScrollBar.Increment := CommonHeight;
    Hgt := CommonHeight * (ModelList.Count);
    CheckListBoxVisible.ItemHeight := CommonHeight;
    CheckListBoxTable.ItemHeight := CommonHeight;
    CheckListBoxGraph.ItemHeight := CommonHeight;
    CheckListBoxSelect.ItemHeight := CommonHeight;
    if Hgt < ScrollBox.Height then
    begin
      lwModels.Height      := ScrollBox.Height;
      CheckListBoxVisible.Height := ScrollBox.Height;
      CheckListBoxTable.Height   := ScrollBox.Height;
      CheckListBoxGraph.Height   := ScrollBox.Height;
      CheckListBoxSelect.Height  := ScrollBox.Height;
    end
    else
    begin
      lwModels.Height      := Hgt;
      CheckListBoxVisible.Height := Hgt;
      CheckListBoxTable.Height   := Hgt;
      CheckListBoxGraph.Height   := Hgt;
      CheckListBoxSelect.Height  := Hgt;
    end;
    ListItem := lwModels.Items.Add;
    ListItem.Caption := NameOnly(Model.ModelName);
    if ModelList[ModelIndex] <> nil then
      ListItem.ImageIndex := ModelList[ModelIndex].ModelType
    else
      ListItem.ImageIndex := 0;
    lwModels.Selected := lwModels.Items[ModelIndex];
    CheckListBoxVisible.Items.Add('');
    CheckListBoxVisible.Checked[ModelIndex] := True;
    CheckListBoxTable.Items.Add('');
    CheckListBoxGraph.Items.Add('');
    CheckListBoxSelect.Items.Add('');
  end;
end;

procedure TfmViewProjectManager.ToolButtonRemoveClick(Sender: TObject);
var
  Num: integer;
begin
  if MessageDlg(LoadResString(@rsRemove) + '?', mtConfirmation,
    mbOKCancel, 0) = mrOk then
  begin
    FRemoved := True;
    if lwModels.Selected = nil then
    begin
      Beep;
      Exit;
    end;
    Num := lwModels.Selected.Index;
    fmMapWindow.RemoveModel(Num);
    lwModels.Items.Delete(Num);
    CheckListBoxVisible.Items.Delete(Num);
    CheckListBoxTable.Items.Delete(Num);
    CheckListBoxGraph.Items.Delete(Num);
    CheckListBoxSelect.Items.Delete(Num);
    if Num = lwModels.Items.Count then
      Dec(Num);
    lwModels.Selected := lwModels.Items[Num];
    lwModelsSelectItem(Self, lwModels.Selected,
      lwModels.Selected <> nil);
    EnableButtons;
    if lwModels.Items.Count = 0 then
    begin
      fmMapWindow.Free;
      fmMapWindow := nil;
    end;
    if fmGeoblock.MDIChildCount = 1 then
    begin
      fmGeoblock.EnableFileItems(False);
      fmGeoblock.EnableMapItems(mtAll, False);
    end;
  end;
  if fmMapWindow <> nil then
    fmMapWindow.Repaint;
end;


procedure TfmViewProjectManager.ToolButtonOpenClick(Sender: TObject);
begin
  fmGeoblock.FileOpenModel.Execute;
end;


procedure TfmViewProjectManager.CheckListBoxVisibleClick(Sender: TObject);
var
  I: integer;
  Count: Integer;
begin
  if fmMapWindow <> nil then
    with fmMapWindow do
    begin
      Count := Min(ModelCount, CheckListBoxVisible.Items.Count);
      for I := 0 to Count - 1 do
        ModelList[I].Visible := CheckListBoxVisible.Checked[I];
      Repaint;
    end;
end;


procedure TfmViewProjectManager.Clear;
begin
  lwModels.Items.Clear;
  CheckListBoxVisible.Items.Clear;
  CheckListBoxTable.Items.Clear;
  CheckListBoxGraph.Items.Clear;
  CheckListBoxSelect.Items.Clear;
  EnableButtons;
end;

procedure TfmViewProjectManager.EnableButtons;
begin
  ToolBar.Buttons[2].Enabled := not (lwModels.Items.Count = 0);
end;


function ConvertPLTtoDataType(Value: TProjectLineType): integer;
begin
  Result := Ord(Value) - (Ord(pltdHoles) - Ord(mtFirst));
end;

function ConvertDataTypeToPLT(Value: integer): TProjectLineType;
begin
  Result := TProjectLineType(Ord(Value) + (Ord(pltdHoles) - Ord(mtFirst)));
end;


procedure TfmViewProjectManager.LoadFromFile(const FileName: TFileName);
var
  I: integer;
  FileText: TStringList;
  CurLine: AnsiString;
  LineType: TProjectLineType;

  {sub}
  function CutWord(var Line: AnsiString): AnsiString;
  const
    ValidChars = ['0'..'9', 'A'..'Z', 'a'..'z', 'À'..'ï', 'ð'..'ÿ'];
  var
    WordEnd:   integer;
    WordStart: integer;
  begin
    Result    := '';
    WordStart := 1;
    while (WordStart <= Length(Line)) and (Line[WordStart] in [' ', #9]) do
      Inc(WordStart);
    WordEnd := WordStart;
    while (WordEnd <= Length(Line)) and (Line[WordEnd] in ValidChars) do
      Inc(WordEnd);
    Result := Copy(Line, WordStart, WordEnd - WordStart);
    Delete(Line, 1, WordEnd);
  end;

  {sub}
  function CutLineType(var Line: AnsiString): TProjectLineType;
  var
    TypeWord: AnsiString;
  begin
    TypeWord := CutWord(Line);
    Result   := (High(LineTypeStrings));
    while (Result > pltUnknown) and (TypeWord <> LineTypeStrings[Result]) do
      Dec(Result);
  end;

begin
  fmMapWindow.Free;
  Clear;
  FileText := TStringList.Create;
  try
    FileText.LoadFromFile(FileName);
    for I := 0 to FileText.Count - 1 do
    begin
      CurLine  := FileText[I];
      LineType := CutLineType(CurLine);
      CurLine  := Trim(CurLine);
      case LineType of
        pltDholes..pltdMesh3D:
        if FileExists(ChangeFileExt(CurLine, TableExt)) then
        try
          fmGeoblock.ShowMap(Trim(CurLine), ConvertPLTtoDataType(LineType));
        except
        end;
      end;
    end;
  finally
    FileText.Free;
  end;
end;

procedure TfmViewProjectManager.SaveToFile(const FileName: TFileName);

{sub}procedure Write(Stream: TStream; Str: AnsiString);
  begin
    Stream.Write(Str[1], Length(Str));
  end;

  {sub}procedure Writeln(Stream: TStream; Str: AnsiString = '');
  begin
    if Length(Str) > 0 then
      Stream.Write(Str[1], Length(Str));
    Str := CRLF;
    Stream.Write(Str[1], Length(Str));
  end;

var
  FileStream: TFileStream;
  FileVersion: cardinal;
  I: integer;

begin
  ForceDirectories(ExtractFilePath(FileName));
  FileStream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  try
    FileStream.Size := 0;
    FileVersion     := GetFileVersion(ParamStr(0));
    Writeln(FileStream, LineTypeStrings[pltVersion] + ' ' + FloatToStr(FileVersion));
    Writeln(FileStream, LineTypeStrings[pltComment] + ' Written by : Geoblock');
    Writeln(FileStream, LineTypeStrings[pltComment] + ' Date       : ' +
      DateTimeToStr(Now));
    Writeln(FileStream);
    if fmMapWindow <> nil then
    begin
      with fmMapWindow, ModelList do
        for I := 0 to Count - 1 do
        begin
          Writeln(FileStream, LineTypeStrings[ConvertDataTypetoPLT(Items[I].ModelType)] +
            ' ' + Items[I].ModelName);
        end;
    end
    else
    begin
      Writeln(FileStream, LineTypeStrings[pltComment] + ' ' +
        LineTypeStrings[pltdHoles] + ' ' + ExpandPath(DirDholes));
      Writeln(FileStream, LineTypeStrings[pltComment] + ' ' +
        LineTypeStrings[pltdPoints2D] + ' ' + ExpandPath(DirPoints2D));
      Writeln(FileStream, LineTypeStrings[pltComment] + ' ' +
        LineTypeStrings[pltdPoints3D] + ' ' + ExpandPath(DirPoints3D));
      Writeln(FileStream, LineTypeStrings[pltComment] + ' ' +
        LineTypeStrings[pltdPolygons] + ' ' + ExpandPath(DirPolygonPoly));
      Writeln(FileStream, LineTypeStrings[pltComment] + ' ' +
        LineTypeStrings[pltdTin] + ' ' + ExpandPath(DirTinFaces));
      Writeln(FileStream, LineTypeStrings[pltComment] + ' ' +
        LineTypeStrings[pltdSolids] + ' ' + ExpandPath(DirSolids));
      Writeln(FileStream, LineTypeStrings[pltComment] + ' ' +
        LineTypeStrings[pltdGrid2D] + ' ' + ExpandPath(DirGrid2D));
      Writeln(FileStream, LineTypeStrings[pltComment] + ' ' +
        LineTypeStrings[pltdGrid3D] + ' ' + ExpandPath(DirGrid3D));
      Writeln(FileStream, LineTypeStrings[pltComment] + ' ' +
        LineTypeStrings[pltdMesh2D] + ' ' + ExpandPath(DirMesh2D));
      Writeln(FileStream, LineTypeStrings[pltComment] + ' ' +
        LineTypeStrings[pltdMesh3D] + ' ' + ExpandPath(DirMesh3D));
    end;
  finally
    FileStream.Free;
  end;
end;

procedure TfmViewProjectManager.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
  inherited;
end;

procedure TfmViewProjectManager.ButtonOKClick(Sender: TObject);
begin
  fmGeoblock.Repaint;
  Hide;
  inherited;
end;


end.
