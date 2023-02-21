//---------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//---------------------------------------------------------------------
(* Conversion routines for Geoblock's model types *)

unit fMethodConversion;

interface

uses
  Winapi.Windows,
  System.SysUtils, 
  System.Variants, 
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
  Vcl.Buttons,
  Vcl.ToolWin,
  Vcl.ExtCtrls,
  //DB
  Bde.DBTables,
  Data.DB,

  dBase,
  dDialogs,
  fMethodDialog;

type
  TfmMethodConversion = class(TfmMethodDialog)
    ToolBarOutput:    TToolBar;
    ToolButton5:      TToolButton;
    ToolButtonDholesOut: TToolButton;
    ToolButtonPoints2DOut: TToolButton;
    ToolButtonPoints3DOut: TToolButton;
    ToolButtonPolygonsOut: TToolButton;
    ToolButtonTinOut: TToolButton;
    ToolButtonSolidsOut: TToolButton;
    ToolButtonGrid2DOut: TToolButton;
    ToolButtonGrid3DOut: TToolButton;
    ToolButtonMesh2DOut: TToolButton;
    ToolButtonMesh3DOut: TToolButton;
    ToolButton16:     TToolButton;
    EditSolidBottom:  TEdit;
    CheckBoxSolidBottom: TCheckBox;
    procedure ToolButtonOutputClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ToolButtonInputClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EditOutNameChange(Sender: TObject);
    procedure SpeedButtonOutputBrowseClick(Sender: TObject);
  private
     
    procedure EnableOutputButtons;
    procedure ConvertTinToMesh2D(const TinTriangleName, Mesh2DName: TFileName);
    procedure ConvertGrid3D_Grid2D(SourceName, DestName: TFileName;
      ProgressBar: TProgressBar);
    procedure ConvertMesh2DToSolids(const Mesh2DName, SolidsName: TFileName);
    procedure ConvertMesh3D_Solids(SourceName, DestName: TFileName;
      ProgressBar: TProgressBar);
    procedure Conversion;
    procedure ReadIniFile;
    procedure WriteIniFile;
  public
     
  end;

var
  fmMethodConversion: TfmMethodConversion;

implementation

uses
  cGlobals,
  uCommon,
  cProfuns,
  uFileCreator;

{$R *.dfm}


const
  IdVertex = 1;
  idNo     = 2;

type
  TIntegerArray = array[IdVertex..idNo] of integer;
  T4NodesArray  = array[1..4] of TIntegerArray;

type
  TSolidCell = class
  private
    FID_CELL3D: integer;
    FN:      T4NodesArray;
    FBodyNo: integer;
  public
    constructor Create(AID_CELL3D: integer; AN: T4NodesArray);
    property N: T4NodesArray Read FN Write FN;
    property BodyNo: integer Read FBodyNo;
  end;

type
  TSolidCellList = array[0..MaxListSize - 1] of TSolidCell;
  PSolidCellList = ^TSolidCellList;

  TSolidList = class(TList)
  private
    function GetItems(Index: integer): TSolidCell;
    procedure SetItems(Index: integer; const Value: TSolidCell);
    function GetList: PSolidCellList;
  public
    property Items[Index: integer]: TSolidCell Read GetItems Write SetItems; default;
    property List: PSolidCellList Read GetList;
    function Add(Item: TSolidCell): integer;
    procedure Clear; override;
    procedure Delete(Index: integer);
    function IndexOf(Item: TSolidCell): integer;
  end;

procedure SortArrayNo(var Arr: T4NodesArray); overload;
var
  I, J, MinJ: integer;
  Temp: TIntegerArray;
begin
  for I := Low(Arr) to High(Arr) - 1 do
  begin
    MinJ := I;
    for J := I + 1 to High(Arr) do
      if Arr[J, idNo] < Arr[MinJ, idNo] then
        MinJ := J;
    if I <> MinJ then
    begin
      Temp      := Arr[MinJ];
      Arr[MinJ] := Arr[I];
      Arr[I]    := Temp;
    end;
  end;
end;

procedure SortArrayID(var Arr: T4NodesArray); overload;
var
  I, J, MinJ: integer;
  Temp: TIntegerArray;
begin
  for I := Low(Arr) to High(Arr) - 1 do
  begin
    MinJ := I;
    for J := I + 1 to High(Arr) do
      if Arr[J, IdVertex] < Arr[MinJ, IdVertex] then
        MinJ := J;
    if I <> MinJ then
    begin
      Temp      := Arr[MinJ];
      Arr[MinJ] := Arr[I];
      Arr[I]    := Temp;
    end;
  end;
end;

function Compare(I1, I2: integer): integer; overload;
begin
  if I1 < I2 then
    Result := -1
  else if I1 > I2 then
    Result := 1
  else
    Result := 0;
end;

function Compare(Arr1, Arr2: T4NodesArray): integer; overload;
var
  I: integer;
  MinSize: integer;
begin
  I := Low(Arr1);
  MinSize := Min(High(Arr1), High(Arr2));
  while (I <= MinSize) and (Arr1[I, IdVertex] = Arr2[I, IdVertex]) do
    Inc(I);
  if (I > MinSize) then
    if High(Arr1) < High(Arr2) then
      Result := -1
    else if High(Arr1) > High(Arr2) then
      Result := 1
    else
      Result := 0
  else
    Result := Compare(Arr1[I, IdVertex], Arr2[I, IdVertex]);
end;

function Compare(Item1, Item2: TSolidCell): integer; overload;
begin
  //  Result := Compare(Item1.FBodyNo,Item1.FBodyNo);
  Result := Compare(Item1.N, Item2.N);
end;

{ TMesh2DCell }

constructor TSolidCell.Create(AID_CELL3D: integer; AN: T4NodesArray);
begin
  FID_Cell3D := AID_CELL3D;
  FN := AN;
  SortArrayID(FN);
end;

{ TSolidList }

function TSolidList.Add(Item: TSolidCell): integer;
var
  Index: integer;
begin
  Index := IndexOf(Item);
  if Index < 0 then
    Result := inherited Add(Item)
  else
  begin
    Result := -1;
    Delete(Index);
    Item.Free;
  end;
end;

procedure TSolidList.Clear;
var
  I: integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Free;
  inherited Clear;
end;

procedure TSolidList.Delete(Index: integer);
begin
  Items[Index].Free;
  inherited Delete(Index);
end;

function TSolidList.GetItems(Index: integer): TSolidCell;
begin
  Result := TSolidCell(inherited Items[Index]);
end;

function TSolidList.GetList: PSolidCellList;
begin
  Result := PSolidCellList(inherited List);
end;

function TSolidList.IndexOf(Item: TSolidCell): integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (compare(Item, Items[Result]) <> 0) do
    Dec(Result);
end;

procedure TSolidList.SetItems(Index: integer; const Value: TSolidCell);
begin
  inherited Items[Index] := Value;
end;


procedure TfmMethodConversion.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
  EnableOutputButtons;
  ToolBarOutput.Buttons[OutModelType + 1].Down := True;
  ToolBarOutput.Buttons[OutModelType + 1].Click;
end;

procedure TfmMethodConversion.ToolButtonInputClick(Sender: TObject);
var
  I: integer;
begin
  inherited;
  EnableOutputButtons;

  for I := 1 to ToolBarOutput.ButtonCount - 1 do //Except two separators 0 and 11
  begin
    if ToolBarOutput.Buttons[I].Enabled then
    begin
      OutModelType := ToolBarOutput.Buttons[I].Tag;
      ToolBarOutput.Buttons[I].Down := True;
      ToolBarOutput.Buttons[I].Click;
      Break;      //On a first button on ToolBarOutput
    end;
  end;
end;

procedure TfmMethodConversion.EnableOutputButtons;
var
  I: integer;
begin
  for  I := 0 to ToolBarOutput.ButtonCount - 1 do
    ToolBarOutput.Buttons[I].Enabled := False;   //Default
  case InModelType of
    mtDholes:
    begin
      ToolButtonPoints3DOut.Enabled := True;
    end;
    mtPoints2D:
    begin
      ToolButtonPoints3DOut.Enabled := True;
    end;
    mtPoints3D:
    begin
      ToolButtonPoints2DOut.Enabled := True;
    end;
    mtPolygons:
    begin
      ToolButtonPoints2DOut.Enabled := True;
      ToolButtonPoints3DOut.Enabled := True;
      ToolButtonTinOut.Enabled      := True;
    end;
    mtTins:
    begin
      ToolButtonPoints2DOut.Enabled := True;
      ToolButtonSolidsOut.Enabled   := True;
      ToolButtonMesh2DOut.Enabled   := True;
    end;
    mtSolids:
    begin
      ToolButtonPoints3DOut.Enabled := True;
      ToolButtonMesh3DOut.Enabled   := True;
    end;
    mtGrids2D:
    begin
      ToolButtonPoints2DOut.Enabled := True;
      ToolButtonGrid3DOut.Enabled   := True;
      ToolButtonMesh2DOut.Enabled   := True;
    end;
    mtGrids3D:
    begin
      ToolButtonPoints3DOut.Enabled := True;
      ToolButtonGrid2DOut.Enabled   := True;
      ToolButtonSolidsOut.Enabled   := True;
      ToolButtonMesh3DOut.Enabled   := True;
    end;
    mtMeshes2D:
    begin
      ToolButtonPoints2DOut.Enabled := True;
      ToolButtonTinOut.Enabled      := True;
      ToolButtonSolidsOut.Enabled   := True;
    end;
    mtMeshes3D:
    begin
      ToolButtonPoints3DOut.Enabled := True;
      ToolButtonSolidsOut.Enabled   := True;
    end;
  end;
end;


procedure TfmMethodConversion.ToolButtonOutputClick(Sender: TObject);
var
  I: integer;
begin
  OutModelType := (Sender as TToolButton).Tag;
  for I := 1 to ToolBarOutput.ButtonCount - 1 do
    ToolBarOutput.Buttons[I].Down := False;
  ToolBarOutput.Buttons[OutModelType + 1].Down := True;
  EditSolidBottom.Visible     := False;
  CheckBoxSolidBottom.Visible := False;

  case OutModelType of
    mtDholes: PanelOutPath.Caption   := ExpandPath(DirDholes);
    mtPoints2D: PanelOutPath.Caption := ExpandPath(DirPoints2D);
    mtPoints3D: PanelOutPath.Caption := ExpandPath(DirPoints3D);
    mtPolygons: PanelOutPath.Caption := ExpandPath(DirPolygonPoly);
    mtTins: PanelOutPath.Caption      := ExpandPath(DirTinFaces);
    mtSolids:
    begin
      PanelOutPath.Caption := ExpandPath(DirSolidCells);
      if (InModelType = mtTins) or (InModelType = mtMeshes2D) then
      begin
        EditSolidBottom.Visible     := True;
        CheckBoxSolidBottom.Visible := True;
      end;
    end;
    mtGrids2D: PanelOutPath.Caption := ExpandPath(DirGrid2D);
    mtGrids3D: PanelOutPath.Caption := ExpandPath(DirGrid3D);
    mtMeshes2D: PanelOutPath.Caption := ExpandPath(DirMesh2D);
    mtMeshes3D: PanelOutPath.Caption := ExpandPath(DirMesh3D);
  end;

  PanelOutPath.Hint := PanelOutPath.Caption;
  OutModelName      := PanelOutPath.Caption + EditOutName.Text;
end;

procedure TfmMethodConversion.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;


procedure TfmMethodConversion.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      OutModelType := ReadInteger(Name, ToolBarOutput.Name, 0);
      CheckBoxSolidBottom.Checked := ReadBool(Name, CheckBoxSolidBottom.Name, True);
      EditSolidBottom.Text := ReadString(Name, EditSolidBottom.Name, '0');
    finally
      IniFile.Free;
    end;
end;

procedure TfmMethodConversion.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteInteger(Name, ToolBarOutput.Name, OutModelType);
      WriteBool(Name, CheckBoxSolidBottom.Name, CheckBoxSolidBottom.Checked);
      WriteString(Name, EditSolidBottom.Name, EditSolidBottom.Text);
    finally
      IniFile.Free;
    end;
end;

procedure TfmMethodConversion.EditOutNameChange(Sender: TObject);
begin
  OutModelName := PanelOutPath.Caption + EditOutName.Text;
end;

procedure TfmMethodConversion.SpeedButtonOutputBrowseClick(Sender: TObject);
begin
  with dmDialogs do
  begin
    OpenDialog.InitialDir := PanelOutPath.Caption;
    OpenDialog.FileName   := NameOnly(OutModelName) + TableExt;
    if OpenDialog.Execute then
    begin
      OutModelName      := OpenDialog.FileName;
      PanelOutPath.Caption := ExtractFilePath(OpenDialog.FileName);
      PanelOutPath.Hint := PanelOutPath.Caption;
      EditOutName.Text  := NameOnly(OpenDialog.FileName);
    end;
  end;
end;

 //-----------------------------------------------------------------\\
 // Converts Tin to Mesh2D data complete
 //-----------------------------------------------------------------\\
procedure TfmMethodConversion.ConvertTinToMesh2D(
  const TinTriangleName, Mesh2DName: TFileName);
var
  I, K: integer;
  TableTinTriangle: TTable;
  TableTinVertex: TTable;

  TableMesh2DLink:    TTable;
  TableMesh2DMatrix:  TTable;
  TableMesh2DElement: TTable;
  TableMesh2DNode:    TTable;

  TinVertexName: TFileName;
  Mesh2DMatrixName, Mesh2DElementName, Mesh2DNodeName: TFileName;
  bFailIfExists: boolean;

  OldFieldName, NewFieldName: string;
  Comma:     char;
  OldCursor: TCursor;

begin
  with dmBase do
  begin
    Screen.Cursor := crHourGlass;
    try
      TableTinTriangle   := TTable.Create(nil);
      TableTinVertex     := TTable.Create(nil);
      TableMesh2DNode    := TTable.Create(nil);
      TableMesh2DElement := TTable.Create(nil);
      TableMesh2DMatrix  := TTable.Create(nil);
      TableMesh2DLink    := TTable.Create(nil);
      TableTinTriangle.TableName := TinTriangleName;
      TinVertexName      := ChangeModelTable(DirTinFaces, DirTinVertices, TinTriangleName);
      TableTinVertex.TableName := TinVertexName;

      Mesh2DMatrixName  := ChangeModelTable(DirMesh2D, DirMesh2DFaces, Mesh2DName);
      Mesh2DNodeName    := ChangeModelTable(DirMesh2D, DirMesh2DVertices, Mesh2DName);

      CreateMesh2DTables(Mesh2DName);   //All four tables
      CopyFile(PChar(TinVertexName + TableExt),
        PChar(Mesh2DNodeName + TableExt), bFailIfExists = False);
      CopyFile(PChar(TinTriangleName + TableExt),
        PChar(Mesh2DElementName + TableExt), bFailIfExists = False);

      //Renames fields ID and writes records in Mesh2DLink table
      TableMesh2DNode.TableName    := Mesh2DNodeName;
      TableMesh2DElement.TableName := Mesh2DElementName;
      TableMesh2DMatrix.TableName  := Mesh2DMatrixName;
      TableMesh2DLink.TableName    := Mesh2DName;
      try
        TableMesh2DMatrix.Open;
        TableMesh2DNode.Open;
        TableMesh2DElement.Open;
        TableMesh2DLink.Open;
        //Renames ID field in Mesh2DNode to ID_NODE field using Query.SQL
        Comma := ' ';
        Query.Close;
        Query.Sql.Clear;
        Query.Sql.Add('SELECT ');
        for I := 0 to TableMesh2DNode.Fields.Count - 1 do
        begin
          Query.Sql[0] := Query.Sql[0] + Format('%s T."%S"',
            [Comma, TableMesh2DNode.Fields[I].FieldName]);
          if CompareText(TableMesh2DNode.Fields[I].FieldName, fldID) = 0 then
            Query.Sql[0] := Query.Sql[0] + Format(' as T."%S"',
              [fldID_NODE]);
          Comma := ',';
        end;
        Query.Sql.Add(Format('FROM "%s" T', [TableMesh2DNode.TableName]));
        TableMesh2DNode.BatchMove(Query, batCopy);
        //Renames ID Field in Mesh2DElement to ID_ELEMENT field
        Comma := ' ';
        Query.Sql.Clear;
        Query.Sql.Add('SELECT ');
        for I := 0 to TableMesh2DElement.Fields.Count - 1 do
        begin
          Query.Sql[0] := Query.Sql[0] + Format('%s T."%S"',
            [Comma, TableMesh2DElement.Fields[I].FieldName]);
          if CompareText(TableMesh2DElement.Fields[I].FieldName, fldID) = 0 then
            Query.Sql[0] := Query.Sql[0] + Format(' as T."%S"',
              [fldID_ELEMENT]);
          Comma := ',';
        end;
        Query.Sql.Add(Format('FROM "%s" T', [TableMesh2DElement.TableName]));
        TableMesh2DElement.BatchMove(Query, batCopy);

        //Writes only one record into Mesh2DMatrix table
        Query.Sql.Clear;
        Query.Sql.Add('SELECT AVG(' + fldX + '), AVG(' + fldY + '), AVG(' +
          fldZ + '),' + ' SUM(' + fldAREA + '), AVG(' + fldMATERIAL +
          ')' + ' FROM "' + TableMesh2DElement.TableName + '"');
        Query.Open;
        TableMesh2DMatrix.Last;
        TableMesh2DMatrix.Append;
        TableMesh2DMatrix.FieldByName(fldID_MATRIX).AsInteger := 1;
        TableMesh2DMatrix.FieldByName(fldX).AsFloat     :=
          RoundTo(Query.Fields[0].AsFloat, Precision);
        TableMesh2DMatrix.FieldByName(fldY).AsFloat     :=
          RoundTo(Query.Fields[1].AsFloat, Precision);
        TableMesh2DMatrix.FieldByName(fldZ).AsFloat     :=
          RoundTo(Query.Fields[2].AsFloat, Precision);
        TableMesh2DMatrix.FieldByName(fldAREA).AsFloat  :=
          RoundTo(Query.Fields[3].AsFloat, Precision);
        TableMesh2DMatrix.FieldByName(fldMATERIAL).AsInteger :=
          Query.Fields[4].AsInteger;
        TableMesh2DMatrix.FieldByName(fldNAME).AsString := NameOnly(TinTriangleName);
        TableMesh2DMatrix.Post;
        TableMesh2DMatrix.Next;

        //Writes ID_MATRIX, ID_ELEMENT and ID_NODE into Mesh2DLink
        TableTinTriangle.Open;
        TableMesh2DLink.Last;
        K := 0;
        for I := 0 to TableTinTriangle.RecordCount - 1 do
        begin
          Inc(K);
          TableMesh2DLink.Append;
          TableMesh2DLink.FieldByName(fldID_MATRIX).AsInteger := 1;
          TableMesh2DLink.FieldByName(fldID_ELEMENT).AsInteger := K;
          TableMesh2DLink.FieldByName(fldID_NODE).AsInteger   :=
            TableTinTriangle.FieldByName(fldV1).AsInteger;
          TableMesh2DLink.Post;
          TableMesh2DLink.Append;
          TableMesh2DLink.FieldByName(fldID_MATRIX).AsInteger := 1;
          TableMesh2DLink.FieldByName(fldID_ELEMENT).AsInteger := K;
          TableMesh2DLink.FieldByName(fldID_NODE).AsInteger   :=
            TableTinTriangle.FieldByName(fldV2).AsInteger;
          TableMesh2DLink.Post;
          TableMesh2DLink.Append;
          TableMesh2DLink.FieldByName(fldID_MATRIX).AsInteger := 1;
          TableMesh2DLink.FieldByName(fldID_ELEMENT).AsInteger := K;
          TableMesh2DLink.FieldByName(fldID_NODE).AsInteger   :=
            TableTinTriangle.FieldByName(fldV3).AsInteger;
          TableMesh2DLink.Post;
          TableTinTriangle.Next;
        end;
      finally
        TableMesh2DNode.Close;
        TableMesh2DElement.Close;
        TableMesh2DMatrix.Close;
        TableMesh2DLink.Close;
        TableTinTriangle.Close;
        Query.Close;
      end;
    finally
      TableMesh2DLink.Free;
      TableMesh2DElement.Free;
      TableMesh2DNode.Free;
      TableMesh2DMatrix.Free;
      TableTinTriangle.Free;
      TableTinVertex.Free;
    end;
    Screen.Cursor := OldCursor;
  end;
end;


//====================== Grid3D to Grid2D ========================\\
{ Converts Grid3D model into Grid 2D model.
  Keeps X and Y of nodes immutable but assigns an upper or lower Z
  from Grid3D columns to Grid2D nodes by selection.
  Finds average values for all real attributes:
   - summarize a value in nodes along a column;
   - divide the sum by number NZ of Grid3D layers.
  Summarizes all indices in integer field MATERIAL of Grid3D
  and saves results as thicknesses (N * DZ) in added fields
  MAT_INDEX1, MAT_INDEX2 etc.                                     }
//==============================================================\\
procedure TfmMethodConversion.ConvertGrid3D_Grid2D(SourceName, DestName: TFileName;
  ProgressBar: TProgressBar);
type
  TParsRec = record
    DX, DY, DZ, XO, YO, ZO: double;
    NX, NY, NZ: integer;
  end;
var
  Par:  TParsRec;
  TableSrc, TableDst: TTable;
  I, J: integer;
  ShiftX, ShiftY: double;

begin
  TableSrc := TTable.Create(Application);
  TableDst := TTable.Create(Application);
  try
    with Par do
    begin
      TableSrc.TableName := SourceName;
      TableDst.TableName := DestName;

      SourceName := ExtractFilePath(SourceName) + NameOnly(SourceName) + ParExt;
      DestName   := ExtractFilePath(DestName) + NameOnly(DestName) + ParExt;
      CopyFile(PChar(SourceName), PChar(DestName), False);

      ReadParFile(DestName, XO, YO, ZO, DX, DY, DZ, NX, NY, NZ);
      TableSrc.Open;
      TableDst.FieldDefs.Clear;
      for I := 0 to TableSrc.FieldCount - 1 do
        with TableSrc.Fields[I] do
        begin
          if (DataType in [ftFloat, ftCurrency]) or (FieldName = fldX) or
            (FieldName = fldY) or (FieldName = fldZ) or
            (FieldName = fldID) then
            TableDst.FieldDefs.Add(FieldName, DataType, 0, Required);
        end;
      TableDst.CreateTable;
      TableDst.Open;
      for I := 0 to NX do
        for J := 0 to NY do
        begin
          TableDst.Append;
          try
            TableDst[fldX] := XO + I * DX;
          except
          end;
          try
            TableDst[fldY] := YO + J * DY;
          except
          end;
          try
            TableDst[fldZ] := ZO;
          except
          end;
          TableDst.Post;
          TableDst.Edit;
          try
            TableDst[fldID] := TableDst.RecNo;
          except
          end;
          TableDst.Post;
        end;
      TableSrc.First;
      TableDst.First;
      ProgressBar.Min := 1;
      ProgressBar.Max := TableSrc.RecordCount;
      ShiftX := DX / 2 * Abs(Round(Frac((TableSrc[fldX] - XO) / DX) * 2));
      ShiftY := DY / 2 * Abs(Round(Frac((TableSrc[fldY] - YO) / DY) * 2));
      for I := 1 to TableSrc.RecordCount do
      begin
        try
          ProgressBar.Position := I;
          J := Round((TableSrc[fldX] - XO + ShiftX) / DX * (NY + 1) +
            (TableSrc[fldY] - YO + ShiftY) / DY) + 1;
          TableDst.RecNo := J;
          TableDst.Edit;
          for J := 0 to TableDst.FieldCount - 1 do
          begin
            with TableDst.Fields[J] do
              if IsRealAttribute(TableDst.Fields[J]) then
                if VarIsNull(TableDst[FieldName]) or
                  VarIsEmpty(TableDst[FieldName]) then
                  TableDst[FieldName] := TableSrc[FieldName]
                else
                  TableDst[FieldName] :=
                    TableDst[FieldName] + TableSrc[FieldName];
          end;
          TableDst.Post;
        except
          Beep;
        end;
        TableSrc.Next;
      end;
    end;
  finally
    try
      TableSrc.Close;
      TableDst.Close;
    except
    end;
    TableSrc.Free;
    TableDst.Free;
  end;
end;

(*-------------------------------------------------------------------
 Converts a set of open Mesh2D models into a Solid model
 1. Copies all mesh nodes to solids vertices
 2. Takes first two meshes from matrix
 3. Adds meshes elements as triangles into first solid
 4. Extracts outer boundaries or exteriers for meshes
 5. Creates binding or link triangles between two meshes
 6. Adds the binding triangles into the solid
 7. Repeats from step 2 for other pairs until end of meshes
 ------------------------------------------------------------------*)
procedure TfmMethodConversion.ConvertMesh2DToSolids(
  const Mesh2DName, SolidsName: TFileName);
begin
end;

 //============================================\\ { Converts Mesh3D model into Solids model     }
 //============================================\\
procedure TfmMethodConversion.ConvertMesh3D_Solids(SourceName, DestName: TFileName;
  ProgressBar: TProgressBar);
type
  TNodeArr = array[1..8] of TIntegerArray;

var
  TableMesh3DElement, TableMesh3DNode, TableMesh3DLink: TTable;

  TableSolidBody, TableSolidFacet, TableSolidVertex, TableSolidLink: TTable;

  Count:     integer; //Nodes For a Cell
  I, J:      integer;
  Mesh3DVertices: TNodeArr;
  SolidVertices: T4NodesArray;
  ID_CELL1, ID_CELL2: integer;
  SolidList: TSolidList;
  Query:     TQuery;
const
  SolidCellNodeNo: array[1..6, 1..4] of byte =
    ((1, 2, 3, 4),
    (1, 2, 5, 6),
    (1, 3, 5, 7),
    (2, 4, 6, 8),
    (3, 4, 7, 8),
    (5, 6, 7, 8));

  {sub}
  procedure SortArray(var Arr: TNodeArr); overload;
  var
    I, J: integer;
    MinNodeNoJ: integer;
    Temp: TIntegerArray;
  begin
    for I := Low(Arr) to High(Arr) - 1 do
    begin
      MinNodeNoJ := I;
      for J := I + 1 to High(Arr) do
        if Arr[J, idNo] < Arr[MinNodeNoJ, idNo] then
          MinNodeNoJ := J;
      if I <> MinNodeNoJ then
      begin
        Temp   := Arr[I];
        Arr[I] := Arr[MinNodeNoJ];
        Arr[MinNodeNoJ] := Temp;
      end;
    end;
  end;

begin
  TableMesh3DElement := TTable.Create(Application);
  try
    TableMesh3DLink := TTable.Create(Application);
    try
      TableMesh3DNode := TTable.Create(Application);
      try
        TableMesh3DElement.TableName := SourceName;
        TableMesh3DLink.TableName    :=
          ChangeModelTable(DirMesh3DElement, DirMesh3D, SourceName);
        TableMesh3DNode.TableName    :=
          ChangeModelTable(DirMesh3DElement, DirMesh3DNode, SourceName);

        TableSolidFacet := TTable.Create(Application);
        try
          TableSolidLink := TTable.Create(Application);
          try
            TableSolidVertex := TTable.Create(Application);
            try
              CreateSolidTables(DestName);
              TableSolidBody := TTable.Create(Application);
              try
                TableSolidBody.TableName := DestName;
                TableSolidBody.Open;
                TableSolidBody.Append;
                TableSolidBody.FieldByName(fldID).AsInteger := 1;
                TableSolidBody.FieldByName(fldNAME).AsString := NameOnly(DestName);
                TableSolidBody.Post;
              finally
                TableSolidBody.Free;
              end;
              TableSolidFacet.TableName  :=
                ChangeModelTable(DirSolidCells, DirSolidFaces, DestName);
              TableSolidLink.TableName   :=
                ChangeModelTable(DirSolidCells, DirSolids, DestName);
              TableSolidVertex.TableName :=
                ChangeModelTable(DirSolidCells, DirSolidFaces, DestName);

              TableMesh3DLink.Open;

              ID_CELL1  := TableMesh3DLink[fldID_ELEMENT];
              SolidList := TSolidList.Create;
              try
                ProgressBar.Min := 1;
                ProgressBar.Max := TableMesh3DLink.RecordCount;
                for I := 1 to 4 do
                  SolidVertices[I, idNo] := I;
                Count := 0;
                for I := 0 to TableMesh3DLink.RecordCount - 1 do
                begin
                  if I mod 100 = 0 then
                    ProgressBar.Position := TableMesh3DLink.RecNo;
                  if Count < 8 then
                    Inc(Count);
                  Mesh3DVertices[Count, IdVertex] := TableMesh3DLink[fldID_VERTEX];
                  Mesh3DVertices[Count, idNo]     := TableMesh3DLink[fldID_NO];
                  TableMesh3DLink.Next;
                  ID_CELL2 := TableMesh3DLink[fldID_ELEMENT];
                  if TableMesh3DLink.EOF then
                    ID_CELL2 := ID_CELL1 + 1;
                  if (ID_CELL1 <> ID_CELL2) then
                  begin
                    if (Count = 8) then
                    begin
                      SortArray(Mesh3DVertices);
                      for J := 1 to 6 do
                      begin
                        SolidVertices[1, IdVertex] :=
                          Mesh3DVertices[SolidCellNodeNo[J, 1], IdVertex];
                        SolidVertices[2, IdVertex] :=
                          Mesh3DVertices[SolidCellNodeNo[J, 2], IdVertex];
                        SolidVertices[3, IdVertex] :=
                          Mesh3DVertices[SolidCellNodeNo[J, 3], IdVertex];
                        SolidVertices[4, IdVertex] :=
                          Mesh3DVertices[SolidCellNodeNo[J, 4], IdVertex];

                        SolidList.Add(TSolidCell.Create(ID_CELL1,
                          SolidVertices));
                      end;
                    end;
                    ID_CELL1 := ID_CELL2;
                    Count    := 0;
                  end;
                end;
                ProgressBar.Position := TableMesh3DLink.RecNo;

                // TableSolidLink.CreateTable;
                TableSolidLink.Open;

                for I := 0 to SolidList.Count - 1 do
                begin
                  SolidVertices := SolidList.Items[I].N;
                  SortArrayNo(SolidVertices);
                  for J := 1 to 4 do
                  begin
                    TableSolidLink.Append;
                    TableSolidLink[fldID_CELL] := 1;  //Temporarily
                    TableSolidLink[fldID_TRIANGLE] := I;
                    TableSolidLink[fldID_VERTEX] := SolidVertices[J, IdVertex];
                    TableSolidLink[fldID_NO] := SolidVertices[J, idNo];
                    TableSolidLink.Post;
                  end;
                end;
              finally
                SolidList.Free;
              end;
              TableSolidLink.Close;
              Query := TQuery.Create(nil);
              try
                Query.SQL.Add('SELECT DISTINCT L.' + fldID_TRIANGLE +
                  ' ID, 1 ID_MATRIX, C3D.*');
                Query.SQL.Add(Format('FROM "%s" L, "%s" C3D',
                  [TableSolidLink.TableName,
                  TableMesh3DElement.TableName]));
                Query.SQL.Add('ORDER BY L.' + fldID_TRIANGLE);
                Query.Open;
                with TBatchMove.Create(nil) do
                  try
                    Source := Query;
                    Destination := TableSolidFacet;
                    Mode := batCopy;
                    Execute;
                  finally
                    Free;
                  end;
                Query.Close;
                Query.SQL.Clear;
                Query.SQL.Add('SELECT DISTINCT N3D.*');
                Query.SQL.Add(Format('FROM "%s" L, "%s" N3D',
                  [TableSolidLink.TableName,
                  TableMesh3DNode.TableName]));
                Query.SQL.Add('WHERE N3D.ID=L.' + fldID_VERTEX);
                Query.Open;
                with TBatchMove.Create(nil) do
                  try
                    Source := Query;
                    Destination := TableSolidVertex;
                    Mode := batCopy;
                    Execute;
                  finally
                    Free;
                  end;
              finally
                Query.Free;
              end;
            finally
              TableSolidVertex.Free;
            end;
          finally
            TableSolidLink.Free;
          end;
        finally
          TableSolidFacet.Free;
        end;
      finally
        TableMesh3DNode.Free;
      end;
    finally
      TableMesh3DLink.Free;
    end;
  finally
    TableMesh3DElement.Free;
  end;
end;
//---------------------------------------------------------\\
// Converts a dataset InModelName with source model type to
//  destination OutModelName model type }
//----------------------------------------------------------\\
procedure TfmMethodConversion.Conversion;
var
  OldCursor: TCursor;

begin
  OldCursor     := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  with dmBase do
    case InModelType of
      mtDholes:
        case OutModelType of
          mtPoints3D: CopyFiles(InModelName + TableExt,
              OutModelName + TableExt, OutModelType, False);
        end;
      mtPoints2D:
        case OutModelType of
          mtPoints3D:
          begin
          end;
          mtTins:
          begin
          end;
        end;
      mtPoints3D:
        ;
      mtPolygons:
        case OutModelType of
          mtPoints2D, mtPoints3D:
          begin
            InModelName := ChangeModelTable(DirPolygonPoly, DirPolygonVertex,
              InModelName);
            CopyFiles(InModelName + TableExt, OutModelName + TableExt,
              OutModelType, False);
          end;
        end;
      mtTins:
        case OutModelType of
          mtPoints2D: CopyFiles(InModelName + TableExt,
              OutModelName + TableExt, OutModelType, False);
          mtMeshes2D: ConvertTinToMesh2D(InModelName, OutModelName);
        end;
      mtGrids2D:
        ;
      mtGrids3D:
        case OutModelType of
          mtPoints3D: CopyFiles(InModelName + TableExt,
              OutModelName + TableExt, OutModelType, False);
          mtGrids2D: ConvertGrid3D_Grid2D(InModelName, OutModelName, ProgressBar);
          mtMeshes3D: ;
        end;
      mtSolids:
        ;
      mtMeshes2D:
        ;
      mtMeshes3D:
        case OutModelType of
          mtSolids: ConvertMesh3D_Solids(InModelName, OutModelName, ProgressBar);
        end;
    end;
  Screen.Cursor := OldCursor;
end;


procedure TfmMethodConversion.ButtonOKClick(Sender: TObject);
begin
  inherited;
  if ModalResult = mrOk then
  begin
    dmBase.TableInput.TableName  := InModelName;
    dmBase.TableOutput.TableName := OutModelName;
    Conversion;
  end;
end;

end.
