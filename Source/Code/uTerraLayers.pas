//-----------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//----------------------------------------------------------------------------
(*
  The GSContours to draw contours of values in GeoScene
*)

 // ------------------------------------------
 //  TERRASCENE LAYERS FILE (*.gsl) FORMAT
 // ------------------------------------------
 //  #TrianglesCount - number of triangles
 //  #Data - a number of triangles for material
 //  ----------------------------------------


unit uTerraLayers;

interface

uses
  System.SysUtils, 
  System.Classes, 
  Vcl.Dialogs,

  
  GLS.Mesh,
  GLS.Texture,
  GLS.Material,
  GLS.VectorLists,


  uTerraModel,
  uGlobals,
  uResStrings,
  uTerraLoader;

const
  FILE_EXT = '.gsl';

type

  TLayerProps = record
    No, MaterialNo, prevMaterialNo: integer;
  end;

  TGeoSceneLayer = class
  public
    TrianglesCount: integer;
    Enabled: boolean;
    Items:   array of TLayerProps;
    layerName: string;
    constructor Create; overload;
    constructor Create(Model: TGeoScenemodel; Name: string); overload;
    destructor Destroy; override;
    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);
    procedure ApplyLayerForMesh(var Mesh: TGLMesh;
      MaterialLibrary: TGLMaterialLibrary; Materials: TTriMaterials);
    procedure Update(Model: TGeoScenemodel);
    procedure ChangeName(NameToSet: string);

  end;

  TGeoSceneLayerList = class
  private
    LayersCount: integer;
    tempArray:   array of TGeoSceneLayer;
    function GetCount(): integer;
  public
    Layers: array of TGeoSceneLayer;
    CurrentLayer: integer;
    constructor Create();
    destructor Destroy();
    procedure Add(var Model: TGeoSceneModel; Name: string);
    procedure AddLayerFromFile(FileName: string);
    procedure Remove(LayerToRemove: string); overload;
    procedure Remove(LayerToRemove: integer); overload;
    function GetLayerNoByName(Name: string): integer;
    procedure ApplyLayer(var Mesh: TGLMesh; MaterialLibrary: TGlMaterialLibrary;
      Materials: TTriMaterials; No: integer);
    procedure ApplyAllLayers(Mesh: TGLMesh; MaterialLibrary: TGlMaterialLibrary;
      Materials: TTriMaterials);
    procedure ReplaceLayers(LayerToReplace1, LayerToReplace2: integer); overload;
    procedure ReplaceLayers(LayerToReplace1, LayerToReplace2: string); overload;
    procedure ChangeName(NameToChange, NameToSet: string); overload;
    procedure ChangeName(LayerNo: integer; NameToSet: string); overload;
    property Count: integer Read GetCount;
  end;

var
  LAYERS_PATH: string;

implementation

uses
  fTerraScene;

// TGeoSceneLayer

procedure TGeoSceneLayer.ChangeName(NameToSet: string);
begin
  layerName := NameToSet;
end;

constructor TGeoSceneLayer.Create(Model: TGeoSceneModel; Name: string);
var
  i, k: integer;
begin
  TrianglesCount := 0;
  for i := 0 to Model.TriCount - 1 do
  begin
    if (Model.TrianglesArray[i].Selected) then
    begin
      Inc(TrianglesCount);
    end;
  end;

  SetLength(Items, TrianglesCount);
  k := 0;
  for i := 0 to Model.TriCount - 1 do
  begin
    if (Model.TrianglesArray[i].Selected) then
    begin
      Items[k].No := Tri.TrianglesArray[i].No;
      Items[k].MaterialNo := Tri.TrianglesArray[i].MaterialNo;
      Inc(k);
    end;
  end;
  Enabled   := True;
  layerName := Name;
end;


constructor TGeoSceneLayer.Create;
begin
  inherited Create;
end;

destructor TGeoSceneLayer.Destroy;
begin
  SetLength(Items, 0);
end;

procedure TGeoSceneLayer.LoadFromFile(FileName: string);
var
  i:     integer;
  tempStr: string;
  strList: TStringList;
  aFile: TextFile;
begin
  layerName := ExtractFileName(FileName);

  try
    try
      assignFile(aFile, FileName);
      reset(aFile);
      readln(aFile, tempStr);
      //if 1-string = #TrianglesCount
      if (trim(tempStr) = '#TrianglesCount') then
      begin
        readln(aFile, tempStr);
      end
      else
      begin
        messageDlg(loadResString(@rsError), mtError, [mbOK], 0);
        exit;
      end;
      TrianglesCount := StrToInt(trim(tempStr));
      SetLength(Items, TrianglesCount);

      readln(aFile, tempStr);
      if (trim(tempStr) = '#Data') then
      begin
        strList := TStringList.Create;
        for i := 0 to self.TrianglesCount - 1 do
        begin
          readln(aFile, tempStr);
          strList.CommaText := Trim(Copy(tempStr, 1, MaxInt));
          if strList.Count = 2 then
          begin
            items[i].No := StrToInt(strList[0]);
            items[i].MaterialNo := StrToInt(strList[1]);
          end;
        end;
        strList.Free;
      end
      else
      begin
        MessageDlg(loadResString(@rsError), mtError, [mbOK], 0);
        Exit;
      end;
    finally
      closeFile(aFile);
    end;
  except
    messageDlg(loadResString(@rsError) + '3', mtError, [mbOK], 0);
    exit;
  end;
end;

procedure TGeoSceneLayer.SaveToFile(FileName: string);

var
  i, k:  integer;
  aFile: TextFile;
begin
  assignFile(aFile, FileName);
  rewrite(aFile);
  writeln(aFile, '#TrianglesCount');
  writeln(aFile, IntToStr(TrianglesCount));
  writeln(aFile, '#Data');
  k := 0;
  for i := 0 to TrianglesCount - 1 do
  begin
    writeln(aFile, Items[k].No, ' ', Items[k].MaterialNo);
    Inc(k);
  end;
  closeFile(aFile);
end;


procedure TGeoSceneLayer.Update(Model: TGeoScenemodel);
begin

end;

procedure TGeoSceneLayer.ApplyLayerForMesh(var Mesh: TGLMesh;
  MaterialLibrary: TGlMaterialLibrary; Materials: TTriMaterials);
var
  i: integer;
begin
  if Enabled then
  begin
    for i := 0 to TrianglesCount - 1 do
    begin
      if (Materials.Add(Items[i].MaterialNo)) then
      begin
        MaterialLibrary.AddTextureMaterial('m' + IntToStr(
          Materials.Materials[Materials.Count - 1].No),
          Materials.Materials[Materials.Count - 1].Image);
      end;
            { ox -
            Mesh.MeshObjects[0].FaceGroups[Items[i].No - 2].Prepare;
            Mesh.MeshObjects[0].FaceGroups[Items[i].No - 2].MaterialName :=
                'm' + IntToStr(Items[i].MaterialNo);
            }
    end;
    Mesh.StructureChanged;
  end;
end;

{ TGeoSceneLayerList }

procedure TGeoSceneLayerList.Add(var Model: TGeoSceneModel; Name: string);
var
  i, Count: integer;

begin
  for i := 0 to Model.TriCount - 1 do
  begin
    if (Model.TrianglesArray[i].Selected) then
    begin
      Inc(Count);
    end;
  end;
  if (Count = 0) then
  begin
    exit;
  end;

  setlength(tempArray, LayersCount);

  for i := 0 to LayersCount - 1 do
  begin
    tempArray[i] := Layers[i];
  end;

  setlength(Layers, 0);

  Inc(LayersCount);

  setlength(Layers, LayersCount);

  for i := 0 to LayersCount - 2 do
  begin
    Layers[i] := tempArray[i];
  end;

  setlength(tempArray, 0);
  Layers[LayersCount - 1] := TGeoSceneLayer.Create(Model, Name);
end;

procedure TGeoSceneLayerList.AddLayerFromFile(FileName: string);
var
  i: integer;
begin
  setlength(tempArray, LayersCount);

  for i := 0 to LayersCount - 1 do
  begin
    tempArray[i] := Layers[i];
  end;

  setlength(Layers, 0);

  Inc(LayersCount);

  setlength(Layers, LayersCount);

  for i := 0 to LayersCount - 2 do
  begin
    Layers[i] := tempArray[i];
  end;

  setlength(tempArray, 0);
  Layers[LayersCount - 1] := TGeoSceneLayer.Create;
  Layers[LayersCount - 1].LoadFromFile(FileName);
  Layers[LayersCount - 1].Enabled := True;
end;

procedure TGeoSceneLayerList.ApplyLayer(var Mesh: TGLMesh;
  MaterialLibrary: TGlMaterialLibrary; Materials: TTriMaterials; No: integer);
begin
  Layers[No].ApplyLayerForMesh(Mesh, MaterialLibrary, Materials);
end;

procedure TGeoSceneLayerList.ChangeName(NameToChange, NameToSet: string);
var
  LayerNo: integer;
begin
  LayerNo := GetLayerNoByName(NameToChange);
  ChangeName(LayerNo, NameToSet);
end;

procedure TGeoSceneLayerList.ChangeName(LayerNo: integer; NameToSet: string);
begin
  Layers[LayerNo].layerName := NameToSet;
end;

constructor TGeoSceneLayerList.Create;
begin
  LayersCount := 0;
  setlength(Layers, 0);
  setlength(tempArray, 0);
end;

destructor TGeoSceneLayerList.Destroy;
var
  i: integer;
begin
  for i := 0 to LayersCount - 1 do
  begin
    Layers[i].Free;
  end;
  setlength(Layers, 0);
  setlength(tempArray, 0);
end;


function TGeoSceneLayerList.GetCount: integer;
begin
  Result := LayersCount;
end;

function TGeoSceneLayerList.GetLayerNoByName(Name: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to LayersCount - 1 do
  begin
    if (Layers[i].layerName = Name) then
    begin
      Result := i;
    end;
  end;
end;

procedure TGeoSceneLayerList.Remove(LayerToRemove: string);
var
  i, k, LayerRemoveNo: integer;
  Removing: boolean;
begin
  Removing := False;
  for i := 0 to LayersCount - 1 do
  begin
    if Layers[i].layerName = LayerToRemove then
    begin
      Removing := True;
    end;
  end;

  if not Removing then
  begin
    exit;
  end;

  setLength(tempArray, LayersCount - 1);
  k := 0;
  for i := 0 to LayersCount - 1 do
  begin
    if (Layers[i].layerName <> LayerToRemove) then
    begin
      tempArray[k] := Layers[i];
      Inc(k);
    end
    else
    begin
      LayerRemoveNo := i;
    end;
  end;

  Layers[LayerRemoveNo].Free;

  setlength(Layers, 0);
  Dec(LayersCount);
  setlength(Layers, LayersCount);

  for i := 0 to LayersCount - 1 do
  begin
    Layers[i] := tempArray[i];
  end;

  setlength(tempArray, 0);

end;

procedure TGeoSceneLayerList.Remove(LayerToRemove: integer);
var
  i, k: integer;
begin
  setLength(tempArray, LayersCount - 1);
  k := 0;
  for i := 0 to LayersCount - 1 do
  begin
    if (i <> (LayerToRemove - 1)) then
    begin
      tempArray[k] := Layers[i];
      Inc(k);
    end;
  end;

  Layers[LayerToRemove - 1].Free;

  setlength(Layers, 0);
  Dec(LayersCount);
  setlength(Layers, LayersCount);

  for i := 0 to LayersCount - 1 do
  begin
    Layers[i] := tempArray[i];
  end;

  setlength(tempArray, 0);
end;

procedure TGeoSceneLayerList.ReplaceLayers(LayerToReplace1, LayerToReplace2: string);
var
  LayerSource, LayerDest: integer;
begin
  LayerSource := GetLayerNoByName(LayerToReplace1);
  LayerDest   := GetLayerNoByName(LayerToReplace2);
  ReplaceLayers(LayerSource, LayerDest);
end;

procedure TGeoSceneLayerList.ReplaceLayers(LayerToReplace1, LayerToReplace2: integer);
var
  tempPtr: TGeoSceneLayer;
begin
  tempPtr := Layers[LayerToReplace1];
  Layers[LayerToReplace1] := Layers[LayerToReplace2];
  Layers[LayerToReplace2] := tempPtr;
end;

procedure TGeoSceneLayerList.ApplyAllLayers(Mesh: TGLMesh;
  MaterialLibrary: TGlMaterialLibrary; Materials: TTriMaterials);
var
  i: integer;
begin
  for i := 0 to LayersCount - 1 do
  begin
    Layers[i].ApplyLayerForMesh(Mesh, MaterialLibrary, Materials);
  end;
end;

end.
