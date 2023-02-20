//------------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//------------------------------------------------------------------------------
{! The reserve calculation dialog with routines}

unit fAnalyseReserves;

interface

uses
  System.SysUtils,
  System.Classes,
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
  Vcl.Grids,
  Vcl.Menus,
  //DB
  Bde.DBTables,
  Data.DB,

  GLS.VectorTypes,
  GLS.VectorGeometry,

  fMethodDialog,
  cGlobals,
  GBEditValue;

type
  TfmAnalyseReserves = class(TfmMethodDialog)
    RadioGroupDensity: TRadioGroup;
    RadioGroupMoisture: TRadioGroup;
    DropdownMenuPoly: TPopupMenu;
    MenuItemVoronoiPolygons: TMenuItem;
    MenuItemParallelProfiles: TMenuItem;
    PopupMenu:      TPopupMenu;
    MenuItemTins:   TMenuItem;
    MenuItemSolids: TMenuItem;
    MenuItemGrids2D: TMenuItem;
    MenuItemGrids3D: TMenuItem;
    MenuItemMeshes2D: TMenuItem;
    MenuItemMeshes3D: TMenuItem;
    MenuItemPoints2D: TMenuItem;
    MenuItemPoints3D: TMenuItem;
    MenuItemHoles:  TMenuItem;
    MenuItemPolygons: TMenuItem;
    GBEditValueDensity: TGBEditValue;
    GBEditValueMoisture: TGBEditValue;
    procedure ListBoxInputNamesClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure MenuItemVoronoiPolygonsClick(Sender: TObject);
    procedure MenuItemParallelProfilesClick(Sender: TObject);
    procedure ToolButtonInputClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FContent, FDensity, FMoisture: double;
    Area, AreaBase, AreaSurf, Volume, Reserve, Tonnage, Variance: double;
    MinX, MaxX, MinY, MaxY, MinZ, MaxZ, Thickness, Plane: double;
    MinRockType: array[0..255] of integer;
    MaxRockType: array[0..255] of integer;

    procedure CreateReportTable;
    procedure Sections;
    procedure TrianglePrizms;
    procedure VoronoiPolygons;
    procedure Cuttings(fldOreType: string);
    procedure CuttingsWithOreSort(fldOreType, fldOreSort: string);
    procedure SobolevskyGrid;
    procedure RegularBlocks;

    function GetDensity: double;
    function GetMoisture: double;
  public
    property Density: double read GetDensity write FDensity;
    property Moisture: double read GetMoisture write FMoisture;
  end;

var
  fmAnalyseReserves: TfmAnalyseReserves;

//=========================================================================
implementation
//=========================================================================

uses
  dBase,
  dDialogs,
  GBGeometry,
  cProfuns,
  cDiscoCore,
  cDiscoMetric,
  cSorting,
  uFileCreator,
  cResStrings,
  uCommon,
  gnuGettext,
  fReserveCutOptions;

{$R *.DFM}

{ TfmAnalyseReserves }

procedure TfmAnalyseReserves.FormCreate(Sender: TObject);
begin
  inherited;
  ToolButtonHoles.Hint    := MenuItemHoles.Hint;
  ToolButtonPoints2D.Hint := MenuItemPoints2D.Hint;
  ToolButtonPoints3D.Hint := MenuItemPoints3D.Hint;
  ToolButtonPolygons.Hint := MenuItemPolygons.Hint;
  ToolButtonTins.Hint     := MenuItemTins.Hint;
  ToolButtonGrids2D.Hint  := MenuItemGrids2D.Hint;
  ToolButtonGrids3D.Hint  := MenuItemGrids3D.Hint;
  ToolButtonMeshes2D.Hint := MenuItemMeshes2D.Hint;
  ToolButtonMeshes3D.Hint := MenuItemMeshes3D.Hint;
end;

procedure TfmAnalyseReserves.ToolButtonInputClick(Sender: TObject);
begin
  inherited;
  PanelOutPath.Caption := ExpandPath(DirReport);
  ListBoxInputNamesClick(Self);
end;

procedure TfmAnalyseReserves.ListBoxInputNamesClick(Sender: TObject);
begin
  inherited;
  with dmBase do
  begin
    if ButtonOK.Enabled then
    begin
      TableInput.Open;
      if TableInput.FindField(fldDENSITY) = nil then
        RadioGroupDensity.ItemIndex := 0;

      if TableInput.FindField(fldMOISTURE) = nil then
        RadioGroupMoisture.ItemIndex := 0;

      TableInput.Close;
    end;
    TableOutput.TableName := PanelOutPath.Caption + EditOutName.Text;
  end;
  ListBoxRealAttribute.ItemIndex := -1;
end;

procedure TfmAnalyseReserves.MenuItemParallelProfilesClick(Sender: TObject);
begin
  ToolButtonPolygons.ImageIndex := MenuItemParallelProfiles.ImageIndex;
  ToolButtonPolygons.Hint := MenuItemParallelProfiles.Hint;
  DropdownMenuPoly.Tag    := 0;
  ToolButtonPolygons.Down := True;
  ToolButtonPolygons.Click;
end;

procedure TfmAnalyseReserves.MenuItemVoronoiPolygonsClick(Sender: TObject);
begin
  ToolButtonPolygons.ImageIndex := MenuItemVoronoiPolygons.ImageIndex;
  ToolButtonPolygons.Hint := MenuItemVoronoiPolygons.Hint;
  DropdownMenuPoly.Tag    := 1;
  ToolButtonPolygons.Down := True;
  ToolButtonPolygons.Click;
end;

procedure TfmAnalyseReserves.CreateReportTable;
begin
  with dmBase do
  begin
    with TableOutput do
    begin
      FieldDefs.Clear;
      FieldDefs.Add(UpperCase(LoadResString(@rsAttribute)), ftString, 20);
      FieldDefs.Add(UpperCase(LoadResString(@rsNumber)), ftInteger);
      FieldDefs.Add(UpperCase(LoadResString(@rsContent)), ftFloat);
      FieldDefs.Add(UpperCase(LoadResString(@rsMinimum)), ftFloat);
      FieldDefs.Add(UpperCase(LoadResString(@rsMaximum)), ftFloat);
      FieldDefs.Add(UpperCase(LoadResString(@rsVolume)), ftFloat);
      FieldDefs.Add(UpperCase(LoadResString(@rsDensity)), ftFloat);
      FieldDefs.Add(UpperCase(LoadResString(@rsMoisture)), ftFloat);
      FieldDefs.Add(UpperCase(LoadResString(@rsReserve)), ftFloat);
      FieldDefs.Add(UpperCase(LoadResString(@rsVariance)), ftFloat);
    end;
    TableOutput.CreateTable;
  end;
end;

function TfmAnalyseReserves.GetDensity: double;
begin
  Result := 3;
  with dmBase do
    try
      if RadioGroupDensity.ItemIndex = 1 then
        Result := TableInput.FieldByName(fldDENSITY).AsFloat
      else
        Abort;
    except
      try
        Result := StrToFloat(GBEditValueDensity.Text);
      except
      end;
    end;
end;

function TfmAnalyseReserves.GetMoisture: double;
begin
  Result := 0;
  with dmBase do
    try
      if RadioGroupMoisture.ItemIndex = 1 then
        Result := TableInput.FieldByName(fldMOISTURE).AsFloat
      else
        Abort;
    except
      try
        Result := StrToFloat(GBEditValueMoisture.Text);
      except
      end;
    end;
end;

//========================== METHODS ================================\\

//------------------------ Sections with Points 2D ------------------------\\

procedure TfmAnalyseReserves.Sections;

var
  InfluenceArea: double;
  strComponent: string;
  Number:  integer;
  Minimum: double;
  Maximum: double;

type
  TProfile = record
    Profile:      string;
    ProfileStart: TPoint2D;
    ProfileEnd:   TPoint2D;
    ProfileArea:  double;
    ProfileReserv: double;
    ProfileLen:   double;
  end;

var
  PrevProfile: TProfile;
  CurrProfile: TProfile;

  {sub}
  function CheckNecessaryFields: boolean;
  begin
    Result := True;
    with dmBase do
    begin
      if TableInput.FindField(fldPROFILE) = nil then
      begin
        ShowMessage(LoadResString(@rsNotFound) + ' ' + fldPROFILE);
        Result := False;
      end;

      if TableInput.FindField(fldTHICKNESS) = nil then
      begin
        ShowMessage(LoadResString(@rsNotFound) + ' ' + fldTHICKNESS);
        Result := False;
      end;
    end;
  end;

  {sub}
  function EOF: boolean;
  begin
    Result := dmBase.TableInput.EOF;
  end;

  {sub}
  function BOP: boolean;
  begin
    Result := CurrProfile.Profile <> dmBase.TableInput.FieldByName(
      fldPROFILE).AsString;
  end;

  {sub}
  function EOP: boolean;
  begin
    Result := EOF or (CurrProfile.Profile <>
      dmBase.TableInput.FieldByName(fldPROFILE).AsString);
  end;

  {sub}
  function GetProfileLength(Profile: TProfile): double;
  begin
    with Profile do
      Result := Norm([ProfileStart.X - ProfileEnd.X, ProfileStart.Y -
        ProfileEnd.Y]);
  end;

  {sub}
  function GetFieldNameComponent: string;
  var
    I: integer;
  begin
    Result := '';
    with dmBase do
      for I := 0 to TableInput.Fields.Count - 1 do
        if Pos('Q_', TableInput.Fields[I].FieldName) = 1 then
        begin
          Result := TableInput.Fields[I].FieldName;
          Exit;
        end;
    if dmBase.TableInput.FindField(fldCOMPONENT) <> nil then
      Result := fldCOMPONENT
    else
      Result := fldZ;
  end;

  {sub}
  procedure ReadProfile;
  var
    PrevX, PrevY, PrevThickness, PrevQ_Component, CurrQ_Component,
    PrevDensity, PrevMoisture: double;
    R:      TSortArray;
    RCount: integer;
    I:      integer;
  begin
    PrevProfile := CurrProfile;
    with dmBase do
      with CurrProfile do
      begin
        Profile := TableInput.FieldByName(fldPROFILE).AsString;
        ProfileStart.X := TableInput.FieldByName(fldX).AsFloat;
        ProfileStart.Y := TableInput.FieldByName(fldY).AsFloat;
        PrevX   := ProfileStart.X;
        PrevY   := ProfileStart.Y;
        ProfileArea := 0;

        RCount := 0;
        try
          while not EOP do
          begin
            Inc(RCount);
            if High(R) < RCount then
              SetLength(R, High(R) + 30);
            with R[RCount - 1] do
            begin
              No := TableInput.RecNo;
              at := Norm([PrevX - TableInput.FieldByName(fldX).AsFloat,
                PrevY - TableInput.FieldByName(fldY).AsFloat]);
            end;
            TableInput.Next;
          end;
          FindMinSort(R, RCount);
          TableInput.RecNo := R[RCount - 1].No;
          ProfileStart.X := TableInput.FieldByName(fldX).AsFloat;
          ProfileStart.Y := TableInput.FieldByName(fldY).AsFloat;
          PrevX := ProfileStart.X;
          PrevY := ProfileStart.Y;
          for I := 0 to RCount - 1 do
          begin
            TableInput.RecNo := R[I].No;
            R[I].at := Norm([PrevX - TableInput.FieldByName(fldX).AsFloat,
              PrevY - TableInput.FieldByName(fldY).AsFloat]);
          end;
          FindMinSort(R, RCount);
          TableInput.RecNo := R[RCount - 1].No;
          ProfileEnd.X     := TableInput.FieldByName(fldX).AsFloat;
          ProfileEnd.Y     := TableInput.FieldByName(fldY).AsFloat;
          ProfileArea      := 0;
          ProfileReserv    := 0;
          for I := 0 to RCount - 2 do
          begin
            TableInput.RecNo := R[I].No;
            PrevX := TableInput.FieldByName(fldX).AsFloat;
            PrevY := TableInput.FieldByName(fldY).AsFloat;
            PrevThickness := TableInput.FieldByName(fldTHICKNESS).AsFloat;
            if Abs(PrevThickness) < 0.000000001 then
              PrevQ_Component := 0
            else
              PrevQ_Component := TableInput.FieldByName(strComponent).AsFloat;
            PrevDensity := GetDensity;
            PrevMoisture := GetMoisture;
            if Minimum > PrevQ_Component then
              Minimum := PrevQ_Component;
            if Maximum < PrevQ_Component then
              Maximum := PrevQ_Component;
            FContent := FContent + PrevQ_Component;
            Density     := Density + PrevDensity;
            Moisture    := Moisture + PrevMoisture;
            TableInput.RecNo := R[I + 1].No;
            ProfileArea := ProfileArea + 0.5 *
              (PrevThickness + TableInput.FieldByName(fldTHICKNESS).AsFloat) *
              Norm([PrevX - TableInput.FieldByName(fldX).AsFloat,
              PrevY - TableInput.FieldByName(fldY).AsFloat]);
            if Abs(TableInput.FieldByName(fldTHICKNESS).AsFloat) <
              0.000000001 then
              CurrQ_Component := 0
            else
              CurrQ_Component := TableInput.FieldByName(strComponent).AsFloat;
            ProfileReserv := ProfileReserv + 0.5 *
              (PrevThickness * PrevQ_Component / 100 * PrevDensity +
              TableInput.FieldByName(fldTHICKNESS).AsFloat *
              CurrQ_Component / 100 * GetDensity) *
              Norm([PrevX - TableInput.FieldByName(fldX).AsFloat,
              PrevY - TableInput.FieldByName(fldY).AsFloat]);
          end;
          PrevThickness := TableInput.FieldByName(fldTHICKNESS).AsFloat;
          if Abs(PrevThickness) < 0.000000001 then
            PrevQ_Component := 0
          else
            PrevQ_Component := TableInput.FieldByName(strComponent).AsFloat;
          if Minimum > PrevQ_Component then
            Minimum := PrevQ_Component;
          if Maximum < PrevQ_Component then
            Maximum := PrevQ_Component;
          FContent := FContent + PrevQ_Component;
          Density    := Density + GetDensity;
          Moisture   := Moisture + GetMoisture;
          ProfileLen := Norm([ProfileStart.X - ProfileEnd.X,
            ProfileStart.Y - ProfileEnd.Y]);
          if Abs(ProfileLen) < 0.0001 then
            ProfileLen := 1;
          while not EOP do
            TableInput.Next;
        finally
          R := nil;
        end;
      end;
  end;

  {sub}
  function GetArea(A: array of TPoint2D): double;
  begin
    Result := 1 / 2 * Max(Abs((A[0].Y + A[1].Y) * (A[0].X - A[1].X) +
      (A[1].Y + A[2].Y) * (A[1].X - A[2].X) + (A[2].Y + A[3].Y) *
      (A[2].X - A[3].X) + (A[3].Y + A[0].Y) * (A[3].X - A[0].X)),
      Abs((A[0].Y + A[1].Y) * (A[0].X - A[1].X) + (A[1].Y + A[3].Y) *
      (A[1].X - A[3].X) + (A[3].Y + A[2].Y) * (A[3].X - A[2].X) +
      (A[2].Y + A[0].Y) * (A[2].X - A[0].X)));
  end;

  {\Subrutines}

begin //Main body of Sections
  with dmBase do
  begin
    TableInput.Close;
    if CheckNecessaryFields = False then
      Exit;
    TableInput.Open;
    CreateReportTable;

    ProgressBar.Min      := 0;
    ProgressBar.Max      := TableInput.RecordCount;
    ProgressBar.Position := 1;

    strComponent := GetFieldNameComponent;

    Number   := TableInput.RecordCount;
    FContent := 0;
    Minimum  := TableInput.FieldByName(strComponent).AsFloat;
    Maximum  := TableInput.FieldByName(strComponent).AsFloat;
    Volume   := 0;
    Density  := 0;
    Moisture := 0;
    Reserve  := 0;
    Variance := 0;

    ReadProfile;
    while not EOF do
    begin
      ProgressBar.Position := TableInput.RecNo;
      ReadProfile;
      InfluenceArea := 1 / 2 *
        GetArea([PrevProfile.ProfileStart, PrevProfile.ProfileEnd,
        CurrProfile.ProfileStart, CurrProfile.ProfileEnd]);
      Volume  := Volume + PrevProfile.ProfileArea / PrevProfile.ProfileLen *
        InfluenceArea + CurrProfile.ProfileArea / CurrProfile.ProfileLen * InfluenceArea;
      Reserve := Reserve + PrevProfile.ProfileReserv /
        PrevProfile.ProfileLen * InfluenceArea + CurrProfile.ProfileReserv /
        CurrProfile.ProfileLen * InfluenceArea;
    end;
    ProgressBar.Position := TableInput.RecNo;

    TableOutput.Open;
    TableOutput.Append;

    TableOutput.FieldByName(LoadResString(@rsATTRIBUTE)).AsString := strComponent;
    TableOutput.FieldByName(LoadResString(@rsNUMBER)).AsInteger   := Number;
    TableOutput.FieldByName(LoadResString(@rsCONTENT)).AsFloat    := FContent / Number;
    TableOutput.FieldByName(LoadResString(@rsMINIMUM)).AsFloat    := Minimum;
    TableOutput.FieldByName(LoadResString(@rsMAXIMUM)).AsFloat    := Maximum;
    TableOutput.FieldByName(LoadResString(@rsVOLUME)).AsFloat     := Volume;
    TableOutput.FieldByName(LoadResString(@rsDENSITY)).AsFloat    := Density / Number;
    TableOutput.FieldByName(LoadResString(@rsMOISTURE)).AsFloat   := Moisture / Number;
    TableOutput.FieldByName(LoadResString(@rsRESERVE)).AsFloat    := Reserve;
    TableOutput.FieldByName(LoadResString(@rsVARIANCE)).AsFloat   := Variance;

    TableOutput.Post;
    TableOutput.Close;
    TransposeNumericTable(TableOutput.TableName);
  end;
end; //Sections with Points2D

 //------------ Verticle sections with Polygons -------------------------\\
 {Volume routines}
type
  TVolumeFunction = function(S1, S2, H: double): double;

function VolumeOfWedge(S1, S2, H: double): double;
begin
  Result := (S1 + S2) / 2 * H;
end;

function VolumeOfPrism(S1, S2, H: double): double;
begin
  Result := (S1 + S2) / 2 * H;
end;

function VolumeOfPyramid(S1, S2, H: double): double;
begin
  Result := (S1 + S2) / 3 * H;
end;

function VolumeOfTruncatePyramid(S1, S2, H: double): double;
begin
  Result := (S1 + S2 + Sqrt(S1 * S2)) / 3 * H;
end;

{\Volume routines}

function GetAreaDif(S1, S2: double): double;
begin
  Result := Abs(S1 - S2) / Max(S1, S2) * 100;
end;

const
  {Volume Function = vf}
  vfWedge   = 1;
  vfPiramid = 2;
  vfPrism   = 3;
  vfTruncatePyramid = 4;

  ArrayOfVolumeFunctions: array[vfWedge..vfTruncatePyramid] of TVolumeFunction =
    (VolumeOfWedge, VolumeOfPyramid, VolumeOfPrism, VolumeOfTruncatePyramid);

procedure TfmAnalyseReserves.Cuttings(fldOreType: string);

const
  { TODO -oBuianov : Move the fields into the global repository!}
  fldPROFILE1 = 'PROFILE1';
  fldAREA1 = 'AREA1';
  fldPROFILE2 = 'PROFILE2';
  fldAREA2 = 'AREA2';
  fldN  = 'N';
  fldN2 = 'N2';

var
  I, J: integer;
  fldX: string[16];
  ComponentFields: TStrings;

  Query:   TQuery;
  Profile: string;
  //  ProfileNext: String;
  //  RecNoNextBlock: Integer;

  Distance: double;

  Length:  double;
  S1, S2:  double;
  AreaDif: double;

  {sub}
  procedure CheckFields;
  var
    S: string;
  begin
    with dmBase do
    begin
      S := '';
      S := S + TableInput.FieldByName(fldOREBODY).AsString;
      S := S + TableInput.FieldByName(fldOREBLOCK).AsString;
      S := S + TableInput.FieldByName(fldPROFILE).AsString;
      S := S + TableInput.FieldByName(fldORETYPE).AsString;
      S := S + 'Good'; //don't delete
      if S = '' then
        Exit; // never work //don't delete
    end;
  end; {CheckFields}

  { TODO -oBuianov : Remove! The same function is in the common unit}
  {sub}
  function IsValidAttribute(Field: TField): boolean;
  begin
    with Field do
      Result := (Field.DataType = ftFloat) and
        (CompareText(FieldName, fldID) <> 0) and
        (CompareText(FieldName, fldX) <> 0) and
        (CompareText(FieldName, fldY) <> 0) and
        (CompareText(FieldName, fldZ) <> 0) and
        (CompareText(FieldName, fldFROM) <> 0) and
        (CompareText(FieldName, fldTO) <> 0) and
        (CompareText(FieldName, fldLENGTH) <> 0) and
        (CompareText(FieldName, fldDENSITY) <> 0) and
        (CompareText(FieldName, fldMOISTURE) <> 0) and
        (CompareText(FieldName, fldDEPTH) <> 0) and
        (CompareText(FieldName, fldAREA) <> 0) and
        (CompareText(FieldName, fldAreaDif) <> 0);
  end; {IsValidAttribute}

  {sub}
  procedure CreateResultTable;
  var
    I: integer;
  begin
    with dmBase do
    begin
      with TableOutput do
      begin
        Close;
        FieldDefs.Clear;
        FieldDefs.Add(fldX, ftFloat);
        FieldDefs.Add(fldOREBODY, ftInteger);
        FieldDefs.Add(fldOREBLOCK, ftInteger);
        FieldDefs.Add(fldORETYPE, ftInteger);
        FieldDefs.Add(fldPROFILE, ftString, 20);
        if TableInput.FindField(fldN) <> nil then
          FieldDefs.Add(fldN, ftString, 16);
        FieldDefs.Add(fldAREA, ftFloat);
        FieldDefs.Add(fldPROFILE2, ftString, 20);
        if TableInput.FindField(fldN) <> nil then
          FieldDefs.Add(fldN2, ftString, 16);
        FieldDefs.Add(fldAREA2, ftFloat);
        FieldDefs.Add(fldLENGTH, ftFloat);

        FieldDefs.Add(fldAreaDif, ftFloat);
        FieldDefs.Add(fldFormula, ftInteger);
        FieldDefs.Add(fldVOLUME, ftFloat);
        FieldDefs.Add(fldDENSITY, ftFloat);
        FieldDefs.Add(fldRESERVE, ftFloat);

        for I := 0 to ComponentFields.Count - 1 do
        begin
          FieldDefs.Add(ComponentFields[I], ftFloat);
        end;
        for I := 0 to ComponentFields.Count - 1 do
        begin
          try
            FieldDefs.Add('Q_' + ComponentFields[I], ftFloat);
          except
          end;
        end;
      end;
      TableOutput.CreateTable;
    end;
  end; {CreateResultTable}

  {sub}
  procedure SavePolygon;
  var
    Length: double;
    I:      integer;
    S:      string;
  begin
    { TODO -oBuianov : Remove the direct field call! }
    with dmBase do
    begin
      S :=
        Format('(Profile=''%s'') and (OreType=%s) and (OreBody=%s) and (OreBlock=%s)',
        [TableInput.FieldByName(fldPROFILE).AsString,
        TableInput.FieldByName(fldORETYPE).AsString,
        TableInput.FieldByName(fldOREBODY).AsString,
        TableInput.FieldByName(fldOREBLOCK).AsString]);

      TableOutput.Filter := S;

      Area   := TableInput.FieldByName(fldAREA).AsFloat;
      Length := TableInput.FieldByName(fldLENGTH).AsFloat;

      if TableOutput.FindFirst then
        TableOutput.Edit
      else
        TableOutput.Append;

      with TableOutput.FieldByName(fldPROFILE) do
        AsString := TableInput.FieldByName(fldPROFILE).AsString;

      if TableInput.FindField(fldN) <> nil then
        with TableOutput.FieldByName(fldN) do
          AsString := TableInput.FieldByName(fldN).AsString;

      with TableOutput.FieldByName(fldX) do
        AsFloat := TableInput.FieldByName(fldX).AsFloat;
      with TableOutput.FieldByName(fldORETYPE) do
        AsFloat := TableInput.FieldByName(fldORETYPE).AsFloat;
      with TableOutput.FieldByName(fldOREBODY) do
        AsFloat := TableInput.FieldByName(fldOREBODY).AsFloat;
      with TableOutput.FieldByName(fldOREBLOCK) do
        AsFloat := TableInput.FieldByName(fldOREBLOCK).AsFloat;

      with TableOutput.FieldByName(fldPROFILE) do
        AsString := TableInput.FieldByName(fldPROFILE).AsString;

      with TableOutput.FieldByName(fldAREA) do
        AsFloat := AsFloat + Area;
      with TableOutput.FieldByName(fldLENGTH) do
        AsFloat := AsFloat + Length;

      for I := 0 to ComponentFields.Count - 1 do
      begin
        with TableOutput.FieldByName(ComponentFields[I]) do
          AsFloat := AsFloat + Length * TableInput.FieldByName(
            ComponentFields[I]).AsFloat;
      end;
      TableOutput.Post;
    end;
  end;

  {sub}
  function GetNextProfileDistance: double;
  var
    RecNo:   integer;
    Profile: string;
  begin
    with dmBase do
    begin
      RecNo := TableOutput.RecNo;
      try
        Result  := TableOutput.FieldByName(fldX).AsFloat;
        Profile := TableOutput.FieldByName(fldPROFILE).AsString;
        while (not TableOutput.EOF) and
          (TableOutput.FieldByName(fldPROFILE).AsString = Profile) do
          TableOutput.Next;
        if TableOutput.FieldByName(fldPROFILE).AsString = Profile then
          while (not TableOutput.Bof) and
            (TableOutput.FieldByName(fldPROFILE).AsString = Profile) do
            TableOutput.Prior;
        Result := Abs(Result - TableOutput.FieldByName(fldX).AsFloat);
      finally
        TableOutput.RecNo := RecNo;
      end;
    end;
  end;

  {sub}
  function GetPrevProfileDistance: double;
  var
    RecNo:   integer;
    Profile: string;
  begin
    with dmBase do
    begin
      RecNo := TableOutput.RecNo;
      try
        Result  := TableOutput.FieldByName(fldX).AsFloat;
        Profile := TableOutput.FieldByName(fldPROFILE).AsString;
        while (not TableOutput.Bof) and
          (TableOutput.FieldByName(fldPROFILE).AsString = Profile) do
          TableOutput.Prior;
        if TableOutput.FieldByName(fldPROFILE).AsString = Profile then
          while (not TableOutput.EOF) and
            (TableOutput.FieldByName(fldPROFILE).AsString = Profile) do
            TableOutput.Next;
        Result := Abs(Result - TableOutput.FieldByName(fldX).AsFloat);
      finally
        TableOutput.RecNo := RecNo;
      end;
    end;
  end;

  {sub}
  function FindNextOreBlock: integer;
  var
    N2: string;
    RecNo: integer;
    OreBlock: integer;
    OreType: integer;
    OreBody: integer;
    Profile: string;
    Profile2: string;
    Area2: double;
    //    Length2: Double;
    Values2: array of double;
    I: integer;
  begin
    with dmBase do
    begin
      Result   := -1;
      RecNo    := TableOutput.RecNo;
      OreType  := TableOutput.FieldByName(fldORETYPE).AsInteger;
      OreBlock := TableOutput.FieldByName(fldOREBLOCK).AsInteger;
      OreBody  := TableOutput.FieldByName(fldOREBODY).AsInteger;
      Profile  := TableOutput.FieldByName(fldPROFILE).AsString;
      try
        while (not TableOutput.EOF) and
          (Profile = TableOutput.FieldByName(fldPROFILE).AsString) do
          TableOutput.Next;
        if (Profile <> TableOutput.FieldByName(fldPROFILE).AsString) then
        begin
          Profile2 := TableOutput.FieldByName(fldPROFILE).AsString;
          while (not TableOutput.EOF) and
            (Profile2 = TableOutput.FieldByName(fldPROFILE).AsString) and
            ((OreType <> TableOutput.FieldByName(fldORETYPE).AsInteger) or
              (OreBlock <> TableOutput.FieldByName(fldOREBLOCK).AsInteger) or
              (OreBody <> TableOutput.FieldByName(fldOREBODY).AsInteger)) do
            TableOutput.Next;
          if (not TableOutput.EOF) and
            (Profile2 = TableOutput.FieldByName(fldPROFILE).AsString) then
          begin
            Result := TableOutput.RecNo;
            Area2  := TableOutput.FieldByName(fldAREA).AsFloat;
            try
              N2 := TableOutput.FieldByName(fldN).AsString;
            except
            end;
            //          Profile2:=Profile2;
            //          Length2:=TableOutput.FieldByName(fldLENGTH).AsFloat;
            SetLength(Values2, ComponentFields.Count);
            try
              for I := 0 to ComponentFields.Count - 1 do
                try
                  Values2[I] := TableOutput.FieldByName(ComponentFields[I]).AsFloat;
                except
                end;
              TableOutput.RecNo := RecNo;
              TableOutput.Edit;
              try
                TableOutput.FieldByName(fldN2).AsString := N2;
              except
              end;
              TableOutput.FieldByName(fldAREA2).AsFloat := Area2;
              TableOutput.FieldByName(fldPROFILE2).AsString := Profile2;
              for I := 0 to ComponentFields.Count - 1 do
                try
                  with TableOutput.FieldByName(ComponentFields[I]) do
                    AsFloat := AsFloat + Values2[I];
                except
                end;
              TableOutput.Post;
            finally
              Values2 := nil;
            end;
          end
          else
          begin
          end;
        end
        else
        begin
        end;
      finally
        TableOutput.RecNo := RecNo;
      end;
    end;
  end;

  {sub}
  function FindPrevOreBlock: integer;
  var
    RecNo: integer;
    N2: string;
    OreBlock: integer;
    OreType: integer;
    OreBody: integer;
    Profile, Profile2: string;
    Area2: double;
    Length, Length2: double;
    Values2: array of double;
    I: integer;
  begin
    with dmBase do
    begin
      Result := -1;
      RecNo  := TableOutput.RecNo;
      try
        OreType  := TableOutput.FieldByName(fldORETYPE).AsInteger;
        OreBlock := TableOutput.FieldByName(fldOREBLOCK).AsInteger;
        OreBody  := TableOutput.FieldByName(fldOREBODY).AsInteger;
        Profile2 := TableOutput.FieldByName(fldPROFILE).AsString;
        Area2    := TableOutput.FieldByName(fldAREA).AsFloat;
        try
          N2 := TableOutput.FieldByName(fldN).AsString;
        except
        end;
        Length2 := TableOutput.FieldByName(fldX).AsFloat;
        SetLength(Values2, ComponentFields.Count);
        try
          for I := 0 to ComponentFields.Count - 1 do
            try
              Values2[I] :=
                RoundTo(TableOutput.FieldByName(ComponentFields[I]).AsFloat,
                Precision);
            except
            end;
          while (not TableOutput.Bof) and (Profile2 =
              TableOutput.FieldByName(fldPROFILE).AsString) do
            TableOutput.Prior;
          if (Profile2 <> TableOutput.FieldByName(fldPROFILE).AsString) then
          begin
            Profile := TableOutput.FieldByName(fldPROFILE).AsString;
            if Profile <> '' then
              while (not TableOutput.Bof) and
                (Profile = TableOutput.FieldByName(fldPROFILE).AsString) and
                ((OreType <> TableOutput.FieldByName(fldORETYPE).AsInteger) or
                  (OreBlock <> TableOutput.FieldByName(fldOREBLOCK).AsInteger) or
                  (OreBody <> TableOutput.FieldByName(fldOREBODY).AsInteger)) do
                TableOutput.Prior;
            //          if Profile2<>TableOutput.FieldByName(fldPROFILE).AsString then TableOutput.Next;
            //          if (Profile='') then TableOutput.Next;
            if (Profile = '') or
              ((OreType <> TableOutput.FieldByName(fldORETYPE).AsInteger) or
              (OreBlock <> TableOutput.FieldByName(fldOREBLOCK).AsInteger) or
              (OreBody <> TableOutput.FieldByName(fldOREBODY).AsInteger)) then
            begin
              if (Profile <> '') then
                TableOutput.Next;
              Length := TableOutput.FieldByName(fldX).AsFloat;
              if (Profile = '') then
                TableOutput.Next;
              Length2 := abs(Length2 - Length) / 2;
              TableOutput.Insert;
              TableOutput.FieldByName(fldX).AsFloat := Length;
              TableOutput.FieldByName(fldPROFILE).AsString := Profile;
              TableOutput.FieldByName(fldPROFILE2).AsString := Profile2;
              try
                TableOutput.FieldByName(fldN2).AsString := N2;
              except
              end;
              TableOutput.FieldByName(fldAREA2).AsFloat  :=
                RoundTo(Area2, Precision);
              TableOutput.FieldByName(fldLENGTH).AsFloat := Length2;
              TableOutput.FieldByName(fldORETYPE).AsInteger := OreType;
              TableOutput.FieldByName(fldOREBODY).AsInteger := OreBody;
              TableOutput.FieldByName(fldOREBLOCK).AsInteger := OreBlock;
              for I := 0 to ComponentFields.Count - 1 do
                try
                  with TableOutput.FieldByName(ComponentFields[I]) do
                    AsFloat := RoundTo(Values2[I] / Area2, Precision);
                except
                end;
              TableOutput.FieldByName(fldFormula).AsInteger := vfWedge;
              TableOutput.FieldByName(fldVOLUME).AsFloat    :=
                RoundTo((Area2 / 2) * Length2, Precision);
              TableOutput.Post;
              Inc(RecNo);
            end
            else
            begin
              Result := TableOutput.RecNo;
            end;
          end
          else
          begin
            Length2 := GetNextProfileDistance / 2;
            Length  := TableOutput.FieldByName(fldX).AsFloat - Length2 * 2;
            TableOutput.Insert;
            TableOutput.FieldByName(fldX).AsFloat := Length;
            TableOutput.FieldByName(fldPROFILE).AsString := '';
            TableOutput.FieldByName(fldPROFILE2).AsString := Profile2;
            try
              TableOutput.FieldByName(fldN2).AsString := N2;
            except
            end;
            TableOutput.FieldByName(fldAREA2).AsFloat     :=
              RoundTo(Area2, Precision);
            TableOutput.FieldByName(fldLENGTH).AsFloat    := Length2;
            TableOutput.FieldByName(fldORETYPE).AsInteger := OreType;
            TableOutput.FieldByName(fldOREBODY).AsInteger := OreBody;
            TableOutput.FieldByName(fldOREBLOCK).AsInteger := OreBlock;
            for I := 0 to ComponentFields.Count - 1 do
              try
                with TableOutput.FieldByName(ComponentFields[I]) do
                  AsFloat := RoundTo(Values2[I] / Area2, Precision);
              except
              end;
            TableOutput.FieldByName(fldFormula).AsInteger := vfWedge;
            TableOutput.FieldByName(fldVOLUME).AsFloat    :=
              RoundTo((Area2 / 2) * Length2, Precision);
            TableOutput.Post;
            Inc(RecNo);
          end;
        finally
          Values2 := nil;
        end;
      finally
        TableOutput.RecNo := RecNo;
      end;
    end;
  end;

  {sub}
  function GetNextProfile: string;
  var
    RecNo: integer;
  begin
    with dmBase do
    begin
      RecNo := TableInput.RecNo;
      try
        Result := '';
        while (not TableInput.EOF) and
          (TableInput.FieldByName(fldPROFILE).AsString = Profile) and
          TableInput.FindNext do
        ;
        if TableInput.FieldByName(fldPROFILE).AsString <> Profile then
          Result := TableInput.FieldByName(fldPROFILE).AsString;
      finally
        TableInput.RecNo := RecNo;
      end;
    end;
  end;

  {sub}
  function GetTableOutputNextProfile: string;
  var
    RecNo:   integer;
    Profile: string;
  begin
    with dmBase do
    begin
      RecNo := TableOutput.RecNo;
      try
        Result  := '';
        Profile := TableOutput.FieldByName(fldPROFILE).AsString;
        while (not TableOutput.EOF) and
          (TableOutput.FieldByName(fldPROFILE).AsString = Profile) and
          TableOutput.FindNext do
        ;
        if TableOutput.FieldByName(fldPROFILE).AsString <> Profile then
          Result := TableOutput.FieldByName(fldPROFILE).AsString;
      finally
        TableOutput.RecNo := RecNo;
      end;
    end;
  end;

  {sub}
  function GetFldX: string;
  var
    TablePolyVert: TTable;
    MinX, MaxX, MinY, MaxY, MinZ, MaxZ, MinD: double;
  begin
    with dmBase do
    begin
      Result := cGlobals.fldX;
      TableInput.Open;
      TableInput.Filter := fldID_TYPE + '=3';
      if not TableInput.FindFirst then
        Exit;

      TablePolyVert := TTable.Create(Self);
      try
        TablePolyVert.TableName :=
          ChangeModelTable(DirPolygonPoly, DirPolygonVertex, TableInput.TableName);
        TablePolyVert.Open;
        TablePolyVert.Filter :=
          fldID_POLY + '=' + IntToStr(TableInput.FieldByName(fldID).AsInteger);
        while not TablePolyVert.FindFirst do
        begin
          if TableInput.FindNext then
            TablePolyVert.Filter :=
              fldID_POLY + '=' + IntToStr(TableInput.FieldByName(fldID).AsInteger)
          else
            Abort;
        end;
        MinX := TablePolyVert.FieldByName(Result).AsFloat;
        MaxX := MinX;
        MinY := TablePolyVert.FieldByName(fldY).AsFloat;
        MaxY := MinY;
        MinZ := TablePolyVert.FieldByName(fldZ).AsFloat;
        MaxZ := MinZ;
        while TablePolyVert.FindNext do
        begin
          if MinX > TablePolyVert.FieldByName(Result).AsFloat then
            MinX := TablePolyVert.FieldByName(Result).AsFloat;
          if MaxX < TablePolyVert.FieldByName(Result).AsFloat then
            MaxX := TablePolyVert.FieldByName(Result).AsFloat;
          if MinY > TablePolyVert.FieldByName(fldY).AsFloat then
            MinY := TablePolyVert.FieldByName(fldY).AsFloat;
          if MaxY < TablePolyVert.FieldByName(fldY).AsFloat then
            MaxY := TablePolyVert.FieldByName(fldY).AsFloat;
          if MinZ > TablePolyVert.FieldByName(fldZ).AsFloat then
            MinZ := TablePolyVert.FieldByName(fldZ).AsFloat;
          if MaxZ < TablePolyVert.FieldByName(fldZ).AsFloat then
            MaxZ := TablePolyVert.FieldByName(fldZ).AsFloat;
        end;
        MinX := Abs(MaxX - MinX);
        MinY := Abs(MaxY - MinY);
        MinZ := Abs(MaxZ - MinZ);
        MinD := Min(Min(MinX, MinY), MinZ);
        if MinD = MinX then
          Result := cGlobals.fldX
        else if MinD = MinY then
          Result := cGlobals.fldY
        else if MinD = MinZ then
          Result := cGlobals.fldZ
        else
          Result := cGlobals.fldX;
      except
      end;
      TablePolyVert.Free;
    end;
  end;

begin
  fldX := 'X';
  with dmBase do
  begin
    try
      TableInput.Open;
      CheckFields;
    except
      ModalResult := mrNone;
      TableInput.Close;
      raise;
    end;

    fldX := GetFldX;

    ProgressBar.Min      := 1;
    ProgressBar.Max      := TableInput.RecordCount;
    ProgressBar.Position := 1;

    ComponentFields := TStringList.Create;
    try
      for I := 0 to TableInput.Fields.Count - 1 do
        if IsValidAttribute(TableInput.Fields[I]) then
        begin
          //if Pos('_',UpperCase(TableInput.Fields[I].FieldName))>0 then Continue;
          ComponentFields.Add(TableInput.Fields[I].FieldName);
        end;

      if ComponentFields.Count = 0 then
      begin
        ShowMessage(_('Real fields not found!'));
        ModalResult := mrNone;
        TableInput.Close;
        Abort;
      end;

      Query := TQuery.Create(Self);
      try
        Query.SQL.Add(Format(
          'SELECT DISTINCT T."Profile", T."OreType", T."OreBody", T."OreBlock" ' +
          'FROM "%s" T WHERE T."%s"=%d', [TableInput.TableName, fldID_TYPE,
          integer(ptPolygon)]));
        Query.Open;

        if Query.RecordCount = 0 then
          Exit;

        TableInput.Filter := fldID_TYPE + '=' + IntToStr(integer(ptPolygon));
        TableInput.FindFirst;

        CreateResultTable;

        TableOutput.Open;
        for I := 1 to TableInput.RecordCount do
        begin
          ProgressBar.Position := TableInput.RecNo;
          SavePolygon;
          if not TableInput.FindNext then
            Break;
        end;
        ProgressBar.Position := ProgressBar.Max;

        TableOutput.Filter := '';

        ProgressBar.Position := 1;
        ProgressBar.Max      := 2 * TableOutput.RecordCount;
        for I := 1 to TableOutput.RecordCount do
        begin
          TableOutput.RecNo := I;
          ProgressBar.Position := I;
          Area   := TableOutput.FieldByName(fldAREA).AsFloat;
          Length := Max(TableOutput.FieldByName(fldLENGTH).AsFloat, 0.0001);
          TableOutput.Edit;
          // TableOutput.FieldByName(fldAREA).AsFloat:=SetPrecision(Area);
          for J := 0 to ComponentFields.Count - 1 do
          begin
            try
              with TableOutput.FieldByName(ComponentFields[J]) do
                AsFloat := ((AsFloat / Length) * Area);
            except
            end;
          end;
          TableOutput.Post;
        end;

        I := 1;

        ProgressBar.Min      := 1;
        ProgressBar.Position := I;
        ProgressBar.Max      := TableOutput.RecordCount;
        TableOutput.First;
        while not TableOutput.EOF do
        begin
          ProgressBar.Position := TableOutput.RecNo;
          ProgressBar.Position := TableOutput.RecordCount + I;
          if FindPrevOreBlock < 0 then
            ProgressBar.Max := TableOutput.RecordCount;
          FindNextOreBlock;
          S1   := TableOutput.FieldByName(fldAREA).AsFloat;
          S2   := TableOutput.FieldByName(fldAREA2).AsFloat;
          Area := S1 + S2;
          Distance := GetNextProfileDistance;

          if TableOutput.FieldByName(fldAREA2).IsNull then
          begin //выклинивание
            Distance := Distance / 2;

            Profile := GetTableOutputNextProfile;
            TableOutput.Edit;
            TableOutput.FieldByName(fldPROFILE2).AsString := Profile;
            TableOutput.FieldByName(fldFormula).AsInteger := vfWedge;
            TableOutput.FieldByName(fldLENGTH).AsFloat    := Distance;
            TableOutput.FieldByName(fldVOLUME).AsFloat    :=
              RoundTo(TableOutput.FieldByName(fldAREA).AsFloat /
              2 * Distance, Precision);
          end
          else
          begin
            TableOutput.Edit;
            AreaDif := GetAreaDif(S1, S2);
            TableOutput.FieldByName(fldAreaDif).AsFloat :=
              RoundTo(AreaDif, Precision);
            if AreaDif > 40 {%} then
            begin
              TableOutput.FieldByName(fldFormula).AsInteger := vfTruncatePyramid;
              TableOutput.FieldByName(fldVOLUME).AsFloat    :=
                RoundTo(ArrayOfVolumeFunctions[vfTruncatePyramid](S1, S2,
                Distance), Precision);
            end
            else
            begin
              TableOutput.FieldByName(fldFormula).AsInteger := vfPrism;
              TableOutput.FieldByName(fldVOLUME).AsFloat    :=
                RoundTo(ArrayOfVolumeFunctions[vfPrism](S1, S2, Distance),
                Precision);
            end;
          end;

          TableOutput.FieldByName(fldLENGTH).AsFloat := Distance;

          for J := 0 to ComponentFields.Count - 1 do
          begin
            try
              with TableOutput.FieldByName(ComponentFields[J]) do
                AsFloat := RoundTo(AsFloat / Max(1, Area), Precision);
            except
            end;
          end;

          with TableOutput.FieldByName(fldAREA) do
            if AsFloat <> 0 then
              AsFloat  := RoundTo(AsFloat, Precision)
            else
              AsString := '';
          with TableOutput.FieldByName(fldAREA2) do
            if AsFloat <> 0 then
              AsFloat  := RoundTo(AsFloat, Precision)
            else
              AsString := '';

          TableOutput.Post;
          TableOutput.Next;
          Inc(I);
        end;

        TableOutput.Close;
      finally
        Query.Free;
      end;
    finally
      ComponentFields.Free;
    end;
  end;
end;

procedure TfmAnalyseReserves.CuttingsWithOreSort(fldOreType, fldOreSort: string);

{ Again the same fields and subs as in previous procedure? }
const
  fldPROFILE1 = 'PROFILE1';
  fldAREA1 = 'AREA1';
  fldPROFILE2 = 'PROFILE2';
  fldAREA2 = 'AREA2';
  fldN  = 'N';
  fldN2 = 'N2';

var
  I, J:   integer;
  S:      string;
  S1, S2: double; //Area1, Area2

  ComponentFields: TStrings;
  Query:   TQuery;
  Profile: string;
  //  ProfileNext: String;

  Length:  double;
  AreaDif: double;

  //  RecNo: Integer;
  //  RecNoNextBlock: Integer;
  Distance: double;

  {sub}
  procedure CheckFields;
  begin
    with dmBase do
    begin
      S := '';
      S := S + TableInput.FieldByName(fldOREBODY).AsString;
      S := S + TableInput.FieldByName(fldOREBLOCK).AsString;
      S := S + TableInput.FieldByName(fldSUBBLOCK).AsString;
      S := S + TableInput.FieldByName(fldPROFILE).AsString;
      S := S + TableInput.FieldByName(fldORETYPE).AsString;
      S := S + 'Good'; //don't delete
      if S = '' then
        Exit; // never work but don't delete
    end;
  end; {CheckFields}

  function IsValidAttribute(Field: TField): boolean;
  begin
    with Field do
      Result := (Field.DataType = ftFloat) and
        (CompareText(FieldName, fldID) <> 0) and
        (CompareText(FieldName, fldX) <> 0) and
        (CompareText(FieldName, fldY) <> 0) and
        (CompareText(FieldName, fldZ) <> 0) and
        (CompareText(FieldName, fldFROM) <> 0) and
        (CompareText(FieldName, fldTO) <> 0) and
        (CompareText(FieldName, fldLENGTH) <> 0) and
        (CompareText(FieldName, fldDENSITY) <> 0) and
        (CompareText(FieldName, fldMOISTURE) <> 0) and
        (CompareText(FieldName, fldDEPTH) <> 0) and
        (CompareText(FieldName, fldAREA) <> 0) and
        (CompareText(FieldName, fldAreaDif) <> 0);
  end; {IsValidAttribute}

  {sub}
  procedure CreateResultTable;
  var
    I: integer;
  begin
    with dmBase do
    begin
      with TableOutput do
      begin
        Close;
        FieldDefs.Clear;
        FieldDefs.Add(fldX, ftFloat);
        FieldDefs.Add(fldOREBODY, ftInteger);
        FieldDefs.Add(fldOREBLOCK, ftInteger);
        FieldDefs.Add(fldSUBBLOCK, ftInteger);
        FieldDefs.Add(fldORETYPE, ftInteger);
        FieldDefs.Add(FldORESORT, ftInteger);
        FieldDefs.Add(fldPROFILE, ftString, 20);
        if TableInput.FindField(fldN) <> nil then
          FieldDefs.Add(fldN, ftString, 16);
        FieldDefs.Add(fldAREA, ftFloat);
        FieldDefs.Add(fldPROFILE2, ftString, 20);
        if TableInput.FindField(fldN) <> nil then
          FieldDefs.Add(fldN2, ftString, 16);
        FieldDefs.Add(fldAREA2, ftFloat);
        FieldDefs.Add(fldLENGTH, ftFloat);

        FieldDefs.Add(fldAreaDif, ftFloat);
        FieldDefs.Add(fldFormula, ftInteger);

        FieldDefs.Add(fldVOLUME, ftFloat);
        FieldDefs.Add(fldDENSITY, ftFloat);
        FieldDefs.Add(fldRESERVE, ftFloat);

        for I := 0 to ComponentFields.Count - 1 do
        begin
          FieldDefs.Add(ComponentFields[I], ftFloat);
        end;

        for I := 0 to ComponentFields.Count - 1 do
        begin
          try
            FieldDefs.Add('Q_' + ComponentFields[I], ftFloat);
          except
          end;
        end;

        {     FieldDefs.Add(SMINIMUM,ftFloat);
              FieldDefs.Add(SMAXIMUM,ftFloat);
              FieldDefs.Add(SDENSITY,ftFloat);
              FieldDefs.Add(SMOISTURE,ftFloat);
              FieldDefs.Add(SVARIANCE,ftFloat);{}
      end;
      TableOutput.CreateTable;
    end;
  end; {CreateResultTable}

  {sub}
  procedure SavePolygon;
  var
    Length: double;
    I:      integer;
    S:      string;
  begin
    with dmBase do
    begin
      S := Format('(Profile=''%s'') and (OreType=%s) and ' +
        '(OreBody=%s) and (OreBlock=%s) and (SubBlock=%s) and (OreSort=%s)',
        [TableInput.FieldByName(fldPROFILE).AsString,
        TableInput.FieldByName(fldORETYPE).AsString,
        TableInput.FieldByName(fldOREBODY).AsString,
        TableInput.FieldByName(fldOREBLOCK).AsString,
        TableInput.FieldByName(fldSUBBLOCK).AsString,
        TableInput.FieldByName(fldORESORT).AsString]);

      TableOutput.Filter := S;

      Area   := TableInput.FieldByName(fldAREA).AsFloat;
      Length := TableInput.FieldByName(fldLENGTH).AsFloat;

      if TableOutput.FindFirst then
        TableOutput.Edit
      else
        TableOutput.Append;

      with TableOutput.FieldByName(fldPROFILE) do
        AsString := TableInput.FieldByName(fldPROFILE).AsString;

      try
        with TableOutput.FieldByName(fldN) do
          AsString := TableInput.FieldByName(fldN).AsString;
      except
      end;

      with TableOutput.FieldByName(fldX) do
        AsFloat := TableInput.FieldByName(fldX).AsFloat;
      with TableOutput.FieldByName(fldORETYPE) do
        AsFloat := TableInput.FieldByName(fldORETYPE).AsFloat;
      with TableOutput.FieldByName(fldOREBODY) do
        AsFloat := TableInput.FieldByName(fldOREBODY).AsFloat;
      with TableOutput.FieldByName(fldOREBLOCK) do
        AsFloat := TableInput.FieldByName(fldOREBLOCK).AsFloat;
      with TableOutput.FieldByName(fldSUBBLOCK) do
        AsFloat := TableInput.FieldByName(fldSUBBLOCK).AsFloat;
      with TableOutput.FieldByName(fldORESORT) do
        AsFloat := TableInput.FieldByName(fldORESORT).AsFloat;

      with TableOutput.FieldByName(fldPROFILE) do
        AsString := TableInput.FieldByName(fldPROFILE).AsString;

      with TableOutput.FieldByName(fldAREA) do
        AsFloat := AsFloat + Area;
      with TableOutput.FieldByName(fldLENGTH) do
        AsFloat := AsFloat + Length;

      for I := 0 to ComponentFields.Count - 1 do
      begin
        with TableOutput.FieldByName(ComponentFields[I]) do
          AsFloat := AsFloat + Length * TableInput.FieldByName(
            ComponentFields[I]).AsFloat;
      end;
      TableOutput.Post;
    end;
  end;

  {sub}
  function GetNextProfileDistance: double;
  var
    RecNo:   integer;
    Profile: string;
  begin
    with dmBase do
    begin
      RecNo := TableOutput.RecNo;
      try
        Result  := TableOutput.FieldByName(fldX).AsFloat;
        Profile := TableOutput.FieldByName(fldPROFILE).AsString;
        while (not TableOutput.EOF) and
          (TableOutput.FieldByName(fldPROFILE).AsString = Profile) do
          TableOutput.Next;
        if TableOutput.FieldByName(fldPROFILE).AsString = Profile then
          while (not TableOutput.Bof) and
            (TableOutput.FieldByName(fldPROFILE).AsString = Profile) do
            TableOutput.Prior;
        Result := Abs(Result - TableOutput.FieldByName(fldX).AsFloat);
      finally
        TableOutput.RecNo := RecNo;
      end;
    end;
  end;

  {sub}
  function GetPrevProfileDistance: double;
  var
    RecNo:   integer;
    Profile: string;
  begin
    with dmBase do
    begin
      RecNo := TableOutput.RecNo;
      try
        Result  := TableOutput.FieldByName(fldX).AsFloat;
        Profile := TableOutput.FieldByName(fldPROFILE).AsString;
        while (not TableOutput.Bof) and
          (TableOutput.FieldByName(fldPROFILE).AsString = Profile) do
          TableOutput.Prior;
        if TableOutput.FieldByName(fldPROFILE).AsString = Profile then
          while (not TableOutput.EOF) and
            (TableOutput.FieldByName(fldPROFILE).AsString = Profile) do
            TableOutput.Next;
        Result := Abs(Result - TableOutput.FieldByName(fldX).AsFloat);
      finally
        TableOutput.RecNo := RecNo;
      end;
    end;
  end;

  {sub}
  function FindNextOreBlock: integer;
  var
    RecNo: integer;
    OreBlock: integer;
    SubBlock: integer;
    OreType: integer;
    OreBody: integer;
    OreSort: integer;
    Profile: string;
    Profile2: string;
    N2:  string;
    Area2: double;
    //    Length2: Double;
    Values2: array of double;
    I:   integer;
    Res: boolean;
  begin
    with dmBase do
    begin
      Result   := -1;
      RecNo    := TableOutput.RecNo;
      OreType  := TableOutput.FieldByName(fldORETYPE).AsInteger;
      OreBlock := TableOutput.FieldByName(fldOREBLOCK).AsInteger;
      SubBlock := TableOutput.FieldByName(fldSUBBLOCK).AsInteger;
      OreBody  := TableOutput.FieldByName(fldOREBODY).AsInteger;
      OreSort  := TableOutput.FieldByName(fldORESORT).AsInteger;
      Profile  := TableOutput.FieldByName(fldPROFILE).AsString;
      try
        while (not TableOutput.EOF) and
          (Profile = TableOutput.FieldByName(fldPROFILE).AsString) do
          TableOutput.Next;
        if (Profile <> TableOutput.FieldByName(fldPROFILE).AsString) then
        begin
          Profile2 := TableOutput.FieldByName(fldPROFILE).AsString;
          Res      := False;
          while (not TableOutput.EOF) and
            (Profile2 = TableOutput.FieldByName(fldPROFILE).AsString) and
            ((OreType <> TableOutput.FieldByName(fldORETYPE).AsInteger) or
              (OreBlock <> TableOutput.FieldByName(fldOREBLOCK).AsInteger) or
              (SubBlock <> TableOutput.FieldByName(fldSUBBLOCK).AsInteger) or
              (OreSort <> TableOutput.FieldByName(fldORESORT).AsInteger) or
              (OreBody <> TableOutput.FieldByName(fldOREBODY).AsInteger)) do
          begin
            Res := Res or ((OreType = TableOutput.FieldByName(
              fldORETYPE).AsInteger) and (OreBlock =
              TableOutput.FieldByName(fldOREBLOCK).AsInteger) and
              (SubBlock = TableOutput.FieldByName(fldSUBBLOCK).AsInteger) and
              (OreBody = TableOutput.FieldByName(fldOREBODY).AsInteger));
            TableOutput.Next;
          end;
          if (not TableOutput.EOF) and
            (Profile2 = TableOutput.FieldByName(fldPROFILE).AsString) then
            // find OreBlock
          begin
            Result := TableOutput.RecNo;
            try
              N2 := TableOutput.FieldByName(fldN).AsString;
            except
            end;
            Area2 := TableOutput.FieldByName(fldAREA).AsFloat;
            //          Profile2:=Profile2;
            //          Length2:=TableOutput.FieldByName(fldLENGTH).AsFloat;
            SetLength(Values2, ComponentFields.Count);
            try
              for I := 0 to ComponentFields.Count - 1 do
                try
                  Values2[I] := TableOutput.FieldByName(ComponentFields[I]).AsFloat;
                except
                end;
              TableOutput.RecNo := RecNo;
              TableOutput.Edit;
              try
                TableOutput.FieldByName(fldN2).AsString := N2;
              except
              end;
              TableOutput.FieldByName(fldAREA2).AsFloat := Area2;
              TableOutput.FieldByName(fldPROFILE2).AsString := Profile2;
              for I := 0 to ComponentFields.Count - 1 do
                try
                  with TableOutput.FieldByName(ComponentFields[I]) do
                    AsFloat := AsFloat + Values2[I];
                except
                end;
              TableOutput.Post;
            finally
              Values2 := nil;
            end;
          end
          else
          begin
            if Res then //OreSort no present
            begin
              TableOutput.Edit;
              TableOutput.FieldByName(fldAREA2).AsFloat := 0;
              TableOutput.FieldByName(fldPROFILE2).AsString := Profile2;
              TableOutput.Post;
            end;
          end;
        end
        else //block not present
        begin
        end;
      finally
        TableOutput.RecNo := RecNo;
      end;
    end;
  end;

  {sub}
  function FindPrevOreBlock: integer;
  var
    RecNo: integer;
    OreBlock: integer;
    SubBlock: integer;
    OreType: integer;
    OreBody: integer;
    OreSort: integer;
    Profile, Profile2: string;
    N2: string;
    Area2: double;
    Length, Length2: double;
    Values2: array of double;
    I: integer;
  begin
    with dmBase do
    begin
      Result := -1;
      RecNo  := TableOutput.RecNo;
      try
        OreType  := TableOutput.FieldByName(fldORETYPE).AsInteger;
        OreBlock := TableOutput.FieldByName(fldOREBLOCK).AsInteger;
        SubBlock := TableOutput.FieldByName(fldSUBBLOCK).AsInteger;
        OreBody  := TableOutput.FieldByName(fldOREBODY).AsInteger;
        OreSort  := TableOutput.FieldByName(fldORESORT).AsInteger;
        Profile2 := TableOutput.FieldByName(fldPROFILE).AsString;
        try
          N2 := TableOutput.FieldByName(fldN).AsString;
        except
        end;
        Area2   := TableOutput.FieldByName(fldAREA).AsFloat;
        Length2 := TableOutput.FieldByName(fldX).AsFloat;
        SetLength(Values2, ComponentFields.Count);
        try
          for I := 0 to ComponentFields.Count - 1 do
            try
              Values2[I] :=
                RoundTo(TableOutput.FieldByName(ComponentFields[I]).AsFloat,
                Precision);
            except
            end;
          while (not TableOutput.Bof) and (Profile2 =
              TableOutput.FieldByName(fldPROFILE).AsString) do
            TableOutput.Prior;
          if (Profile2 <> TableOutput.FieldByName(fldPROFILE).AsString) then
          begin
            Profile := TableOutput.FieldByName(fldPROFILE).AsString;
            if Profile <> '' then
              while (not TableOutput.Bof) and
                (Profile = TableOutput.FieldByName(fldPROFILE).AsString) and
                ((OreType <> TableOutput.FieldByName(fldORETYPE).AsInteger) or
                  (OreBlock <> TableOutput.FieldByName(fldOREBLOCK).AsInteger) or
                  (SubBlock <> TableOutput.FieldByName(fldSUBBLOCK).AsInteger) or
                  (OreBody <> TableOutput.FieldByName(fldOREBODY).AsInteger)) do
                //oresort?
                TableOutput.Prior;
            //          if Profile2<>TableOutput.FieldByName(fldPROFILE).AsString then TableOutput.Next;
            //          if (Profile='') then TableOutput.Next;
            if (Profile = '') or
              ((OreType <> TableOutput.FieldByName(fldORETYPE).AsInteger) or
              (OreBlock <> TableOutput.FieldByName(fldOREBLOCK).AsInteger) or
              (SubBlock <> TableOutput.FieldByName(fldSUBBLOCK).AsInteger) or
              (OreBody <> TableOutput.FieldByName(fldOREBODY).AsInteger)) then
              //oresort?
            begin
              if (Profile <> '') then
                TableOutput.Next;
              Length := TableOutput.FieldByName(fldX).AsFloat;
              if (Profile = '') then
                TableOutput.Next;
              Length2 := abs(Length2 - Length) / 2;
              TableOutput.Insert;
              TableOutput.FieldByName(fldX).AsFloat := Length;
              TableOutput.FieldByName(fldPROFILE).AsString := Profile;
              TableOutput.FieldByName(fldPROFILE2).AsString := Profile2;
              try
                TableOutput.FieldByName(fldN2).AsString := N2;
              except
              end;
              TableOutput.FieldByName(fldAREA2).AsFloat  := RoundTo(Area2, Precision);
              TableOutput.FieldByName(fldLENGTH).AsFloat := Length2;
              TableOutput.FieldByName(fldORETYPE).AsInteger := OreType;
              TableOutput.FieldByName(fldOREBODY).AsInteger := OreBody;
              TableOutput.FieldByName(fldOREBLOCK).AsInteger := OreBlock;
              TableOutput.FieldByName(fldSUBBLOCK).AsInteger := SubBlock;
              TableOutput.FieldByName(fldORESORT).AsInteger := OreSort;
              for I := 0 to ComponentFields.Count - 1 do
                try
                  with TableOutput.FieldByName(ComponentFields[I]) do
                    AsFloat := RoundTo(Values2[I] / Area2, Precision);
                except
                end;
              TableOutput.FieldByName(fldFORMULA).AsInteger := vfWedge;
              TableOutput.FieldByName(fldVOLUME).AsFloat    :=
                RoundTo((Area2 / 2) * Length2, Precision);
              TableOutput.Post;
              Inc(RecNo);
            end
            else
            begin
              while (not TableOutput.Bof) and
                (Profile = TableOutput.FieldByName(fldPROFILE).AsString) and
                (OreType = TableOutput.FieldByName(fldORETYPE).AsInteger) and
                (OreBlock = TableOutput.FieldByName(fldOREBLOCK).AsInteger) and
                (SubBlock = TableOutput.FieldByName(fldSUBBLOCK).AsInteger) and
                (OreBody = TableOutput.FieldByName(fldOREBODY).AsInteger) and
                (OreSort <> TableOutput.FieldByName(fldORESORT).AsInteger) do
                TableOutput.Prior;
              if (Profile = TableOutput.FieldByName(fldPROFILE).AsString) and
                (OreType = TableOutput.FieldByName(fldORETYPE).AsInteger) and
                (OreBlock = TableOutput.FieldByName(fldOREBLOCK).AsInteger) and
                (OreSort = TableOutput.FieldByName(fldORESORT).AsInteger) and
                (OreBody = TableOutput.FieldByName(fldOREBODY).AsInteger) and
                (OreSort = TableOutput.FieldByName(fldORESORT).AsInteger) then
                Result :=
                  TableOutput.RecNo //Subtype exist
              else
              begin
                if (Profile <> '') then
                  TableOutput.Next;
                Length := TableOutput.FieldByName(fldX).AsFloat;
                if (Profile = '') then
                  TableOutput.Next;
                Length2 := abs(Length2 - Length) / 2;
                TableOutput.Insert;
                TableOutput.FieldByName(fldX).AsFloat := Length;
                TableOutput.FieldByName(fldPROFILE).AsString := Profile;
                TableOutput.FieldByName(fldPROFILE2).AsString := Profile2;
                try
                  TableOutput.FieldByName(fldN2).AsString := N2;
                except
                end;
                TableOutput.FieldByName(fldAREA2).AsFloat :=
                  RoundTo(Area2, Precision);
                TableOutput.FieldByName(fldLENGTH).AsFloat := Length2;
                TableOutput.FieldByName(fldORETYPE).AsInteger := OreType;
                TableOutput.FieldByName(fldOREBODY).AsInteger := OreBody;
                TableOutput.FieldByName(fldOREBLOCK).AsInteger := OreBlock;
                TableOutput.FieldByName(fldSUBBLOCK).AsInteger := SubBlock;
                TableOutput.FieldByName(fldORESORT).AsInteger := OreSort;
                for I := 0 to ComponentFields.Count - 1 do
                  try
                    with TableOutput.FieldByName(ComponentFields[I]) do
                      AsFloat := RoundTo(Values2[I] / Area2, Precision);
                  except
                  end;
                TableOutput.FieldByName(fldFORMULA).AsInteger := vfWedge;
                TableOutput.FieldByName(fldVOLUME).AsFloat    :=
                  RoundTo((Area2 / 2) * Length2, Precision);
                TableOutput.Post;
                Inc(RecNo);
              end;
            end;
          end
          else
          begin
            Length2 := GetNextProfileDistance / 2;
            Length  := TableOutput.FieldByName(fldX).AsFloat - Length2 * 2;
            TableOutput.Insert;
            TableOutput.FieldByName(fldX).AsFloat := Length;
            TableOutput.FieldByName(fldPROFILE).AsString := '';
            TableOutput.FieldByName(fldPROFILE2).AsString := Profile2;
            try
              TableOutput.FieldByName(fldN2).AsString := N2;
            except
            end;
            TableOutput.FieldByName(fldAREA2).AsFloat     :=
              RoundTo(Area2, Precision);
            TableOutput.FieldByName(fldLENGTH).AsFloat    := Length2;
            TableOutput.FieldByName(fldORETYPE).AsInteger := OreType;
            TableOutput.FieldByName(fldOREBODY).AsInteger := OreBody;
            TableOutput.FieldByName(fldOREBLOCK).AsInteger := OreBlock;
            TableOutput.FieldByName(fldSUBBLOCK).AsInteger := SubBlock;
            TableOutput.FieldByName(fldORESORT).AsInteger := OreSort;
            for I := 0 to ComponentFields.Count - 1 do
              try
                with TableOutput.FieldByName(ComponentFields[I]) do
                  AsFloat := RoundTo(Values2[I] / Area2, Precision);
              except
              end;
            TableOutput.FieldByName(fldFORMULA).AsInteger := vfWedge;
            TableOutput.FieldByName(fldVOLUME).AsFloat    :=
              RoundTo((Area2 / 2) * Length2, Precision);
            TableOutput.Post;
            Inc(RecNo);
          end;
        finally
          Values2 := nil;
        end;
      finally
        TableOutput.RecNo := RecNo;
      end;
    end;
  end;

  {sub}
  function GetNextProfile: string;
  var
    RecNo: integer;
  begin
    with dmBase do
    begin
      RecNo := TableInput.RecNo;
      try
        Result := '';
        while (not TableInput.EOF) and
          (TableInput.FieldByName(fldPROFILE).AsString = Profile) and
          TableInput.FindNext do
        ;
        if TableInput.FieldByName(fldPROFILE).AsString <> Profile then
          Result := TableInput.FieldByName(fldPROFILE).AsString;
      finally
        TableInput.RecNo := RecNo;
      end;
    end;
  end;

  {sub}
  function GetTableOutputNextProfile: string;
  var
    RecNo:   integer;
    Profile: string;
  begin
    with dmBase do
    begin
      RecNo := TableOutput.RecNo;
      try
        Result  := '';
        Profile := TableOutput.FieldByName(fldPROFILE).AsString;
        while (not TableOutput.EOF) and
          (TableOutput.FieldByName(fldPROFILE).AsString = Profile) and
          TableOutput.FindNext do
        ;
        if TableOutput.FieldByName(fldPROFILE).AsString <> Profile then
          Result := TableOutput.FieldByName(fldPROFILE).AsString;
      finally
        TableOutput.RecNo := RecNo;
      end;
    end;
  end;

  {sub}
  function GetFldX: string;
  var
    TablePolyVert: TTable;
    MinX, MaxX, MinY, MaxY, MinZ, MaxZ, MinD: double;

  begin
    with dmBase do
    begin
      Result := 'X';
      TableInput.Open;
      TableInput.Filter := fldID_TYPE + '=3';
      if not TableInput.FindFirst then
        Exit;

      TablePolyVert := TTable.Create(Self);
      try
        TablePolyVert.TableName :=
          ChangeModelTable(DirPolygonPoly, DirPolygonVertex,
          TableInput.TableName);
        TablePolyVert.Open;
        TablePolyVert.Filter :=
          fldID_POLY + '=' + IntToStr(TableInput.FieldByName(fldID).AsInteger);
        while not TablePolyVert.FindFirst do
        begin
          if TableInput.FindNext then
            TablePolyVert.Filter :=
              fldID_POLY + '=' + IntToStr(TableInput.FieldByName(fldID).AsInteger)
          else
            Abort;
        end;
        MinX := TablePolyVert.FieldByName(Result).AsFloat;
        MaxX := MinX;
        MinY := TablePolyVert.FieldByName(fldY).AsFloat;
        MaxY := MinY;
        MinZ := TablePolyVert.FieldByName(fldZ).AsFloat;
        MaxZ := MinZ;
        while TablePolyVert.FindNext do
        begin
          if MinX > TablePolyVert.FieldByName(Result).AsFloat then
            MinX := TablePolyVert.FieldByName(Result).AsFloat;
          if MaxX < TablePolyVert.FieldByName(Result).AsFloat then
            MaxX := TablePolyVert.FieldByName(Result).AsFloat;
          if MinY > TablePolyVert.FieldByName(fldY).AsFloat then
            MinY := TablePolyVert.FieldByName(fldY).AsFloat;
          if MaxY < TablePolyVert.FieldByName(fldY).AsFloat then
            MaxY := TablePolyVert.FieldByName(fldY).AsFloat;
          if MinZ > TablePolyVert.FieldByName(fldZ).AsFloat then
            MinZ := TablePolyVert.FieldByName(fldZ).AsFloat;
          if MaxZ < TablePolyVert.FieldByName(fldZ).AsFloat then
            MaxZ := TablePolyVert.FieldByName(fldZ).AsFloat;
        end;
        MinX := Abs(MaxX - MinX);
        MinY := Abs(MaxY - MinY);
        MinZ := Abs(MaxZ - MinZ);
        MinD := Min(Min(MinX, MinY), MinZ);
        if MinD = MinX then
          Result := cGlobals.fldX
        else if MinD = MinY then
          Result := cGlobals.fldY
        else if MinD = MinZ then
          Result := cGlobals.fldZ
        else
          Result := cGlobals.fldX;
      except
        TablePolyVert.Free;
      end;
      TablePolyVert.Free;
    end;
  end;

begin
  with dmBase do
  begin
    try
      TableInput.Open;
      CheckFields;
    except
      ModalResult := mrNone;
      TableInput.Close;
      raise;
    end;

    fldX := GetFldX;

    ProgressBar.Min      := 1;
    ProgressBar.Max      := TableInput.RecordCount;
    ProgressBar.Position := 1;

    ComponentFields := TStringList.Create;
    try
      for I := 0 to TableInput.Fields.Count - 1 do
        if IsValidAttribute(TableInput.Fields[I]) then
        begin
          //if Pos('_',UpperCase(TableInput.Fields[I].FieldName))>0 then Continue;
          ComponentFields.Add(TableInput.Fields[I].FieldName);
        end;

      if ComponentFields.Count = 0 then
      begin
        ShowMessage('No real fields!');
        ModalResult := mrNone;
        TableInput.Close;
        Abort;
      end;

      Query := TQuery.Create(Self);
      try
        Query.SQL.Add(Format(
          'SELECT DISTINCT T."Profile", T."OreType", T."OreBody", T."OreBlock", T."SubBlock" '
          + 'FROM "%s" T WHERE T."ID_TYPE"=3 and T."OreBody"<>0 ',
          [TableInput.TableName]));
        Query.Open;

        if Query.RecordCount = 0 then
          Exit;

        TableInput.Filter := fldID_TYPE + '=3 AND ' + fldOreBody + '<>0';
        TableInput.FindFirst;

        CreateResultTable;

        TableOutput.Open;
        for I := 1 to TableInput.RecordCount do
        begin
          ProgressBar.Position := TableInput.RecNo;
          SavePolygon;
          if not TableInput.FindNext then
            Break;
        end;
        ProgressBar.Position := ProgressBar.Max;

        TableOutput.Filter := '';

        ProgressBar.Position := 1;
        ProgressBar.Max      := 2 * TableOutput.RecordCount;
        for I := 1 to TableOutput.RecordCount do
        begin
          TableOutput.RecNo := I;
          ProgressBar.Position := I;
          Area   := TableOutput.FieldByName(fldAREA).AsFloat;
          Length := Max(TableOutput.FieldByName(fldLENGTH).AsFloat, 1);
          TableOutput.Edit;
          //        TableOutput.FieldByName(fldAREA).AsFloat:=SetPrecision(Area);
          for J := 0 to ComponentFields.Count - 1 do
          begin
            try
              with TableOutput.FieldByName(ComponentFields[J]) do
                AsFloat := ((AsFloat / Length) * Area);
            except
            end;
          end;
          TableOutput.Post;
        end;

        I := 1;
        ProgressBar.Min := 1;
        ProgressBar.Position := I;
        ProgressBar.Max := TableOutput.RecordCount;
        TableOutput.First;
        while not TableOutput.EOF do
        begin
          ProgressBar.Position := TableOutput.RecNo;
          ProgressBar.Position := TableOutput.RecordCount + I;
          if FindPrevOreBlock < 0 then
            ProgressBar.Max := TableOutput.RecordCount;
          FindNextOreBlock;
          S1   := TableOutput.FieldByName(fldAREA).AsFloat;
          S2   := TableOutput.FieldByName(fldAREA2).AsFloat;
          Area := S1 + S2;
          Distance := GetNextProfileDistance;

          if TableOutput.FieldByName(fldAREA2).IsNull then
          begin
            Distance := Distance / 2;

            Profile := GetTableOutputNextProfile;
            TableOutput.Edit;
            TableOutput.FieldByName(fldPROFILE2).AsString := Profile;
            TableOutput.FieldByName(fldLENGTH).AsFloat    := Distance;
            TableOutput.FieldByName(fldFORMULA).AsInteger := vfWedge;
            TableOutput.FieldByName(fldVOLUME).AsFloat    :=
              RoundTo(ArrayOfVolumeFunctions[vfWedge](S1, S2, Distance),
              Precision);
          end
          else
          begin
            TableOutput.Edit;

            AreaDif := GetAreaDif(S1, S2);
            TableOutput.FieldByName(fldAreaDif).AsFloat := RoundTo(AreaDif, Precision);
            if AreaDif > 40 {%} then
            begin
              TableOutput.FieldByName(fldFORMULA).AsInteger := vfTruncatePyramid;
              TableOutput.FieldByName(fldVOLUME).AsFloat    :=
                RoundTo(ArrayOfVolumeFunctions[vfTruncatePyramid](S1, S2,
                Distance), Precision);
            end
            else
            begin
              TableOutput.FieldByName(fldFORMULA).AsInteger := vfPrism;
              TableOutput.FieldByName(fldVOLUME).AsFloat    :=
                RoundTo(ArrayOfVolumeFunctions[vfPrism](S1, S2, Distance),
                Precision);
            end;
          end;

          TableOutput.FieldByName(fldLENGTH).AsFloat := Distance;

          for J := 0 to ComponentFields.Count - 1 do
          begin
            try
              with TableOutput.FieldByName(ComponentFields[J]) do
                AsFloat := RoundTo(AsFloat / Max(1, Area), Precision);
            except
            end;
          end;

          with TableOutput.FieldByName(fldAREA) do
            if AsFloat <> 0 then
              AsFloat  := RoundTo(AsFloat, Precision)
            else
              AsString := '';
          with TableOutput.FieldByName(fldAREA2) do
            if AsFloat <> 0 then
              AsFloat  := RoundTo(AsFloat, Precision)
            else
              AsString := '';

          TableOutput.Post;
          TableOutput.Next;
          Inc(I);
        end;

        TableOutput.Close;
      finally
        Query.Free;
      end;
    finally
      ComponentFields.Free;
    end;
  end;
end;

//------------------------- RegularBlocks -----------------------------\\

procedure TfmAnalyseReserves.RegularBlocks;

type
  TFloatVec = array of Float;

var
  CurrentName:  string;
  I, J, K:      integer;
  ValueFloat:   Float;
  ValueInteger: integer;

var
  NBlockArray:   TFloatVec;
  NRockArray:    TFloatVec;
  RDensArray:    TFloatVec;
  ContentArray:  TFloatVec;
  DispersArray:  TFloatVec;
  DensityArray:  TFloatVec;
  MoistureArray: TFloatVec;
var
  DX, DY, DZ, XO, YO, ZO: double; //Size of block
  NX, NY, NZ:    integer; //Numbers of blocks

  Query: TQuery;

  {sub}
  procedure ClearFloatVec(var Value: TFloatVec);
  var
    I: integer;
  begin
    for I := Low(Value) to High(Value) do
      Value[I] := 0;
  end;

begin
  with dmBase do
  begin
    Variance := 0;
    ReadParFile(TableInput.TableName, XO, YO, ZO, DX, DY, DZ, NX, NY, NZ);
    TableInput.Open;
    TableInput.First;
    ProgressBar.Min := 0;
    ProgressBar.Max := TableInput.RecordCount;
    ProgressBar.StepIt;
    ProgressBar.StepIt;
    ProgressBar.Position := 0;
    { TODO : Define MinRockType and MaxRockType for integer fields }
    { TODO : Change ListBoxRealAttribute.Count to AttributeList of Float+Integer fields }

    // Creating the New Report TableOutput
    CreateReportTable;
    TableOutput.Open;
    // Calculation the components and ore reserves for every record
    SetLength(NBlockArray, ListBoxRealAttribute.Count + 1);
    SetLength(ContentArray, ListBoxRealAttribute.Count + 1);
    SetLength(DensityArray, ListBoxRealAttribute.Count + 1);
    SetLength(MoistureArray, ListBoxRealAttribute.Count + 1);
    SetLength(DispersArray, ListBoxRealAttribute.Count + 1);
    try
      ClearFloatVec(NBlockArray);
      ClearFloatVec(ContentArray);
      ClearFloatVec(DensityArray);
      ClearFloatVec(MoistureArray);
      ClearFloatVec(DispersArray);

      for I := 0 to TableInput.RecordCount - 1 do
      begin
        ProgressBar.Position := TableInput.RecNo;
        if RadioGroupDensity.ItemIndex = 1 then
          Density := TableInput.FieldByName(fldDENSITY).AsFloat;
        if RadioGroupDensity.ItemIndex = 1 then
          Moisture := TableInput.FieldByName(fldMOISTURE).AsFloat;
        for J := 0 to ListBoxRealAttribute.Count - 1 do
        begin
          CurrentName := ListBoxRealAttribute.Items[J];
          if TableInput.FieldByName(CurrentName).IsNull = False then // Not blank
          begin // Calculate and write reserves
            if (TableInput.FieldByName(CurrentName).DataType = ftFloat) or
              (TableInput.FieldByName(CurrentName).DataType = ftCurrency) then
            begin
              ValueFloat      := TableInput.FieldByName(CurrentName).AsFloat;
              NBlockArray[J]  := NBlockArray[J] + 1;
              ContentArray[J] := ContentArray[J] + ValueFloat;
              DensityArray[J] := DensityArray[J] + Density;
              MoistureArray[J] := MoistureArray[J] + Moisture;
              DispersArray[J] := DispersArray[J] + Variance;
            end
            else
            begin
              ValueInteger := TableInput.FieldByName(CurrentName).AsInteger;
              if (ValueInteger >= MinRockType[J]) and
                (ValueInteger <= MaxRockType[J]) then
              begin
                NBlockArray[J]   := NBlockArray[J] + 1;
                ContentArray[J]  := ContentArray[J] + 1;
                //MatIntArray[Round(ValueFloat)][J]:=MatIntArray[][J]+1;
                DensityArray[J]  := DensityArray[J] + Density;
                MoistureArray[J] := MoistureArray[J] + Moisture;
                DispersArray[J]  := DispersArray[J] + Variance;
              end;
            end;
          end;
        end;
        TableInput.Next;
      end;
      for J := 0 to ListBoxRealAttribute.Count - 1 do
      begin
        CurrentName := ListBoxRealAttribute.Items[J];
        if (TableInput.FieldByName(CurrentName).DataType = ftFloat) or
          (TableInput.FieldByName(CurrentName).DataType = ftCurrency) then
        begin
          TableOutput.Append;
          TableOutput.Fields[0].Value := ListBoxRealAttribute.Items[J];
          TableOutput.Fields[1].Value := NBlockArray[J];
          FContent := ContentArray[J] / NBlockArray[J];
          TableOutput.Fields[2].Value := RoundTo(FContent, Precision);

          TableOutput.Fields[3].Value := RoundTo(FContent, Precision);
          TableOutput.Fields[4].Value := RoundTo(FContent, Precision);

          Volume := Round(NBlockArray[J] * DX * DY * DZ / 1000);
          TableOutput.Fields[5].Value := Volume;

          if RadioGroupDensity.ItemIndex <> 0 then //???
            Density := DensityArray[J] / NBlockArray[J];
          TableOutput.Fields[6].Value :=
            RoundTo(Density, Precision);
          if RadioGroupMoisture.ItemIndex <> 0 then //???
            Moisture := MoistureArray[J] / NBlockArray[J];
          TableOutput.Fields[7].Value :=
            RoundTo(Moisture, Precision);

          if StrUpper(PChar(CurrentName)) = fldG then
            Tonnage := Round(FContent * NBlockArray[J])
          else if (StrUpper(PChar(CurrentName)) = fldZ) or
            (StrUpper(PChar(CurrentName)) = 'H') then
            Tonnage := Round(Volume * Density)
          else
            Tonnage := Round(0.01 * FContent * Volume * Density);
          TableOutput.Fields[8].Value := Tonnage;
          Variance := DispersArray[J] / NBlockArray[J];
          TableOutput.Fields[9].Value := Variance;
        end
        else  //Count integer Rocktypes
        begin
          for K := Round(MinRockType[J]) to Round(MaxRockType[J]) do
          begin
            Query := TQuery.Create(Self);
            with Query do
            begin
              try
                Sql.Add('Select count(D."' + CurrentName + '") D."Count"');
                if RadioGroupDensity.ItemIndex <> 0 then
                  Sql.Add(',Sum(D."Density") D.Density');
                if RadioGroupMoisture.ItemIndex <> 0 then //???
                  Sql.Add(',Sum(D."Moisture") D.Moisture');
                Sql.Add('From "' + TableInput.TableName + '" D');
                Sql.Add('Where D."' + CurrentName + '"=' + IntToStr(K));
                Open;
                if FieldValues['Count'] <> 0 then
                begin
                  TableOutput.Append;
                  TableOutput.Fields[0].Value := CurrentName + '_' + IntToStr(K);
                  TableOutput.Fields[1].Value := FieldValues['Count'];
                  FContent := K;
                  TableOutput.Fields[2].Value := FContent;
                  TableOutput.Fields[3].Value := 0;
                  TableOutput.Fields[4].Value := 255;

                  Volume := RoundTo(FieldValues['Count'] * DX *
                    DY * DZ / 1000, Precision);
                  TableOutput.Fields[5].Value := Volume;

                  if RadioGroupDensity.ItemIndex <> 0 then //???
                    Density := FieldValues[fldDENSITY] / NBlockArray[J];
                  TableOutput.Fields[6].Value := RoundTo(Density, Precision);
                  if RadioGroupMoisture.ItemIndex <> 0 then //???
                    Moisture := FieldValues[fldMOISTURE] / NBlockArray[J];
                  TableOutput.Fields[7].Value := RoundTo(Moisture, Precision);

                  Tonnage  := RoundTo(Volume * Density, Precision);
                  TableOutput.Fields[8].Value := Tonnage;
                  Variance := DispersArray[J] / NBlockArray[J];
                  TableOutput.Fields[9].Value := Variance;
                  try
                    TableOutput.Post;
                  except
                  end;
                end;
              except
              end;
              Free;
            end;
          end;

          TableOutput.Append;
          TableOutput.FieldByName(LoadResString(@rsAttribute)).AsString :=
            CurrentName + '=[' + FloatToStr(MinRockType[J]) + ';' +
            FloatToStr(MaxRockType[J]) + ']';
          TableOutput.FieldByName(LoadResString(@rsNumber)).Value := NBlockArray[J];
          FContent := ContentArray[J] / NBlockArray[J];
          TableOutput.FieldByName(LoadResString(@rsContent)).Value :=
            RoundTo(FContent, Precision);
          TableOutput.FieldByName(LoadResString(@rsMinimum)).Value := MinRockType[J];
          TableOutput.FieldByName(LoadResString(@rsMaximum)).Value := MaxRockType[J];

          Volume := RoundTo(NBlockArray[J] * DX * DY * DZ / 1000, Precision);
          TableOutput.FieldByName(LoadResString(@rsVolume)).AsFloat := Volume;

          if RadioGroupDensity.ItemIndex <> 0 then //???
            Density := DensityArray[J] / NBlockArray[J];
          TableOutput.FieldByName(LoadResString(@rsDensity)).Value :=
            RoundTo(Density, Precision);
          if RadioGroupMoisture.ItemIndex <> 0 then //???
            Moisture := MoistureArray[J] / NBlockArray[J];
          TableOutput.FieldByName(LoadResString(@rsMoisture)).Value :=
            RoundTo(Moisture, Precision);

          Tonnage := RoundTo(Volume * Density, Precision);
          TableOutput.FieldByName(LoadResString(@rsReserve)).AsFloat := Tonnage;

          Variance := DispersArray[J] / NBlockArray[J];
          TableOutput.FieldByName(LoadResString(@rsVariance)).AsFloat := Variance;
          try
            TableOutput.Post;
          except
          end;
        end;
        try
          TableOutput.Post;
        except
        end;
        TableOutput.Next;
      end;
      TableInput.Close;
      TableOutput.Close;
      ModalResult := mrOk;
      //  TransposeNumericTable(TableOutput.TableName);
    finally
      NBlockArray   := nil;
      ContentArray  := nil;
      DensityArray  := nil;
      MoistureArray := nil;
      DispersArray  := nil;
    end;
  end;
end;

procedure TfmAnalyseReserves.SobolevskyGrid;
begin
  RegularBlocks;
end;

//-------------------- Triangular Prizms ------------------------------\\
procedure TfmAnalyseReserves.TrianglePrizms;
var
  TableVertex: TTable;
  CurrentName: string;
  I, J: integer;
  V1, V2, V3: integer;
  px1, py1, pz1, // Coordinates of V1
  px2, py2, pz2, // Coordinates of V2
  px3, py3, pz3, // Coordinates of V3
  Aedge, Bedge, Cedge: double; // Edges of a triangle
  Aux, AreaXY, AreaXYZ: double;
  TotalThickness, TotalAreaXY, TotalAreaXYZ, TotalVolume: double;

begin
  with dmBase do
  begin
    TableInput.Open;
    CreateReportTable;
    TableOutput.Open;
    if TableInput.FindField(fldAREA) = nil then
    begin
      TableInput.Close;
      AddTableField(TableInput.TableName, fldAREA, ftFloat);
      TableInput.Open;
    end;
    if TableInput.FindField(fldVOLUME) = nil then
    begin
      TableInput.Close;
      AddTableField(TableInput.TableName, fldVOLUME, ftFloat);
      TableInput.Open;
    end;
    if TableInput.FindField(fldTHICKNESS) = nil then
    begin
      TableInput.Close;
      AddTableField(TableInput.TableName, fldTHICKNESS, ftFloat);
      TableInput.Open;
    end;

    ProgressBar.Max      := TableInput.RecordCount;
    ProgressBar.Position := 1;
    ProgressBar.Min      := 1;

    TableVertex := TTable.Create(Self);
    try
      TableVertex.TableName :=
        ChangeModelTable(DirTinFaces, DirTinVertices, TableInput.TableName);

      TableVertex.AddIndex('', fldID, [ixPrimary], ''); //Adds primary index
      TableVertex.Open;

      TableVertex.First;
      MinX := TableVertex.FieldByName(fldX).AsFloat;
      MaxX := MinX;
      MinY := TableVertex.FieldByName(fldY).AsFloat;
      MaxY := MinY;
      MinZ := TableVertex.FieldByName(fldZ).AsFloat;
      MaxZ := MinZ;
      for I := 0 to TableInput.RecordCount - 1 do
      begin
        TableInput.RecNo     := I + 1;
        ProgressBar.Position := TableInput.RecNo;

        V1 := TableInput.FieldByName(fldV1).AsInteger;
        if TableVertex.FindKey([V1]) then
        begin
          px1 := TableVertex.FieldByName(fldX).AsFloat;
          py1 := TableVertex.FieldByName(fldY).AsFloat;
          pz1 := TableVertex.FieldByName(fldZ).AsFloat;
        end;

        V2 := TableInput.FieldByName(fldV2).AsInteger;
        if TableVertex.FindKey([V2]) then
        begin
          px2 := TableVertex.FieldByName(fldX).AsFloat;
          py2 := TableVertex.FieldByName(fldY).AsFloat;
          pz2 := TableVertex.FieldByName(fldZ).AsFloat;
        end;
        V3 := TableInput.FieldByName(fldV3).AsInteger;
        if TableVertex.FindKey([V3]) then
        begin
          px3 := TableVertex.FieldByName(fldX).AsFloat;
          py3 := TableVertex.FieldByName(fldY).AsFloat;
          pz3 := TableVertex.FieldByName(fldZ).AsFloat;
        end;
        if px1 < MinX then
          MinX := px1;
        if px1 > MaxX then
          MaxX := px1;
        if px2 < MinX then
          MinX := px2;
        if px2 > MaxX then
          MaxX := px2;
        if px3 < MinX then
          MinX := px3;
        if px3 > MaxX then
          MaxX := px3;

        if py1 < MinY then
          MinY := py1;
        if py1 > MaxY then
          MaxY := py1;
        if py2 < MinY then
          MinY := py2;
        if py2 > MaxY then
          MaxY := py2;
        if py3 < MinY then
          MinY := py3;
        if py3 > MaxY then
          MaxY := py3;

        if pz1 < MinZ then
          MinZ := pz1;
        if pz1 > MaxZ then
          MaxZ := pz1;
        if pz2 < MinZ then
          MinZ := pz2;
        if pz2 > MaxZ then
          MaxZ := pz2;
        if pz3 < MinZ then
          MinZ := pz3;
        if pz3 > MaxZ then
          MaxZ := pz3;

        aux := px1 * (py2 - py3) + px2 * (py3 - py1) + px3 * (py1 - py2);

        Aedge  := SQRT(SQR(px2 - px1) + SQR(py2 - py1));
        Bedge  := SQRT(SQR(px3 - px2) + SQR(py3 - py2));
        Cedge  := SQRT(SQR(px1 - px3) + SQR(py1 - py3));
        AreaXY := AreaTriangle(Aedge, Bedge, Cedge);
        //the area of projection to XY plane

        Aedge   := SQRT(SQR(px2 - px1) + SQR(py2 - py1) + SQR(pz2 - pz1));
        Bedge   := SQRT(SQR(px3 - px2) + SQR(py3 - py2) + SQR(pz3 - pz2));
        Cedge   := SQRT(SQR(px1 - px3) + SQR(py1 - py3) + SQR(pz1 - pz3));
        AreaXYZ := AreaTriangle(Aedge, Bedge, Cedge); //the real area in 3D

        Thickness   := (pz1 + pz2 + pz3) / 3;
        Volume      := AreaXY * Thickness; //projection to
        TotalThickness := TotalThickness + Thickness;
        TotalVolume := TotalVolume + Volume;
        TotalAreaXY := TotalAreaXY + AreaXY;
        TotalAreaXYZ := TotalAreaXYZ + AreaXYZ;

        TableInput.Edit;
        TableInput.FieldByName(fldTHICKNESS).AsFloat :=
          RoundTo(Thickness, Precision);
        TableInput.FieldByName(fldAREA).AsFloat      :=
          RoundTo(AreaXYZ, Precision);
        TableInput.FieldByName(fldVOLUME).AsFloat    :=
          RoundTo(Volume, Precision);
        TableInput.Post;
      end;
      ProgressBar.Position := ProgressBar.Max;
    finally
      TableVertex.Close;
      TableVertex.Free;
      DeleteFile(TableVertex.TableName + TableInd);   //Delete primary index
    end;
(*
    for J := 0 to ListBoxRealAttribute.Count-1 do
    begin
      CurrentName := ListBoxRealAttribute.Items[J];
      if (TableInput.FieldByName(CurrentName).DataType = ftFloat) or
        (TableInput.FieldByName(CurrentName).DataType = ftCurrency) then
      begin
        TableOutput.Append;
        TableOutput.Fields[0].Value := ListBoxRealAttribute.Items[J];
        TableOutput.Fields[1].Value := I;
        FContent := 100; // ContentArray[J] / NBlockArray[J];
        TableOutput.Fields[2].Value := SetPrecision(FContent, Precision);

        TableOutput.Fields[3].Value := SetPrecision(FContent, Precision);
        TableOutput.Fields[4].Value := SetPrecision(FContent, Precision);

        TableOutput.Fields[5].Value := SetPrecision(TotalVolume, Precision);

        if RadioGroupDensity.ItemIndex <> 0 then
//          Density := DensityArray[J] / NBlockArray[J];
        else
          Density := 1; // TotalDensity[J]/I;
        TableOutput.Fields[6].Value := SetPrecision(Density, Precision);
      end;
    end;

    if RadioGroupMoisture.ItemIndex <> 0 then
//      Moisture := MoistureArray[J] / NBlockArray[J];
    else
      Moisture := 0; // TotalMoisture[J]/I;
    TableOutput.Fields[7].Value := SetPrecision(Moisture, Precision);

    Tonnage := SetPrecision(TotalVolume * Density, Precision);
    TableOutput.FieldByName(LoadResString(@sgbReserve)).AsFloat := Tonnage;
*)
    TableOutput.Append;
    TableOutput.FieldByName(LoadResString(@rsAttribute)).AsString := 'Z';
    TableOutput.FieldByName(LoadResString(@rsNumber)).Value := I; //Number of Prizms

    TableOutput.FieldByName(LoadResString(@rsContent)).Value := 100; //No Real Attribute
    TableOutput.FieldByName(LoadResString(@rsMinimum)).Value := MinZ;
    TableOutput.FieldByName(LoadResString(@rsMaximum)).Value := MaxZ;

    TableOutput.FieldByName(LoadResString(@rsVolume)).AsFloat :=
      RoundTo(TotalVolume, Precision);

    if RadioGroupDensity.ItemIndex = 0 then
      TableOutput.FieldByName(LoadResString(@rsDensity)).AsFloat :=
        RoundTo(Density, Precision)
    else
      Density := 1; // TotalDensity[J]/I;
    if RadioGroupMoisture.ItemIndex = 0 then
      TableOutput.FieldByName(LoadResString(@rsMoisture)).AsFloat :=
        RoundTo(Moisture, Precision)
    else
      Moisture := 0; // TotalMoisture[J]/I;

    Tonnage := RoundTo(TotalVolume * Density, Precision);
    TableOutput.FieldByName(LoadResString(@rsReserve)).AsFloat := Tonnage;

    //  Variance:=DispersArray[J]/NBlockArray[J];
    //  TableOutput.FieldByName(SVariance).AsFloat:=Variance;
    try
      TableOutput.Post;
    except
    end;
    TableOutput.Next;

    TableInput.Close;
    TableOutput.Close;
  end;
end;


procedure TfmAnalyseReserves.VoronoiPolygons;
var
  TablePolyVertex: TTable;
  I: integer;
  Polygon3D: TPolygonArr3D;
  Polygon2D: TPolygonArr2D;
  M: TMatrix4d;
  Z: double;
  xp, yp, zp: array of single;
  VertexCount: integer;

  TotalArea, TotalVolume: double;

begin
  with dmBase do
  begin
    TableInput.Open;
    CreateReportTable;
    TableOutput.Open;
    if TableInput.FindField(fldAREA) = nil then
    begin
      TableInput.Close;
      AddTableField(TableInput.TableName, fldAREA, ftFloat);
      TableInput.Open;
    end;
    if TableInput.FindField(fldVOLUME) = nil then
    begin
      TableInput.Close;
      AddTableField(TableInput.TableName, fldVOLUME, ftFloat);
      TableInput.Open;
    end;

    TablePolyVertex := TTable.Create(Self);
    try
      TablePolyVertex.TableName :=
        ChangeModelTable(DirPolygonPoly, DirPolygonVertex, TableInput.TableName);
      TablePolyVertex.Open;
      ProgressBar.Max      := TableInput.RecordCount;
      ProgressBar.Position := 1;
      ProgressBar.Min      := 1;
      for I := 0 to TableInput.RecordCount - 1 do
      begin
        TableInput.RecNo     := I + 1;
        ProgressBar.Position := TableInput.RecNo;

        if not ReadPolygon(TableInput, TablePolyVertex, xp, yp, zp, VertexCount) then
          Continue;
        { TODO -oPV -c1 : Replace polygon3d with xp,yp,zp!!! }
        Polygon2D := Polygon3DTo2D(Polygon3D, M);
        Area   := GetPolygonArea(Polygon2D);
        Z      := TableInput.FieldByName(fldZ).AsFloat;
        Volume := Area * Z;

        TotalArea   := TotalArea + Area;
        TotalVolume := TotalVolume + Volume;

        TableInput.Edit;
        TableInput.FieldByName(fldAREA).AsFloat   := RoundTo(Area, Precision);
        TableInput.FieldByName(fldVOLUME).AsFloat := RoundTo(Volume, Precision);
        TableInput.Post;
      end;
      ProgressBar.Position := ProgressBar.Max;
    finally
      TablePolyVertex.Free;
    end;

    TableOutput.Append;
    TableOutput.FieldByName(LoadResString(@rsAttribute)).AsString := 'Z';
    TableOutput.FieldByName(LoadResString(@rsNumber)).Value := I; //Number of Prizms

    TableOutput.FieldByName(LoadResString(@rsContent)).Value := 100; //No Real Attribute
    TableOutput.FieldByName(LoadResString(@rsMinimum)).Value := MinZ;
    TableOutput.FieldByName(LoadResString(@rsMaximum)).Value := MaxZ;

    TableOutput.FieldByName(LoadResString(@rsVolume)).AsFloat := RoundTo(TotalVolume, Precision);

    if RadioGroupDensity.ItemIndex = 0 then
      TableOutput.FieldByName(LoadResString(@rsDensity)).AsFloat :=
        RoundTo(Density, Precision)
    else
      Density := 1; // TotalDensity[J]/I;
    if RadioGroupMoisture.ItemIndex = 0 then
      TableOutput.FieldByName(LoadResString(@rsMoisture)).AsFloat :=
        RoundTo(Moisture, Precision)
    else
      Moisture := 0; // TotalMoisture[J]/I;

    Tonnage := RoundTo(TotalVolume * Density, Precision);
    TableOutput.FieldByName(LoadResString(@rsReserve)).AsFloat := Tonnage;

    //  Variance:=DispersArray[J]/NBlockArray[J];
    //  TableOutput.FieldByName(SVariance).AsFloat:=Variance;
    try
      TableOutput.Post;
    except
    end;
    TableOutput.Next;

    TableInput.Close;
    TableOutput.Close;
  end;
end;

procedure TfmAnalyseReserves.ButtonOKClick(Sender: TObject);
begin
  inherited;
  with dmBase do
    case ToolBarInput.Tag of
      mtDholes: ;   //Holes
      mtPoints2D: Sections; //Points2D
      mtPoints3D: ; //Points3D
      mtPolygons:
        case DropdownMenuPoly.Tag of
          0: //Vertical sections - Polygons
            with TfmReserveCutOptions.Create(Self) do
              try
                SetTableInput(TableInput);
                if ShowModal = mrOk then
                  if CalcBySort then
                    CuttingsWithOreSort(fldOreTypeName, fldOreSortName)
                  else
                    Cuttings(fldOreTypeName)
                else
                  Self.ModalResult := mrNone;
              finally
                Free;
              end;
          1: VoronoiPolygons;
        end;
      mtTins: TrianglePrizms;
      mtGrids2D: SobolevskyGrid;
      mtGrids3D: RegularBlocks;
    end;
end;

end.
