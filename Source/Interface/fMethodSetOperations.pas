//------------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//---------------------------------------------------------------------------
{! The unit for set operations }
//-----------------------------------------------------------------------------

unit fMethodSetOperations;

interface

uses
  Winapi.Windows, 
  Winapi.Messages,
  System.SysUtils, 
  System.Math, 
  System.IniFiles,
  System.Classes,
  System.ImageList,
  Vcl.Graphics, 
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,

  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ToolWin,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  Vcl.Clipbrd,
  Vcl.ImgList,

  //BDE
  Bde.DBTables,
  Data.DB,

  GLS.VectorTypes,
  GLS.VectorGeometry,

  dBase,
  GBGeometry,
  fMethodDualDialog;

type
  TfmMethodSetOperations = class(TfmMethodDualDialog)
    GroupBoxOperation:   TGroupBox;
    RadioGroupOperation: TRadioGroup;
    procedure RadioGroupOperationClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListBoxInputNamesClick(Sender: TObject);
    procedure ListBoxInputNamesBClick(Sender: TObject);
    procedure ToolButtonInputBClick(Sender: TObject);
    procedure ToolButtonInputClick(Sender: TObject);
  private
    procedure MoveRecordFromTo(TableSource, TableDest: TTable);
    procedure Dholes_Uni_Polygons;
    procedure Dholes_Int_Grid3D;
    procedure Points_Points;       // Union, Difference and Intersection
    procedure Points2D_Polygons;   // Union, Difference and Intersection
    procedure Polygons_Int_Dholes(TablePolygons, TableHoles: TTable);
    procedure Polygons_Uni_Points2D;
    procedure Polygons_Int_Points2D;
    procedure Tin_Uni_Tin;
    procedure Grid2D_Uni_Grid2D;
    procedure Grid2D_Int_Grid2D;
    procedure Grid3D_Uni_Grid3D;
    procedure Grid3D_Int_Grid3D;
    procedure Mesh2D_Uni_Tin;
    function ExecuteOperations: boolean;
  private
    procedure ReadIniFile;
    procedure WriteIniFile;
  end;

var
  fmMethodSetOperations: TfmMethodSetOperations;

//=========================================================================
implementation
//=========================================================================

uses
  uCommon,
  cProfuns,
  cDiscoCore,
  cGlobals,
  cResStrings,
  uFileCreator,
  fMethodDialog,
  uNormal;

{$R *.DFM}

var
  PolygonSumLength: double;
  TablePolyVertex:  TTable;
  PolygonArray:     array of TSegment3D;
  VertexCount:      integer;

  DX, DY, DZ, XO, YO, ZO: double;
  NX, NY, NZ: integer;


{ TfmMethodSetOperations }

procedure TfmMethodSetOperations.ToolButtonInputClick(Sender: TObject);
begin
  inherited;
  ListBoxInputNamesClick(Sender);
end;


procedure TfmMethodSetOperations.ToolButtonInputBClick(Sender: TObject);
begin
  inherited;
  ListBoxInputNamesBClick(Sender);
end;


procedure TfmMethodSetOperations.ListBoxInputNamesClick(Sender: TObject);
begin
  inherited;
  RadioGroupOperationClick(Sender);
  ListBoxRealAttribute.ItemIndex := -1;
end;

procedure TfmMethodSetOperations.ListBoxInputNamesBClick(Sender: TObject);
begin
  inherited;
  RadioGroupOperationClick(Sender);
  ListBoxRealAttributeB.ItemIndex := -1;
end;


procedure TfmMethodSetOperations.RadioGroupOperationClick(Sender: TObject);
begin
  EditOutName.Text := '';
  with dmBase do
    case RadioGroupOperation.ItemIndex of
      {uni} 0:
      begin
        EditOutName.Text := NameOnly(TableInput.TableName) + '_uni_' +
          NameOnly(TableInputB.TableName);
      end;
      {dif} 1:
      begin
        EditOutName.Text := NameOnly(TableInput.TableName) + '_dif_' +
          NameOnly(TableInputB.TableName);
      end;
      {int} 2:
      begin
        EditOutName.Text := NameOnly(TableInput.TableName) + '_int_' +
          NameOnly(TableInputB.TableName);
      end;
    end;
end;

//============================= OPERATIONS ================================\\

function CalcPolygonArea: extended;
var
  I: integer;
begin
  Result := 0;
  for I := 0 to High(PolygonArray) do
  begin
    Result := Result + (PolygonArray[I].YStart + PolygonArray[I].YEnd) *
      (PolygonArray[I].XEnd - PolygonArray[I].XStart);
  end;
  Result := Abs(Result / 2);
end;


// ------------------- Dholes ---------------------\\
procedure TfmMethodSetOperations.Dholes_Int_Grid3D;
var
  I: integer;
begin
  for I := 0 to dmBase.TableInput.RecordCount - 1 do
  begin
    //if
  end;
end;

procedure TfmMethodSetOperations.Dholes_Uni_Polygons;
var
  I: integer;
begin
  for I := 0 to dmBase.TableInput.RecordCount - 1 do
  begin
    //if
  end;
end;

procedure TfmMethodSetOperations.MoveRecordFromTo(TableSource, TableDest: TTable);
var
  I:      integer;
  Buffer: PChar;
begin
  //  Buffer := TableSource.ActiveBuffer;
  //  TableSource.GetCurrentRecord(Buffer);
  //  TableDest.AppendRecord(Values);
  { TODO -oVas -c2 : Speed up the movement of record from temp to out! }
  TableDest.Append;
  TableDest.Edit;
  for I := 0 to TableSource.FieldCount - 1 do
  begin
    TableDest.Fields[I].AsVariant := TableSource.Fields[I].AsVariant;
  end;
  TableDest.Post;
end;

//--------------- Points2D ----------------------\\

procedure TfmMethodSetOperations.Points_Points;
var
  I, J: integer;
  PointA, PointB: TPoint3D;

begin
  with dmBase do
  begin
    try
      TableOutput.EmptyTable;

      TableInputB.Open; //The second model of points
      TableTemp.Open;   //The copy of TableInput
      TableOutput.Open; //We need it opened to move records from TableTemp

      ProgressBar.Position := 0;
      ProgressBar.Min      := 0;
      ProgressBar.Max      := TableTemp.RecordCount;
      ProgressBar.Step     := ProgressBar.Max;
      TableInputB.First;
      if (RadioGroupOperation.ItemIndex = 0) then //Union of points and points
        for I := 0 to TableInputB.RecordCount - 1 do
        begin
          TableTemp.Append;
          { TODO 1 -oVas : We should compare structures and add all fields }
          for J := 0 to 3 do
            TableTemp.Fields[J].AsVariant := TableInputB.Fields[J].AsVariant;
          TableTemp.Post;
          TableInputB.Next;
        end
      else //Difference and Intersection of two models of points
        for I := 1 to TableInputB.RecordCount do
        begin
          PointB.X := TableTemp.FieldByName(fldX).AsFloat;
          PointB.Y := TableTemp.FieldByName(fldY).AsFloat;
          PointB.Z := TableTemp.FieldByName(fldZ).AsFloat;

          if not TableTemp.FindFirst then
            Continue;
          repeat
            PointA.X := TableTemp.FieldByName(fldX).AsFloat;
            PointA.Y := TableTemp.FieldByName(fldY).AsFloat;
            PointA.Z := TableTemp.FieldByName(fldZ).AsFloat;
            if EqualPoints3d(PointA, PointB) then //intersection
            begin
              MoveRecordFromTo(TableTemp, TableOutput);
              TableTemp.Delete;
            end
            else
              TableTemp.Next;
            ProgressBar.StepIt;
          until TableTemp.EOF;
          if not TableInputB.FindNext then
            Break;
        end;
    finally
      TableInputB.Close;
      TableTemp.Close;
      TableOutput.Close;
    end;
    // Replace output table with temporary one for union and difference operations
    if (RadioGroupOperation.ItemIndex <> 2) then
      CopyFiles(TableTemp.TableName, TableOutput.TableName, InModelType, False);

    //Calculate IDs
    TableOutput.Open;
    TableOutput.First;
    for I := 1 to TableOutput.RecordCount do
    begin
      TableOutput.Edit;
      TableOutput.FieldByName(fldID).AsInteger := I;
      TableOutput.Post;
      TableOutput.Next;
    end;
    TableOutput.Close;
    ProgressBar.Position := ProgressBar.Max;
  end;
end;

 // ===========================================================================\\
 // Input: Points2D table and Polygon table with a clipping polygon.
 // Output: Points2D table inside clipping polygon.

 // Description: A search point can be only in one polygon, so...
 // We create a TempTable, because it's undesirable to damage ModelA Table
 // then loop polygons from a Polygon Table searching points inside the TempTable
 // If we have several polygons then after finding a point inside first of it
 // we move the point into TableOutput and therefore for next polygon
 // in the TempTable should have much lesser points to search
 //============================================================================\\

procedure TfmMethodSetOperations.Points2D_Polygons;
var
  I, J:  integer;
  Xp, Yp, Zp: array of single;
  Point: array[0..2] of single;
  VertexCount: integer;

begin
  with dmBase do
  begin
    try
      TableOutput.EmptyTable; // Points 2D
      TableInputB.Open;       // Polygon.Poly
      TableTemp.Open;         // Points 2D
      TableOutput.Open;       //Move records from TableTemp
      TablePolyVertex := TTable.Create(Self);
      TablePolyVertex.TableName :=
        ChangeModelTable(DirPolygonPoly, DirPolygonVertex, InModelNameB);
      TablePolyVertex.Open;

      SetLength(XP, TablePolyVertex.RecordCount);
      SetLength(YP, TablePolyVertex.RecordCount);
      SetLength(ZP, TablePolyVertex.RecordCount);

      ProgressBar.Position := 0;
      ProgressBar.Min      := 0;
      ProgressBar.Max      := TableTemp.RecordCount * TableInputB.RecordCount;
      if (RadioGroupOperation.ItemIndex = 0) then //Union of points and vertices
        for I := 0 to TablePolyVertex.RecordCount - 1 do
        begin
          TableTemp.Append;
          for J := 0 to 3 do //Moves only ID, X, Y and Z fields from the polygon!
            TableTemp.Fields[J].AsVariant :=
              TablePolyVertex.Fields[J].AsVariant;
          TableTemp.Post;
          TablePolyVertex.Next;
        end
      else    //Difference and Intersection
        for I := 0 to TableInputB.RecordCount - 1 do
        begin
          ProgressBar.Position := TableInputB.RecNo;
          if not ReadPolygon(TableInputB, TablePolyVertex, XP, YP, ZP, VertexCount) then
            Continue;
          //Uses temporary point table
          if not TableTemp.FindFirst then
            Continue;
          repeat
            Point[0] := TableTemp.FieldByName(fldX).AsFloat;
            Point[1] := TableTemp.FieldByName(fldY).AsFloat;
            Point[2] := TableTemp.FieldByName(fldZ).AsFloat;
            if PointInPolygon(XP, YP, Point[0], Point[1]) then
            begin
              MoveRecordFromTo(TableTemp, TableOutput);
              TableTemp.Delete;
            end
            else
              TableTemp.Next;
          until TableTemp.EOF;
          if not TableInputB.FindNext then
            Break;
        end;
    finally
      TablePolyVertex.Free;
      TableInputB.Close;
      TableTemp.Close;
      TableOutput.Close;
      XP := nil;
      YP := nil;
      ZP := nil;
      ProgressBar.Position := ProgressBar.Max;
    end;
    // Replace output table with temporary one for union and difference operations
    if (RadioGroupOperation.ItemIndex <> 2) then
    begin
      CopyFiles(TableTemp.TableName, TableOutput.TableName, InModelType,
        False);
    end;

    //Recalculates IDs
    TableOutput.Open;
    TableOutput.First;
    for I := 1 to TableOutput.RecordCount do
    begin
      TableOutput.Edit;
      TableOutput.FieldByName(fldID).AsInteger := I;
      TableOutput.Post;
      TableOutput.Next;
    end;
    TableOutput.Close;
  end;
end;

//---------------------------- Grids2D ----------------------------\\

procedure TfmMethodSetOperations.Grid2D_Int_Grid2D;
begin
end;

 //---------------------------------------------------------------------\\
 // Sometimes we do not have completed grid but only a subset of blocks \\
 // So we need to loop over every node of the grid and search nodes in A\\
 // and B grids for nodes with the same X, Y coordinates to insert in\\
 // OutTable \\
 // The nodes in Table B upgrade nodes in Table A      \\
 //----------------------------------------------------\\

procedure TfmMethodSetOperations.Grid2D_Uni_Grid2D;
var
  I:     integer;
  Polygon: TPolygonArr3d;
  xp, yp, zp: array of single;
  Point: array[0..2] of single;

begin
  with dmBase do
  begin
    ReadParFile(TableTemp.TableName, XO, YO, ZO, DX, DY, DZ, NX, NY, NZ);
    TablePolyVertex := TTable.Create(Self);
    TablePolyVertex.TableName :=
      ChangeModelTable(DirPolygonPoly, DirPolygonVertex, TableInputB.TableName);
    try
      TableTemp.Open;
      TableOutput.Open;
      TableInputB.Open;
      ProgressBar.Max      := TableInputB.RecordCount;
      ProgressBar.Position := 0;
      ProgressBar.Min      := 0;
      ProgressBar.Max      := NX * NY * NZ;
      if (ProgressBar.Max > 20) then
        ProgressBar.Step := ProgressBar.Max div 20
      else
        ProgressBar.Step := ProgressBar.Max;
      for I := 1 to TableInputB.RecordCount do
      begin
        if not ReadPolygon(TableInputB, TablePolyVertex, xp, yp, zp, VertexCount) then
          Continue;
        if not TableTemp.FindFirst then
          Continue;
        repeat
          Point[0] := TableTemp.FieldByName(fldX).AsFloat;
          Point[1] := TableTemp.FieldByName(fldY).AsFloat;
          //we use Z for the bottom of the cell to compare with Z of polygon
          Point[2] := TableTemp.FieldByName(fldZ).AsFloat - DZ / 2;
          if (Point[2] < PolygonArray[I].ZStart) or
            (Point[2] > PolygonArray[I].Zend) then
          begin
            TableTemp.Next;
            Continue;
          end;
          if not PointInPolygon(xp, yp, Point[0], Point[1]) then
          begin
            MoveRecordFromTo(TableTemp, TableOutput);
            TableTemp.Delete;
          end
          else
            TableTemp.Next;
        until TableTemp.EOF;
        ProgressBar.StepIt;
        if not TableInputB.FindNext then
          Break;
      end;
      ProgressBar.Position := ProgressBar.Max;
    finally
      TablePolyVertex.Free;
      TableInputB.Close;
      TableTemp.Close;
      TableOutput.Close;
    end;
  end;
end;

procedure TfmMethodSetOperations.Grid3D_Int_Grid3D;
begin
  //TODO 1 -oVassiliev 2 -cSet Operation: Intersection of 3D Grids.
end;

procedure TfmMethodSetOperations.Grid3D_Uni_Grid3D;
begin
  //TODO 2 -cSet Operation -oVassiliev 2: Union of Grids in 3D
end;

//--------------------------- Polygons ----------------------------\\

procedure TfmMethodSetOperations.Polygons_Int_Dholes(TablePolygons, TableHoles: TTable);

var
  I, J:   integer;
  TablePolyVertex: TTable;
  Polygon3d: array of TVector3d;
  M:      TMatrix4d;
  Polygon: TPolyline2D;
  Sample: TLine2D;
  VertexCount: integer;
  SampleStart: integer;
  Sample3d: array[0..1] of TVector3d;
  Report: TStrings;
  CurrHole: string;
  CopyFields: TStrings;
  PolygonSumLength: double;

  {sub}
  function GetMidle(Segment: TLine2D): TPoint2D;
  begin
    Result.X := (Segment.XStart + Segment.XEnd) / 2;
    Result.Y := (Segment.YStart + Segment.YEnd) / 2;
  end;

  {sub}
  function isSampleCrossBoundFromTop(Sample, Bound: TLine2D): boolean;
  var
    dl2x,  (*Change in X in line segment 2*)
    dl2y,  (*Change in Y in line segment 2*)
    dl1x,  (*Change in X in line segment 1*)
    dl1y,  (*Change in X in line segment 1*)
    dl12x, (*Change in X between line segments 1 and 2*)
    dl12y, (*Change in Y between line segments 1 and 2*)
    denom, (*Denominator*)
    parm1, parm2: double; (*Misc*)
  begin    (*IntersectLs*)
    Result := False;
    dl2x   := Bound.EndPoint.X - Bound.StartPoint.x;
    dl2y   := Bound.EndPoint.y - Bound.StartPoint.y;
    dl1x   := Sample.EndPoint.x - Sample.StartPoint.x;
    dl1y   := Sample.EndPoint.y - Sample.StartPoint.y;
    dl12x  := Bound.StartPoint.x - Sample.StartPoint.x;
    dl12y  := Bound.StartPoint.y - Sample.StartPoint.y;
    denom  := dl2y * dl1x - dl2x * dl1y;
    if (denom <> 0) then
    begin
      parm1  := ((dl2y * dl12x) - (dl2x * dl12y)) / denom;
      parm2  := ((dl1y * dl12x) - (dl1x * dl12y)) / denom;
      Result := (0 < parm1) and (Parm1 < 1.0) and
        //line1 and line2 intersect inner Segment1 and
        (0 <= parm2) and (Parm2 <= 1.0);
      //line1 and line2 intersect inner Segment2
    end;
  end;

  {sub}
  function isSampleCrossBoundFromBottom(Sample, Bound: TLine2D): boolean;
  var
    dl2x,  (*Change in X in line segment 2*)
    dl2y,  (*Change in Y in line segment 2*)
    dl1x,  (*Change in X in line segment 1*)
    dl1y,  (*Change in X in line segment 1*)
    dl12x, (*Change in X between line segments 1 and 2*)
    dl12y, (*Change in Y between line segments 1 and 2*)
    denom, (*Denominator*)
    parm1, parm2: double; (*Misc*)
  begin    (*IntersectLs*)
    Result := False;
    dl2x   := Bound.EndPoint.X - Bound.StartPoint.x;
    dl2y   := Bound.EndPoint.y - Bound.StartPoint.y;
    dl1x   := Sample.EndPoint.x - Sample.StartPoint.x;
    dl1y   := Sample.EndPoint.y - Sample.StartPoint.y;
    dl12x  := Bound.StartPoint.x - Sample.StartPoint.x;
    dl12y  := Bound.StartPoint.y - Sample.StartPoint.y;
    denom  := dl2y * dl1x - dl2x * dl1y;
    if (denom <> 0) then
    begin
      parm1  := ((dl2y * dl12x) - (dl2x * dl12y)) / denom;
      parm2  := ((dl1y * dl12x) - (dl1x * dl12y)) / denom;
      Result := (0.5 <= parm1) and (Parm1 <= 1.0) and
        //line1 and line2 intersect inner Segment1 and
        (0 <= parm2) and (Parm2 <= 1.0);
      //line1 and line2 intersect inner Segment2
    end;
  end;


  {sub}
  function Pnt_In_Poly(Polygon: array of TLine2D; Point: TPoint2D): boolean;
  var
    I: integer;
  begin
    Result := False;
    for I := 0 to VertexCount - 1 do
    begin
      if ((((Polygon[I].YStart <= Point.Y) and (Point.Y < Polygon[I].YEnd)) or
        ((Polygon[I].YEnd <= Point.Y) and (Point.Y < Polygon[I].YStart))) and
        (Point.X < (Polygon[I].XEnd - Polygon[I].XStart) *
        (Point.Y - Polygon[I].YStart) / (Polygon[I].YEnd - Polygon[I].YStart) +
        Polygon[I].XStart)) then
        Result := not Result;
    end;
  end;

  {PointInPolygon3D defines if a point projection on a polygon
   lies inside the polygon in 3D using the next algorithm:
  1. With 3 polygon points (that not lie on a line, ò.å. n<>(0;0;0))
     defines a Normal for the polygon,
  2. alpha=arccos(z/|n|), where alpha - rotation angle,
                               z    - z coordinate of the normal,
                              |n|   - length of the normal
  3. r = Oz x n, where r  - rotation axis,  Oz - basis along z (0;0;1)
  4. Transformation of data:
    à) Transfers the origin of coordinates into the polygon centre,
    á) Rotates the polygon and centre around r on -alpha degrees;
  5. Checkouts belongings of the point projection to polygon in XY plane.
  }

  { TODO -ovassiliev -c1 : The same in common unit... }
  {sub}
  function Polygon3dTo2d(Polygon: array of TVector3d; var M: TMatrix4d):
  TPolyline2D;
  var
    Normal: TVector3d;      //Normal to polygon
    R:      TVector3d;      //Rotation axis
    O:      TVector3d;      //Polygon centre
    I, J, K: integer;
    Alpha:  double;
    Vector3f: TVector3f;
    Vector4d: TVector4d;
    AffineDblVector: TAffineDblVector;
    AffineFltVector: TAffineFltVector;
  begin
    {1.} K := High(Polygon) - 1;
    J      := K + 1;
    I      := 0;
    repeat
      Normal := GetNormal(MakeVector3d([Polygon[K].V[0] - Polygon[J].V[0],
        Polygon[K].V[1] - Polygon[J].V[1], Polygon[K].V[2] - Polygon[J].V[2]]),
        MakeVector3d([Polygon[I].V[0] - Polygon[J].V[0], Polygon[I].V[1] -
        Polygon[J].V[1], Polygon[I].V[2] - Polygon[J].V[2]]));
      K      := J;
      J      := I;
      I      := I + 1;
      Vector3f.V[0] := Normal.V[0];
      Vector3f.V[1] := Normal.V[1];
      Vector3f.V[2] := Normal.V[2];

    until (I > High(Polygon)) or (VectorNorm(Vector3f) > 1E-9);
    {2.}
    Alpha := ArcCos(Normal.V[2] / VectorNorm(Vector3f));
    {3.}
    R     := GetNormal(Normal, MakeVector3d([0, 0, 1]));
    {4.}
    M     := IdentityHmgDblMatrix;
    {a)}
    O     := MakeVector3d([0, 0, 0]);
    for i := 0 to High(Polygon) do
    begin
      O.V[0] := O.V[0] + Polygon[I].V[0];
      O.V[1] := O.V[1] + Polygon[I].V[1];
      O.V[2] := O.V[2] + Polygon[I].V[2];
    end;
    O.V[0] := -O.V[0] / (High(Polygon) + 1);
    O.V[1] := -O.V[1] / (High(Polygon) + 1);
    O.V[2] := -O.V[2] / (High(Polygon) + 1);

    M := CreateTranslationMatrix4d(O);

    {á)}
    R := NormalizeVector3d(R);
    M := MultiplyMatrix4d(M, CreateRotationMatrix4d(R, Alpha));

    for I := 0 to High(Polygon) do
    begin
      Vector4d   := Vector3dTo4d(Polygon[I]);
      Polygon[I] := Vector4dTo3d(VectorTransform4d(Vector4d, M));
    end;

    SetLength(Result, High(Polygon) + 1);

    J := 0;
    for I := High(Result) downto 0 do
    begin
      Result[I].XStart := Polygon[I].V[0];
      Result[I].YStart := Polygon[I].V[1];
      Result[I].XEnd   := Polygon[J].V[0];
      Result[I].YEnd   := Polygon[J].V[1];
    end;
  end;

  {sub}
  function ReadSample: boolean;
  begin
    {
            Result:=True;
            CurrHole:=TableHoles.FieldByName(fldDHOLE).AsString;
            SampleStart:=TableHoles.RecNo;
            Sample.XStart:=TableHoles.FieldByName(fldY).AsFloat;
            Sample.YStart:=TableHoles.FieldByName(fldZ).AsFloat;
            TableHole.FindNext;
            Sample.XEnd:=TableHoles.FieldByName(fldY).AsFloat;
            Sample.YEnd:=TableHoles.FieldByName(fldZ).AsFloat;
            {}
    CurrHole    := TableHoles.FieldByName(fldDHOLE).AsString;
    SampleStart := TableHoles.RecNo;
    Sample3d[0].V[0] := TableHoles.FieldByName(fldX).AsFloat;
    Sample3d[0].V[1] := TableHoles.FieldByName(fldY).AsFloat;
    Sample3d[0].V[2] := TableHoles.FieldByName(fldZ).AsFloat;
    Result      := TableHoles.FindNext;
    Sample3d[1].V[0] := TableHoles.FieldByName(fldX).AsFloat;
    Sample3d[1].V[1] := TableHoles.FieldByName(fldY).AsFloat;
    Sample3d[1].V[2] := TableHoles.FieldByName(fldZ).AsFloat;

    Sample3d[0] := Vector4dTo3d(VectorTransform4d(Vector3dTo4d(Sample3d[0]), M));
    Sample3d[1] := Vector4dTo3d(VectorTransform4d(Vector3dTo4d(Sample3d[1]), M));

    Sample.XStart := Sample3d[0].V[0];
    Sample.YStart := Sample3d[0].V[1];
    Sample.XEnd   := Sample3d[1].V[0];
    Sample.YEnd   := Sample3d[1].V[1];
  end;

  {sub}
  procedure CopyNamesToClipboard;
  var
    I: integer;
    S: string;
  begin
    S := TablePolygons.TableName + #13 + TableHoles.TableName + #13;
    //       S:='';
    S := S + TablePolygons.Fields[0].FieldName;
    for I := 1 to TablePolygons.FieldCount - 1 do
    begin
      if CopyFields.IndexOf(TablePolygons.Fields[I].FieldName) >= 0 then
        continue;
      S := S + #9 + TablePolygons.Fields[I].FieldName;
    end;
    for I := 0 to TableHoles.FieldCount - 1 do
    begin
      if CopyFields.IndexOf(TableHoles.Fields[I].FieldName) < 0 then
        continue;
      S := S + #9 + TableHoles.Fields[I].FieldName;
    end;

    Report.Add(S);
  end;

  {sub}
  procedure CopyValueToClipboard;
  var
    I:      integer;
    S:      string;
    Length: double;
  begin
    if TableHoles.FieldByName(fldLENGTH).IsNull then
      Exit;

    Length := TableHoles.FieldByName(fldLENGTH).AsFloat;
    PolygonSumLength := PolygonSumLength + Length;

    {       S:=format('Sample ((%f,%f),(%f,%f)); Boundary ((%f,%f),(%f,%f))'#13,
             [Sample.XStart,Sample.YStart,Sample.XEnd,Sample.YEnd,
              Boundary.XStart,Boundary.YStart,Boundary.XEnd,Boundary.YEnd]);{}
    S := '';
    S := S {+TablePolygons.Fields[0].AsString{};
    for I := 1 to TablePolygons.FieldCount - 1 do
    begin
      if CopyFields.IndexOf(TablePolygons.Fields[I].FieldName) >= 0 then
        Continue;
      S := S + #9 {+TablePolygons.Fields[I].AsString{};
    end;
    for I := 0 to TableHoles.FieldCount - 1 do
    begin
      if CopyFields.IndexOf(TableHoles.Fields[I].FieldName) < 0 then
        continue;
      with TableHoles.Fields[I] do
        try
          TablePolygons.Edit;
          TablePolygons.FieldByName(FieldName).AsFloat :=
            TablePolygons.FieldByName(FieldName).AsFloat + AsFloat * Length;
          TablePolygons.Post;
        except
        end;
      S := S + #9 + TableHoles.Fields[I].AsString;
    end;
    Report.Add(S);
  end;

  {sub}
  procedure CopySumToClipboard;
  var
    I: integer;
    S: string;
  begin
    S := '';
    S := LoadResString(@rsAverage) + #13;

    TablePolygons.Edit;
    TablePolygons.FieldByName(fldLENGTH).AsFloat := PolygonSumLength;
    TablePolygons.Post;

    S := S + TablePolygons.Fields[0].AsString;
    for I := 1 to TablePolygons.FieldCount - 1 do
    begin
      if CopyFields.IndexOf(TablePolygons.Fields[I].FieldName) >= 0 then
        continue;
      S := S + #9 + TablePolygons.Fields[I].AsString;
    end;
    for I := 0 to TableHoles.FieldCount - 1 do
    begin
      if CopyFields.IndexOf(TableHoles.Fields[I].FieldName) < 0 then
        continue;
      with TableHoles.Fields[I] do
      begin
        try
          TablePolygons.Edit;
          if CompareText(FieldName, fldLENGTH) = 0 then
            TablePolygons.FieldByName(FieldName).AsFloat :=
              RoundTo(PolygonSumLength, Precision)
          else
            TablePolygons.FieldByName(FieldName).AsFloat :=
              RoundTo(TablePolygons.FieldByName(FieldName).AsFloat /
              Max(1, PolygonSumLength), Precision);
          TablePolygons.Post;
        except
        end;
        try
          S := S + #9 + TablePolygons.FieldByName(FieldName).AsString;
        except
          S := S + #9;
        end;
      end;
    end;
    Report.Add(S + #13);
  end;

  {sub}
  function SampleCrossPolygonFromTop: boolean;
  var
    I: integer;
    MidlePoint: TPoint2D;
  begin
    Result     := False;
    MidlePoint := GetMidle(Sample);
    for I := 0 to VertexCount - 1 do
    begin
      if isSampleCrossBoundFromTop(Sample, Polygon[I]) then
      begin
        Result := True;
        Exit;
      end;
      if Pnt_In_Poly(Polygon, MidlePoint) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;

  {sub}
  function SampleCrossPolygonFromBottom: boolean;
  var
    I: integer;
  begin
    Result := False;
    for I := 0 to VertexCount - 1 do
      if isSampleCrossBoundFromBottom(Sample, Polygon[I]) then
      begin
        Result := True;
        Exit;
      end;
  end;

  {sub}
  function GetAreaOfPolygon: extended;
  var
    I: integer;
  begin
    Result := 0;
    for I := 0 to High(Polygon) do
    begin
      Result := Result + (Polygon[I].YStart + Polygon[I].YEnd) *
        (Polygon[I].XEnd - Polygon[I].XStart);
    end;
    Result := Abs(Result / 2);
  end;

  {sub}
  function ReadPolyVertices: boolean;
  var
    I, J: integer;
  begin
    PolygonSumLength := 0;
    TablePolyVertex.Filter := Format(fldID_POLY + '=%d',
      [TablePolygons.FieldByName(fldID).AsInteger]);
    Result := TablePolyVertex.FindFirst;
    if not Result then
      Exit;
    VertexCount := 0;
    Polygon     := nil;
    Polygon3d   := nil;
    repeat
      Inc(VertexCount);
      if High(Polygon3D) < VertexCount then
        SetLength(Polygon3D,
          High(Polygon3d) + 10);
      Polygon3d[VertexCount - 1] :=
        MakeVector3d([TablePolyVertex.FieldByName(fldX).AsFloat,
        TablePolyVertex.FieldByName(fldY).AsFloat,
        TablePolyVertex.FieldByName(fldZ).AsFloat]);
      //  Polygon[VertexCount-1].XStart:=TablePolyVertex.FieldByName(fldY).AsFloat;
      //  Polygon[VertexCount-1].YStart:=TablePolyVertex.FieldByName(fldZ).AsFloat;
    until not TablePolyVertex.FindNext;
    SetLength(Polygon3d, VertexCount);
    Polygon := Polygon3dTo2d(Polygon3d, M);
    J := VertexCount - 1;
    for I := 0 to VertexCount - 1 do
    begin
      Polygon[J].EndPoint := Polygon[I].StartPoint;
      J := I;
    end;
    try
      TablePolygons.Edit;
      TablePolygons.FieldByName(fldAREA).AsFloat := GetAreaOfPolygon;
      TablePolygons.Post;
    except
    end;
  end;

  { TODO -oVassiliev -cDatabase : Remove direct call of fieldnames }
  function GetFldINTERVAL: string;
  const
    Arr: array[0..9] of string = ('ERROR:"ORETYPE=0"'
      , 'VARIANT1_C1', 'VARIANT2_C1', 'VARIANT3_C1'
      , 'VARIANT1_C2', 'VARIANT2_C2', 'VARIANT3_C2'
      , 'VARIANT1_C3', 'VARIANT2_C3', 'VARIANT3_C3');
  begin
    Result := fldORETYPE;
    try
      if TablePolygons.FindField(fldINTERVAL) <> nil then
        Result :=
          TablePolygons.FieldByName(fldINTERVAL).AsString
      else
        Result := Arr[TablePolygons.FieldByName(fldORETYPE).AsInteger];
    except
    end;
  end;

var
  IntervalValue:    integer;
  IntervalStart:    integer;
  IntervalEnd:      integer;
  strINTERVAL:      string;
  FindNextInterval: boolean;

  {sub}
  function ReadInterval: boolean;
  var
    Value: integer;
  begin
    FindNextInterval := True;
    try
      while (TableHoles.FieldByName(strINTERVAL).AsInteger <> IntervalValue) do
      begin
        if not (TableHoles.FindNext) then
        begin
          FindNextInterval := False;
          Break;
        end;
      end;
      Result := TableHoles.FieldByName(strINTERVAL).AsInteger = IntervalValue;
    except
      Result := False;
    end;
    if Result then
    begin
      CurrHole      := TableHoles.FieldByName(fldDHOLE).AsString;
      IntervalStart := TableHoles.RecNo;
      IntervalEnd   := IntervalStart - 1;
      //  Value:=TableHoles.FieldByName(strINTERVAL).AsInteger;
      while (TableHoles.FieldByName(strINTERVAL).AsInteger = IntervalValue) and
        (TableHoles.FieldByName(fldDHOLE).AsString = CurrHole) do
      begin
        Inc(IntervalEnd);
        if not (TableHoles.FindNext) then
        begin
          FindNextInterval := False;
          Break;
        end;
      end;
    end;
  end;

  {sub}
  function IntervalCrossPolygon: boolean;
  var
    I: integer;
  begin
    Result := False;
    TableHoles.RecNo := IntervalStart;
    for I := IntervalStart to IntervalEnd do
    begin
      ReadSample;
      Result := SampleCrossPolygonFromTop;
      if Result then
        Exit;
    end;
  end;

begin //body
  CopyFields := TStringList.Create;
  try
    TablePolygons.Filter := Format('%s=%d AND %s<>'''' AND %s <> ''''',
      [fldID_TYPE, integer(ptPolygon), fldPROFILE, fldINTERVAL]);
    TablePolygons.Close;
    //  TableHoles.Filter:=Format('PROFILE="%s"',[TableModelB.FieldByName(fldPROFILE).AsString]);
    TableHoles.Open;

    for I := 0 to TableHoles.FieldCount - 1 do
    begin
      TablePolygons.Open;
      if TableHoles.Fields[I].DataType = ftFloat then
        if TablePolygons.FindField(TableHoles.Fields[I].FieldName) = nil then
          with TableHoles.Fields[I] do
          begin
            CopyFields.Add(FieldName);
            if {(CompareText(FieldName,fldLENGTH)<>0)
            and {}(CompareText(FieldName, fldDEPTH) <> 0) and
              (CompareText(FieldName, fldCORE) <> 0) and
              (CompareText(FieldName, fldCORE_PERCENT) <> 0) then
            begin
              TablePolygons.Close;
              AddTableField(TablePolygons.TableName, FieldName, DataType, Size);
            end;
          end;
    end;
    if CopyFields.Count = 0 then
    begin
      ShowMessage(LoadResString(@rsRepeatedIntersection));
      Exit;
    end;
    CopyFields.Add(fldDHOLE);
    TablePolygons.Close;
    AddTableField(TablePolygons.TableName, fldAREA, ftFloat);
    TablePolygons.Open;
    TablePolygons.FindFirst;
    TableHoles.First;
    TablePolyVertex := TTable.Create(Self);
    ProgressBar.Max := TablePolygons.RecordCount * TableHoles.RecordCount;
    try
      Report := TStringList.Create;
      try
        TablePolyVertex.TableName :=
          ChangeModelTable(DirPolygonPoly, DirPolygonVertex,
          TablePolygons.TableName);
        TablePolyVertex.Open;
        CopyNamesToClipboard;
        for I := 1 to TablePolygons.RecordCount do
        begin
          ProgressBar.Position := I * TableHoles.RecordCount;
          if not ReadPolyVertices then
            Continue;
          strINTERVAL := GetFldINTERVAL;

          IntervalValue := TablePolygons.FieldByName(strINTERVAL).AsInteger;

          TableHoles.Filter :=
            Format('%s=''%s''', [fldPROFILE, TablePolygons.FieldByName(
            fldPROFILE).AsString]);
          if not TableHoles.FindFirst then
            continue;

          while ReadInterval do
          begin
            if IntervalCrossPolygon then
            begin
              TableHoles.RecNo := IntervalStart;
              for J := IntervalStart to IntervalEnd do
              begin
                CopyValueToClipboard;
                TableHoles.FindNext;
              end;
            end;
            if not FindNextInterval then
              Break;
          end;
          CopySumToClipboard;
          if not TablePolygons.FindNext then
            Break;
        end;
        ProgressBar.Position := ProgressBar.Max;
        Clipboard.AsText     := Report.Text;
      finally
        Report.Free;
      end;
    finally
      TablePolyVertex.Free;
    end;
  finally
    CopyFields.Free;
  end;
  TablePolygons.Close;
  TableHoles.Close;
  Polygon := nil;
end;

 //-----------------------------------------------------------------------\\
 // InputA: Polygon
 // InputB: Points2D
 // Output: Polygon that embraces all vertices and points
 //   Area field and all attribute fields from Points2D table
 //   with calculated avarage real and integer values
 //-----------------------------------------------------------------------\\
procedure TfmMethodSetOperations.Polygons_Uni_Points2D;
begin

end;


 //-----------------------------------------------------------------------\\
 // InputA: Clipping Polygon
 // InputB: Points 2D
 // Output: Clipping Polygon with
 //   Area field and all attribute fields from Points 2D table
 //   with calculated avarage real and integer values
 //-----------------------------------------------------------------------\\
procedure TfmMethodSetOperations.Polygons_Int_Points2D;
var
  I, J:    integer;
  Xp, Yp, Zp: array of single;
  Point:   array[0..2] of single;
  TableIntPoints: TTable;
  Area:    double;
  Polygon: PAffineVectorArray;

begin
  with dmBase do
  begin
    try
      TableIntPoints := TTable.Create(Self);
      TableIntPoints.TableName :=
        InModelNameB + '_int_' + NameOnly(InModelName);

      TablePolyVertex := TTable.Create(Self);
      TablePolyVertex.TableName :=
        ChangeModelTable(DirPolygonPoly, DirPolygonVertex, OutModelName);
      CopyFiles(InModelNameB + TableExt,
        TableIntPoints.TableName + TableExt, InModelTypeB, False);

      TableIntPoints.EmptyTable;
      TableOutput.Open;         // Polygon.Poly
      DeleteFiles(TableTemp.TableName, InModelType);

      //Create a temporary Point  2D table
      TableTemp.TableName := ExtractFileDir(InModelNameB) + PathDelim +
        'Temporary' + TableExt;
      CopyFiles(ChangeFileExt(InModelNameB, '.*'),
        TableTemp.TableName, InModelTypeB, False);
      TableTemp.Open;
      TableIntPoints.Open;     //to Move records from TableTemp
      TablePolyVertex.Open;

      if TableOutput.FindField(fldAREA) = nil then
      begin
        TableOutput.Close;
        AddTableField(TableOutput.TableName, fldAREA, ftFloat);
        TableOutput.Open;
      end;

      SetLength(XP, TablePolyVertex.RecordCount);
      SetLength(YP, TablePolyVertex.RecordCount);
      SetLength(ZP, TablePolyVertex.RecordCount);

      ProgressBar.Position := 0;
      ProgressBar.Min      := 0;
      ProgressBar.Max      := TableTemp.RecordCount * TableOutput.RecordCount;
      //New(Polygon);

      for I := 0 to TableOutput.RecordCount - 1 do
      begin
        ProgressBar.Position := TableOutput.RecNo;
        if not ReadPolygon(TableOutput, TablePolyVertex, XP, YP, ZP, VertexCount) then
          Continue;

        //Uses temporary point table
        if not TableTemp.FindFirst then
          Continue;
        repeat
          Point[0] := TableTemp.FieldByName(fldX).AsFloat;
          Point[1] := TableTemp.FieldByName(fldY).AsFloat;
          Point[2] := TableTemp.FieldByName(fldZ).AsFloat;
          if PointInPolygon(XP, YP, Point[0], Point[1]) then
          begin
            MoveRecordFromTo(TableTemp, TableIntPoints);
            TableTemp.Delete; //Deletes the active record
          end
          else
            TableTemp.Next;
        until TableTemp.EOF;

        //Area := PolygonArea(Polygon, VertexCount);
        {
        for J := 0 to TableIntPoints.FieldCount -1 do
        begin
          if TableOutput.FindField(TableIntPoints.Fields[J].FieldName) = nil then
          begin
            TableOutput.Close;
            AddTableField(TableOutput.TableName, TableIntPoints.Fields[J].FieldName,
                          TableIntPoints.Fields[J].DataType);
            TableOutput.Open;
          end;
        end;
        {}
        TableOutput.Next;
      end;
    finally
      TablePolyVertex.Free;
      TableTemp.Close;
      TableOutput.Close;
      Polygon := nil;
      XP      := nil;
      YP      := nil;
      ZP      := nil;
      ProgressBar.Position := ProgressBar.Max;
    end;

    //Recalculates IDs
    TableIntPoints.Open;
    TableIntPoints.First;
    for I := 1 to TableIntPoints.RecordCount do
    begin
      TableIntPoints.Edit;
      TableIntPoints.FieldByName(fldID).AsInteger := I;
      for J := 1 to TableIntPoints.FieldCount do
      begin

      end;
      TableIntPoints.Post;
      TableIntPoints.Next;
    end;


    TableIntPoints.Close;
    TableIntPoints.Free;

  end;
end;

(*---------------------------------------------------------------------------
  The Union operation with two Tins
   1. Copies TinA Tables to TinC Output Tables, TinCVertex and TinCTriangle
   2. Reads TinBVertex table records and add it to the end of TinCVertex
      table increasing IDs and not changing other attributes
   3. Appends all records from TinBTriangle table to the end of TinCTriangle
      table and recalculating V1,V2,V3 as V1 (or V2, or V3) +
      TinBVertex.RecordCount and recalculating N1,N2,N3 as
      N1(N2,N3)+ TinATriangle.RecordCount not changing other attributes
   So there can be saved unrestricted number of tins within one tin data set
 ---------------------------------------------------------------------------*)
procedure TfmMethodSetOperations.Tin_Uni_Tin;
var
  I, J: integer;
  V1, V2, V3, N1, N2, N3: integer;
  TableVertexB, TableVertexC: TTable;
  PriorV, PriorT: integer;   //Last ID vertex and last ID triangle

begin
  with dmBase do
  begin
    try
      TableVertexB := TTable.Create(nil);
      TableVertexC := TTable.Create(nil);

      TableVertexB.TableName :=
        ChangeModelTable(DirTinFaces, DirTinVertices, InModelNameB);
      TableVertexC.TableName :=
        ChangeModelTable(DirTinFaces, DirTinVertices, OutModelName);

      TableInputB.Open; //InputB triangles
      TableOutput.Open; //Output triangles
      TableVertexB.Open;
      TableVertexC.Open;

      ProgressBar.Position := 0;
      ProgressBar.Min      := 0;
      ProgressBar.Max      := TableVertexB.RecordCount + TableInputB.RecordCount;
      ProgressBar.Step     := 1;

      //Vertices
      TableVertexB.First;
      TableVertexC.Last;
      PriorV := TableVertexC.RecordCount;
      I      := 0;
      while not TableVertexB.EOF do
      begin
        TableVertexC.Append;
        TableVertexC.Fields[0].AsInteger := PriorV + I;   //Next identifier
        for J := 1 to TableVertexB.FieldCount - 1 do
        begin
          TableVertexC.Fields[J] := TableVertexB.Fields[J];
        end;
        TableVertexC.Post;
        TableVertexB.Next;
        Inc(I);
        ProgressBar.StepIt;
      end;

      //Triangles
      TableInputB.First;
      TableOutput.Last;
      PriorT := TableOutput.RecordCount;
      I      := 0;
      while not TableInputB.EOF do
      begin
        Inc(I);
        TableOutput.Append;
        TableOutput.Fields[0].AsInteger := PriorT + I;  //Next identifier
        for J := 1 to TableInputB.FieldCount - 1 do
        begin
          TableOutput.Fields[J] := TableInputB.Fields[J];
        end;
        TableOutput[fldV1] := TableOutput[fldV1] + PriorV;
        TableOutput[fldV2] := TableOutput[fldV2] + PriorV;
        TableOutput[fldV3] := TableOutput[fldV3] + PriorV;
        TableOutput[fldN1] := TableOutput[fldN1] + PriorT;
        TableOutput[fldN2] := TableOutput[fldN2] + PriorT;
        if TableOutput[fldN3] <> -1 then
          TableOutput[fldN3] := TableOutput[fldN3] + PriorT;
        TableOutput.Post;
        TableInputB.Next;
        ProgressBar.StepIt;
      end;
    finally
      TableInputB.Close;
      TableOutput.Close;
      TableVertexB.Close;
      TableVertexC.Close;
    end;
    ProgressBar.Position := ProgressBar.Max;
  end;
end;

 //-----------------------------------------------------------------------\\
 // Combines an existed Mesh2D data complete with Tin model               \\
 //-----------------------------------------------------------------------\\
procedure TfmMethodSetOperations.Mesh2D_Uni_Tin;
var
  I, J, K: integer;
  TableTinVertex, TableMesh2DNode, TableMesh2DElement, TableMesh2DMatrix,
  TableMesh2DLink: TTable;
  PriorN, PriorE: integer;   //Prior numbers of nodes and elements

begin
  with dmBase do
  begin
    TableTinVertex     := TTable.Create(nil);
    TableMesh2DNode    := TTable.Create(nil);
    TableMesh2DElement := TTable.Create(nil);
    TableMesh2DMatrix  := TTable.Create(nil);
    TableMesh2DLink    := TTable.Create(nil);
    try
      TableTinVertex.TableName :=
        ChangeModelTable(DirTinFaces, DirTinVertices, InModelNameB);

      TableMesh2DLink.TableName    := OutModelName;
      TableMesh2DNode.TableName    := ChangeModelTable(DirMesh2D, DirMesh2DVertices, OutModelName);
      TableMesh2DMatrix.TableName  := ChangeModelTable(DirMesh2D, DirMesh2DFaces, OutModelName);

      TableInputB.Open; //InputB triangles
      TableTinVertex.Open;
      TableMesh2DElement.Open; //Output elements
      TableMesh2DNode.Open;
      TableMesh2DMatrix.Open;
      TableMesh2DLink.Open;

      ProgressBar.Position := 0;
      ProgressBar.Min      := 0;
      ProgressBar.Max      := TableTinVertex.RecordCount + TableInputB.RecordCount;
      ProgressBar.Step     := ProgressBar.Max div 10;

      //Nodes and vertices
      TableTinVertex.First;
      TableMesh2DNode.Last;
      PriorN := TableMesh2DNode.RecordCount;
      I      := 0;
      while not TableTinVertex.EOF do
      begin
        TableMesh2DNode.Append;
        TableMesh2DNode.Fields[0].AsInteger := PriorN + I;   //Next identifier
        for J := 1 to TableMesh2DNode.FieldCount - 1 do
        begin
          TableMesh2DNode.Fields[J] := TableTinVertex.Fields[J];
        end;
        TableMesh2DNode.Post;
        TableTinVertex.Next;
        Inc(I);
        ProgressBar.StepIt;
      end;

      //Elements and triangles
      TableInputB.First;
      TableMesh2DElement.Last;
      PriorE := TableMesh2DElement.RecordCount;
      I      := 0;
      while not TableInputB.EOF do
      begin
        Inc(I);
        TableMesh2DElement.Append;
        TableMesh2DElement.Fields[0].AsInteger := PriorE + I;  //Next identifier

        for J := 1 to TableMesh2DElement.FieldCount - 1 do
        begin
          TableMesh2DElement.Fields[J] := TableInputB.Fields[J];
        end;
        TableMesh2DElement[fldV1] := TableMesh2DElement[fldV1] + PriorN;
        TableMesh2DElement[fldV2] := TableMesh2DElement[fldV2] + PriorN;
        TableMesh2DElement[fldV3] := TableMesh2DElement[fldV3] + PriorN;
        TableMesh2DElement[fldN1] := TableMesh2DElement[fldN1] + PriorE;
        TableMesh2DElement[fldN2] := TableMesh2DElement[fldN2] + PriorE;
        if TableMesh2DElement[fldN3] <> -1 then
          TableMesh2DElement[fldN3] := TableMesh2DElement[fldN3] + PriorE;

        {
        TableMesh2DElement.FieldByName(fldX).AsFloat :=
          TableInputB.FieldByName(fldX).AsFloat;
        TableMesh2DElement.FieldByName(fldY).AsFloat :=
          TableInputB.FieldByName(fldY).AsFloat;
        TableMesh2DElement.FieldByName(fldZ).AsFloat :=
          TableInputB.FieldByName(fldZ).AsFloat;
        TableMesh2DElement.FieldByName(fldSLOPE).AsFloat :=
          TableInputB.FieldByName(fldSLOPE).AsFloat;
        TableMesh2DElement.FieldByName(fldMATERIAL).AsInteger :=
          TableInputB.FieldByName(fldMATERIAL).AsInteger;
        }
        TableMesh2DElement.Post;
        TableInputB.Next;
        ProgressBar.StepIt;
      end;

      //Matrices - adds a record into Mesh2DMatrix table
      Query.Sql.Clear;
      Query.Sql.Add('SELECT AVG(' + fldX + '), AVG(' + fldY + '), AVG(' +
        fldZ + '),' + ' SUM(' + fldAREA + '), AVG(' + fldMATERIAL +
        ')' + ' FROM "' + TableInputB.TableName + '"');
      Query.Open;
      I := TableMesh2DMatrix.RecordCount;
      TableMesh2DMatrix.Last;
      TableMesh2DMatrix.Append;
      TableMesh2DMatrix.FieldByName(fldID_MATRIX).AsInteger := I + 1;
      TableMesh2DMatrix.FieldByName(fldX).AsFloat     :=
        RoundTo(Query.Fields[0].AsFloat, Precision);
      TableMesh2DMatrix.FieldByName(fldY).AsFloat     :=
        RoundTo(Query.Fields[1].AsFloat, Precision);
      TableMesh2DMatrix.FieldByName(fldZ).AsFloat     :=
        RoundTo(Query.Fields[2].AsFloat, Precision);
      TableMesh2DMatrix.FieldByName(fldAREA).AsFloat  :=
        RoundTo(Query.Fields[3].AsFloat, Precision);
      TableMesh2DMatrix.FieldByName(fldMATERIAL).AsInteger := Query.Fields[4].AsInteger;
      TableMesh2DMatrix.FieldByName(fldNAME).AsString := NameOnly(InModelNameB);
      TableMesh2DMatrix.Post;
      TableMesh2DMatrix.Next;

      //Writes ID_MATRIX, ID_ELEMENT and ID_NODE into Mesh2DLink
      TableMesh2DLink.Last;
      K := 0;
      for I := 0 to TableInputB.RecordCount - 1 do
      begin
        Inc(K);
        TableMesh2DLink.Append;
        TableMesh2DLink.FieldByName(fldID_MATRIX).AsInteger :=
          TableMesh2DMatrix.RecordCount;
        TableMesh2DLink.FieldByName(fldID_ELEMENT).AsInteger := K + PriorE;
        TableMesh2DLink.FieldByName(fldID_NODE).AsInteger   :=
          TableInputB.FieldByName(fldV1).AsInteger + PriorN;
        TableMesh2DLink.Post;
        TableMesh2DLink.Append;
        TableMesh2DLink.FieldByName(fldID_MATRIX).AsInteger :=
          TableMesh2DMatrix.RecordCount;
        TableMesh2DLink.FieldByName(fldID_ELEMENT).AsInteger := K + PriorE;
        TableMesh2DLink.FieldByName(fldID_NODE).AsInteger   :=
          TableInputB.FieldByName(fldV2).AsInteger + PriorN;
        TableMesh2DLink.Post;
        TableMesh2DLink.Append;
        TableMesh2DLink.FieldByName(fldID_MATRIX).AsInteger :=
          TableMesh2DMatrix.RecordCount;
        TableMesh2DLink.FieldByName(fldID_ELEMENT).AsInteger := K + PriorE;
        TableMesh2DLink.FieldByName(fldID_NODE).AsInteger   :=
          TableInputB.FieldByName(fldV3).AsInteger + PriorN;
        TableMesh2DLink.Post;
        TableInputB.Next;
        ProgressBar.StepIt;
      end;

      TableInputB.Close;
      TableOutput.Close;
      TableTinVertex.Close;
      TableMesh2DElement.Close;
      TableMesh2DNode.Close;
      TableMesh2DMatrix.Close;
      TableMesh2DLink.Close;
      Query.Close;
    finally
      TableTinVertex.Free;
      TableMesh2DElement.Free;
      TableMesh2DNode.Free;
      TableMesh2DMatrix.Free;
      TableMesh2DLink.Free;
    end;
    ProgressBar.Position := ProgressBar.Max;
  end;
end;


//======================================================\\

function TfmMethodSetOperations.ExecuteOperations: boolean;
begin
  with dmBase do
  begin
    TableTemp.TableName := PanelOutPath.Caption + 'Temporary' + TableExt;
    CopyFiles(InModelName + TableExt,
      OutModelName + TableExt, InModelType, False);
    CopyFiles(ChangeFileExt(InModelName, '.*'),
      TableTemp.TableName, InModelType, False);
    case InModelType of
      mtDholes:
        case InModelTypeB of
          mtPolygons:
            case RadioGroupOperation.ItemIndex of
              0: Dholes_Uni_Polygons;
            end;
          mtGrids3D:
            case RadioGroupOperation.ItemIndex of
              2: Dholes_Int_Grid3D;
            end;
        end;
      mtPoints2D:
      begin
        case InModelTypeB of
          mtPoints2D: Points_Points;
          mtPolygons: Points2D_Polygons;
        end;
      end;
      mtPoints3D:
        case InModelTypeB of
          mtPolygons: ;
          mtGrids3D: case RadioGroupOperation.ItemIndex of
              2: ;
            end;
        end;
      mtPolygons:
        case InModelTypeB of
          mtDholes:
            case RadioGroupOperation.ItemIndex of
              0, 1: ;
              2: Polygons_Int_Dholes(TableOutput, TableInputB);
            end;
          mtPoints2D:
          begin
            case RadioGroupOperation.ItemIndex of
              0, 1: ;
              2: Polygons_Int_Points2D;
            end;
          end;
        end;
      mtTins:
        case RadioGroupOperation.ItemIndex of
          0: Tin_Uni_Tin;
          1, 2: ;
        end;
      mtSolids:
        ;
      mtGrids2D:
      begin
        case InModelTypeB of
          mtPolygons: Points2D_Polygons;
          mtGrids2D: Points_Points;
        end;
      end;
      mtGrids3D:
        case InModelTypeB of
          mtPolygons: Points2D_Polygons;
          mtGrids3D: Points_Points;
        end;
      mtMeshes2D:
        case InModelTypeB of
          mtTins: case RadioGroupOperation.ItemIndex of
              0: Mesh2D_Uni_Tin;
              1, 2: ;
            end;

        end;
    end;
    DeleteFiles(TableTemp.TableName, InModelType);
  end;
end;

//------------------------------------------------------------------\\

procedure TfmMethodSetOperations.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
  ListBoxRealAttribute.ItemIndex  := -1;
  ListBoxRealAttributeB.ItemIndex := -1;
end;

procedure TfmMethodSetOperations.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      RadioGroupOperation.ItemIndex := ReadInteger(Name, RadioGroupOperation.Name, 0);
    finally
      IniFile.Free;
    end;
end;

procedure TfmMethodSetOperations.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteInteger(Name, RadioGroupOperation.Name, RadioGroupOperation.ItemIndex);
    finally
      IniFile.Free;
    end;
end;

procedure TfmMethodSetOperations.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;

procedure TfmMethodSetOperations.ButtonOKClick(Sender: TObject);
begin
  inherited;
  if ModalResult = mrOk then
    with dmBase do
    begin
      if (InModelName = InModelNameB) then
      begin
        MessageDlg(LoadResString(@rsTable) + ' À = ' +
          LoadResString(@rsTable) + ' Â!', mtError, [mbOK], 0);
        ModalResult := mrNone;
        Exit;
      end;
      if ExtractFileExt(OutModelName) = '' then
        TableOutput.TableName := OutModelName + TableExt
      else
        TableOutput.TableName := OutModelName;

      ExecuteOperations; // case with models operations
      TableOutput.Open;
      if TableOutput.IsEmpty then
      begin
        if MessageDlg(LoadResString(@rsEmpty) + ', ' + LoadResString(@rsDelete) +
          ' ' + NameOnly(OutModelName) + '?', mtConfirmation,
          [mbYes, mbNo], 0) = mrYes then
        begin
          TableOutput.Close;
          DeleteFiles(OutModelName, InModelType);
          ModalResult := mrNone;
          Close;
          Exit;
        end;
      end;
      TableOutput.Close;
      OutModelType := InModelType;  //Always for set operations
    end;
end;

end.
