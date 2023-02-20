//-----------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//-----------------------------------------------------------------------------
{! The dialog for Triangulation in 2D and 3D}

unit fMethodTriangulation;

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
  Vcl.FileCtrl,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ToolWin,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  Vcl.Samples.Spin,

  Bde.DBTables,
  Data.DB,
  GLS.VectorTypes,
  GLS.VectorGeometry,

  dBase,
  fMethodDialog,
  gnuGettext,
  GBGeometry,
  uCommon,
  cGlobals,
  cProfuns,
  uDelaunay2D,
  cDiscoCore,
  cResStrings;

type
  TfmMethodTriangulation = class(TfmMethodDialog)
    RadioGroupMethod: TRadioGroup;
    ComboBoxBreakLines: TComboBox;
    FileListBoxBreaklines: TFileListBox;
    GroupBoxAddField:   TGroupBox;
    SpinEditMaterialID: TSpinEdit;
    CheckBoxConvexHull: TCheckBox;
    CheckBoxVoronoiDiagram: TCheckBox;
    procedure ListBoxInputNamesClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure RadioGroupMethodClick(Sender: TObject);
    procedure ToolButtonInputClick(Sender: TObject);
    procedure ListBoxRealAttributeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure Delaunay2DExecute;
    procedure ConstrainedTINExecute;
    procedure Test;
    procedure ReadIniFile;
    procedure WriteIniFile;
  end;

var
  fmMethodTriangulation: TfmMethodTriangulation;

//=========================================================================
implementation
//=========================================================================

uses
  uFileCreator,
  uDrawVor;

{$R *.DFM}

procedure TfmMethodTriangulation.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
end;


procedure TfmMethodTriangulation.ToolButtonInputClick(Sender: TObject);
begin
  inherited;
  if ToolButtonPoints2D.Down then
  begin
    Caption := _(rsTriangulation);
    InModelType := mtPoints2D;
    OutModelType := mtTins;
    RadioGroupMethod.Items[0] := _('Delaunay triangulation');
    RadioGroupMethod.Items[1] := _('Restricted triangulation');
    CheckBoxVoronoiDiagram.Caption := _('Voronoi polygons');
    PanelOutPath.Caption := ExpandPath(DirTinFaces);
    PanelOutPath.Hint    := PanelOutPath.Caption;
  end;
  if ToolButtonPoints3D.Down then
  begin
    Caption := _(rsTetrahedralization);
    InModelType := mtPoints3D;
    OutModelType := mtMeshes3D;
    InModelName := ChangeModelTable(DirPoints3D, DirPoints3D, InModelName);
    PanelInputPath.Caption := ExtractFilePath(InModelName);
    PanelInputPath.Hint := PanelInputPath.Caption;
    UpdateInputNames(PanelInputPath.Caption + TableMask);
    ListBoxInputNames.ItemIndex := 0;
    RadioGroupMethod.Items[0] := _('Delaunay tetrahedralization');
    RadioGroupMethod.Items[1] := _('Restricted tetrahedralization');
    CheckBoxVoronoiDiagram.Caption := _('Voronoi polyhedrons');
    // fmMethodTriangulation.Caption := InModelName;
    // LabelMaterial.Caption := InModelName;
    PanelOutPath.Caption := ExpandPath(DirMesh3DMatrix);
    PanelOutPath.Hint    := PanelOutPath.Caption;
  end;
  OutModelName := PanelOutPath.Caption + EditOutName.Text;
  ListBoxInputNamesClick(Self);
  RadioGroupMethodClick(Self);
end;

procedure TfmMethodTriangulation.ListBoxInputNamesClick(Sender: TObject);
begin
  inherited;
  RadioGroupMethodClick(Self);
end;

procedure TfmMethodTriangulation.RadioGroupMethodClick(Sender: TObject);
var
  Index: integer;
begin
  if ListBoxInputNames.ItemIndex = -1 then
    Exit
  else
    case RadioGroupMethod.ItemIndex of
      0:
      begin //Delaunay 2D
        CheckBoxConvexHull.Enabled := True;
        CheckBoxVoronoiDiagram.Enabled := True;
        ComboBoxBreakLines.Enabled := False;
      end;
      1:
      begin //Breaklines
        CheckBoxConvexHull.Enabled     := False;
        CheckBoxVoronoiDiagram.Enabled := False;
        ComboBoxBreakLines.Enabled     := True;
        ComboBoxBreakLines.Items.Clear;
        FileListBoxBreaklines.Directory := ExpandPath(DirPolygonVertex);
        if FileListBoxBreaklines.Items.Count > 0 then
        begin
          ComboBoxBreakLines.Items.AddStrings(FileListBoxBreaklines.Items);
          ComboBoxBreakLines.ItemIndex := 0;
        end;
        Index := ComboBoxBreakLines.Items.IndexOf(ComboBoxBreakLines.Text);
        if Index > -1 then
          ComboBoxBreakLines.ItemIndex := Index;
      end;
    end;
end;

procedure TfmMethodTriangulation.Test;
{
var
TesTT: TTable;
i, nc: integer;
}
begin
{
  ReadNodes('D:\Geoblock\Data\Base\Files\my.nodes');
  nc := Length(GlobalNodes);
  TesTT := TTable.Create(nil);
  TesTT.TableName := 'D:\Geoblock\Data\Base\Files\Test.db';
  TesTT.Open;
  Testt.First;
  for I := 0 to nc - 1 do
  begin
    TesTT.Edit;
    TesTT.Fields[1].AsFloat:=GlobalNodes[i].x;
    TesTT.Fields[2].AsFloat:=GlobalNodes[i].y;
    TesTT.Fields[3].AsFloat:=GlobalNodes[i].z;
    TesTT.Fields[4].AsFloat:=GlobalNodes[i].data;
    TesTT.Post;
    testt.Next;
  end;
  TesTT.close;
  //  inherited;
  //ButtonHelp.Caption:=InModelName;
}
end;

procedure TfmMethodTriangulation.ListBoxRealAttributeClick(Sender: TObject);
begin
  inherited;
  //  LabelMaterial.Caption := PanelInputPath.Caption;
  ListBoxRealAttribute.ItemIndex := -1;
end;

//==============================================================\\

procedure TfmMethodTriangulation.Delaunay2DExecute;
var
  I, J:     integer;
  TIN: TTriangulator2D;
  FileNameTinTriangle: TFileName;
  FileNameTinVertex: TFileName;
  FileNameVorFacet: TFileName;
  FileNameVorVertex: TFileName;
  TableTinTriangle: TTable; //Triangulation
  TableTinVertex: TTable;
  TableVorFacet: TTable; //Voronoi diagram
  TableVorVertex: TTable;
  TableHullFacet: TTable; //Convex hull
  TableHullVertex: TTable;
  TableVor: TTable; //Convex hull
  FirstPointNum: integer;

  P1, P2, P3:  TAffineVector;     //Vertices of a current triangle
  Center:      TAffineVector;     //The Center of a current triangle
  V1, V2:      TAffineVector;
  AngleCosine: single;

  AreaXYZ: double;   //The area of a triangle
  Slope:   double;   //The Slope angle of a triangle and XY plane (0-90)
  Npe1, Npe2: NormalPlaneEq;
  ss:      double;
  NXArr:   array of single;
  NYArr:   array of single;
  NZArr:   array of single;
  NVArr:   array of single;
  VorFile: TFileName;

begin
  //  3D Triangulation
  if InModelType = mtPoints3D then
  begin
    dmBase.Table.TableName := InModelName;
    dmBase.Table.Open;
    with dmBase.Table do
    begin
      First;
      SetLength(NXArr, RecordCount);
      SetLength(NYArr, RecordCount);
      SetLength(NZArr, RecordCount);
      SetLength(NVArr, RecordCount);
      for i := 0 to RecordCount - 1 do
      begin
        NXArr[i] := Fields[1].AsFloat;
        NYArr[i] := Fields[2].AsFloat;
        NZArr[i] := Fields[3].AsFloat;
        NVArr[i] := Fields[4].AsFloat;
        Next;
      end;
    end;
    VorFile := OutModelName;
    CalcDelaunayVorFromDB(VorFile, NXArr, NYArr, NZArr, NVArr);
    dmBase.Table.Close;
    SetLength(NXArr, 0);
    SetLength(NYArr, 0);
    SetLength(NZArr, 0);
    SetLength(NVArr, 0);
    Close;
  end;
  //  2D Triangulation
  if (InModelType = mtPoints2D) then
  begin
    FileNameTinTriangle := SlashSep(ExpandPath(DirTinFaces), EditOutName.Text);
    FileNameTinVertex   := SlashSep(ExpandPath(DirTinVertices), EditOutName.Text);
    TIN                 := TTriangulator2D.Create;
    with dmBase do
      try
        TableTinTriangle := TTable.Create(nil);
        TableTinVertex   := TTable.Create(nil);
        TableTinTriangle.TableName := FileNameTinTriangle;
        TableTinVertex.TableName := FileNameTinVertex;

        CreateTinTables(FileNameTinTriangle);

        //Copies the input file of Points to the output file of Tin vertices
        CopyFiles(InModelName + TableExt, FileNameTinVertex + TableExt,
          mtPoints2D, False);

        TableTinVertex.Open;
        FirstPointNum := TableTinVertex.FieldByName(fldID).AsInteger;
        TIN.Npts := TableTinVertex.RecordCount;
        //Fill work arrays
        TableTinVertex.First;
        for I := 0 to TableTinVertex.RecordCount - 1 do
        begin
          TableTinVertex.Edit;
          TableTinVertex.FieldByName(fldID).AsInteger := I;
          TableTinVertex.Post;

          TIN.Px^[I] := TableTinVertex.FieldByName(fldX).AsFloat;
          TIN.Py^[I] := TableTinVertex.FieldByName(fldY).AsFloat;
          TIN.Pz^[I] := TableTinVertex.FieldByName(fldZ).AsFloat;
          TableTinVertex.Next;
        end;
        TableTinVertex.Close;
        TIN.Delaunay2D;     //Create Delaunay triangulation
        TableTinTriangle.Open;
        ProgressBar.Min      := 0;
        ProgressBar.Position := 0;
        ProgressBar.Max      := TIN.Ntri;
        for I := 0 to TIN.Ntri do
          begin
            ProgressBar.Position := I;
            TableTinTriangle.Append;
            TableTinTriangle.FieldByName(fldID).AsInteger := I + 1;

            //Calculate triangle centers
            P1.V[0] := TIN.Px^[TIN.vt1^[I]];
            P1.V[1] := TIN.Py^[TIN.vt1^[I]];
            P1.V[2] := TIN.Pz^[TIN.vt1^[I]];

            P2.V[0] := TIN.Px^[TIN.vt2^[I]];
            P2.V[1] := TIN.Py^[TIN.vt2^[I]];
            P2.V[2] := TIN.Pz^[TIN.vt2^[I]];

            p3.V[0] := TIN.px^[TIN.vt3^[I]];
            p3.V[1] := TIN.py^[TIN.vt3^[I]];
            p3.V[2] := TIN.pz^[TIN.vt3^[I]];

            Center.V[0] := (P1.V[0] + P2.V[0] + P3.V[0]) / 3;
            Center.V[1] := (P1.V[1] + P2.V[1] + P3.V[1]) / 3;
            Center.V[2] := (P1.V[2] + P2.V[2] + P3.V[2]) / 3;

            TableTinTriangle.FieldByName(fldX).AsFloat :=
              RoundTo(Center.V[0], Precision);
            TableTinTriangle.FieldByName(fldY).AsFloat :=
              RoundTo(Center.V[1], Precision);
            TableTinTriangle.FieldByName(fldZ).AsFloat :=
              RoundTo(Center.V[2], Precision);

            TableTinTriangle.FieldByName(fldV1).AsInteger := TIN.vt1^[I];
            TableTinTriangle.FieldByName(fldV2).AsInteger := TIN.vt2^[I];
            TableTinTriangle.FieldByName(fldV3).AsInteger := TIN.vt3^[I];
            TableTinTriangle.FieldByName(fldN1).AsInteger := TIN.nt1^[I];
            TableTinTriangle.FieldByName(fldN2).AsInteger := TIN.nt2^[I];
            TableTinTriangle.FieldByName(fldN3).AsInteger := TIN.nt3^[I];

            TableTinTriangle.FieldByName(fldMATERIAL).AsInteger :=
              SpinEditMaterialID.Value;
            //Area calculation
            AreaXYZ := TriangleArea(p1, p2, p3);  //the real area in 3D
            TableTinTriangle.FieldByName(fldAREA).AsFloat :=
              RoundTo(AreaXYZ, Precision);
            //Slope calculation
            V1      := CalcPlaneNormal(p1, p2, p3);
            V2.V[0]   := 10;
            V2.V[1]   := 0;
            V2.V[2]   := 0;
            AngleCosine := VectorAngleCosine(V1, V2);
            Slope   := Abs(RadToDeg(AngleCosine));
            TableTinTriangle.FieldByName(fldSLOPE).AsFloat :=
              RoundTo(Slope, Precision);
            TableTinTriangle.Post;
            Application.ProcessMessages;
          end;
        TableTinTriangle.Free;

        // Creates and writes Voronoi diagram in the table
        if CheckBoxVoronoiDiagram.Checked then
        begin
          TIN.Voronoi2D; // Create 2D Voronoi diagram
          //Records results to tables
          FileNameVorFacet  := ExpandPath(DirPolygonPoly) + EditOutName.Text + '_Vor';
          FileNameVorVertex := ExpandPath(DirPolygonVertex) + EditOutName.Text + '_Vor';
          TableVorFacet     := TTable.Create(Self);
          TableVorVertex    := TTable.Create(Self);
          TableVorFacet.TableName := FileNameVorFacet;
          TableVorVertex.TableName := FileNameVorVertex;
          try
            CreatePolygonTables(FileNameVorFacet);
            //The Tin vertices represent the Vor nodes

            CopyFiles(FileNameTinVertex + TableExt,
              TableVorFacet.TableName + TableExt, mtPoints2D, False);
            AddTableField(FileNameVorFacet, fldID_TYPE, ftInteger);
            AddTableField(FileNameVorFacet, fldMATERIAL, ftInteger);
            AddTableField(FileNameVorFacet, fldAREA, ftFloat);

            TableVorFacet.Open;
            TableVorVertex.Open;

            ProgressBar.Min      := 0;
            ProgressBar.Position := 0;
            ProgressBar.Max      := TableVorFacet.RecordCount - 1;
            for I := 0 to TableVorFacet.RecordCount - 1 do
            begin
              ProgressBar.Position := I;
              TableVorFacet.Edit;
              TableVorFacet.FieldByName(fldID_TYPE).AsInteger := integer(ptPolygon);
              TableVorFacet.Post;
              begin
                for J := 1 to High(TIN.VorDiagram[I]) do
                begin
                  TableVorVertex.Append;
                  TableVorVertex.FieldByName(fldID).AsInteger :=
                    TableVorVertex.RecordCount + 1;
                  TableVorVertex.FieldByName(fldX).AsFloat    := TIN.VorDiagram[I][J].V[0];
                  TableVorVertex.FieldByName(fldY).AsFloat    := TIN.VorDiagram[I][J].V[1];
                  TableVorVertex.FieldByName(fldZ).AsVariant  := TIN.VorDiagram[I][J].V[2];
                  TableVorVertex.FieldByName(fldID_POLY).AsInteger := I + 1;
                  TableVorVertex.FieldByName(fldID_NO).AsInteger := J;
                  TableVorVertex.Post;
                end;
                TableVorVertex.Append;
                TableVorVertex.FieldByName(fldID).AsInteger :=
                  TableVorVertex.RecordCount + 1;
                TableVorVertex.FieldByName(fldX).AsFloat    := TIN.VorDiagram[I][1].V[0];
                TableVorVertex.FieldByName(fldY).AsFloat    := TIN.VorDiagram[I][1].V[1];
                TableVorVertex.FieldByName(fldZ).AsFloat    := TIN.VorDiagram[I][1].V[2];
                TableVorVertex.FieldByName(fldID_POLY).AsInteger := I;
                TableVorVertex.FieldByName(fldID_NO).AsInteger := Length(TIN.VorDiagram[I]);
                TableVorVertex.Post;
              end;
              TableVorFacet.Next;
            end;
          finally
            TableVorFacet.Close;
            TableVorVertex.Close;

            TableVorFacet.Free;
            TableVorVertex.Free;
          end;
        end;

        //Writes convex hull in table
        if CheckBoxConvexHull.Checked then
        begin
          TableHullFacet  := TTable.Create(nil);
          TableHullVertex := TTable.Create(nil);
          try
            TableHullFacet.TableName  :=
              ExpandPath(DirPolygonPoly) + EditOutName.Text + '_Hull';
            TableHullVertex.TableName :=
              ExpandPath(DirPolygonVertex) + EditOutName.Text + '_Hull';

            CreatePolygonTables(TableHullFacet.TableName);

            TableHullFacet.Open;
            TableHullVertex.Open;
            begin
              TableHullFacet.Edit;
              TableHullFacet.FieldByName(fldID).AsInteger := 1; //Only one hull!
              TableHullFacet.FieldByName(fldX).AsFloat    := (TIN.Xmin + TIN.Xmax) / 2;
              TableHullFacet.FieldByName(fldY).AsFloat    := (TIN.Ymin + TIN.Ymax) / 2;
              TableHullFacet.FieldByName(fldZ).AsFloat    := (TIN.Zmin + TIN.Zmax) / 2;
              TableHullFacet.FieldByName(fldID_TYPE).AsInteger :=
                integer(ptPolygon);
              TableHullFacet.FieldByName(fldMATERIAL).AsInteger :=
                SpinEditMaterialID.Value;
              TableHullFacet.Post;

              for J := 0 to High(TIN.ConvexHull) do
              begin
                TableHullVertex.Append;
                TableHullVertex.FieldByName(fldID).AsInteger := J + 1;
                TableHullVertex.FieldByName(fldX).AsFloat    := TIN.Px^[TIN.ConvexHull[J]];
                TableHullVertex.FieldByName(fldY).AsFloat    := TIN.Py^[TIN.ConvexHull[J]];
                TableHullVertex.FieldByName(fldZ).AsFloat    := TIN.Pz^[TIN.ConvexHull[J]];
                TableHullVertex.FieldByName(fldID_POLY).AsInteger := 1;
                //Only one hull!
                TableHullVertex.FieldByName(fldID_NO).AsInteger := J + 1;
                TableHullVertex.Post;
              end;
            end;
          finally
            TableHullFacet.Free;
            TableHullVertex.Free;
          end;
        end;
      finally
        TableTinVertex.Free;
      end;
    TIN.Free;
  end;
end;

{===================================================
 Triangulation with breaklines.
====================================================}
procedure TfmMethodTriangulation.ConstrainedTINExecute;
var
  FileNameBreakLines:  TFileName;
  FileNameTinTriangle: TFileName;
  FileNameTinVertex:   TFileName;

  TIN: TTriangulator2D;
  TableTinVertex: TTable;
  TableTinTriangle: TTable;
  TableBreakLines: TTable;
  I, J, K:      integer;
  PREV_ID_NO, ID_NO: integer;
  BreakSegment: TSegment3D;
  FirstPointNum: integer;

  p1, p2, p3: TAffineVector; //vertices of a current triangle
  Center:     TAffineVector; //the center of a current triangle

  V1, V2:      TAffineVector;
  AngleCosine: single;

  AreaXYZ:    double; //The area of a triangle
  Slope:      double; //The Slope angle of a triangle and XY plane (0-90)
  Npe1, Npe2: NormalPlaneEq;

begin
  FileNameBreakLines  := ExpandPath(DirPolygonVertex) + ComboBoxBreakLines.Text;
  FileNameTinTriangle := SlashSep(ExpandPath(DirTinFaces), EditOutName.Text);
  FileNameTinVertex   := SlashSep(ExpandPath(DirTinVertices), EditOutName.Text);

  TIN := TTriangulator2D.Create;
  with dmBase do
    try
      TableTinVertex := TTable.Create(nil);
      try
        CreateTinTables(FileNameTinTriangle);
        CopyFiles(PChar(InModelName + TableExt),
          PChar(FileNameTinVertex + TableExt), mtPoints2D, False);
        //Get the output filename of tin vertices from the tin faces
        TableTinVertex.TableName := FileNameTinVertex;
        TableTinVertex.Open;
        FirstPointNum := TableTinVertex.FieldByName(fldID).AsInteger;
        TIN.Npts := TableTinVertex.RecordCount;
        for I := 0 to TableTinVertex.RecordCount - 1 do
        begin
          TableTinVertex.RecNo  := I + 1;
          TIN.px^[I] := TableTinVertex.FieldByName(fldX).AsFloat;
          TIN.py^[I] := TableTinVertex.FieldByName(fldY).AsFloat;
          TIN.pz^[I] := TableTinVertex.FieldByName(fldZ).AsFloat;
        end;
        TableTinVertex.Close;
      finally
        TableTinVertex.Free;
      end;
      TIN.Delaunay2D; //first - delaunay triangulation!
      TIN.SortNtCW;
      TableBreakLines := TTable.Create(nil);
      try
        TableBreakLines.TableName := FileNameBreakLines;
        TableBreakLines.Open;
        TableBreakLines.First;
        ID_NO := TableBreakLines.FieldByName(fldID_POLY).AsInteger;
        BreakSegment.EndPoint.V[0] := TableBreakLines.FieldByName(fldX).AsFloat;
        BreakSegment.EndPoint.V[1] := TableBreakLines.FieldByName(fldY).AsFloat;
        BreakSegment.EndPoint.V[2] := TableBreakLines.FieldByName(fldZ).AsFloat;

        K := 0;
        ProgressBar.Min := 0;
        ProgressBar.Position := 0;
        ProgressBar.Max := TableBreakLines.RecordCount + TIN.Ntri;
        repeat
          Inc(K);
          ProgressBar.Position := K;
          TableBreakLines.Next;
          PREV_ID_NO := ID_NO;
          Id_No      := TableBreakLines.FieldByName(fldID_POLY).AsInteger;
          BreakSegment.StartPoint := BreakSegment.EndPoint;
          BreakSegment.EndPoint.V[0] := TableBreakLines.FieldByName(fldX).AsFloat;
          BreakSegment.EndPoint.V[1] := TableBreakLines.FieldByName(fldY).AsFloat;
          BreakSegment.EndPoint.V[2] := TableBreakLines.FieldByName(fldZ).AsFloat;
          if ID_NO = Prev_ID_NO then
            TIN.AddBreakSegment(BreakSegment);
        until TableBreakLines.EOF;
        TableBreakLines.Close;
      finally
        TableBreakLines.Free;
      end;

      TableTinTriangle := TTable.Create(Application);
      try
        TableTinTriangle.TableName := FileNameTinTriangle;

        TableTinTriangle.Open;
        for I := 0 to TIN.Ntri do
          begin
            ProgressBar.Position := K + I;
            TableTinTriangle.Append;
            TableTinTriangle.FieldByName(fldID).AsInteger := I + 1;

            //Calculation of the triangle center
            p1.V[0] := TIN.px^[TIN.vt1^[I]];
            p1.V[1] := TIN.py^[TIN.vt1^[I]];
            p1.V[2] := TIN.pz^[TIN.vt1^[I]];

            p2.V[0] := TIN.px^[TIN.vt2^[I]];
            p2.V[1] := TIN.py^[TIN.vt2^[I]];
            p2.V[2] := TIN.pz^[TIN.vt2^[I]];

            p3.V[0] := TIN.px^[TIN.vt3^[I]];
            p3.V[1] := TIN.py^[TIN.vt3^[I]];
            p3.V[2] := TIN.pz^[TIN.vt3^[I]];

            Center.V[0] := (p1.V[0] + p2.V[0] + p3.V[0]) / 3;
            Center.V[1] := (p1.V[1] + p2.V[1] + p3.V[1]) / 3;
            Center.V[2] := (p1.V[2] + p2.V[2] + p3.V[2]) / 3;

            TableTinTriangle.FieldByName(fldX).AsFloat    :=
              RoundTo(Center.V[0], Precision);
            TableTinTriangle.FieldByName(fldY).AsFloat    :=
              RoundTo(Center.V[1], Precision);
            TableTinTriangle.FieldByName(fldZ).AsFloat    :=
              RoundTo(Center.V[2], Precision);
            TableTinTriangle.FieldByName(fldV1).AsInteger := TIN.vt1^[I] + FirstPointNum;
            TableTinTriangle.FieldByName(fldV2).AsInteger := TIN.vt2^[I] + FirstPointNum;
            TableTinTriangle.FieldByName(fldV3).AsInteger := TIN.vt3^[I] + FirstPointNum;
            TableTinTriangle.FieldByName(fldN1).AsInteger := TIN.nt1^[I] {+FirstPointNum};
            TableTinTriangle.FieldByName(fldN2).AsInteger := TIN.nt2^[I] {+FirstPointNum};
            TableTinTriangle.FieldByName(fldN3).AsInteger := TIN.nt3^[I] {+FirstPointNum};

            TableTinTriangle.FieldByName(fldMATERIAL).AsInteger :=
              SpinEditMaterialID.Value;

            //Area calculation
            AreaXYZ := TriangleArea(p1, p2, p3);
            TableTinTriangle.FieldByName(fldAREA).AsFloat := Round(AreaXYZ);

            //Calculation of a triangle slope angle
            V1    := CalcPlaneNormal(p1, p2, p3);
            V2.V[0] := 10;
            V2.V[1] := 0;
            V2.V[2] := 0;
            AngleCosine := VectorAngleCosine(V1, V2);
            Slope := Abs(RadToDeg(AngleCosine));
            TableTinTriangle.FieldByName(fldSLOPE).AsFloat :=
              RoundTo(Slope, Precision);

            TableTinTriangle.Post;
            Application.ProcessMessages;
          end;
        TableTinTriangle.Close;
      finally
        TableTinTriangle.Free;
      end;
    finally
      TIN.Free;
    end;
end;

procedure TfmMethodTriangulation.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      SpinEditMaterialID.Value   := ReadInteger(Name, SpinEditMaterialID.Name, 0);
      CheckBoxVoronoiDiagram.Checked :=
        ReadBool(Name, CheckBoxVoronoiDiagram.Name, False);
      CheckBoxConvexHull.Checked := ReadBool(Name, CheckBoxConvexHull.Name, False);
      RadioGroupMethod.ItemIndex   := ReadInteger(Name, RadioGroupMethod.Name, 0);
    finally
      IniFile.Free;
    end;
end;

procedure TfmMethodTriangulation.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteInteger(Name, SpinEditMaterialID.Name, SpinEditMaterialID.Value);
      WriteBool(Name, CheckBoxVoronoiDiagram.Name, CheckBoxVoronoiDiagram.Checked);
      WriteBool(Name, CheckBoxConvexHull.Name, CheckBoxConvexHull.Checked);
      WriteInteger(Name, RadioGroupMethod.Name, RadioGroupMethod.ItemIndex);
    finally
      IniFile.Free;
    end;
end;


procedure TfmMethodTriangulation.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;

procedure TfmMethodTriangulation.ButtonOKClick(Sender: TObject);
begin
  //inherited;
  if ModalResult = mrOk then
  begin
    dmBase.TableOutput.TableName := OutModelName;
    OutModelType := mtTins;
    case RadioGroupMethod.ItemIndex of
      0: Delaunay2DExecute;
      1: ConstrainedTINExecute;
    end;
  end;
end;


end.
