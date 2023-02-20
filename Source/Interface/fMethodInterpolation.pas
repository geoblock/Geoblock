//----------------------------------------------------------------------------
// The unit of Geoblock, http://sourceforge.net/projects/geoblock
//---------------------------------------------------------------------------

unit fMethodInterpolation;

(* Interpolation dialog with gridding method calls *)

interface

uses
  System.SysUtils,
  System.Classes,
  System.IniFiles,
  System.Variants,
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
  Vcl.ActnList,
  Vcl.Menus,
  Vcl.CheckLst,

  //DB
  Data.DB,
  Bde.DBTables,
  GnuGetText,

  dBase,
  dDialogs,
  fInitialDialog,
  fMethodDualDialog,
  cInterpol,

  OpenCL.Import,
  OpenCL.Platform,
  OpenCL.GL;

type
  Tcl_platform_device = record
    platform_id: Tcl_platform_id;  // index for platform_id
    device_type: Tcl_device_type;  // CL_DEVICE_TYPE_CPU; CL_DEVICE_TYPE_GPU; CL_DEVICE_TYPE_ACCELERATOR
  end;

type
  TfmMethodInterpolation = class(TfmMethodDualDialog)
    StaticTextArrow: TStaticText;
    RadioGroupMethod: TRadioGroup;
    GroupBoxDimension: TGroupBox;
    LabelDX: TLabel;
    StaticTextDX: TStaticText;
    LabelDY: TLabel;
    StaticTextDY: TStaticText;
    LabelDZ: TLabel;
    StaticTextDZ: TStaticText;
    LabelNX: TLabel;
    StaticTextNX: TStaticText;
    LabelNY: TLabel;
    StaticTextNY: TStaticText;
    LabelNZ: TLabel;
    StaticTextNZ: TStaticText;
    LabelXO: TLabel;
    StaticTextXO: TStaticText;
    LabelYO: TLabel;
    StaticTextYO: TStaticText;
    LabelZO: TLabel;
    StaticTextZO: TStaticText;
    GroupBoxParameters: TGroupBox;
    LabelExtraValue: TLabel;
    EditExtraValue: TEdit;
    GroupBoxAnisotropy: TGroupBox;
    LabelRatio: TLabel;
    LabelAngle: TLabel;
    LabelZMagnification: TLabel;
    ButtonOptions: TButton;
    EditRatio: TEdit;
    EditAngle: TEdit;
    EditVertAnisotropy: TEdit;
    CheckBoxVariance: TCheckBox;
    BtnReadOpenCLCouples: TBitBtn;
    ListCouple: TListBox;
    CheckBoxOpenCL: TCheckBox;
    procedure ButtonOptionsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RadioGroupMethodClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ToolButtonInputBClick(Sender: TObject);
    procedure ListBoxInputNamesBClick(Sender: TObject);
    procedure ListBoxInputNamesClick(Sender: TObject);
    procedure ToolButtonInputClick(Sender: TObject);
  private
    //  Fields:TFields;           //WorkFields from Scatter Point Table
    errcode_ret: Tcl_int; // error code returned from api calls
    platform_devices: array of Tcl_platform_device;
  	num_devices_returned: Tcl_uint;
  	device_ids: array of Tcl_device_id; // compute device IDs
  	context: Tcl_context; // compute context
    KrigingParamsSet: boolean;
    procedure Interpolate(TablePoints, TableNodes:TTable);
    procedure UpdateAttributeListB;
    procedure ReadGridParameters;
    procedure ReadIniFile;
    procedure WriteIniFile;
  end;

var
  fmMethodInterpolation: TfmMethodInterpolation;


//==========================================================================
implementation
//==========================================================================

uses
  cGlobals,
  uCommon,
  cProfuns,
  fInterInverseDistance,
  fInterKriging,
  fInterNaturalNeighbours,
  fInterPolynomRegression,
  uInverseDistance,
  uLinearByTin,
  uNaturalNeighbors,
  uKriging,
  uClosestPointInt,
  uPolynomialRegression;

{$R *.DFM}

const
  // Interpolation methods - The ItemIndexes for ComboBoxMethods.Items
  imInverseDistances = 0;
  imClosestPoint = 1;
  imLinearByTin = 2;
  imPolynomialRegression = 3;
  imKriging = 4;
  imNaturalNeighbors = 5;
  c_device_types:array[1..3] of cardinal=(CL_DEVICE_TYPE_CPU, CL_DEVICE_TYPE_GPU, CL_DEVICE_TYPE_ACCELERATOR);

//-----------------------------------------------------------------------------

procedure TfmMethodInterpolation.ReadGridParameters;
var
 XO, YO, ZO, DX, DY, DZ: Double;
 Nx, Ny, Nz: integer;
begin
  ReadParFile(InModelName, XO, YO, ZO, DX, DY, DZ, NX, NY, NZ);
  StaticTextXO.Caption := FloatToStr(XO);
  StaticTextYO.Caption := FloatToStr(YO);
  StaticTextZO.Caption := FloatToStr(ZO);
  StaticTextDX.Caption := FloatToStr(DX);
  StaticTextDY.Caption := FloatToStr(DY);
  StaticTextDZ.Caption := FloatToStr(DZ);
  StaticTextNX.Caption := IntToStr(NX);
  StaticTextNY.Caption := IntToStr(NY);
  StaticTextNZ.Caption := IntToStr(NZ);
end;

//-----------------------------------------------------------------------------

procedure TfmMethodInterpolation.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
  ReadGridParameters;
  KrigingParamsSet := false;
end;

//-----------------------------------------------------------------------------

procedure TfmMethodInterpolation.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini') );
  with IniFile do
  try
    RadioGroupMethod.ItemIndex := ReadInteger(Name, RadioGroupMethod.Name, 0);
    CheckBoxVariance.Checked := ReadBool(Name, CheckBoxVariance.Name, True);
    EditExtraValue.Text := ReadString(Name, EditExtraValue.Name, '0');
    EditRatio.Text := ReadString(Name, EditRatio.Name, '1');
    EditAngle.Text := ReadString(Name, EditAngle.Name, '0');
    EditVertAnisotropy.Text := ReadString(Name, EditVertAnisotropy.Name, '1');
  finally
    IniFile.Free;
  end;
end;

//-----------------------------------------------------------------------------

procedure TfmMethodInterpolation.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini') );
  with IniFile do
  try
    WriteInteger(Name, RadioGroupMethod.Name, RadioGroupMethod.ItemIndex);
    WriteBool(Name, CheckBoxVariance.Name, CheckBoxVariance.Checked);
    WriteString(Name, EditExtraValue.Name, EditExtraValue.Text);
    WriteString(Name, EditRatio.Name, EditRatio.Text);
    WriteString(Name, EditAngle.Name, EditAngle.Text);
    WriteString(Name, EditVertAnisotropy.Name, EditvertAnisotropy.Text);
  finally
    IniFile.Free;
  end;
end;

//-----------------------------------------------------------------------------

procedure TfmMethodInterpolation.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;

//-----------------------------------------------------------------------------

procedure TfmMethodInterpolation.ToolButtonInputBClick(Sender: TObject);
begin
  inherited;
  case ToolBarInputB.Tag of
  1: begin  // Points 2D
       ToolButtonGrids2D.Down := True;
       ToolButtonGrids2D.Click;
       ToolButtonGrids2D.Visible := True;
       ToolButtonGrids3D.Visible := False;
       ToolButtonMeshes2D.Visible := True;
       ToolButtonMeshes3D.Visible := False;
     end;
  2: begin  // Points 3D
       ToolButtonGrids3D.Down := True;
       ToolButtonGrids3D.Click;
       ToolButtonGrids2D.Visible := False;
       ToolButtonGrids3D.Visible := True;
       ToolButtonMeshes2D.Visible := False;
       ToolButtonMeshes3D.Visible := True;
     end;
  end;
  Refresh;

  ListBoxInputNamesClick(Self);
end;

//-----------------------------------------------------------------------------

procedure TfmMethodInterpolation.ListBoxInputNamesBClick(Sender: TObject);
begin
  inherited;
  UpdateAttributeListB;
end;

//-----------------------------------------------------------------------------

procedure TfmMethodInterpolation.UpdateAttributeListB;
begin
  if ListBoxRealAttributeB.Items.Count > 0 then
  begin
     ListBoxRealAttributeB.ItemIndex := 0;
     ListBoxRealAttributeB.Selected[0] := True;
  end
  else
     ListBoxRealAttributeB.ItemIndex := -1;
end;

//-----------------------------------------------------------------------------

procedure TfmMethodInterpolation.RadioGroupMethodClick(Sender: TObject);
begin
{
  ToolButtonPoints2DB.Enabled := True;
  ToolButtonPoints3DB.Enabled := True;
  ListBoxRealAttributeB.Enabled := True;
  EditExtraValue.Enabled := True;
  LabelExtraValue.Enabled := True;
  CheckBoxVariance.Enabled := false;
  ButtonOptions.Enabled := true;
  case RadioGroupMethod.ItemIndex of
    imInverseDistances:  fmInterInverseDistance.InitParams;
    imLinearByTin:  ButtonOptions.Enabled := False;
    imClosestPoint:
    begin
      ButtonOptions.Enabled := False;
      EditExtraValue.Enabled := False;
      LabelExtraValue.Enabled := False;
    end;
    imKriging: CheckBoxVariance.Enabled := True;
  end;
}
end;

//-----------------------------------------------------------------------------

procedure TfmMethodInterpolation.ButtonOptionsClick(Sender: TObject);
begin
  case RadioGroupMethod.ItemIndex of
    imInverseDistances:
      begin
        with TfmInterInverseDistance.Create(Self) do
        try
          ShowModal;
        finally
          Free;
        end;
      end;
    imClosestPoint: ; // no options
    imLinearByTin:  ; // no options
    imPolynomialRegression:
      begin
        with TfmInterPolynomialRegression.Create(Self) do
        try
          ShowModal;
        finally
          Free;
        end;
      end;
    imKriging:
      begin
        with TfmInterKriging.Create(Self) do
        try
          if ShowModal=mrOk then KrigingParamsSet := True;
        finally
          Free;
        end;
      end;
    imNaturalNeighbors:
      begin
        with TfmInterNaturalNeighbours.Create(Self) do
        try
          ShowModal;
        finally
          Free;
        end;
      end;
  end;
end;

//-----------------------------------------------------------------------------

procedure TfmMethodInterpolation.Interpolate(TablePoints, TableNodes:TTable);

var
  OldCursor: TCursor;
  // Vars for Points and Nodes
  PointsArr, NodesArr: TCoordinateArray;
  i, j: Integer;
  // vars for triangulatuion for Linear by Tin interpolation
  VertexTableName, TriangleTableName: string;
  TrianglesArr: Variant;
  TableTinVertex, TableTinTriangle: TTable;
  //opencl
  OpenCLProgram: Tcl_program;
  OpenCLVectorAdd: Tcl_kernel;
  CommandQueue: Tcl_command_queue;
  returned_size: TSize_t;
  // general
  ExtraValue: double;
  FieldName: string;
  Field_included: boolean;

begin
  ExtraValue := StrToFloat(EditExtraValue.Text);

  OldCursor := Screen.Cursor;
  ActiveAttributeB := ListBoxRealAttributeB.Items[ListBoxRealAttributeB.ItemIndex];
  TablePoints.Open;
  TableNodes.Open;

  ProgressBar.Min   := 1;
  ProgressBar.Max := TableNodes.RecordCount;
  ProgressBar.Position := ProgressBar.Min;
  ProgressBar.Step  := 1;

{
  ProgressBar.Min := Low(Nodes);
  ProgressBar.Max := High(Nodes) + 1;
  ProgressBar.Position := ProgressBar.Min;
  ProgressBar.Step := 1;
}

  try
    Screen.Cursor := crHourGlass;
    {// Get compute devices from platform
	errcode_ret:=clGetDeviceIDs(platform_devices[ListCouple.ItemIndex].platform_id, platform_devices[ListCouple.ItemIndex].device_type, 0, nil, @num_devices_returned);
  SetLength(device_ids, num_devices_returned);
  errcode_ret:=clGetDeviceIDs(platform_devices[ListCouple.ItemIndex].platform_id, platform_devices[ListCouple.ItemIndex].device_type, num_devices_returned, @device_ids[0], @num_devices_returned);
   // Create a compute context
	context:=clCreateContext(nil, num_devices_returned, @device_ids[0], nil, nil, @errcode_ret);
  // End (Init Context)
  }
    case RadioGroupMethod.ItemIndex of
      //_________________Inverse Distance Interpolation________________
      imInverseDistances:
      begin
        // Read TableNodes
        SetLength(NodesArr, TableNodes.RecordCount);
        TableNodes.Open;
        TableNodes.First;
        for I := 0 to TableNodes.RecordCount - 1 do
        begin
          ProgressBar.Position := I;

          NodesArr[i].X := TableNodes.FieldByName(fldX).AsFloat;
          NodesArr[i].Y := TableNodes.FieldByName(fldY).AsFloat;
          if Mode3d then
            NodesArr[i].Z := TableNodes.FieldByName(fldZ).AsFloat
          else
            NodesArr[i].Z := 0;
          TableNodes.Next;
         end;
        TableNodes.Close;

        // Read TablePoints
        SetLength(PointsArr, TablePoints.RecordCount);
        TablePoints.Open;
        TablePoints.First;
        j := 0;
        for I := 0 to TablePoints.RecordCount - 1 do
        begin
          if not TablePoints.FieldByName(ActiveAttributeB).IsNull then
          begin
            PointsArr[j].X := TablePoints.FieldByName(fldX).AsFloat;
            PointsArr[j].Y := TablePoints.FieldByName(fldY).AsFloat;
            if Mode3d then
              PointsArr[j].Z := TablePoints.FieldByName(fldZ).AsFloat
            else
              PointsArr[j].Z := 0;
            PointsArr[j].Value := TablePoints.FieldByName(ActiveAttributeB).AsFloat;
            Inc(j);
          end;
          TablePoints.Next;
        end;
        SetLength(PointsArr, j);
        TablePoints.Close;

        InverseDistanceInterpolation(PointsArr, NodesArr, InvDistPars);

        // Write to TableNodes
        AddTableField(TableNodes.TableName, ActiveAttributeB + '_IDW', ftFloat);
        TableNodes.Open;
        TableNodes.First;
        for i := 0 to TableNodes.RecordCount - 1 do
        begin
          TableNodes.Edit;
          TableNodes.FieldByName(ActiveAttributeB + '_IDW').Value :=
            RoundTo(NodesArr[i].Value, Precision);
          TableNodes.Post;
          TableNodes.Next;

          ProgressBar.StepIt;

        end;
        TableNodes.Close;

        // releasing the memory
        SetLength(PointsArr, 0);
        SetLength(NodesArr, 0);
      end;   // end of imInverseDistances case

      //_________________Closest Point Gridding________________
      imClosestPoint:
      begin
        // Read TableNodes
        SetLength(NodesArr, TableNodes.RecordCount);
        TableNodes.Open;
        TableNodes.First;
        for i := 0 to TableNodes.RecordCount - 1 do
        begin
          NodesArr[i].X := TableNodes.FieldByName(fldX).AsFloat;
          NodesArr[i].Y := TableNodes.FieldByName(fldY).AsFloat;
          if Mode3d then
            NodesArr[i].Z := TableNodes.FieldByName(fldZ).AsFloat
          else
            NodesArr[i].Z := 0;
          TableNodes.Next;
        end;
        TableNodes.Close;

        // Read TablePoints
        SetLength(PointsArr, TablePoints.RecordCount);
        TablePoints.Open;
        TablePoints.First;
        j := 0;
        for I := 0 to TablePoints.RecordCount - 1 do
        begin
          if not TablePoints.FieldByName(ActiveAttributeB).IsNull then
          begin
            PointsArr[j].ID := TablePoints.FieldByName(fldID).AsInteger;
            PointsArr[j].X   := TablePoints.FieldByName(fldX).AsFloat;
            PointsArr[j].Y   := TablePoints.FieldByName(fldY).AsFloat;
            if Mode3d then
              PointsArr[j].Z := TablePoints.FieldByName(fldZ).AsFloat
            else
              PointsArr[j].Z := 0;
            PointsArr[j].Value := TablePoints.FieldByName(ActiveAttributeB).AsFloat;
            Inc(j);
          end;
          TablePoints.Next;
        end;
        SetLength(PointsArr, j);
        TablePoints.Close;

        ClosestPointGridding(PointsArr, NodesArr, ProgressBar);

        // Write to TableNodes
        AddTableField(TableNodes.TableName, ActiveAttributeB + '_CPI', ftFloat);
        begin
          TableNodes.Open;
          TableNodes.First;
          for i := 0 to TableNodes.RecordCount - 1 do
          begin
            TableNodes.Edit;
            TableNodes.FieldByName(ActiveAttributeB + '_CPI').Value :=
              RoundTo(NodesArr[i].Value, Precision);
            TableNodes.Post;
            TableNodes.Next;
          end;
          TableNodes.Close;
        end;
        // Releasing the memory
        SetLength(PointsArr, 0);
        SetLength(NodesArr, 0);
      end; // of imClosestPoint case

      //_________Linear interpolation by tin_____________
      imLinearByTin:
      begin
        VertexTableName   := ChangeModelTable(DirPoints2D, DirTinVertices, TablePoints.TableName);
        TriangleTableName :=
          ChangeModelTable(DirPoints2D, DirTinFaces, TablePoints.TableName);
        SetLength(NodesArr, TableNodes.RecordCount);
        TableNodes.Open;
        TableNodes.First;
        for i := 0 to TableNodes.RecordCount - 1 do
        begin
          NodesArr[i].X := TableNodes.FieldByName(fldX).AsFloat;
          NodesArr[i].Y := TableNodes.FieldByName(fldY).AsFloat;
          if Mode3D then
            NodesArr[i].Z := TableNodes.FieldByName(fldZ).AsFloat
          else
            NodesArr[i].Z := 0;
          TableNodes.Next;
        end;
        TableNodes.Close;

        if not FileExists(VertexTableName + TableExt) or not
          FileExists(TriangleTableName + TableExt) then
        begin
          // Read TablePoints
          SetLength(PointsArr, TablePoints.RecordCount);
          TablePoints.Open;
          TablePoints.First;
          for i := 0 to TablePoints.RecordCount - 1 do
          begin
            PointsArr[i].X     := TablePoints.FieldByName(fldX).AsFloat;
            PointsArr[i].Y     := TablePoints.FieldByName(fldY).AsFloat;
            PointsArr[i].Value := TablePoints.FieldByName(ActiveAttributeB).AsFloat;
            TablePoints.Next;
          end;
          TablePoints.Close;

          Linear2DInterpolation(PointsArr, NodesArr, ExtraValue, ProgressBar);
        end  // if

        else
        begin
          TableTinVertex := TTable.Create(self);
          TableTinTriangle := TTable.Create(self);
          TableTinVertex.TableName := VertexTableName;
          TabletinTriangle.TableName := TriangleTableName;
          with TableTinVertex do
          begin
            Open;
            SetLength(PointsArr, RecordCount);
            First;
            for i := 0 to RecordCount - 1 do
            begin
                PointsArr[i].X := FieldByName(fldX).AsFloat;
                PointsArr[i].Y := FieldByName(fldY).AsFloat;
                PointsArr[i].Value := FieldByName(ActiveAttributeB).AsFloat;
                Next;
            end;
            Close;
          end; // of with

          with TableTinTriangle do
          begin
            // Triangles array line format: [V1,V2,V3,N1,N2,N3]
            Open;
            TrianglesArr := VarArrayCreate([1, RecordCount, 1, 6], varInteger);
            First;
            for I := 1 to RecordCount do
            begin
              TrianglesArr[i, 1] := FieldByName(fldV1).AsInteger;
              TrianglesArr[i, 2] := FieldByName(fldV2).AsInteger;
              TrianglesArr[i, 3] := FieldByName(fldV3).AsInteger;
              TrianglesArr[i, 4] := FieldByName(fldN1).AsInteger;
              TrianglesArr[i, 5] := FieldByName(fldN2).AsInteger;
              TrianglesArr[i, 6] := FieldByName(fldN3).AsInteger;
              Next;
            end;
            Close;
          end;
          TableTinVertex.Free;
          TableTinTriangle.Free;
          Linear2DInterpolation(PointsArr, NodesArr, TrianglesArr, ExtraValue, ProgressBar);
        end; // if

        AddTableField(TableNodes.TableName, ActiveAttributeB + '_TIN', ftFloat);

        //Write TableNodes
        TableNodes.Open;
        TableNodes.First;
        for i := 0 to TableNodes.RecordCount - 1 do
        begin
          TableNodes.Edit;
          TableNodes.FieldByName(ActiveAttributeB + '_TIN').Value :=
            RoundTo(NodesArr[i].Value, Precision);
          TableNodes.Post;
          TableNodes.Next;
        end;
        TableNodes.Close;

        SetLength(PointsArr, 0);
        SetLength(NodesArr, 0);
      end; // of imLinearByTin case

      //_________PolynomialRegression_________
      imPolynomialRegression:
      begin
        //Loading Points from DB
        SetLength(PointsArr, TablePoints.RecordCount);
        SetLength(NodesArr, TableNodes.RecordCount);
        if Mode3D then
        begin  // 3D mode
          // Read TablePoints
          SetLength(PointsArr, TablePoints.RecordCount);
          TablePoints.Open;
          TablePoints.First;
          for i := 0 to TablePoints.RecordCount - 1 do
          begin
            PointsArr[I].X := TablePoints.FieldValues[fldX];
            PointsArr[I].Y := TablePoints.FieldValues[fldY];
            PointsArr[I].Z := TablePoints.FieldValues[fldZ];
            if (TablePoints.FieldByName(ActiveAttributeB).AsString <> '') then
              PointsArr[I].Value := TablePoints.FieldValues[ActiveAttributeB]
            else
              PointsArr[I].Value := 0.0;
            TablePoints.Next;
          end;

          // Read TableNodes
          SetLength(NodesArr, TableNodes.RecordCount);
          TableNodes.Open;
          TableNodes.First;
          for i := 0 to TableNodes.RecordCount - 1 do
          begin
            NodesArr[I].X := TableNodes.FieldValues[fldX];
            NodesArr[I].Y := TableNodes.FieldValues[fldY];
            NodesArr[I].Z := TableNodes.FieldValues[fldZ];
            TableNodes.Next;
          end;
          TableNodes.Close;
        end
        else // 2D mode
        begin
          // Read TablePoints
          SetLength(PointsArr, TablePoints.RecordCount);
          TablePoints.Open;
          TablePoints.First;
          for I := 0 to TablePoints.RecordCount - 1 do
          begin
            PointsArr[I].X     := TablePoints.FieldValues[fldX];
            PointsArr[I].Y     := TablePoints.FieldValues[fldY];
            PointsArr[I].Value := TablePoints.FieldValues[ActiveAttributeB];
            TablePoints.Next;
          end;
          TablePoints.Close;

          // Read TableNodes
          SetLength(NodesArr, TableNodes.RecordCount);
          TableNodes.Open;
          TableNodes.First;
          for i := 0 to TableNodes.RecordCount - 1 do
          begin
            NodesArr[I].X := TableNodes.FieldValues[fldX];
            NodesArr[I].Y := TableNodes.FieldValues[fldY];
            TableNodes.Next;
          end;
          TableNodes.Close;
        end;

        // Invoke Trend analysis function
        NodesArr := PolyRegressInterpolation(PointsArr, NodesArr, PolyRegressOrder);

        //Write TableNodes
        AddTableField(TableNodes.TableName, ActiveAttributeB + '_PRI', ftFloat);
        TableNodes.Open;
        TableNodes.First;
        for I := 0 to TableNodes.RecordCount - 1 do
        begin
          try
            TableNodes.Edit;
            //write extrapolation value
            TableNodes.FieldByName(ActiveAttributeB + '_PRI').AsFloat :=
              RoundTo(NodesArr[I].Value, Precision);
          finally
            TableNodes.Post;
          end;
          TableNodes.Next;
        end;
        TableNodes.Close;
      end;

      //_________Kriging interpolation_________
      imKriging:
      begin
        // Read TablePoints
        SetLength(PointsArr, TablePoints.RecordCount);
        TablePoints.Open;
        TablePoints.First;
        j := 0;
        for i := 0 to TablePoints.RecordCount - 1 do
        begin
          if not TablePoints.FieldByName(ActiveAttributeB).IsNull then
          begin
            PointsArr[j].X := TablePoints.FieldByName(fldX).AsFloat;
            PointsArr[j].Y := TablePoints.FieldByName(fldY).AsFloat;
            if Mode3d then
              PointsArr[j].Z := TablePoints.FieldByName(fldZ).AsFloat
            else
              PointsArr[j].Z := 0;
            PointsArr[j].Value := TablePoints.FieldByName(ActiveAttributeB).AsFloat;
            Inc(j);
          end;
          TablePoints.Next;
        end;
        SetLength(PointsArr, j);
        TablePoints.Close;

        // Read TableNodes
        SetLength(NodesArr, TableNodes.RecordCount);
        TableNodes.Open;
        TableNodes.First;
        for i := 0 to TableNodes.RecordCount - 1 do
        begin
          NodesArr[i].X := TableNodes.FieldByName(fldX).AsFloat;
          NodesArr[i].Y := TableNodes.FieldByName(fldY).AsFloat;
          if Mode3d then
            NodesArr[i].Z := TableNodes.FieldByName(fldZ).AsFloat
          else
            NodesArr[i].Z := 0;
          TableNodes.Next;
        end;
        TableNodes.Close;

        // Krining Interpolation
        Kt_3d(PointsArr, NodesArr, KrigingParams, ProgressBar);
        //  if Mode3d then kt_3d(PointsArr, NodesArr, KrigingParams, ProgressBar)
        //  else kb_2d(PointsArr, NodesArr, ProgressBar);
        AddTableField(TableNodes.TableName, ActiveAttributeB + '_KRI', ftFloat);
        if CheckBoxVariance.Checked then
          AddTableField(TableNodes.TableName, ActiveAttributeB + '_VAR', ftFloat);

        // Write TableNodes
        TableNodes.Open;
        TableNodes.First;
        for i := 0 to TableNodes.RecordCount - 1 do
          // for i := 0 to High(NodesArr) do
        begin
          TableNodes.Edit;
          TableNodes.FieldByName(ActiveAttributeB + '_KRI').Value :=
            RoundTo(NodesArr[i].Value, Precision);
          if CheckBoxVariance.Checked then
            TableNodes.FieldByName(ActiveAttributeB + '_VAR').Value :=
              RoundTo(NodesArr[i].Variance, Precision * 2)
          else
          if TableNodes.FindField(ActiveAttributeB + '_VAR') <> nil then
            TableNodes.FieldByName(ActiveAttributeB + '_VAR').Value := 0;
          // FieldByName(ActiveAttributeB + '_KRIGING_ERROR').Value :=
          // RoundTo(NodesArr[i].Error, Precision*2);
          TableNodes.Post;
          TableNodes.Next;
        end;
        TableNodes.Close;

        // release the memory
        SetLength(PointsArr, 0);
        SetLength(NodesArr, 0);
      end;

      //_________Natural Neighbour Interpolation_________
      imNaturalNeighbors:
      begin
        // Read TableNodes
        SetLength(NodesArr, TableNodes.RecordCount);
        TableNodes.Open;
        TableNodes.First;
        for i := 0 to TableNodes.RecordCount - 1 do
        begin
          NodesArr[i].X := TableNodes.FieldByName(fldX).AsFloat;
          NodesArr[i].Y := TableNodes.FieldByName(fldY).AsFloat;
          if Mode3d then
            NodesArr[i].Z := TableNodes.FieldByName(fldZ).AsFloat
          else
            NodesArr[i].Z := 0;
          TableNodes.Next;
        end;
        TableNodes.Close;

        // Read TablePoints
        SetLength(PointsArr, TablePoints.RecordCount);
        TablePoints.Open;
        TablePoints.First;
        j := 0;
        for i := 0 to TablePoints.RecordCount - 1 do
        begin
          if not TablePoints.FieldByName(ActiveAttributeB).IsNull then
          begin
            PointsArr[j].X := TablePoints.FieldByName(fldX).AsFloat;
            PointsArr[j].Y := TablePoints.FieldByName(fldY).AsFloat;
            if Mode3d then
              PointsArr[j].Z := TablePoints.FieldByName(fldZ).AsFloat
            else
              PointsArr[j].Z := 0;
            PointsArr[j].Value := TablePoints.FieldByName(ActiveAttributeB).AsFloat;
            Inc(j);
          end;
          TablePoints.Next;
        end;
        SetLength(PointsArr, j);
        TablePoints.Close;

        if Mode3d then
          ShowMessage('3D NNI not implemented yet!')
          // NaturalNeighborsInterpolation3D(PointsArr, NodesArr, ExtraValue, ProgressBar);
        else
          NaturalNeighborsInterpolation2D(PointsArr, NodesArr, ExtraValue, ProgressBar);

        // Write TableNodes 
        AddTableField(TableNodes.TableName, ActiveAttributeB + '_NNI', ftFloat);
        TableNodes.Open;
        TableNodes.First;
        for i := 0 to TableNodes.RecordCount - 1 do
        begin
          TableNodes.Edit;
          TableNodes.FieldByName(ActiveAttributeB + '_NNI').Value :=
            RoundTo(NodesArr[i].Value, Precision);
          TableNodes.Post;
          TableNodes.Next;
        end;
        TableNodes.Close;

          // release the memory
          SetLength(PointsArr, 0);
          SetLength(NodesArr, 0);
        end;
    end;
  finally
    TableNodes.Close;
    TablePoints.Close;
  end;
  Screen.Cursor := OldCursor;
end;

//-----------------------------------------------------------------------------

procedure TfmMethodInterpolation.ListBoxInputNamesClick(Sender: TObject);
begin
  inherited;
  ReadGridParameters;
end;

//-----------------------------------------------------------------------------

procedure TfmMethodInterpolation.ToolButtonInputClick(Sender: TObject);
begin
  inherited;
  ReadGridParameters;
end;

//-----------------------------------------------------------------------------
(*
procedure TfmMethodInterpolation.BtnReadOpenCLCouplesClick(Sender: TObject);
var
  i, j: integer;
	num_platforms_returned: Tcl_uint;
  platform_id: array of Tcl_platform_id; // platform id
  i_pl: integer;
  i_type: integer;
  device_vendor, device_name: AnsiString;
  returned_size: Tsize_t;

begin
  inherited;
  {
  if not OpenCL_loaded then begin
    ShowMessage('Error: OpenCL.dll not loaded!');
    exit;
  end;
  }
  // Number of platforms
	errcode_ret := clGetPlatformIDs(0, nil, @num_platforms_returned);
	if (errcode_ret <> CL_SUCCESS) then
  begin
		ShowMessage('Error: Failed to get number of platforms!');
		exit;
  end;
  // Connect to a platform
  SetLength(platform_id, num_platforms_returned);
	errcode_ret := clGetPlatformIDs(num_platforms_returned, @platform_id[0], @num_platforms_returned);
	if (errcode_ret <> CL_SUCCESS) then begin
		ShowMessage('Error: Failed to find platform!');
		exit;
  end;

	// find platform+device_type
	SetLength(platform_devices, 0);
  for i_pl:=0 to num_platforms_returned-1 do for i_type:=1 to 3 do
  	if (CL_SUCCESS=clGetDeviceIDs(platform_id[i_pl], c_device_types[i_type], 0, nil, @num_devices_returned)) and (num_devices_returned>=1)
    then begin
    	SetLength(platform_devices, Length(platform_devices)+1);
		  Platform_devices[High(platform_devices)].platform_id := platform_id[i_pl];
			Platform_devices[High(platform_devices)].device_type := c_device_types[i_type];
    end;
	// list platform+device_type couples
  ListCouple.Clear;
	for i:=0 to High(platform_devices) do begin
		errcode_ret:=clGetDeviceIDs(platform_devices[i].platform_id, platform_devices[i].device_type, 0, nil, @num_devices_returned);
		if (errcode_ret<>CL_SUCCESS) then begin
  		ShowMessage('Error: Failed to create a device group!');
	  	exit;
    end;
		SetLength(device_ids, num_devices_returned);
		errcode_ret:=clGetDeviceIDs(platform_devices[i].platform_id, platform_devices[i].device_type, num_devices_returned, @device_ids[0], @num_devices_returned);
		for j:=0 to num_devices_returned-1 do begin
      SetLength(device_name, 1024);
			clGetDeviceInfo(device_ids[j], CL_DEVICE_NAME, Length(device_name), PAnsiChar(device_name), @returned_size);
      SetLength(device_name, Min(Pos(#0, device_name)-1, returned_size-1));
      SetLength(device_vendor, 1024);
			clGetDeviceInfo(device_ids[j], CL_DEVICE_VENDOR, Length(device_vendor), PAnsiChar(device_vendor), @returned_size);
      SetLength(device_vendor, Min(Pos(#0, device_vendor)-1, returned_size-1));
      ListCouple.Items.Add('Vendor: '+device_vendor+'; Device: '+device_name);
		end;
		SetLength(device_ids, 0);
	end;
  if ListCouple.Items.Count>0 then ListCouple.ItemIndex:=0;
  // Get compute devices from platform
	errcode_ret:=clGetDeviceIDs(platform_devices[ListCouple.ItemIndex].platform_id, platform_devices[ListCouple.ItemIndex].device_type, 0, nil, @num_devices_returned);
  SetLength(device_ids, num_devices_returned);
  errcode_ret:=clGetDeviceIDs(platform_devices[ListCouple.ItemIndex].platform_id, platform_devices[ListCouple.ItemIndex].device_type, num_devices_returned, @device_ids[0], @num_devices_returned);
  if (errcode_ret<>CL_SUCCESS) then begin
    ShowMessage('Error: Failed to create a device group!');
    exit;
  end;
	// Create a compute context
	context:=clCreateContext(nil, num_devices_returned, @device_ids[0], nil, nil, @errcode_ret);
  if (errcode_ret<>CL_SUCCESS) then begin
    ShowMessage('Error: Failed to create a compute context!!');
    exit;
  end;
  ShowMessage('Context created.');
 	clReleaseContext(context);
end;
*)

procedure TfmMethodInterpolation.ButtonOKClick(Sender: TObject);
var
  I: integer;
begin
  inherited;
  if (RadioGroupMethod.ItemIndex = imKriging) and not KrigingParamsset then
  with KrigingParams do
  begin
    k_type := 1;
	  search_type := NORMAL_SEARCH;
    min_points := 5;
	  max_points := 20;
	  max_radius := 100;
    for i := 1 to 9 do
	  drift[i] := 0;
    model.nst_count := 1;
    with Model.structures[1] do
    begin
      Model_type := 1;
	    Nugget := 0;
	    Contribution := 10;
	    Range := 10;
      Anis1 := 1;
	    Anis2 := 1;
	    Azimuth := 0;
	    Dip := 0;
	    Plunge := 0;
    end;
  end;
  if ModalResult <> mrNone then
  with dmBase do
  begin
    if ToolBarInputB.Tag = 2 then
       Mode3D := True
    else
       Mode3D := False;
    TableInput.TableName := InModelName;
    TableInputB.TableName := InModelNameB;
    Interpolate(TableInputB, TableInput);
  end
  else
    Exit;
end;

end.
