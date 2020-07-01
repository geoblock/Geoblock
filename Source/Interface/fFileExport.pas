//----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//----------------------------------------------------------------------------
{! The form for export routines }

unit fFileExport;

interface

uses
  System.SysUtils, 
  System.Variants, 
  System.Classes, 
  Vcl.Graphics, 
  Vcl.Controls,
  Vcl.Forms, 
  Vcl.Dialogs, 
  Vcl.StdCtrls, 
  Vcl.ExtCtrls, 
  Vcl.Buttons, 
  Vcl.ComCtrls,
  //DB
  Data.DB, 
  Bde.DBTables,
  
  dBase,
  dDialogs,
  fInitialDialog;

type
  TfmFileExport = class(TfmInitialDialog)
    GroupBoxInput:  TGroupBox;
    PanelInputFile: TPanel;
    GroupBoxOutput: TGroupBox;
    SpeedButtonBrowse: TSpeedButton;
    LabelType:      TLabel;
    PanelOutputPath: TPanel;
    EditOutputName: TEdit;
    ComboBoxType:   TComboBox;
    ProgressBar:    TProgressBar;
    procedure SpeedButtonBrowseClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ComboBoxTypeChange(Sender: TObject);
    procedure EditOutputNameChange(Sender: TObject);
  private
    FOutputName: string;
    FOutputPath: string;
    function GetOutputName: string;
    procedure SetOutputName(const Value: string);
    function GetSourceFile: string;
    procedure SetSourceFile(const Value: string);
    function GetOutputFile: string;
    procedure SetOutputFile(const Value: string);

    procedure DBTable_AsciiTXT;
    procedure Points2D_To_DAT;
    procedure Grid2D_SurferGRD;
    procedure Grid3D_4XMOD;
    procedure Grid3D_MeshFeMAP;
    procedure Polygons_DXF;
  public
    property OutputName: string Read GetOutputName Write SetOutputName;
    property OutputFile: string Read GetOutputFile Write SetOutputFile;
    property SourceFile: string Read GetSourceFile Write SetSourceFile;
  end;

var
  fmFileExport: TfmFileExport;

//======================================================================
implementation
//======================================================================

uses
  uProfuns,
  uCommon,
  uGlobals,
  uWhittle,
  gnuGettext,
  uResStrings;

{$R *.dfm}

{ TfmFileExport }

procedure TfmFileExport.DBTable_AsciiTXT;
var
  FileTxt: System.Text;
  I: integer;
  J: longint;
  SourceFieldCount: integer;
  S: string;
  Separator: string;
begin
  AssignFile(FileTxt, ChangeFileExt(OutputFile, '.txt'));
  Rewrite(FileTxt);
  with dmBase do
    try
      TableExport.Open;
      try
        ProgressBar.Max := TableExport.RecordCount;
        SourceFieldCount := TableExport.FieldCount;
        Separator := '';
        for I := 0 to SourceFieldCount - 1 do
        begin
          S := TableExport.FieldDefs[I].Name;
          //        S := AnsiQuotedStr(S,'"');
          Write(FileTxt, Separator, S);
          Separator := ',';
        end;
        WriteLn(FileTxt);
        for J := 0 to TableExport.RecordCount - 1 do
        begin
          TableExport.RecNo := J + 1;
          Separator := '';
          for I := 0 to SourceFieldCount - 1 do
          begin
            try
              S := VarToStr(TableExport.Fields[I].AsVariant);
              //  if TableExport.FieldDefs[I].DataType=ftString then S:=AnsiQuotedStr(S,'"');
              Write(FileTxt, Separator, S);
              Separator := ',';
            except
            end;
          end;
          ProgressBar.StepIt;
          //  if j mod 10 = 0 then Application.ProcessMessages;
          WriteLn(FileTxt);
        end;
      finally
        TableExport.Close;
      end;
    finally
      CloseFile(FileTxt);
    end;
end;

function TfmFileExport.GetOutputFile: string;
begin
  Result := SlashSep(FOutputPath, FOutputName);
end;

function TfmFileExport.GetOutputName: string;
begin
  Result := FOutputName;
end;

function TfmFileExport.GetSourceFile: string;
begin
  Result := dmBase.TableExport.TableName;
end;

procedure TfmFileExport.Grid2D_SurferGRD;

var
  F:     System.Text;
  I:     integer;
  J, K:  longint;
  SourceFieldCount: integer;
  NR:    integer;
  Exist: boolean;
  FileName: TFilename;
  S, S1, S2: string;
  Separator: string;
  Short, C: integer;
  NX, NY, NZ: integer;
  XO, YO, ZO, DX, DY, DZ,
  XE, YE, ZE, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax: Double;
  X, Y, Z: Double;
  IX, IY, IZ, IDXY, Code: integer;
  N:     byte;

  Blank: boolean;
  Value: Single;

begin
  FileName := OutputFile + '.grd';
  AssignFile(F, FileName);
  Rewrite(F);
  with dmBase do
    try
      TableExport.Open;
      try
        ProgressBar.Max  := TableExport.RecordCount;
        SourceFieldCount := TableExport.FieldCount;
        Writeln(F, 'DSAA');
        ReadParFile(TableExport.TableName, XO, YO, ZO, DX, DY, DZ, NX, NY, NZ);
        MinMaxXYZ(TableExport.TableName, Xmin, Ymin, Zmin, Xmax, Ymax, Zmax,
          ProgressBar);
        XE := XO + DX * NX;
        YE := YO + DY * NY;

        { Title }
        S1 := IntToStr(NX);
        Trim(S1);
        S2 := IntToStr(NY);
        Trim(S2);
        S := S1 + ' ' + S2;
        Writeln(F, S);
        System.Str(XO: 10: 2, S1);
        Trim(S1);
        System.Str(XE: 10: 2, S2);
        Trim(S2);
        S := S1 + ' ' + S2;
        Writeln(F, S);
        System.Str(YO: 10: 2, S1);
        Trim(S1);
        System.Str(YE: 10: 2, S2);
        Trim(S2);
        S := S1 + ' ' + S2;
        Writeln(F, S);
        System.Str(Zmin: 10: 2, S1);
        Trim(S1);
        System.Str(Zmax: 10: 2, S2);
        Trim(S2);
        S := S1 + ' ' + S2;
        Writeln(F, S);
        S := '';

        if TableExport.FieldByName(fldZ).Datatype <> ftFloat then
          Exit;

      (*
      // Create TableTemp with complete grid nodes
      TableTemp.TableName := ExtractFilePath(TableExport.TableName) +
        '\' + 'temporary' + TableExt;
      TableTemp.FieldDefs.Clear;
      //      TableTemp.FieldDefs.Add(fldID,ftAutoInc,0,False);        //!!!
      TableTemp.FieldDefs.Add(fldID, ftInteger);
      TableTemp.FieldDefs.Add(fldX, ftFloat);
      TableTemp.FieldDefs.Add(fldY, ftFloat);
      TableTemp.FieldDefs.Add(fldZ, ftFloat);
      TableTemp.CreateTable;
      TableTemp.Open;
      ProgressBar.Position := 0;
      ProgressBar.Min := 0;
      ProgressBar.Max := NX * NY * NZ;
      X := XO + DX / 2;
      for I := 1 to NX do
      begin
        Y := YO + DY / 2;
        for J := 1 to NY do
        begin
          Z := ZO + DZ / 2;
          for K := 1 to NZ do
          begin
            TableTemp.Append;
            try
              TableTemp.FieldByName(fldX).AsFloat := X;
              TableTemp.FieldByName(fldY).AsFloat := Y;
              TableTemp.FieldByName(fldZ).AsFloat := Z;
            finally
              TableTemp.Post;
            end;
            Z := Z + DZ;
          end;
          Y := Y + DY;
        end;
        if (I * NY * NZ) mod 100 = 0 then
        begin
          ProgressBar.Position := (I * NY * NZ);
          Application.ProcessMessages;
        end;
        if ModalResult = mrCancel then
          Exit;
        X := X + DX;
      end; {}
      ProgressBar.Position := 0;
      ProgressBar.Min := 0;
      ProgressBar.Max := TableExport.RecordCount;

      // Refresh Temptable with nodes from TableExport
      for I := 0 to TableExport.RecordCount - 1 do
      begin
        TableExport.RecNo := I + 1;
        X := TableExport.FieldByName(fldX).AsFloat;
        Y := TableExport.FieldByName(fldY).AsFloat;
        if (TableExport.FieldByName(fldZ).isNull) then
          //sometimes it may be empty
          S := '-1'
        else
          Z := TableExport.FieldByName(fldZ).AsFloat;

        IX := round((X - XO + DX / 2) / DX);
        IY := round((YO + DY / 2 + DY * NY - Y) / DY);
        IDxy := IX + (IY - 1) * NX;

        TableTemp.SetKey;
        TableTemp.FieldByName(fldID).AsFloat := IDxy;
        TableTemp.GotoNearest;
        TableTemp.Edit;
        TableTemp.FieldByName(fldX).AsFloat := X;
        TableTemp.FieldByName(fldY).AsFloat := Y;
        TableTemp.FieldByName(fldZ).AsFloat := Z;
        TableTemp.Post;

        ProgressBar.StepIt;
      end;
      *)
        // Export TempTable to grd file
        ProgressBar.Position := 0;
        ProgressBar.Min := 0;
        ProgressBar.Max := TableExport.RecordCount;
        K := 1;
        for J := 0 to TableExport.RecordCount - 1 do
        begin
          TableExport.RecNo := J + 1;
          X := TableExport.FieldByName(fldX).AsFloat;
          Y := TableExport.FieldByName(fldY).AsFloat;
          //        Separator:=' ';
          try
            if (TableExport.FieldByName(fldZ).isNull) then
              S := '-1'
            else
            begin
              Z := TableExport.FieldByName(fldZ).AsFloat;
              System.Str(Z: 4: 2, S);
            end;
            Trim(S);
            S := S + ' ';
            Write(F, S);
            if (K mod NX) = 0 then
            begin
              Writeln(F);
              Writeln(F);
              S := '';
            end;
            //        Write(F,Separator,S);
          except
          end;
          ProgressBar.StepIt;
          Inc(K);
        end;
      finally
        TableExport.Close;
        TableTemp.Close;
      end;
    finally
      CloseFile(F);
    end;
end; { Grid2D_SurferGRD }

procedure TfmFileExport.Grid3D_4XMOD;
type
  TString4 = string

    [4];
var
  ParamertFile: Twt4XParametersFile;
  DX, DY, DZ, XO, YO, ZO: double;
  NX, NY, NZ: integer;
  EMessage:  string;
  Query:     TQuery;
  I, J, PosInMod: integer;
  RockExist: boolean;
  ModFile:   System.Text;
  FieldName: string;
  ElementItem: TElement;
  ElementsPosInDB: array[1..300] of integer;

  {sub}
  function ValidElementName(Value: string): boolean;
  begin
    Result := (Value <> fldID) and (Value <> fldX) and (Value <> fldY) and
      (Value <> fldZ) and (Value <> fldDENSITY) and (Value <> fldMOISTURE) and
      (Value <> fldDEPTH) and (Value <> 'MININGCAF') and
      (Value <> 'PROCESSINGCAF') and (Value <> 'TOTALTONNAGE') and (Value <> 'ROCK');
  end;

begin
  with dmBase do
  begin
    TableExport.Open;
    EMessage := '';
    if TableExport.FindField(fldX) = nil then
      EMessage := 'X,';
    if TableExport.FindField(fldY) = nil then
      EMessage := EMessage + 'Y,';
    if TableExport.FindField(fldZ) = nil then
      EMessage := EMessage + 'Z,';
    if TableExport.FindField('TOTALTONNAGE') = nil then
      EMessage := EMessage + 'TOTALTONNAGE,';
    if EMessage <> '' then
    begin
      EMessage := EMessage + ': ' + LoadResString(@rsFieldsNotFound) +
        ': ' + TableExport.TableName;
      ShowMessage(EMessage);
    end
    else if ReadParFile(TableExport.TableName, XO, YO, ZO, DX, DY,
      DZ, NX, NY, NZ) then
    begin
      ParamertFile := Twt4XParametersFile.Create(Self);
      try
        ParamertFile.XO := XO;
        ParamertFile.YO := YO;
        ParamertFile.ZO := ZO;
        ParamertFile.DX := DX;
        ParamertFile.DY := DY;
        ParamertFile.DZ := DZ;
        ParamertFile.NX := NX;
        ParamertFile.NY := NY;
        ParamertFile.NZ := NZ;
        PosInMod := 0;
        for I := 0 to TableExport.FieldCount - 1 do
        begin
          FieldName := UpperCase(TableExport.Fields[I].FieldName);
          if ValidElementName(FieldName) then
          begin
            Inc(PosInMod);
            ElementItem := TElement.Create;
            ElementItem.ElementTypeCode := FieldName;
            ElementItem.PositionInModelFile := PosInMod;
            ElementItem.DPElementUnitsInBlock := 8;
            ElementItem.DPTotalsElementUnits := 8;
            ElementItem.DPElementGradesAndCutoffs := 4;
            ParamertFile.Elements.Add(ElementItem);
            ElementsPosInDB[PosInMod] := TableExport.Fields[I].FieldNo - 1;
          end;
        end;

        RockExist := TableExport.FindField('ROCK') <> nil;
        if not RockExist then
          ParamertFile.RockList.AddRockType
          (' 21  R001      1.00000000 0.0000000 1.0000000')
        else
        begin
          Query := TQuery.Create(Self);
          try
            Query.SQL.Add('SELECT DISTINCT ROCK');
            Query.SQL.Add('FROM "' + TableExport.TableName + '"');
            Query.SQL.Add('WHERE ROCK IS NOT NULL');
            Query.Open;
            for I := 1 to Query.RecordCount do
            begin
              Query.RecNo := I;
              ParamertFile.RockList.AddRockType(' 21  ' +
                format('R%3.3d      1.00000000 0.0000000 1.0000000',
                [Query.FieldByName('ROCK').AsInteger]));
            end;
          finally
            Query.Free;
          end;
        end;

        ParamertFile.FileName := ChangeFileExt(OutputFile, ParExt);
        if ParamertFile.EditDialog = mrOk then
        begin
          ;
          Application.ProcessMessages;
          AssignFile(ModFile, ChangeFileExt(OutputFile, '.mod'));
          Rewrite(ModFile);
          try
            ParamertFile.LoadFromFile(ParamertFile.FileName);
            TableExport.First;
            ProgressBar.Min      := 0;
            ProgressBar.Position := 1;
            ProgressBar.Max      := TableExport.RecordCount;
            with ParamertFile do
              for I := 1 to TableExport.RecordCount do
              begin
                Write(ModFile, ' ', Round((TableExport[fldX] - XO) / DX): 3,
                  Round((TableExport[fldY] - YO) / DY): 3,
                  Round((TableExport[fldZ] - ZO) / DZ): 3);
                if TableExport.FieldByName('TotalTonnage').AsInteger <> 0 then
                begin
                  Write(ModFile, '  1');
                  if TableExport.FindField('MiningCAF') <> nil then
                    Write(ModFile, TableExport['MiningCAF']: 13)
                  else
                    Write(ModFile, MiningCAF: 13);
                  if TableExport.FindField('ProcessingCAF') <> nil then
                    Write(ModFile, TableExport['ProcessingCAF']: 11)
                  else
                    Write(ModFile, ProcessingCAF: 11);
                  Write(ModFile, TableExport['TotalTonnage']: 11);
                  Writeln(ModFile);

                  Write(ModFile, ' ', Round((TableExport['X'] - XO) / DX): 3,
                    Round((TableExport['Y'] - YO) / DY): 3,
                    Round((TableExport['Z'] - ZO) / DZ): 3);
                  if RockExist then
                    Write(ModFile, Format(' R%3.3d',
                      [TableExport.FieldByName('Rock').AsInteger]))
                  else
                    Write(ModFile, ' R001');
                  Write(ModFile, TableExport['TotalTonnage']: 11);
                  for J := 1 to PosInMod do
                    Write(ModFile,
                      TableExport.Fields[ElementsPosInDB[J]].Value: 11);
                  Writeln(ModFile);
                end
                else
                begin
                  Write(ModFile, '  0');
                  if TableExport.FindField('MiningCAF') <> nil then
                    Write(ModFile, TableExport['MiningCAF']: 13)
                  else
                    Write(ModFile, MiningCAF: 13);
                  if TableExport.FindField('ProcessingCAF') <> nil then
                    Write(ModFile, TableExport['ProcessingCAF']: 11)
                  else
                    Write(ModFile, ProcessingCAF: 11);
                  Write(ModFile, 0: 11);
                  Writeln(ModFile);
                end;
                TableExport.Next;
                if (I and $0FF) = 0 then
                  ProgressBar.Position := I;
              end;
          finally
            ProgressBar.Position := TableExport.RecordCount;
            CloseFile(ModFile);
          end;
        end;
      finally
        ParamertFile.Free;
      end;
    end
    else
    begin
      EMessage := LoadResString(@rsParFileNotFound) + ': ' +
        ChangeFileExt(TableExport.TableName, ParExt);
      ShowMessage(EMessage);
    end;
  end;
end;

procedure TfmFileExport.Grid3D_MeshFeMAP;
var
  F:    System.Text;
  I:    integer;
  J, K: longint;
  S:    string;
  Separator: string;

var
  V1, V2, V3, V4, V5, V6, V7, V8: integer;
  FileName:   TFileName;
  S1, S2:     string;
  NX, NY, NZ: integer;

  DX, DY, DZ, XO, YO, ZO, XE, YE, ZE: double;

  ID:    integer;
  X, Y, Z, XC, YC, ZC, // Coordinates of the cell center
  Grade: double;
  Garr:  array of double; //Grades of cells from Table

  Value: double;
  Num:   integer;

  {sub}
  procedure WriteCellIDs(Num: integer; I, J, K: integer);
  begin
    Str(Num: 7, S);
    V1 := I + 1 + J * (NX + 2) + K * (NX + 2) * (NY + 2);
    Str(V1: 7, S1);
    S  := S + ',' + S1;
    V2 := I + 2 + J * (NX + 2) + K * (NX + 2) * (NY + 2);
    Str(V2: 7, S1);
    S  := S + ',' + S1;
    V3 := I + 2 + (J + 1) * (NX + 2) + K * (NX + 2) * (NY + 2);
    Str(V3: 7, S1);
    S  := S + ',' + S1;
    V4 := I + 1 + (J + 1) * (NX + 2) + K * (NX + 2) * (NY + 2);
    Str(V4: 7, S1);
    S := S + ',' + S1;

    V5 := I + 1 + J * (NX + 2) + (K + 1) * (NX + 2) * (NY + 2);
    Str(V5: 7, S1);
    S  := S + ',' + S1;
    V6 := I + 2 + J * (NX + 2) + (K + 1) * (NX + 2) * (NY + 2);
    Str(V6: 7, S1);
    S  := S + ',' + S1;
    V7 := I + 2 + (J + 1) * (NX + 2) + (K + 1) * (NX + 2) * (NY + 2);
    Str(V7: 7, S1);
    S  := S + ',' + S1;
    V8 := I + 1 + (J + 1) * (NX + 2) + (K + 1) * (NX + 2) * (NY + 2);
    Str(V8: 7, S1);
    S := S + ',' + S1;
    Writeln(F, S);
    S  := '';
    S1 := '';
  end;

begin
  FileName := OutputFile + '.inp';
  AssignFile(F, FileName);
  Rewrite(F);
  with dmBase do
    try
      TableExport.Open;
      try
        ProgressBar.Min := 1;
        ReadParFile(TableExport.TableName, XO, YO, ZO, DX, DY, DZ, NX, NY, NZ);
        XE := XO + DX * NX;
        YE := YO + DY * NY;
        ZE := ZO + DZ * NZ;

        SetLength(Garr, NX * NY * NZ);
        ProgressBar.Max := 4 * NX * NY * NZ;

        { Title }
        Writeln(F, '** **********************************');
        Writeln(F, '** Written by : Geoblock for FEMAP **');
        Writeln(F, '** Version    : 1.53               **');
        Writeln(F, '** From Model : GRID.DB            **');
        Writeln(F, '** Date       : 2002               **');
        Writeln(F, '*************************************');
        Writeln(F, '**');
        Writeln(F, '*NODE, NSET=GLOBAL');

        // ------- Fill Grade Array for NX * NY * NZ cells --------\\
        TableExport.First;
        ID := 0;
        for I := 0 to NX - 1 do
          for J := 0 to NY - 1 do
            for K := 0 to NZ - 1 do
            begin
              Inc(ID); // ID := I + 1 + J * NX + K * NX * NY;
              ProgressBar.Position := ID;
              // Search the table to locate a record with currect ID \\
              if TableExport.FindKey([ID]) then
              begin
                X := TableExport.Fields[1].AsFloat;
                Y := TableExport.Fields[2].AsFloat;
                Z := TableExport.Fields[3].AsFloat;
                if TableExport.Fields[4].isNull then
                  Garr[ID] := 0
                else
                  Garr[ID] := TableExport.Fields[4].Value;
              end
              else
                Garr[ID] := 0;
            end;

        // ---------- Grid3D Nodes -> Mesh3D Vertices ----------\\
        // NX*NY*NZ nodes -> (NX+2)*(NY+2)*(NZ+2) vertices      \\
        ID := 0;
        for K := 0 to NZ + 1 do
          for J := 0 to NY + 1 do
            for I := 0 to NX + 1 do
            begin
              Inc(ID);
              ProgressBar.Position := ID;
              Str(ID: 8, S);
              X := XO + I * DX;
              Y := YO + J * DY;
              Z := ZO + K * DZ;

              Str(X: 12: 2, S1);
              S := S + ',' + S1;
              Str(Y: 12: 2, S1);
              S := S + ',' + S1;
              Str(Z: 12: 2, S1);
              S := S + ',' + S1;
              Writeln(F, S);
              S  := '';
              S1 := '';
            end;


        // Loop on grid nodes and write cell vertices IDs of meshes
        // The number of grid nodes is equal to number of mesh cells
        Num := 0;
        Writeln(F, '*ELEMENT, TYPE=C3D8, ELSET=P0, Z > 275, AIR ');
        for I := 0 to NX - 1 do
          for J := 0 to NY - 1 do
            for K := 0 to NZ - 1 do
            begin
              Z := ZO + K * DZ + DZ / 2;    //for cell center!
              if (Z > 275) then
              begin
                Inc(Num);
                WriteCellIDs(Num, I, J, K);
              end;
            end;

        Writeln(F, '*ELEMENT, TYPE=C3D8, ELSET=P1, -175 < Z <= 275, CLAY + SAND');
        for I := 0 to NX - 1 do
          for J := 0 to NY - 1 do
            for K := 0 to NZ - 1 do
            begin
              Z := ZO + K * DZ + DZ / 2;
              if (Z <= 275) and (Z > -175) then
              begin
                Inc(Num);
                WriteCellIDs(Num, I, J, K);
              end;
            end;

        Writeln(F, '*ELEMENT, TYPE=C3D8, ELSET=P2, -275 < Z <= -175, LIMESTONE');
        for I := 0 to NX - 1 do
          for J := 0 to NY - 1 do
            for K := 0 to NZ - 1 do
            begin
              Z := ZO + K * DZ + DZ / 2;
              if (Z <= -175) and (Z > -275) then
              begin
                Inc(Num);
                WriteCellIDs(Num, I, J, K);
              end;
            end;

        ID := 0;
        Writeln(F, '*ELEMENT, TYPE=C3D8, ELSET=P4, Z <= -275, QUARTZITE');
        for I := 0 to NX - 1 do
          for J := 0 to NY - 1 do
            for K := 0 to NZ - 1 do
            begin
              Z     := ZO + K * DZ + DZ / 2;
              Grade := Garr[ID];
              Inc(ID);
              if (Z <= -275) and ((Grade >= 0) and (Grade < 50)) then
              begin
                Inc(Num);
                WriteCellIDs(Num, I, J, K);
              end;
            end;

        ID := 0;
        Writeln(F, '*ELEMENT, TYPE=C3D8, ELSET=P3, Z <= -275, ORE: Fe >= 60');
        for I := 0 to NX - 1 do
          for J := 0 to NY - 1 do
            for K := 0 to NZ - 1 do
            begin
              Grade := Garr[ID];
              Z     := ZO + K * DZ + DZ / 2;
              Inc(ID);
              if (Z <= -275) and (Grade >= 60) then
              begin
                Inc(Num);
                WriteCellIDs(Num, I, J, K);
              end;
            end;

        // -------------- IDs of bottom model face ---------------------\\
        Writeln(F, ' Bottom Vertices: IDs for Z = ZO');
        ID := 0;
        for K := 0 to NZ + 1 do
          for J := 0 to NY + 1 do
            for I := 0 to NX + 1 do
            begin
              Inc(ID);
              Z := ZO + K * DZ;
              if (Z > ZO) then
                break;
              Str(ID: 8, S);
              Writeln(F, S);
              S := '';
            end;
        //IDs of top model face
        Writeln(F, ' Top Vertices: IDs for Z = ZE');
        ID := 0;
        for K := 0 to NZ + 1 do
          for J := 0 to NY + 1 do
            for I := 0 to NX + 1 do
            begin
              Inc(ID);
              Z := ZO + K * DZ;
              if (Z < ZO + (NZ + 1) * DZ) then
                continue;
              Str(ID: 8, S);
              Writeln(F, S);
              S := '';
            end;

        //IDs of left model face
        Writeln(F, ' Left Vertices: IDs for X = XO');
        ID := 0;
        for K := 0 to NZ + 1 do
          for J := 0 to NY + 1 do
            for I := 0 to NX + 1 do
            begin
              Inc(ID);
              X := XO + I * DX;
              if (X <> XO) then
                continue;
              Str(ID: 8, S);
              Writeln(F, S);
              S := '';
            end;

        //IDs of right model face
        Writeln(F, ' Right Vertices: IDs for X = XE');
        ID := 0;
        for K := 0 to NZ + 1 do
          for J := 0 to NY + 1 do
            for I := 0 to NX + 1 do
            begin
              Inc(ID);
              X := XO + I * DX;
              if (X <> XO + (NX + 1) * DX) then
                continue;
              Str(ID: 8, S);
              Writeln(F, S);
              S := '';
            end;

        //IDs of front model face
        Writeln(F, ' Front Vertices: IDs for Y = YO');
        ID := 0;
        for K := 0 to NZ + 1 do
          for J := 0 to NY + 1 do
            for I := 0 to NX + 1 do
            begin
              Inc(ID);
              Y := YO + J * DY;
              if (Y <> YO) then
                continue;
              Str(ID: 8, S);
              Writeln(F, S);
              S := '';
            end;

        //IDs of back model face
        Writeln(F, ' BACK Vertices: IDs for Y = YE');
        ID := 0;
        for K := 0 to NZ + 1 do
          for J := 0 to NY + 1 do
            for I := 0 to NX + 1 do
            begin
              Inc(ID);
              Y := YO + J * DY;
              if (Y <> YO + (NY - 1) * DY) then
                continue;
              Str(ID: 8, S);
              Writeln(F, S);
              S := '';
            end;

        ProgressBar.Position := 0;
        ProgressBar.Min      := 0;
        ProgressBar.Max      := TableExport.RecordCount;

      finally
        TableExport.Close;
      end;
    finally
      CloseFile(F);
    end;
end;

// Export Points2D fataset to dat of Surfer
procedure TfmFileExport.Points2D_To_DAT;
var
  FileTxt: System.Text;
  I: integer;
  J: longint;
  SourceFieldCount: integer;
  S: string;
  Separator: string;
begin
  AssignFile(FileTxt, ChangeFileExt(OutputFile, 'dat'));
  Rewrite(FileTxt);
  with dmBase do
    try
      TableExport.Open;
      try
        ProgressBar.Max := TableExport.RecordCount;
        SourceFieldCount := TableExport.FieldCount;
        Separator := ' ';
        for I := 1 to SourceFieldCount - 1 do //Skip 0 - ID Field
        begin
          S := TableExport.FieldDefs[I].Name;
          S := AnsiQuotedStr(S, '"');
          Write(FileTxt, Separator, S);
        end;
        Separator := '';
        WriteLn(FileTxt);
        for J := 0 to TableExport.RecordCount - 1 do
        begin
          TableExport.RecNo := J + 1;
          Separator := ' ';
          for I := 1 to SourceFieldCount - 1 do //Skip 0 - ID Field
          begin
            try
              if (TableExport.Fields[I].DataType = ftFloat) then
              begin
                if (TableExport.Fields[I].isNull) then
                  S := '-1'
                else
                  S := VarToStr(TableExport.Fields[I].AsVariant);
              end
              else
              begin
                if (TableExport.Fields[I].isNull) then
                  S := ' '
                else
                  S := VarToStr(TableExport.Fields[I].AsVariant);
                S := AnsiQuotedStr(S, '"');
              end;
              Write(FileTxt, Separator, S);
            except
            end;
          end;
          ProgressBar.StepIt;
          WriteLn(FileTxt);
        end;
      finally
        TableExport.Close;
      end;
    finally
      CloseFile(FileTxt);
    end;
end;

procedure TfmFileExport.Polygons_DXF;
begin
  ShowMessage(_('Not implemented'));  // not implemented yet
end;

procedure TfmFileExport.SetOutputFile(const Value: string);
begin
  if CompareText(OutputFile, Value) <> 0 then
  begin
    FOutputPath := ExtractFilePath(Value);
    PanelOutputPath.Hint := FOutputPath;
    PanelOutputPath.Caption := FOutputPath;
    FOutputName := ExtractFileName(Value);
    EditOutputName.Text := NameOnly(Value);
  end;
end;

procedure TfmFileExport.SetOutputName(const Value: string);
begin
  if CompareText(FOutputName, ExtractFileName(Value)) <> 0 then
  begin
    OutputFile := SlashSep(FOutputPath, ExtractFileName(Value));
  end;
end;

procedure TfmFileExport.SetSourceFile(const Value: string);
begin
  if (SourceFile <> Value) then
  begin
    dmBase.TableExport.TableName := Value;
    PanelInputFile.Caption := Value;
  end;
end;

procedure TfmFileExport.SpeedButtonBrowseClick(Sender: TObject);
begin
  with dmDialogs do
  begin
    SaveDialogExport.InitialDir := ExtractFilePath(OutputFile);
    SaveDialogExport.FileName   := OutputFile;
    if SaveDialogExport.Execute then
      OutputFile := SaveDialogExport.FileName;
  end;
end;

procedure TfmFileExport.FormActivate(Sender: TObject);
begin
  ComboBoxType.ItemIndex := 0;
  OutputFile := ExpandPath(DirFiles) +
    NameOnly(dmBase.TableExport.TableName) + '.txt';
end;

procedure TfmFileExport.ButtonOKClick(Sender: TObject);
begin
  case ComboBoxType.ItemIndex of
    0: ;
    1: DBTable_AsciiTXT;
    2: Grid3D_4XMOD;
    3: Points2D_To_DAT;
    4: Grid2D_SurferGRD;
    5: Grid3D_MeshFeMAP;
    {
       6 : Grid3D_WhittleRES;
       7 : Grid3D_WhittleECO;
       8 : PolyDB_PolyKDR;{}
    9: Polygons_DXF;
    else
      DBTable_AsciiTXT;
  end;
end;

procedure TfmFileExport.ComboBoxTypeChange(Sender: TObject);
begin
  PanelOutputPath.Caption := FOutputPath;
  EditOutputName.Text := FOutputName;
  ComboBoxType.Hint := ComboBoxType.Text;
end;

procedure TfmFileExport.EditOutputNameChange(Sender: TObject);
begin
  OutputName := EditOutputName.Text;
end;

end.
