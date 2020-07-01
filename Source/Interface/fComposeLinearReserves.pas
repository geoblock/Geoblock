//-----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//-----------------------------------------------------------------------------
{! The dialog to set parameters for linear reserve calculations}

unit fComposeLinearReserves;

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

  //DB
  Bde.DBTables,
  Data.DB,


  fMethodDialog,
  dBase;

/// The TfmCompositingLinearReserves class includes methods to calculate linear reserves
type
  TfmComposeLinearReserves = class(TfmMethodDialog)
    ListBoxIndicator:   TListBox;
    ListBoxActiveInteger: TListBox;
    LabelIndicator:     TLabel;
    LabelActiveOreType: TLabel;
    procedure ListBoxIndicatorClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ListBoxInputNamesClick(Sender: TObject);
    procedure ToolButtonInputClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    // Private declarations
    procedure UpdateIntegerAttributes;
    procedure UpdateActiveOreType;
  public
    /// <summary>
    /// The function calculates linear reserve of ores and components along drill holes
    /// </summary>
    /// <limitation>
    /// Only global variables are allowed to be specified.
    /// </limitation>
    procedure DefineLinearReserves(TableHoles, TablePoints2D: TTable;
      FieldNameOreType: string; OreTypeValue: integer; RealAttribute: string);
  end;

var
  fmComposeLinearReserves: TfmComposeLinearReserves;

//===========================================================
implementation
//===========================================================

uses
  uGlobals,
  uCommon,
  uFileCreator,
  uResStrings,
  uProfuns;

{$R *.DFM}

{ TfmCompositingLinearReserve }

procedure TfmComposeLinearReserves.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
  ToolButtonHoles.Click;
end;

procedure TfmComposeLinearReserves.ToolButtonInputClick(Sender: TObject);
begin
  inherited;
  PanelOutPath.Caption := ExpandPath(DirPoints2D);
  PanelOutPath.Hint    := PanelOutPath.Caption;
  ListBoxInputNamesClick(Self);
end;

procedure TfmComposeLinearReserves.ListBoxInputNamesClick(Sender: TObject);
begin
  inherited;
  UpdateIntegerAttributes;
  UpdateActiveOreType;
end;

procedure TfmComposeLinearReserves.ListBoxIndicatorClick(Sender: TObject);
begin
  inherited;
  UpdateActiveOreType;
end;

procedure TfmComposeLinearReserves.UpdateIntegerAttributes;
var
  I: integer;
begin
  with dmBase do
  begin
    ListBoxIndicator.Items.Clear;
    try
      TableInput.Open;
      for I := 0 to TableInput.FieldCount - 1 do
      begin
        if IsIntegerAttribute(TableInput.Fields[I]) then
          ListBoxIndicator.Items.Add(TableInput.Fields[I].FieldName);
      end;
      TableInput.Close;
    except
      TableInput.Close;
    end;
    try
      CheckItemIndex(ListBoxIndicator);
    except
    end;
    ButtonOK.Enabled := ListBoxIndicator.Items.Count - 1 <> 0;
    UpdateActiveOreType;
  end;
end;

procedure TfmComposeLinearReserves.UpdateActiveOreType;
var
  OreTypeFieldName: string;
  OldCursor: TCursor;
  Query: TQuery;
  I: integer;
begin
  OldCursor := Screen.Cursor;
  with dmBase do
    try
      Screen.Cursor := crHourGlass;
      ListBoxActiveInteger.Items.Clear;
      try
        with ListBoxIndicator do
        begin
          ItemIndex := Min(Items.Count - 1, Max(ItemIndex, 0));
          if ItemIndex < 0 then
            Exit;
          OreTypeFieldName := Items[ItemIndex];
        end;
        Query := TQuery.Create(Self);
        with Query do
          try
            SQL.Add('SELECT DISTINCT T."' + OreTypeFieldName + '"');
            SQL.Add('FROM "' + TableInput.TableName + '" T');
            SQL.Add('WHERE T."' + OreTypeFieldName + '" IS NOT NULL');
            Query.Open;
            for I := 1 to Query.RecordCount do
            begin
              Query.RecNo := I;
              ListBoxActiveInteger.Items.Add(Query.FieldByName(
                OreTypeFieldName).AsString);
            end;
          finally
            Free;
          end;
      except
        TableInput.Close;
      end;
    finally
      Screen.Cursor := OldCursor;
    end;
  try
    CheckItemIndex(ListBoxActiveInteger);
  except
  end;
  ButtonOK.Enabled := ListBoxActiveInteger.Items.Count - 1 <> 0;
end;

 //--------------------------------------------------------
 // Description: calculate linear reserves for drillholes
 // Input:
 //   TableHoles - a table with sample contact coordinates
 //                from the DHOLE directory
 // Output:
 //   TablePoints2D - a table with coordinates of 2D Points with
 //                  Linear Reserves written to POINT2D directory
 //--------------------------------------------------------

procedure TfmComposeLinearReserves.DefineLinearReserves(
  TableHoles, TablePoints2D: TTable; FieldNameOreType: string;
  OreTypeValue: integer; RealAttribute: string);

var
  ID:      integer;
  X, Y, Z: double;
  Drillhole: string;
  Profile: string;
  Density: double;
  Thickness: double;
  SumGrade: double;
  C_Component, Q_Component: double;
  //  Moisture : Double;
  SampleCount: integer;

  {Sub}
  function BODH: boolean;
  begin
    Result := DrillHole <> TableHoles.FieldByName(fldDHOLE).AsString;
  end;

  {Sub}
  function EOF: boolean;
  begin
    Result := TableHoles.EOF;
  end;

  {Sub}
  function EODH: boolean;
  begin
    Result := (DrillHole <> TableHoles.FieldByName(fldDHOLE).AsString);
    Result := Result or EOF;
  end;

  {Sub}
  function BOP: boolean;
  begin
    Result := TableHoles.FieldByName(FieldNameOreType).AsInteger = OreTypeValue;
  end;

  {Sub}
  function GetSampleLength: double;
  var
    X, Y, Z:  double;
    OldRecNo: longint;
  begin
    OldRecNo := TableHoles.RecNo;
    try
      X := TableHoles.FieldByName(fldX).AsFloat;
      Y := TableHoles.FieldByName(fldY).AsFloat;
      Z := TableHoles.FieldByName(fldZ).AsFloat;
      TableHoles.Next;
      if not EODH then
        Result := Norm([X - TableHoles.FieldByName(fldX).AsFloat,
          Y - TableHoles.FieldByName(fldY).AsFloat, Z -
          TableHoles.FieldByName(fldZ).AsFloat])
      else
        Result := 0.00001; //The thickness of last samples in drillholes
    finally
      TableHoles.RecNo := OldRecNo;
    end;
  end;

  {Sub}
  procedure ReadHole;
  var
    SampleLength: double;
  begin
    ID := ID + 1;
    X  := TableHoles.FieldByName(fldX).AsFloat;
    Y  := TableHoles.FieldByName(fldY).AsFloat;
    Z  := TableHoles.FieldByName(fldZ).AsFloat;

    Drillhole   := TableHoles.FieldByName(fldDHOLE).AsString;
    Profile     := TableHoles.FieldByName(fldPROFILE).AsString;
    Density     := 0;
    Thickness   := 0;
    C_Component := 0;
    Q_Component := 0;
    SampleCount := 0;
    repeat
      if TableHoles.FieldByName(FieldNameOreType).AsInteger = OreTypeValue then
      begin
        SampleLength := GetSampleLength;
        try
          Inc(SampleCount);
          Density   := Density + TableHoles.FieldByName(fldDENSITY).AsFloat *
            SampleLength;
          Thickness := Thickness + SampleLength;
          SumGrade  := SumGrade + TableHoles.FieldByName(
            RealAttribute).AsFloat * SampleLength;
        except
        end;
      end;
      TableHoles.Next;
    until EODH;
    TablePoints2D.Append;
    TablePoints2D.FieldByName(fldID).AsInteger   := ID;
    TablePoints2D.FieldByName(fldX).AsFloat      := X;
    TablePoints2D.FieldByName(fldY).AsFloat      := Y;
    TablePoints2D.FieldByName(fldZ).AsFloat      := Z;
    TablePoints2D.FieldByName(fldDHOLE).AsString := Drillhole;
    TablePoints2D.FieldByName(fldPROFILE).AsString := Profile;
    TablePoints2D.FieldByName(fldTHICKNESS).AsFloat :=
      RoundTo(Thickness, Precision);
    try
      if abs(Thickness) < 0.00000001 then
      begin
        TablePoints2D.FieldByName(fldDENSITY).AsFloat := 0;
        TablePoints2D.FieldByName('C_' + RealAttribute).AsFloat := 0;
        TablePoints2D.FieldByName('Q_' + RealAttribute).AsFloat := 0;
      end
      else
      begin
        TablePoints2D.FieldByName(fldDENSITY).AsFloat := Density / Thickness;
        C_Component := RoundTo(SumGrade / Thickness, Precision);
        TablePoints2D.FieldByName('C_' + RealAttribute).AsFloat :=
          C_Component;
        TablePoints2D.FieldByName('Q_' + RealAttribute).AsFloat :=
          RoundTo(C_Component * Density, Precision);
      end;
    except
    end;
    TablePoints2D.Post;
  end;

  {\Subroutines}

begin
  TableHoles.Open;
  TablePoints2D.TableName := OutModelName;
  CreatePoint2DTables(TablePoints2D.TableName);
  TablePoints2D.Open;

  TablePoints2D.FieldDefs.Add(fldPROFILE, ftString, 32, False);
  TablePoints2D.FieldDefs.Add(fldDHOLE, ftString, 32, False);

  TablePoints2D.FieldDefs.Add(fldDENSITY, ftFloat);
  TablePoints2D.FieldDefs.Add(fldTHICKNESS, ftFloat);
  TablePoints2D.FieldDefs.Add('C_' + RealAttribute, ftFloat);
  TablePoints2D.FieldDefs.Add('Q_' + RealAttribute, ftFloat);

  TablePoints2D.Close;
  TablePoints2D.CreateTable;
  TablePoints2D.Open;

  ProgressBar.Min      := 0;
  ProgressBar.Max      := TableHoles.RecordCount;
  ProgressBar.Position := 1;

  Drillhole := TableHoles.FieldByName(fldDHOLE).AsString + '_';

  ID := 0;
  while not EOF do
  begin
    ProgressBar.Position := TableHoles.RecNo;
    ReadHole;
  end;
  TablePoints2D.Close;
  TableHoles.Close;
end;

procedure TfmComposeLinearReserves.ButtonOKClick(Sender: TObject);
begin
  inherited;
  with dmBase do
  begin
    OutModelType := mtPoints2D;
    if ModalResult <> mrNone then
      DefineLinearReserves(TableInput, TableOutput,
        GetCurrentItem(ListBoxIndicator),
        StrToInt(GetCurrentItem(ListBoxActiveInteger)),
        GetCurrentItem(ListBoxRealAttribute))
    else
      Exit;
  end;
end;

end.
