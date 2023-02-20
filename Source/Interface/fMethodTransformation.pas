//---------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//---------------------------------------------------------------------------

unit fMethodTransformation;

interface

uses
  Winapi.Windows, 
  Winapi.Messages,
  System.SysUtils, 
  System.Variants,
  System.Classes,
  System.IniFiles,
  System.Math,
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
  Data.DB, 
  Bde.DBTables,

  GLS.VectorTypes,
  GLS.VectorGeometry,

  
  fMethodDialog,
  GBEditValue,
  GBGeometry, System.ImageList, Vcl.ImgList;

type
  TfmMethodTransformation = class(TfmMethodDialog)
    RadioGroupMode: TRadioGroup;
    GroupBoxMatrix: TGroupBox;
    Label1:      TLabel;
    Label2:      TLabel;
    Label3:      TLabel;
    Label4:      TLabel;
    Label5:      TLabel;
    Label6:      TLabel;
    Label7:      TLabel;
    Label8:      TLabel;
    GroupBoxCommand: TGroupBox;
    LabelAngle:  TLabel;
    LabelX:      TLabel;
    LabelY:      TLabel;
    LabelZ:      TLabel;
    ButtonTransfer: TButton;
    ButtonRotate: TButton;
    ButtonScale: TButton;
    ButtonReset: TButton;
    GBEditValue00: TGBEditValue;
    GBEditValue01: TGBEditValue;
    GBEditValue02: TGBEditValue;
    GBEditValue03: TGBEditValue;
    GBEditValue10: TGBEditValue;
    GBEditValue11: TGBEditValue;
    GBEditValue12: TGBEditValue;
    GBEditValue13: TGBEditValue;
    GBEditValue20: TGBEditValue;
    GBEditValue21: TGBEditValue;
    GBEditValue22: TGBEditValue;
    GBEditValue23: TGBEditValue;
    GBEditValue30: TGBEditValue;
    GBEditValue31: TGBEditValue;
    GBEditValue32: TGBEditValue;
    GBEditValue33: TGBEditValue;
    GBEditValueTransferX: TGBEditValue;
    GBEditValueTransferY: TGBEditValue;
    GBEditValueTransferZ: TGBEditValue;
    GBEditValueRotateX: TGBEditValue;
    GBEditValueRotateY: TGBEditValue;
    GBEditValueRotateZ: TGBEditValue;
    GBEditValueScaleX: TGBEditValue;
    GBEditValueScaleY: TGBEditValue;
    GBEditValueScaleZ: TGBEditValue;
    GBEditValueRotateAngle: TGBEditValue;
    procedure RadioGroupModeClick(Sender: TObject);
    procedure ButtonResetClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonTransferClick(Sender: TObject);
    procedure ButtonRotateClick(Sender: TObject);
    procedure ButtonScaleClick(Sender: TObject);
    procedure EditMatrixChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListBoxInputNamesClick(Sender: TObject);
  private
     
    Matrix:     TGLMatrix;
    Tables:     TStringList;
    FEditArray: array[0..3, 0..3] of TGBEditValue;
    FOnUpdate:  TNotifyEvent;
    procedure UpdateEditValue;
    procedure DoUpdate(Sender: TObject);
     
    /// <summary>
    /// The procedure Transform for space transformation of data
    /// </summary>
    procedure Transform(Table: TTable);
    procedure ReadIniFile;
    procedure WriteIniFile;

  public
  end;

var
  fmMethodTransformation: TfmMethodTransformation;

//===============================================================
implementation
//===============================================================

uses
  dBase,
  cGlobals,
  cProfuns,
  uFileCreator;

{$R *.dfm}

procedure TfmMethodTransformation.RadioGroupModeClick(Sender: TObject);
begin
  case RadioGroupMode.ItemIndex of
    0:
    begin
      GBEditValue00.Enabled      := True;
      GBEditValue01.Enabled      := True;
      GBEditValue02.Enabled      := True;
      GBEditValue03.Enabled      := True;
      GBEditValue10.Enabled      := True;
      GBEditValue11.Enabled      := True;
      GBEditValue12.Enabled      := True;
      GBEditValue13.Enabled      := True;
      GBEditValue20.Enabled      := True;
      GBEditValue21.Enabled      := True;
      GBEditValue22.Enabled      := True;
      GBEditValue23.Enabled      := True;
      GBEditValue30.Enabled      := True;
      GBEditValue31.Enabled      := True;
      GBEditValue32.Enabled      := True;
      GBEditValue33.Enabled      := True;
      ButtonTransfer.Enabled   := False;
      ButtonRotate.Enabled     := False;
      ButtonScale.Enabled      := False;
      GBEditValueTransferX.Enabled := False;
      GBEditValueTransferY.Enabled := False;
      GBEditValueTransferZ.Enabled := False;
      GBEditValueRotateX.Enabled := False;
      GBEditValueRotateY.Enabled := False;
      GBEditValueRotateZ.Enabled := False;
      GBEditValueRotateAngle.Enabled := False;
      GBEditValueScaleX.Enabled  := False;
      GBEditValueScaleY.Enabled  := False;
      GBEditValueScaleZ.Enabled  := False;
    end;
    1:
    begin
      GBEditValue00.Enabled      := False;
      GBEditValue01.Enabled      := False;
      GBEditValue02.Enabled      := False;
      GBEditValue03.Enabled      := False;
      GBEditValue10.Enabled      := False;
      GBEditValue11.Enabled      := False;
      GBEditValue12.Enabled      := False;
      GBEditValue13.Enabled      := False;
      GBEditValue20.Enabled      := False;
      GBEditValue21.Enabled      := False;
      GBEditValue22.Enabled      := False;
      GBEditValue23.Enabled      := False;
      GBEditValue30.Enabled      := False;
      GBEditValue31.Enabled      := False;
      GBEditValue32.Enabled      := False;
      GBEditValue33.Enabled      := False;
      ButtonTransfer.Enabled   := True;
      ButtonRotate.Enabled     := True;
      ButtonScale.Enabled      := True;
      GBEditValueTransferX.Enabled := True;
      GBEditValueTransferY.Enabled := True;
      GBEditValueTransferZ.Enabled := True;
      GBEditValueRotateX.Enabled := True;
      GBEditValueRotateY.Enabled := True;
      GBEditValueRotateZ.Enabled := True;
      GBEditValueRotateAngle.Enabled := True;
      GBEditValueScaleX.Enabled  := True;
      GBEditValueScaleY.Enabled  := True;
      GBEditValueScaleZ.Enabled  := True;
    end;
  end;
end;

procedure TfmMethodTransformation.ButtonResetClick(Sender: TObject);
var
  I, J: integer;
begin
  Matrix    := IdentityHmgMatrix;
  FOnUpdate := nil;
  for I := 0 to 3 do
    for j := 0 to 3 do
    begin
      FEditArray[I, J] := FindComponent('EditValue' + IntToStr(I) +
        IntToStr(J)) as TGBEditValue;
      with FEditArray[I, J] do
      begin
        Tag      := I * 4 + J;
        asDouble := Matrix.V[I].V[J];
        EnabledConvert := True;
      end;
    end;
  FOnUpdate := DoUpdate;

  GBEditValueTransferX.asDouble := 0;
  GBEditValueTransferY.asDouble := 0;
  GBEditValueTransferZ.asDouble := 0;

  GBEditValueRotateX.asDouble     := 0;
  GBEditValueRotateY.asDouble     := 0;
  GBEditValueRotateZ.asDouble     := 1;
  GBEditValueRotateAngle.asDouble := 0;

  GBEditValueScaleX.asDouble := 1;
  GBEditValueScaleY.asDouble := 1;
  GBEditValueScaleZ.asDouble := 1;
  RadioGroupMode.ItemIndex := 1;
  RadioGroupModeClick(Self);
end;

procedure TfmMethodTransformation.Transform(Table: TTable);
const
  X = 0;
  Y = 1;
  Z = 2;
  W = 3;

var
  I:      integer;
  Vector: TGLVector;
  AffineVector: TAffineVector;
begin
  Table.Open;
  ProgressBar.Max := Table.RecordCount;
  ProgressBar.Min := 1;
  for I := 0 to Table.RecordCount - 1 do
  begin
    ProgressBar.Position := I;
    AffineVector.V[X]      := Table.FieldByName(fldX).AsFloat;
    AffineVector.V[Y]      := Table.FieldByName(fldY).AsFloat;
    AffineVector.V[Z]      := Table.FieldByName(fldZ).AsFloat;
    MakeVector(Vector, AffineVector);
    Vector.V[W] := 1;
    Vector    := VectorTransform(Vector, Matrix);
    if Abs(Vector.V[W]) < 0.00001 then
      Vector.V[W] := 0.00001;
    Table.Edit;
    Table.FieldByName(fldX).AsFloat :=
      RoundTo(Vector.V[X] / Vector.V[W], Precision);
    Table.FieldByName(fldY).AsFloat :=
      RoundTo(Vector.V[Y] / Vector.V[W], Precision);
    Table.FieldByName(fldZ).AsFloat :=
      RoundTo(Vector.V[Z] / Vector.V[W], Precision);
    Table.Post;
    Table.Next;
  end;
  Table.Close;
end;

procedure TfmMethodTransformation.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
  RadioGroupModeClick(Self);
  ListBoxRealAttribute.ItemIndex := 0;
end;

procedure TfmMethodTransformation.DoUpdate(Sender: TObject);
begin
  with Sender as TGBEditValue do
    Matrix.V[Tag div 4].V[Tag mod 4] := asDouble;
end;

procedure TfmMethodTransformation.UpdateEditValue;
var
  I, J: byte;

begin
  FOnUpdate := nil;
  try
    for I := 0 to 3 do
      for J := 0 to 3 do
        FEditArray[I][J].AsFloat := Matrix.V[I].V[J];
  finally
    FOnUpdate := DoUpdate;
  end;
end;

procedure TfmMethodTransformation.ButtonTransferClick(Sender: TObject);
var
  V: array[0..2] of double;
  AffineDblVector: TAffineDblVector;
  AffineFltVector: TAffineFltVector;
begin
  V[0] := GBEditValueTransferX.AsDouble;
  V[1] := GBEditValueTransferY.AsDouble;
  V[2] := GBEditValueTransferZ.AsDouble;
  AffineDblVector := MakeAffineDblVector(V);
  AffineFltVector := VectorAffineDblToFlt(AffineDblVector);

  Matrix := MatrixMultiply(Matrix, CreateTranslationMatrix(AffineFltVector));
  UpdateEditValue;
end;

procedure TfmMethodTransformation.ButtonRotateClick(Sender: TObject);
var
  V: array[0..2] of double;
  AffineDblVector: TAffineDblVector;
  AffineFltVector: TAffineFltVector;
begin
  V[0] := GBEditValueRotateX.AsDouble;
  V[1] := GBEditValueRotateY.AsDouble;
  V[2] := GBEditValueRotateZ.AsDouble;
  AffineDblVector := MakeAffineDblVector(V);
  AffineFltVector := VectorAffineDblToFlt(AffineDblVector);

  Matrix := MatrixMultiply(Matrix, CreateRotationMatrix(AffineFltVector,
    DegToRad(GBEditValueRotateAngle.asDouble)));
  UpdateEditValue;
end;

procedure TfmMethodTransformation.ButtonScaleClick(Sender: TObject);
var
  V: array[0..2] of double;
  AffineDblVector: TAffineDblVector;
  AffineFltVector: TAffineFltVector;
begin
  V[0] := GBEditValueScaleX.AsDouble;
  V[1] := GBEditValueScaleY.AsDouble;
  V[2] := GBEditValueScaleZ.AsDouble;
  AffineDblVector := MakeAffineDblVector(V);
  AffineFltVector := VectorAffineDblToFlt(AffineDblVector);

  Matrix := MatrixMultiply(Matrix, CreateScaleMatrix(AffineFltVector));
  UpdateEditValue;
end;

procedure TfmMethodTransformation.EditMatrixChange(Sender: TObject);
begin
  if Assigned(FOnUpdate) then
    FOnUpdate(Sender);
end;

procedure TfmMethodTransformation.ButtonOKClick(Sender: TObject);
begin
  inherited;
  if ModalResult = mrOk then
  begin
    dmBase.TableOutput.TableName := OutModelName;
    CopyFiles(InModelName + TableExt, OutModelName + TableExt,
      OutModelType, False);
    Transform(dmBase.TableOutput);
  end;
end;


procedure TfmMethodTransformation.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      RadioGroupMode.ItemIndex  := ReadInteger(Name, RadioGroupMode.Name, 0);
      GBEditValueRotateAngle.Text := ReadString(Name, GBEditValueRotateAngle.Name, '0');
    finally
      IniFile.Free;
    end;
end;

procedure TfmMethodTransformation.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteInteger(Name, RadioGroupMode.Name, RadioGroupMode.ItemIndex);
      WriteString(Name, GBEditValueRotateAngle.Name, GBEditValueRotateAngle.Text);
    finally
      IniFile.Free;
    end;
end;

procedure TfmMethodTransformation.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;

procedure TfmMethodTransformation.ListBoxInputNamesClick(Sender: TObject);
begin
  inherited;
  ListBoxRealAttribute.ItemIndex := -1;
end;

end.
