//
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//
(*
  EditValue component
*)

unit GBEditValue;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.CheckLst,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  
  GLS.VectorGeometry;


type
  TSetOfChar = set of char {'+'..'E'{};
  Float      = single;

const
  IntegerValidChar: TSetOfChar = ['0'..'9', '+', '-', #8];
  DoubleValidChar: TSetOfChar  = ['0'..'9', '+', '-', #8, 'E', 'e', '.'];

type
  TValueType = (vtInteger, {Press F1 on TVarRec}
    vtBoolean,
    vtChar,
    vtExtended,
    vtString,
    vtPointer,
    vtPChar,
    vtObject,
    vtClass,
    vtWideChar,
    vtPWideChar,
    vtAnsiString,
    vtCurrency,
    vtVariant,
    {Additional datatypes}
    vtDouble,
    vtFloat,
    vtSingle);

  TGBEditValue = class(TEdit)
  private
    FEnabledConvert: boolean;
    FError:      boolean;
    FValueType:  TValueType; {Press F1 on TVarRec}
    FValue:      variant;
    FPrevText:   TCaption;
    FValidChars: TSetOfChar;
    FOnChangeError: TNotifyEvent;
    FOnChangeValueType: TNotifyEvent;
    FOnChangeValue: TNotifyEvent;
    FPrecision: integer;
    procedure SetAsDouble(Value: double);
    procedure SetAsFloat(Value: Float);
    procedure SetAsInteger(Value: integer);
    procedure SetValueType(Value: TValueType);
    procedure SetError(Value: boolean);
    function GetAsFloat: Float;
    function GetAsDouble: double;
    function GetAsInteger: integer;
    function GetValueType: TValueType;
    property PrevText: TCaption Read FPrevText Write FPrevText;
  protected
    procedure ChangeError; dynamic;
    procedure ChangeValueType; dynamic;
    procedure ChangeValue; dynamic;
    procedure Change; override;
    procedure DoExit; override;
    procedure KeyPress(var Key: char); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure MakeInteger;
    procedure ValueToText;
    procedure TextToValue;
    property ValidChars: TSetOfChar Read FValidChars Write FValidChars;
  published
    property Parent;
    property EnabledConvert: boolean Read FEnabledConvert Write FEnabledConvert;
    property AsInteger: integer Read GetAsInteger Write SetAsInteger;
    property ValueType: TValueType Read GetValueType Write SetValueType;
    property AsDouble: double Read GetAsDouble Write SetAsDouble;
    property AsFloat: Float Read GetAsFloat Write SetAsFloat;
    property Error: boolean Read fError Write SetError;
    property OnChangeValue: TNotifyEvent Read FOnChangeValue Write FOnChangeValue;
    property OnChangeValueType: TNotifyEvent
      Read FOnChangeValueType Write FOnChangeValueType;
    property OnChangeError: TNotifyEvent Read FOnChangeError Write FOnChangeError;
    property Precision: integer Read FPrecision Write FPrecision;
  end;

procedure Register;

//=======================================================================
implementation
//=======================================================================

function RoundNDecimal(Value: double; N: integer = 2): double; stdcall;
begin
  Result := Round(Value * PowerInteger(10, N)) / PowerInt64(10, N);
end;

function MakeStr(const Args: array of const): string;
const
  BoolChars: array[boolean] of char = ('F', 'T');
var
  I: integer;
begin
  Result := '';
  for I := 0 to High(Args) do
    with Args[I] do
      case TValueType(VType) of
        vtInteger: Result  := Result + IntToStr(VInteger);
        vtBoolean: Result  := Result + BoolChars[VBoolean];
        vtChar: Result     := Result + VChar;
        vtExtended: Result := Result + FloatToStr(VExtended^);
        vtString: Result   := Result + VString^;
        vtPChar: Result    := Result + VPChar;
        vtObject: Result   := Result + VObject.ClassName;
        vtClass: Result    := Result + VClass.ClassName;
        vtAnsiString: Result := Result + string(VAnsiString);
        vtCurrency: Result := Result + CurrToStr(VCurrency^);
        vtVariant: Result  := Result + string(VVariant^);
      end;
end;

constructor TGBEditValue.Create(AOwner: TComponent);
begin
  inherited;
  EnabledConvert := True;
  ValueType := vtDouble;
  ValidChars := DoubleValidChar;
  PrevText := '0';
  Change;
end;

procedure TGBEditValue.MakeInteger;
begin
  EnabledConvert := True;
  ValueType := vtInteger;
  ValidChars := IntegerValidChar;
  PrevText := '0';
  Change;
end;

procedure TGBEditValue.SetAsInteger(Value: integer);
begin
  if AsInteger <> Value then
  begin
    fValue := Value;
    ChangeValue;
    ValueToText;
    PrevText := Text;
  end;
end;

function TGBEditValue.GetAsInteger: integer;
begin
  Result := fValue;
end;

procedure TGBEditValue.SetValueType(Value: TValueType);
begin
  if fValueType <> Value then
  begin
    fValueType := Value;
    ChangeValueType;
    ValueToText;
    Change;
  end;
end;

function TGBEditValue.GetValueType: TValueType;
begin
  Result := fValueType;
end;

procedure TGBEditValue.SetAsDouble(Value: double);
begin
  if asDouble <> Value then
  begin
    fValue := Value;
    ChangeValue;
    ValueToText;
    PrevText := Text;
  end;
end;

function TGBEditValue.GetAsDouble: double;
begin
  Result := fValue;
end;

procedure TGBEditValue.SetError(Value: boolean);
const
  ColorCase: array[False..True] of TColor = (clBlack, clRed);
  StyleCase: array[False..True] of TFontStyles = ([], [fsBold]);
var
  Temp: boolean;
begin
  if (Error <> Value) then
  begin
    fError := Value;
    Temp   := EnabledConvert;
    try
      EnabledConvert := False;
      Font.Color     := ColorCase[Error];
      Font.Style     := StyleCase[Error];
    finally
      EnabledConvert := Temp;
    end;

    ChangeError;
  end;
end;

procedure TGBEditValue.ChangeError;
begin
  if Assigned(FOnChangeError) then
    FOnChangeError(Self);
end;

procedure TGBEditValue.Change;
begin
  TextToValue;
  inherited;
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TGBEditValue.KeyPress(var Key: char);
begin
  if (Key in ValidChars) then
    inherited
  else if Key = #27 then
    ValueToText
  else
  begin
    Key := #0;
    beep;
  end;
end;

procedure TGBEditValue.ValueToText;
begin
  if EnabledConvert then
  begin
    EnabledConvert := False;
    try
      case ValueType of
        vtInteger: Text  := MakeStr([AsInteger]);
        vtExtended: Text := MakeStr([asDouble]);
        vtDouble: Text   := MakeStr([asDouble]);
        vtFloat: Text    := Format('%.*g', [Precision, AsFloat]);
        vtSingle: Text   := FloatToStr(AsFloat);
        else
          Text := MakeStr([asDouble]);
      end;
      Error := False;
    except
      Error := True;
    end;
    EnabledConvert := True;
  end;
end;

procedure TGBEditValue.TextToValue;
var
  LText: string;
begin
  if EnabledConvert then
  begin
    EnabledConvert := False;
    try
      LText := Text;
      if LText = '' then
        LText := '0';
      case ValueType of
        vtInteger: AsInteger := StrToInt(LText);
        vtExtended: asDouble := StrToFloat(LText);
        vtDouble: asDouble   := StrToFloat(LText);
        vtFloat: asDouble    := StrToFloat(LText);
        else
          asDouble := StrToFloat(LText);
      end;
      Error := False;
    except
      Error := True;
    end;
    EnabledConvert := True;
  end;
end;

procedure TGBEditValue.ChangeValue;
begin
  if Assigned(FOnChangeValue) then
    OnChangeValue(Self);
end;

procedure TGBEditValue.DoExit;
begin
  inherited;
  ValueToText;
end;

procedure TGBEditValue.ChangeValueType;
begin
  case ValueType of
    vtInteger: ValidChars  := IntegerValidChar;
    vtExtended: ValidChars := DoubleValidChar;
    vtDouble: ValidChars   := DoubleValidChar;
  end;
  if Assigned(FOnChangeValueType) then
    OnChangeValueType(Self);
end;

function TGBEditValue.GetAsFloat: Float;
begin
  Result := FValue;
end;

procedure TGBEditValue.SetAsFloat(Value: Float);
begin
  if Precision < 1000 then
    Value := RoundNDecimal(Value, Precision);
  if AsFloat <> Value then
  begin
    fValue := Value;
    ChangeValue;
    ValueToText;
    PrevText := Text;
  end;
end;


// ============= Register ===================
procedure Register;

begin
  RegisterComponents('Geoblock',[TGBEditValue]);
end;


end.
