//
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//

unit GBEditRange;

(* GBEditRange component *)

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.CheckLst,
  Vcl.Controls, 
  Vcl.Forms,
  Vcl.Dialogs, 
  GBEditValue;

const
  {ScaleState}
  ssStart  = $10;
  ssFinish = $08;
  ssLength = $04;
  ssStep   = $02;
  ssStepNumber = $01;
  ssNone   = $00;
  ss00 = 00;
  ss11 = 11;
  ss13 = 13;
  ss14 = 14;
  ss15 = 15;
  ss19 = 19;
  ss21 = 21;
  ss22 = 22;
  ss23 = 23;
  ss25 = 25;
  ss26 = 26;
  ss30 = 30;

type
  TGBRangeState = ss11..ss30;
  TGBEditRange  = class;

  TGBEditScaleValue = class(TGBEditValue)
  private
    FOwnerScale: TGBEditRange;
    procedure SetOwnerScale(Value: TGBEditRange);
    function GetOwnerScale: TGBEditRange;
  protected
    property OwnerScale: TGBEditRange Read GetOwnerScale Write SetOwnerScale;
    procedure ChangeValue; override;
    procedure doEnter; override;
    procedure doExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Scale: TGBEditRange Read FOwnerScale;
  end;

  TGBEditScaleInteger = class(TGBEditScaleValue)
  private
    procedure SetValue(AValue: integer);
    function GetValue: integer;
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Value: integer Read GetValue Write SetValue;
  end;

  TGBEditScaleDouble = class(TGBEditScaleValue)
  private
    procedure SetValue(AValue: double);

    function GetValue: double;
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Value: double Read GetValue Write SetValue;
  end;

  TGBEditRange = class(TComponent)
  private
    FEditStart, FEditFinish, FEditLength, FEditStep: TGBEditScaleDouble;
    FEditStepNumber: TGBEditScaleInteger;
    FStart, FFinish, FLength, FStep: double;
    FStepNumber: integer;
    FState: TGBRangeState;
    FCalculation: boolean;
    FCurrentEdit: TGBEditValue;
    procedure SetStart(Value: double);
    procedure SetFinish(Value: double);
    procedure SetLength(Value: double);
    procedure SetStep(Value: double);
    procedure SetStepNumber(Value: integer);
    procedure SetEditStart(Value: TGBEditScaleDouble);
    procedure SetEditFinish(Value: TGBEditScaleDouble);
    procedure SetEditLength(Value: TGBEditScaleDouble);
    procedure SetEditStep(Value: TGBEditScaleDouble);
    procedure SetEditStepNumber(Value: TGBEditScaleInteger);
    procedure SetCurrentEdit(Value: TGBEditValue);
    procedure SetState(Value: TGBRangeState);
    procedure SetCalculation(Value: boolean);
    function GetStart: double;
    function GetFinish: double;
    function GetLength: double;
    function GetStep: double;
    function GetStepNumber: integer;
    function GetEditStart: TGBEditScaleDouble;
    function GetEditFinish: TGBEditScaleDouble;
    function GetEditLength: TGBEditScaleDouble;
    function GetEditStep: TGBEditScaleDouble;
    function GetEditStepNumber: TGBEditScaleInteger;
    function GetCurrentEdit: TGBEditValue;
    function GetState: TGBRangeState;
    function GetCalculation: boolean;
  protected
    procedure SetName(const NewName: TComponentName); override;
    procedure ExitEdit(Sender: TObject);
    procedure ChangeValue(Sender: TObject);
  public
    procedure RemoveEdit(Edit: TGBEditValue);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Start: double read GetStart write SetStart;
    property Finish: double read GetFinish write SetFinish;
    property Length: double read GetLength write SetLength;
    property Step: double read GetStep write SetStep;
    property StepNumber: integer read GetStepNumber write SetStepNumber default 100;
    property EditStart: TGBEditScaleDouble read GetEditStart write SetEditStart;
    property EditFinish: TGBEditScaleDouble read GetEditFinish write SetEditFinish;
    property EditLength: TGBEditScaleDouble read GetEditLength write SetEditLength;
    property EditStep: TGBEditScaleDouble read GetEditStep write SetEditStep;
    property EditStepNumber: TGBEditScaleInteger read GetEditStepNumber write SetEditStepNumber;
    property CurrentEdit: TGBEditValue read GetCurrentEdit write SetCurrentEdit;
    property State: TGBRangeState read GetState write SetState default ss11;
    property Calculation: boolean read GetCalculation write SetCalculation default False;
  end;

//=================================================================
implementation
//=================================================================

function GetIndexOf(const Arr: array of const): longint; {See TVarRec}
var
  i: longint;
begin
  Result := 0;
  I      := 0;
  while (Result < 1) and (I < High(Arr)) do
  begin
    Inc(I);
    if (Arr[0].VPointer = Arr[I].VPointer) then
      Result := I;
  end;
end;

//---------------------------------------
// TGBEditScaleValue
//---------------------------------------

constructor TGBEditScaleValue.Create(AOwner: TComponent);
begin
  if (AOwner is TGBEditRange) then
  begin
    inherited Create(AOwner.Owner);
    Parent      := ((AOwner as TGBEditRange).Owner as TWinControl);
    FOwnerScale := (AOwner as TGBEditRange);
  end
  else
  begin
    inherited Create(AOwner);
    Parent := (AOwner as TWinControl);
  end;
end;

destructor TGBEditScaleValue.Destroy;
begin
  if Assigned(FOwnerScale) then
    OwnerScale.RemoveEdit(Self);
  inherited;
end;

procedure TGBEditScaleValue.DoEnter;
begin
  inherited;
  if Assigned(FOwnerScale) then
    OwnerScale.CurrentEdit := Self;
end;

procedure TGBEditScaleValue.doExit;
begin
  inherited;
  if Assigned(FOwnerScale) then
    OwnerScale.ExitEdit(Self);
end;

procedure TGBEditScaleValue.ChangeValue;
begin
  inherited;
  if Assigned(FOwnerScale) then
    OwnerScale.ChangeValue(Self);
end;

procedure TGBEditScaleValue.SetOwnerScale(Value: TGBEditRange);
begin
  if (OwnerScale <> Value) then
  begin
    if Assigned(FOwnerScale) then
      OwnerScale.RemoveEdit(Self);
    FOwnerScale := Value;
  end;
end;

function TGBEditScaleValue.GetOwnerScale: TGBEditRange;
begin
  Result := FOwnerScale;
end;

//------------------------------
// TGBEditScaleInteger
//------------------------------

constructor TGBEditScaleInteger.Create(AOwner: TComponent);
begin
  inherited;
  ValueType := vtInteger;
end;

procedure TGBEditScaleInteger.SetValue(AValue: integer);
begin
  if (AsInteger <> AValue) then
  begin
    AsInteger := AValue;
  end;
end;

function TGBEditScaleInteger.GetValue: integer;
begin
  Result := AsInteger;
end;

//---------------------------------------
// TGBEditScaleDouble
//---------------------------------------

constructor TGBEditScaleDouble.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TGBEditScaleDouble.SetValue(AValue: double);
begin
  if (asDouble <> AValue) then
  begin
    asDouble := AValue;
  end;
end;

function TGBEditScaleDouble.GetValue: double;
begin
  Result := asDouble;
end;

//---------------------------------------
// TGBEditRange
//---------------------------------------

constructor TGBEditRange.Create(AOwner: TComponent);
begin
  inherited;
  try
    Calculation := True;

{
    EditStart     :=TGBEditScaleDouble.Create(Self);
    EditFinish    :=TGBEditScaleDouble.Create(Self);
    EditLength    :=TGBEditScaleDouble.Create(Self);
    EditStep      :=TGBEditScaleDouble.Create(Self);
    EditStepNumber:=TGBEditScaleInteger.Create(Self);
{}
    CurrentEdit := EditStart;
    Start  := 0;
    Finish := 1000;
    Length := 1000;
    Step   := 10;
    StepNumber := 100;
  finally
    Calculation := False;
  end;
end;

destructor TGBEditRange.Destroy;
begin
  //{
  EditStart  := nil;
  EditFinish := nil;
  EditLength := nil;
  EditStep   := nil;
  EditStepNumber := nil;
(*{}
  EditStart.Free;
  EditFinish.Free;
  EditLength.Free;
  EditStep.Free;
  EditStepNumber.Free;
(**)
  inherited;
end;

procedure TGBEditRange.SetName(const NewName: TComponentName);
begin
  inherited;
  if Assigned(FEditStart) then
    EditStart.Name := Name + 'edStart';
  if Assigned(FEditFinish) then
    FEditFinish.Name := Name + 'edFinish';
  if Assigned(FEditLength) then
    FEditLength.Name := Name + 'edLength';
  if Assigned(FEditStep) then
    FEditStep.Name := Name + 'edStep';
  if Assigned(FEditStepNumber) then
    FEditStepNumber.Name := Name + 'edStepNumber';
end;

procedure TGBEditRange.ExitEdit(Sender: TObject);
begin
  try
    Calculation := True;
    if State in [13, 21, 25] then
      case GetIndexOf([Sender, FEditStart, FEditFinish,
          FEditLength, FEditStep]) of
        1: Start     := Finish - Length;
        2: Finish    := Start + Length;
        3, 4: Length := abs(Finish - Start);
      end;
  finally
    Calculation := False;
  end;
end;

procedure TGBEditRange.ChangeValue(Sender: TObject);
begin
  case GetIndexOf([Sender, FEditStart, FEditFinish, FEditLength,
      FEditStep, FEditStepNumber]) of
    1: Start  := EditStart.Value;
    2: Finish := EditFinish.Value;
    3: Length := EditLength.Value;
    4: Step   := EditStep.Value;
    5: StepNumber := EditStepNumber.Value;
  end;
end;

procedure TGBEditRange.RemoveEdit(Edit: TGBEditValue);
begin
  case GetIndexOf([Edit, FEditStart, FEditFinish, FEditLength,
      FEditStep, FEditStepNumber]) of
    1: EditStart  := nil;
    2: EditFinish := nil;
    3: EditLength := nil;
    4: EditStep   := nil;
    5: EditStepNumber := nil;
  end;
end;

procedure TGBEditRange.SetStart(Value: double);
begin
  if Start <> Value then
  begin
    FStart := Value;
    if Assigned(FEditStart) then
      EditStart.Value := Start;
    if not Calculation then
    begin
      try
        Calculation := True;
        case State of
          ss23:
          begin
            Finish := Start + Length;
          end;
          ss25:
          begin
            StepNumber := abs(trunc((Finish - Start) / Step));
            Length     := abs(Step * StepNumber);
          end;
          ss26:
          begin
            Length := abs(Finish - Start);
            Step   := abs(Length / StepNumber);
          end;
        end;
      finally
        Calculation := False;
      end;
    end;
  end;
end;

procedure TGBEditRange.SetFinish(Value: double);
begin
  if Finish <> Value then
  begin
    FFinish := Value;
    if Assigned(FEditFinish) then
      EditFinish.Value := Finish;
    if not Calculation then
    begin
      try
        Calculation := True;
        case State of
          ss15:
          begin
            Start := Finish - Length;
          end;
          ss25:
          begin
            StepNumber := abs(trunc((Finish - Start) / Step));
            Length     := abs(Step * StepNumber);
          end;
          ss26:
          begin
            Length := abs(Finish - Start);
            Step   := abs(Length / StepNumber);
          end;
        end;
      finally
        Calculation := False;
      end;
    end;
  end;
end;

procedure TGBEditRange.SetLength(Value: double);
begin
  if Length <> Value then
  begin
    FLength := Value;
    if Assigned(FEditLength) then
      EditLength.Value := Length;
    if not Calculation then
    begin
      try
        Calculation := True;
        case State of
          ss13:
          begin
            StepNumber := abs(trunc(Length / Step));
            Start      := Finish - abs(Step * StepNumber);
          end;
          ss14:
          begin
            Step  := abs(Length / StepNumber);
            Start := Finish - Length;
          end;
          ss21:
          begin
            StepNumber := abs(trunc(Length / Step));
            Finish     := Start + abs(Step * StepNumber);
          end;
          ss22:
          begin
            Step   := abs(trunc(Length / StepNumber));
            Finish := Start + Length;
          end;
        end;
      finally
        Calculation := False;
      end;
    end;
  end;
end;

procedure TGBEditRange.SetStep(Value: double);
begin
  if Step <> Value then
  begin
    FStep := Value;
    if Assigned(FEditStep) then
      EditStep.Value := Step;
    if not Calculation then
    begin
      try
        Calculation := True;
        case State of
          ss21:
          begin
            StepNumber := abs(trunc(Length / Step));
            Finish     := Start + abs(Step * StepNumber);
          end;
          ss19:
          begin
            Length := abs(Step * StepNumber);
            Finish := Start + Length;
          end;
          ss11:
          begin
            Length := abs(Step * StepNumber);
            Start  := Finish - Length;
          end;
          ss13:
          begin
            StepNumber := abs(trunc(Length / Step));
            Start      := Finish - abs(Step * StepNumber);
          end;
        end;
      finally
        Calculation := False;
      end;
    end;
  end;
end;

procedure TGBEditRange.SetStepNumber(Value: integer);
begin
  if StepNumber <> Value then
  begin
    FStepNumber := Value;
    if Assigned(FEditStepNumber) then
      EditStepNumber.Value := StepNumber;
    if not Calculation then
    begin
      try
        Calculation := True;
        case State of
          ss30:
          begin
            Step := Length / StepNumber;
          end;
          ss19:
          begin
            Length := abs(Step * StepNumber);
            Finish := Start + Length;
          end;
          ss11:
          begin
            Length := abs(Step * StepNumber);
            Start  := Finish - Length;
          end;
        end;
      finally
        Calculation := False;
      end;
    end;
  end;
end;

procedure TGBEditRange.SetEditStart(Value: TGBEditScaleDouble);
var
  OldEdit: TGBEditScaleValue;
begin
  if EditStart <> Value then
  begin
    OldEdit    := EditStart;
    FEditStart := Value;
    if OldEdit = CurrentEdit then
      CurrentEdit := Value;
    if Assigned(OldEdit) then
      OldEdit.OwnerScale := nil;
    if Assigned(FEditStart) then
    begin
      EditStart.OwnerScale := Self;
      EditStart.Value      := Start;
    end;
  end;
end;

procedure TGBEditRange.SetEditFinish(Value: TGBEditScaleDouble);
var
  OldEdit: TGBEditScaleValue;
begin
  if EditFinish <> Value then
  begin
    OldEdit     := EditFinish;
    FEditFinish := Value;
    if OldEdit = CurrentEdit then
      CurrentEdit := Value;
    if Assigned(OldEdit) then
      OldEdit.OwnerScale := nil;
    if Assigned(FEditFinish) then
    begin
      EditFinish.OwnerScale := Self;
      EditFinish.Value      := Finish;
    end;
  end;
end;

procedure TGBEditRange.SetEditLength(Value: TGBEditScaleDouble);
var
  OldEdit: TGBEditScaleValue;
begin
  if EditLength <> Value then
  begin
    OldEdit     := EditLength;
    FEditLength := Value;
    if OldEdit = CurrentEdit then
      CurrentEdit := Value;
    if Assigned(OldEdit) then
      OldEdit.OwnerScale := nil;
    if Assigned(FEditLength) then
    begin
      EditLength.OwnerScale := Self;
      EditLength.Value      := Length;
    end;
  end;
end;

procedure TGBEditRange.SetEditStep(Value: TGBEditScaleDouble);
var
  OldEdit: TGBEditScaleValue;
begin
  if EditStep <> Value then
  begin
    OldEdit   := EditStep;
    FEditStep := Value;
    if OldEdit = CurrentEdit then
      CurrentEdit := Value;
    if Assigned(OldEdit) then
      OldEdit.OwnerScale := nil;
    if Assigned(FEditStep) then
    begin
      EditStep.OwnerScale := Self;
      EditStep.Value      := Step;
    end;
  end;
end;

procedure TGBEditRange.SetEditStepNumber(Value: TGBEditScaleInteger);
var
  OldEdit: TGBEditScaleValue;
begin
  if EditStepNumber <> Value then
  begin
    OldEdit := EditStepNumber;
    FEditStepNumber := Value;
    if OldEdit = CurrentEdit then
      CurrentEdit := Value;
    if Assigned(OldEdit) then
      OldEdit.OwnerScale := nil;
    if Assigned(FEditStepNumber) then
    begin
      EditStepNumber.OwnerScale := Self;
      EditStepNumber.Value      := StepNumber;
    end;
  end;
end;

procedure TGBEditRange.SetCurrentEdit(Value: TGBEditValue);
begin
  if CurrentEdit <> Value then
  begin
    case GetIndexOf([Value, EditStart, EditFinish, EditLength,
        EditStep, EditStepNumber]) of
      1:
      begin
        FCurrentEdit := Value;
        State := 23;
      end;
      2:
      begin
        FCurrentEdit := Value;
        State := 25;
      end;
      3:
      begin
        FCurrentEdit := Value;
        State := 21;
      end;
      4:
      begin
        FCurrentEdit := Value;
        State := 19;
      end;
      5:
      begin
        FCurrentEdit := Value;
        State := 19;
      end;
    end;
  end;
end;

procedure TGBEditRange.SetState(Value: TGBRangeState);
begin
  if State <> Value then
  begin
    FState := Value;
  end;
end;

procedure TGBEditRange.SetCalculation(Value: boolean);
begin
  if Calculation <> Value then
  begin
    FCalculation := Value;
  end;
end;

function TGBEditRange.GetStart: double;
begin
  Result := FStart;
end;

function TGBEditRange.GetFinish: double;
begin
  Result := FFinish;
end;

function TGBEditRange.GetLength: double;
begin
  Result := FLength;
end;

function TGBEditRange.GetStep: double;
begin
  Result := FStep;
end;

function TGBEditRange.GetStepNumber: integer;
begin
  Result := FStepNumber;
end;

function TGBEditRange.GetEditStart: TGBEditScaleDouble;
begin
  Result := FEditStart;
end;

function TGBEditRange.GetEditFinish: TGBEditScaleDouble;
begin
  Result := FEditFinish;
end;

function TGBEditRange.GetEditLength: TGBEditScaleDouble;
begin
  Result := FEditLength;
end;

function TGBEditRange.GetEditStep: TGBEditScaleDouble;
begin
  Result := FEditStep;
end;

function TGBEditRange.GetEditStepNumber: TGBEditScaleInteger;
begin
  Result := FEditStepNumber;
end;

function TGBEditRange.GetCurrentEdit: TGBEditValue;
begin
  Result := FCurrentEdit;
end;

function TGBEditRange.GetState: TGBRangeState;
begin
  Result := FState;
end;

function TGBEditRange.GetCalculation: boolean;
begin
  Result := FCalculation;
end;

end.
