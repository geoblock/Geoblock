//----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//----------------------------------------------------------------------------

{! Help Context property }

unit GBHelpContext;

interface

uses
  System.Classes,
  System.SysUtils, 
  DesignIntf, 
  DesignEditors,  
  Vcl.Dialogs;

type
  THelpContextProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(AddValue: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    function GetValue: string; override;
    function GetValueFromExpression(const Expression: string): string;
  private
    FileIDH:      TextFile;
    FileIDHNames: TextFile;
  end;

//=====================================================================
implementation
//=====================================================================

const
  SFiles_IDH: string = 'D:\Geoblock\Help\IdhFiles.txt';

function SwapStr(S: string): string;
var
  iBeg: integer;
begin
  Result := S;
  iBeg   := Pos('=', S);
  if iBeg > 0 then
  begin
    Result := copy(S, iBeg + 1, Length(S) - iBeg) + '=' + copy(S, 1, iBeg - 1);
  end;
end;


function THelpContextProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect, paSortList];
end;

function THelpContextProperty.GetValue: string;
var
  Str: string;
  IDH_ForValueExist: boolean;
begin
  IDH_ForValueExist := False;
  Result := inherited GetValue;
  try
    AssignFile(FileIDHNames, SFiles_IDH);
    Reset(FileIDHNames);
    try
      while (not EOF(FileIDHNames)) and (not IDH_ForValueExist) do
      begin
        Readln(FileIDHNames, Str);
        if not FileExists(Str) then
          continue;
        AssignFile(FileIDH, Str);
        Reset(FileIDH);
        try
          while (not EOF(FileIDH)) and (not IDH_ForValueExist) do
          begin
            Readln(FileIDH, Str);
            System.Delete(Str, 1, 8);         //delete #define
            Str := Trim(Str);
            IDH_ForValueExist := Result = GetValueFromExpression(Str);
          end;
          if IDH_ForValueExist then
            Result := Str;
        finally
          CloseFile(FileIDH);
        end;
      end;
    finally
      CloseFile(FileIDHNames);
    end;
  except
  end;
end;

function THelpContextProperty.GetValueFromExpression(const Expression: string): string;
var
  MidResult: string;
  EqualPos:  integer;
begin
  Result    := '0';
  MidResult := Trim(Expression);
  if MidResult <> '' then
  begin
    EqualPos := LastDelimiter(' ', MidResult);
    if EqualPos > 0 then
    begin
      try
        MidResult := IntToStr(StrToInt(Copy(MidResult, 1, EqualPos - 1)));
      except
        MidResult := Copy(MidResult, EqualPos + 1, Length(MidResult) - EqualPos);
      end;
    end;
    MidResult := Trim(MidResult);
    if MidResult <> '' then
      Result := MidResult;
  end;
end;

procedure THelpContextProperty.GetValues(AddValue: TGetStrProc);
var
  Str:   string;
  IDH_ForValueExist: boolean;
  Value: string;
begin
  IDH_ForValueExist := False;
  Value := GetValue;
  try
    AssignFile(FileIDHNames, SFiles_IDH);
    Reset(FileIDHNames);
    try
      while not EOF(FileIDHNames) do
      begin
        Readln(FileIDHNames, Str);
        if not FileExists(Str) then
          continue;
        AssignFile(FileIDH, Str);
        Reset(FileIDH);
        try
          while not EOF(FileIDH) do
          begin
            Readln(FileIDH, Str);
            System.Delete(Str, 1, 8);         //delete #define
            Str := Trim(Str);
            if Str <> '' then
            begin
              AddValue(Str);
              //              Str:=SwapStr(Str);
              //              AddValue(Str);
            end;
            IDH_ForValueExist :=
              IDH_ForValueExist or (GetValueFromExpression(Value) =
              GetValueFromExpression(Str));
          end;
        finally
          CloseFile(FileIDH);
        end;
      end;
    finally
      CloseFile(FileIDHNames);
    end;
  except
  end;
  if not IDH_ForValueExist then
    AddValue(GetValue);
end;

procedure THelpContextProperty.SetValue(const Value: string);
begin
  inherited SetValue(GetValueFromExpression(Value));
end;

end.
