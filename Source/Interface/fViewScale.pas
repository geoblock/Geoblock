//------------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//---------------------------------------------------------------------------
(* Recalculating scales for a dataset in tabes *)

unit fViewScale;

interface

uses
  Winapi.Windows, 
  Winapi.Messages, 
  System.SysUtils, 
  System.Classes, 
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs,
  Vcl.StdCtrls, 
  Vcl.ExtCtrls,
  
  fInitialDialog ;

type
  TfmViewScale = class(TfmInitialDialog)
    RadioGroupScale: TRadioGroup;
    EditScale: TEdit;
    procedure EditScaleEnter(Sender: TObject);
    procedure EditScaleExit(Sender: TObject);
  private
     
  public
     
    function GetScaleRatio: double;
    procedure SetScaleRatio(const Value: double);
    property ScaleRatio: double Read GetScaleRatio Write SetScaleRatio;
  end;

var
  fmViewScale: TfmViewScale;

implementation

{$R *.DFM}

function TfmViewScale.GetScaleRatio: double;
begin
  if RadioGroupScale.ItemIndex = 9 then
    Result := Round(StrToFloat(EditScale.Text))
  else
    Result := Round(StrToFloat(RadioGroupScale.Items[RadioGroupScale.ItemIndex]));
end;

procedure TfmViewScale.SetScaleRatio(const Value: double);
var
  I: integer;
begin
  I := 0;
  while (I < RadioGroupScale.Items.Count - 1) and
    (StrToFloat(RadioGroupScale.Items[I]) <> Value) do
    Inc(I);
  RadioGroupScale.ItemIndex := I;
  EditScale.Text := FloatToStr(Value);
end;

procedure TfmViewScale.EditScaleEnter(Sender: TObject);
begin
  RadioGroupScale.ItemIndex := RadioGroupScale.Items.Count - 1;
end;


procedure TfmViewScale.EditScaleExit(Sender: TObject);
begin
  try
    EditScale.Text := FloatToStr(StrToFloat(EditScale.Text)); //validate number
  except
    EditScale.Text := '100'; //correct if invalid
  end;
end;

end.
