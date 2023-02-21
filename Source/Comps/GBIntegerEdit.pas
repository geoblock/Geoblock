//------------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//------------------------------------------------------------------------------
(* Edit Box component for integers *)

unit GBIntegerEdit;

interface

uses
  Winapi.Windows,
  Winapi.Messages, 
  System.Classes, 
  System.Variants,
  System.Math, 
  Vcl.Controls, 
  Vcl.Forms, 

  Vcl.StdCtrls, 
  System.SysUtils, 
  Vcl.Dialogs, 

  GBFloatEdit;

type
  TGBIntegerEdit = class(TCustomEdit)
  private
    fIsNull : boolean;
    fLastText : string;
    fMaxValue : integer;
    fMinValue : integer;
    fReturnStop : boolean;
    fUseMinBound : boolean;
    fUseMaxBound : boolean;
    fValidate : TGBValidateOption;
    fValue : integer;
  protected
    function ConvertText(sValue:string):integer;
    procedure DoEnter;override;
		function GetText: string;
		procedure KeyPress(var Key: Char); override;
		procedure Refresh; virtual;
		procedure SetReturnStop(bValue:boolean);
		procedure SetText(sValue: String);
    procedure SetValidate(aGE:TGBValidateOption);
    procedure SetValue(iValue:integer);
  	procedure WMKillFocus(VAR Message: TWMSetFocus); message WM_KILLFOCUS;
  public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
    property IsNull : boolean read fIsNull;
    property Value : integer read fValue write SetValue;
  published
		property Text : string read GetText write SetText;
    property MinValue : integer read fMinValue write fMinValue;
    property MaxValue : integer read fMaxValue write fMaxValue;
		property ReturnStop:Boolean read fReturnStop write SetReturnStop;
    property UseMinBound : boolean read fUseMinBound write fUseMinBound;
    property UseMaxBound : boolean read fUseMaxBound write fUseMaxBound;
    property Validate : TGBValidateOption read fValidate write SetValidate;

// expose from TCustomEdit;
    property Align;

    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation

// ----- TGBIntegerEdit.ConvertText ----------------------------------------------
function TGBIntegerEdit.ConvertText(sValue:string):integer;

var
  s:string;

begin
// premultiply the number if no mantissa present
  s := svalue;
  if (sValue = '') then
  begin
    fIsNull := true;
    result := 0
  end else
  begin
    try
      result := StrToInt(s);
      fIsNull := false;
    except on EConvertError do
      begin
        result := fValue; // revert back to old value
        fIsNull := false;
      end;
    end;
  end;
end;
// ------ TGBIntegerEdit.DoEnter -------------------------------------------------
procedure TGBIntegerEdit.DoEnter;

begin
  if ISNull then
    inherited Text := ''
  else
    inherited Text := IntToStr(value);

  inherited DoEnter;
end;
// ----- TGBIntegerEdit.GetText --------------------------------------------------
function TGBIntegerEdit.GetText: string;

begin
  result := inherited Text;
end;
// ----- TGBIntegerEdit.KeyPress -------------------------------------------------
procedure TGBIntegerEdit.KeyPress(VAR Key: Char);

var
 iTemp:integer;
 sTemp:string;

// only allow numbers, 0..9, e,E
function CharIsNumber(const C: AnsiChar): Boolean;

begin
  Result := (C in ['0'..'9']) or (C in ['-','+','e','E','$','a','b','c','d','f']);
end;

begin
// allow blanks - for null
  if (Ord(Key) = Ord(' ')) then
    Key := #0
  else if (Ord(Key)) < Ord(' ') then
  begin
    inherited KeyPress(Key);
    exit;
  end else
  if (Key = Char(VK_RETURN)) then
  begin
    if ReturnStop <> true then
    begin
      stemp:=inherited Text;
      itemp := ConvertText(stemp);
      if not IsNull then
      begin
        Value := itemp;
        SelectAll;
      end else
      begin
        Refresh;
      end;

    end else
// return stop - ie a tabstop
    begin
{ do an equivalent tab out - use the screen global var ie if the key is pressed
then the parent on the must be focused I think}
     Key := #0;
     Screen.ActiveForm.Perform(WM_NEXTDLGCTL, 0, 0);  //method of form
    end;
	end else
  begin
// if not a screen number return null
    if not CharIsNumber(AnsiChar(Key)) then
      Key := #0;
  end;
  inherited KeyPress(Key);
end;
// ----- TGBIntegerEdit.Refresh ------------------------------------------------
procedure TGBIntegerEdit.Refresh;

begin
  if IsNull then
    inherited Text := ''
  else
    inherited Text := IntToStr(Value);

  fLastText := inherited Text;
end;
// ----- TGBIntegerEdit.SetReturnStop ------------------------------------------
procedure TGBIntegerEdit.SetReturnStop(bValue:boolean);
// only allow this if tabstop is true
begin
 if bValue <> fReturnStop then fReturnStop:= (bValue and TabStop);
end;
// ----- TGBIntegerEdit.SetText ------------------------------------------------
procedure TGBIntegerEdit.SetText(sValue: String);

begin
	if not (csLoading in ComponentState) then
  begin
    if (sValue <> '') then
      Value := ConvertText(sValue)
    else
    begin
      Value := 0;
      fIsNull := true;
    end;
  end;
  if IsNull then
    inherited Text := ''
  else
    inherited Text := IntToStr(Value);
  fLastText := inherited Text;
end;
// ----- TGBIntegerEdit.SetValidate --------------------------------------------
procedure TGBIntegerEdit.SetValidate(aGE:TGBValidateOption);

begin
  if (aGE <> fValidate) then
    fValidate := aGE;
end;
// ----- TGBIntegerEdit.SetValue -----------------------------------------------
procedure TGBIntegerEdit.SetValue(iValue:integer);
var
  bValid : boolean;

begin
// check for NAN
  if IsNan(iValue) then
  begin
    fValue := 0;
    fIsNull := true;
  end else
  begin


// check for validity
    bValid := true;
    if (fValidate = vdSilent) or (fValidate = vdWarning) then
    begin
      if (iValue < fMinValue) and fUseMinBound then
        bValid := false;
      if (iValue > fMaxValue) and fUseMaxBound then
        bValid := false;
    end;

    if bvalid then
    begin
      fIsNull := false;
      fValue := iValue;
    end else
    begin

    end;
  end;
  Refresh;
end;
// ----- TGBIntegerEdit.WMKillFocus --------------------------------------------
procedure TGBIntegerEdit.WMKillFocus(Var Message: TWMSetFocus);

var
 itemp:integer;
 stemp:string;

begin
  try
    stemp:=inherited Text;
    if (stemp <> fLastText) then
    begin
      itemp := ConvertText(sTemp);
      if not IsNull then
        Value := itemp
      else begin
//       if (InputWarning) and not (AlreadyWarned) then
//         MessageDlg(strtemp+ ' is invalid!', mtInformation, [mbOK], 0);
        Refresh;
      end;
    end;
    inherited;
  except
    if inherited Text <> '' then
    begin
      Self.SelectAll;
      Self.SetFocus;
    end else
    begin
      Value := 0;
      fIsNull := true;
      inherited;
    end;
  end;
end;
// ----- TGBIntegerEdit.Create -------------------------------------------------
constructor TGBIntegerEdit.Create(AOwner: TComponent);

begin
	inherited Create(AOwner);
  fValidate := vdNone;
	fMinValue := 0;
	fMaxValue := 0;
  fUseMinBound := false;
  fUseMaxBound := false;
  fIsNull := false;
  fValue := 0;
  fLastText := inherited Text;
end;
// ----- TGBIntegerEdit.Destroy ------------------------------------------------
destructor TGBIntegerEdit.Destroy;

begin
	inherited Destroy;
end;

// ============= Register ===================
procedure Register;

begin
  RegisterComponents('Geoblock',[TGBIntegerEdit]);
end;

end.
