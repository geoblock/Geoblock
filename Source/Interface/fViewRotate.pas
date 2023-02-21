//------------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//---------------------------------------------------------------------------
(* Options for rotating and changing distances in datasets *)
//-----------------------------------------------------------------------------

unit fViewRotate;

interface

uses
  System.SysUtils, 
  System.Classes,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs,
  Vcl.StdCtrls, 
  Vcl.ExtCtrls, 
  Vcl.Mask,

  
  fInitialDialog;

type
  TfmViewRotate = class(TfmInitialDialog)
    GroupBoxAzimuth: TGroupBox;
    lbMinBearing:   TLabel;
    lbMaxBearing:   TLabel;
    ScrollBarAzimuth: TScrollBar;
    MaskEditAzimuth: TMaskEdit;
    GroupBoxSlope:  TGroupBox;
    lbMinSlope:     TLabel;
    lbMaxSlope:     TLabel;
    ScrollBarSlope: TScrollBar;
    MaskEditSlope:  TMaskEdit;
    EditDistance:   TEdit;
    LabelDistance:  TLabel;
    procedure ScrollBarAzimuthChange(Sender: TObject);
    procedure MaskEditAzimuthChange(Sender: TObject);
    procedure ScrollBarSlopeChange(Sender: TObject);
    procedure MaskEditSlopeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
     
  public
     
  end;

var
  fmViewRotate: TfmViewRotate;

implementation

{$R *.DFM}

procedure TfmViewRotate.ScrollBarAzimuthChange(Sender: TObject);
begin
  MaskEditAzimuth.Text := IntToStr(ScrollBarAzimuth.Position);
end;

procedure TfmViewRotate.MaskEditAzimuthChange(Sender: TObject);
var
  S: string;
begin
  S := Trim(MaskEditAzimuth.Text);
  if (S = '') or (StrToInt(S) > ScrollBarAzimuth.Max) or
    (StrToInt(S) < ScrollBarAzimuth.Min) then
    MaskEditAzimuth.Text      := IntToStr(ScrollBarAzimuth.Position)
  else
    ScrollBarAzimuth.Position := StrToInt(S);
end;

procedure TfmViewRotate.ScrollBarSlopeChange(Sender: TObject);
begin
  MaskEditSlope.Text := IntToStr(ScrollBarSlope.Position);
end;

procedure TfmViewRotate.MaskEditSlopeChange(Sender: TObject);
var
  S: string;
begin
  S := Trim(MaskEditSlope.Text);
  if (S = '') or (StrToInt(S) > ScrollBarSlope.Max) or
    (StrToInt(S) < ScrollBarSlope.Min) then
    MaskEditSlope.Text      := IntToStr(ScrollBarSlope.Position)
  else
    ScrollBarSlope.Position := StrToInt(S);
end;

procedure TfmViewRotate.FormCreate(Sender: TObject);
begin
  inherited;
  MaskEditAzimuth.Text := IntToStr(ScrollBarAzimuth.Position);
  MaskEditSlope.Text   := IntToStr(ScrollBarSlope.Position);
end;

procedure TfmViewRotate.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
