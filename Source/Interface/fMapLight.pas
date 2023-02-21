//-----------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//----------------------------------------------------------------------------
(* The dialog for MapLight and Position parameters *)

unit fMapLight;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Samples.Spin,
  Vcl.ExtCtrls,
  
  fInitialDialog,
  dDialogs;


type
  TfmMapLighting = class(TfmInitialDialog)
    GroupBoxPosition: TGroupBox;
    Label2:    TLabel;
    Label3:    TLabel;
    Label5:    TLabel;
    SpinEditX: TSpinEdit;
    SpinEditY: TSpinEdit;
    SpinEditZ: TSpinEdit;
    GroupBoxReflection: TGroupBox;
    LabelAmbient: TLabel;
    LabelDifusion: TLabel;
    LabelSpecular: TLabel;
    PanelAmbient: TPanel;
    PanelDiffuse: TPanel;
    PanelSpecular: TPanel;
    CheckBoxLightColor: TCheckBox;
    procedure PanelReflectionEnter(Sender: TObject);
    procedure PanelReflectionMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  private
     
  public
     
  end;

var
  fmMapLighting: TfmMapLighting;

//=========================================================================
implementation
//=========================================================================

{$R *.DFM}

procedure TfmMapLighting.PanelReflectionEnter(Sender: TObject);
begin
  with (Sender as TPanel) do
  begin
    //    Canvas.DrawFocusRect(
  end;
end;

procedure TfmMapLighting.PanelReflectionMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  with (Sender as TPanel), dmDialogs do
  begin
    SetFocus;
    ColorDialog.Color := Color;
    if ColorDialog.Execute then
      Color := ColorDialog.Color;
  end;
end;

end.
