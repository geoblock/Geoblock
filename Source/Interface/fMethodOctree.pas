//------------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//------------------------------------------------------------------------------
(* The dialog for octree model parameters *)

unit fMethodOctree;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.ImageList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ImgList,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Buttons,
  Vcl.ToolWin,
  Vcl.ExtCtrls,
  Vcl.Samples.Spin,
  fMethodDialog;

type
  TfmMethodOctree = class(TfmMethodDialog)
    lbLevels: TLabel;
    seNumberOfLevels: TSpinEdit;
    procedure ButtonOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMethodOctree: TfmMethodOctree;

implementation

{$R *.dfm}

procedure TfmMethodOctree.ButtonOKClick(Sender: TObject);
begin
  OutModelName := OutModelName + '_octree';
  // Construct Octree;
  // Write Octree in 3D Grid type
end;

end.
