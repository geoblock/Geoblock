//------------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//------------------------------------------------------------------------------
(* Splash window for TerraScene *)

unit fTerraSplash;

interface

uses
  Winapi.Windows, 
  Winapi.Messages, 
  System.SysUtils, 
  System.Variants, 
  System.Classes, 
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms,
  Vcl.Dialogs, 
  Vcl.ComCtrls, 
  Vcl.StdCtrls, 
  Vcl.ExtCtrls, 
  jpeg;

type
  TfmGS_Splash = class(TForm)
    ProgressPanel: TPanel;
    ProgressBar: TProgressBar;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
  private
     
  public
     
  end;

var
  fmGS_Splash: TfmGS_Splash;

implementation

{$R *.dfm}

procedure TfmGS_Splash.FormCreate(Sender: TObject);
begin
  //ProgressPanel.Parent:=fmMain;
   {ProgressPanel.Left:=(Width-ProgressPanel.Width) div 2;
   ProgressPanel.Visible:=True;
   ProgressBar.Max:=GetMaxProgress;  }
end;

end.
