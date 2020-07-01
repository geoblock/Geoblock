//----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//----------------------------------------------------------------------------
{! The startup form }

unit fStartup;

interface

uses
  System.Classes,
  Vcl.Graphics, 
  Vcl.Forms, 
  Vcl.Controls, 
  Vcl.StdCtrls,
  Vcl.Buttons, 
  Vcl.ComCtrls, 
  Vcl.ExtCtrls, 
  Vcl.Imaging.Jpeg, 
  Vcl.Dialogs,
  //DB
  Data.DB, 
  Bde.DBTables;

type
  TfmStartup = class(TForm)
    Image1: TImage;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
     
  public
     
  end;

var
  fmStartup: TfmStartup;


implementation

{$R *.DFM}

procedure TfmStartup.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
