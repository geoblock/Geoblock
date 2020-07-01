//------------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//------------------------------------------------------------------------------
{!  The form to edit XML files }


unit fEditFileXML;

interface

uses
  System.SysUtils, 
  System.Variants, 
  System.Classes, 
  Vcl.Graphics, 
  Vcl.Controls,
  Vcl.Forms, 
  Vcl.Dialogs, 
  Vcl.Grids, 
  Vcl.DBGrids, 
  Vcl.StdCtrls, 
  Vcl.ExtCtrls,
  //DB
  Data.DB, 
  Datasnap.DBClient,

  
  fInitialDialog;

type
  TfmEditFileXML = class(TfmInitialDialog)
    DBGrid:     TDBGrid;
    DataSource: TDataSource;
    ClientDataSet: TClientDataSet;
    Button:     TButton;
    procedure ButtonClick(Sender: TObject);
  private
     
  public
     
  end;

var
  fmEditFileXML: TfmEditFileXML;

implementation

{$R *.dfm}

procedure TfmEditFileXML.ButtonClick(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do
    try
      if Execute then
        ClientDataSet.LoadFromFile(FileName);
    finally
      Free;
    end;
end;

end.
