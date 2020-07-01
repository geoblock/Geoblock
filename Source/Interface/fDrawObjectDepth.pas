//------------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//------------------------------------------------------------------------------
{!  The dialog for drawing depth parameters }


unit fDrawObjectDepth;

interface

uses
  Winapi.Windows,
  System.SysUtils, 
  System.Classes,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs,
  Vcl.StdCtrls, 
  Vcl.ExtCtrls,

  
  fInitialDialog,
  GBEditValue;

type
  TfmDrawObjectDepth = class(TfmInitialDialog)
    GroupBoxDepth:      TGroupBox;
    RadioButtonAverage: TRadioButton;
    RadioButtonSpecified: TRadioButton;
    EditValueDepth:     TGBEditValue;
    procedure RadioButtonAverageClick(Sender: TObject);
    procedure RadioButtonSpecifiedClick(Sender: TObject);
  private
     
  public
     
  end;

var
  fmDrawObjectDepth: TfmDrawObjectDepth;

implementation

{$R *.DFM}

procedure TfmDrawObjectDepth.RadioButtonAverageClick(Sender: TObject);
begin
  EditValueDepth.Enabled := False;
end;

procedure TfmDrawObjectDepth.RadioButtonSpecifiedClick(Sender: TObject);
begin
  EditValueDepth.Enabled := True;
end;

end.
