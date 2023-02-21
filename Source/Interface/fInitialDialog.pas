//---------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//---------------------------------------------------------------------------
(* fInitialDialog is the parent form for inherited method dialogs *)

unit fInitialDialog;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Controls,
  Vcl.ExtCtrls,
  
  fInitialForm;

type
  TfmInitialDialog = class(TfmInitialForm)
    PanelTop:     TPanel;
    PanelMiddle:  TPanel;
    PanelBottom:  TPanel;
    ButtonOK:     TButton;
    ButtonCancel: TButton;
    ButtonHelp:   TButton;
    procedure ButtonHelpClick(Sender: TObject);
  private
     
  protected
  public
     
    function Execute: boolean; virtual;
  published
     
  end;

var
  fmInitialDialog: TfmInitialDialog;

implementation

{$R *.DFM}

procedure TfmInitialDialog.ButtonHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;


function TfmInitialDialog.Execute: boolean;
begin
  Result := ShowModal = mrOk;
end;

end.
