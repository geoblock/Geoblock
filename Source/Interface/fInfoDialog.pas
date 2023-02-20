//-----------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//-----------------------------------------------------------------------------
{! The dialog to output information and translated messages instead of System dialogs }

unit fInfoDialog;

interface

uses
  Winapi.Windows,
  System.SysUtils, 
  System.Variants, 
  System.Classes,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs, 
  Vcl.ComCtrls,
  Vcl.ToolWin, 
  Vcl.StdCtrls, 
  Vcl.ExtCtrls,

  
  fInitialDialog;

type
  TfmInfoDialog = class(TfmInitialDialog)
    RichEdit: TRichEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    // Private declarations
  public
    procedure CopyToClipboard;
    procedure PasteFromClipboard;
  end;

var
  fmInfoDialog: TfmInfoDialog;

implementation

uses
  cGlobals,
  uCommon;

{$R *.dfm}

procedure TfmInfoDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfmInfoDialog.CopyToClipboard;
begin
  RichEdit.CopyToClipboard;
end;

procedure TfmInfoDialog.PasteFromClipboard;
begin
  RichEdit.PasteFromClipboard;
end;

end.
