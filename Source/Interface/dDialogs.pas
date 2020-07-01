 //------------------------------------------------------------------------------
 // This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
 //------------------------------------------------------------------------------
{! The Datamodule for dialogs }

unit dDialogs;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs, 
  Vcl.ExtDlgs;

type
  TdmDialogs = class(TDataModule)
    PrintDialog:    TPrintDialog;
    PrintSetupDialog: TPrinterSetupDialog;
    SaveDialog:     TSaveDialog;
    SavePictureDialog: TSavePictureDialog;
    ColorDialog:    TColorDialog;
    OpenPictureDialog: TOpenPictureDialog;
    FindDialog:     TFindDialog;
    ReplaceDialog:  TReplaceDialog;
    OpenDialog:     TOpenDialog;
    OpenDialogProject: TOpenDialog;
    OpenDialogSQL:  TOpenDialog;
    SaveDialogSQL:  TSaveDialog;
    SaveDialogExport: TSaveDialog;
    OpenDialogImport: TOpenDialog;
    SaveDialogText: TSaveDialog;
    OpenDialogText: TOpenDialog;
    FontDialog:     TFontDialog;
  private
     
  public
     
  end;

var
  dmDialogs: TdmDialogs;

implementation

{$R *.DFM}

end.
