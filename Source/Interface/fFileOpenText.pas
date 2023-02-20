//------------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//------------------------------------------------------------------------------
{
  It includes routines for common text usage
}

unit fFileOpenText;

interface

uses
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
  TfmFileOpenText = class(TfmInitialDialog)
    RichEdit: TRichEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
  private
    FTextFileName: TFileName;
    function GetTextFileName: TFileName;
  public
    procedure CopyToClipboard;
    procedure Print;
    procedure PasteFromClipboard;
    procedure SaveTextFileAs;
    procedure LoadFile;
    function OpenFile: boolean;
    property TextFileName: TFileName Read GetTextFileName Write FTextFileName;
  end;

var
  fmFileOpenText: TfmFileOpenText;

//=========================================================================
implementation
//=========================================================================

uses
  cGlobals,
  dDialogs,
  uCommon;

{$R *.dfm}

procedure TfmFileOpenText.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfmFileOpenText.ButtonCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfmFileOpenText.ButtonOKClick(Sender: TObject);
begin
  RichEdit.Lines.SaveToFile(TextFileName);
  Close;
end;


procedure TfmFileOpenText.CopyToClipboard;
begin
  RichEdit.CopyToClipboard;
end;

procedure TfmFileOpenText.LoadFile;
begin
  RichEdit.Lines.LoadFromFile(TextFileName);
end;

function TfmFileOpenText.OpenFile: boolean;
begin
  Result := dmDialogs.OpenDialogText.Execute;
  if Result then
  begin
    LoadFile;
  end;
end;

procedure TfmFileOpenText.PasteFromClipboard;
begin
  RichEdit.PasteFromClipboard;
end;

procedure TfmFileOpenText.Print;
begin
  RichEdit.Print(TextFileName);
end;

procedure TfmFileOpenText.SaveTextFileAs;
begin
  with dmDialogs do
  begin
    SaveDialogText.InitialDir  := ExpandPath(DirFiles);
    SaveDialogText.DefaultExt  := TextExt;
    SaveDialogText.FilterIndex := 1;   //*.txt
    SaveDialogText.FileName    := TextFileName;
    if SaveDialogText.Execute then
      RichEdit.Lines.SaveToFile(SaveDialogText.FileName);
  end;
end;

function TfmFileOpenText.GetTextFileName: TFileName;
begin
  Result := FTextFileName;
end;

end.
