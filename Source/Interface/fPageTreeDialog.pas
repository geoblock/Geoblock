//-----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//-----------------------------------------------------------------------------
{!  Parent page dialog }

unit fPageTreeDialog;

interface

uses
  System.SysUtils, 
  System.Variants, 
  System.Classes, 
  System.IniFiles,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs,
  Vcl.StdCtrls, 
  Vcl.ComCtrls, 
  Vcl.ExtCtrls,

  fPageDialog,
  uGlobals;

type
  TfmPageTreeDialog = class(TfmPageDialog)
    TreeView: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
  public
    procedure ReadIniFile; override;
    procedure WriteIniFile; override;
  end;

var
  fmPageTreeDialog: TfmPageTreeDialog;

//=======================================================
implementation
//=======================================================

{$R *.dfm}

{ TfmPageTreeDialog }

procedure TfmPageTreeDialog.ReadIniFile;
begin
  inherited;
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      TreeView.Tag := ReadInteger(Name, 'TreeViewTag', 0);
    finally
      IniFile.Free;
    end;
end;

procedure TfmPageTreeDialog.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteInteger(Name, 'TreeViewTag', TreeView.Tag);
    finally
      IniFile.Free;
    end;
  inherited;
end;

procedure TfmPageTreeDialog.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
end;

procedure TfmPageTreeDialog.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;

end.
