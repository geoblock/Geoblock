//-----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//-----------------------------------------------------------------------------
{!  Parent page dialog }

unit fPageDialog;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IniFiles,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs,
  Vcl.ExtCtrls, 
  Vcl.ComCtrls, 
  Vcl.StdCtrls,

  
  uGlobals,
  fInitialDialog;

type
  TfmPageDialog = class(TfmInitialDialog)
    PageControl: TPageControl;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  public
    procedure ReadIniFile; override;
    procedure WriteIniFile; override;
  end;

var
  fmPageDialog: TfmPageDialog;

//===========================================================================
implementation
//===========================================================================

{$R *.DFM}

{ TfmPageDialog }

procedure TfmPageDialog.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      PageControl.TabIndex := ReadInteger(Name, 'TabIndex', 0);
    finally
      IniFile.Free;
    end;
end;

procedure TfmPageDialog.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteInteger(Name, 'TabIndex', PageControl.ActivePageIndex);
    finally
      IniFile.Free;
    end;
end;

procedure TfmPageDialog.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
end;

procedure TfmPageDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;

end.
