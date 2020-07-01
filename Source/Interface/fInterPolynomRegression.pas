//
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//
{!
   Implements the polynomial regression scheem of interpolation
}
unit fInterPolynomRegression;

interface

uses
  System.SysUtils, 
  System.Classes, 
  System.IniFiles,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs,
  Vcl.StdCtrls, 
  Vcl.ExtCtrls, 
  Vcl.Samples.Spin,

  
  fInitialDialog;

type
  TfmInterPolynomialRegression = class(TfmInitialDialog)
    RadioGroupSurface: TRadioGroup;
    PanelPlanar: TPanel;
    PanelBiLinear: TPanel;
    PanelQuadratic: TPanel;
    Panel1: TPanel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    procedure ReadIniFile;
    procedure WriteIniFile;
  public
     
  end;

var
  fmInterPolynomialRegression: TfmInterPolynomialRegression;

implementation

uses
  uGlobals,
  uInterpol;

{$R *.DFM}

{ TfmInterPolynomialRegression }

procedure TfmInterPolynomialRegression.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    RadioGroupSurface.ItemIndex := ReadInteger(Name, RadioGroupSurface.Name, 0);
end;

procedure TfmInterPolynomialRegression.WriteIniFile;
begin
  PolyRegressOrder := RadioGroupSurface.ItemIndex + 1;
  with IniFile do
    WriteInteger(Name, RadioGroupSurface.Name, RadioGroupSurface.ItemIndex);
  IniFile.Free;
end;

procedure TfmInterPolynomialRegression.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
end;

procedure TfmInterPolynomialRegression.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;

end.
