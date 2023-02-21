//------------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//------------------------------------------------------------------------------
(* The Form to show grid parameters *)

unit fFileEditGridPars;

interface

uses
  System.SysUtils, 
  System.Classes,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs,
  Vcl.ComCtrls, 
  Vcl.ExtCtrls, 
  Vcl.StdCtrls,

  
  uWhittle;

type
  TfmFileEditGridPars = class(TForm)
    EditFileName: TEdit;
    MemoFileContents: TMemo;
    Panel1:     TPanel;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FComponent: TCustomGridParametersFile;
    procedure SetComponent(const Value: TCustomGridParametersFile);
    procedure ChangeParameters(Sender: TObject);
  public
    property Component: TCustomGridParametersFile read FComponent write SetComponent;
  end;

var
  fmFileEditGridPars: TfmFileEditGridPars;

implementation

{$R *.DFM}

{ TfmCustomGridParametersFileEditDialog }

procedure TfmFileEditGridPars.ChangeParameters
  (Sender: TObject);
var
  Stream: TStream;
begin
  EditFileName.Text := Component.FileName;
  try
    Stream := TMemoryStream.Create;
    try
      //      Component.SaveToStream(Stream);
      Stream.Seek(0, soFromBeginning);
      MemoFileContents.Lines.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  except
  end;
end;

procedure TfmFileEditGridPars.SetComponent(const Value: TCustomGridParametersFile);
begin
  if FComponent <> nil then
    FComponent.Assign(Value);
end;

procedure TfmFileEditGridPars.FormCreate(Sender: TObject);
begin
  FComponent := TCustomGridParametersFile.Create(Self);
  Component.onChange := ChangeParameters;
end;

procedure TfmFileEditGridPars.FormDestroy(Sender: TObject);
begin
  MemoFileContents.Lines.SaveToFile(Component.FileName);
  Component.Free;
end;

end.
