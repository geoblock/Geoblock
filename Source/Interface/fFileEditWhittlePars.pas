//----------------------------------------------------------------------------
// This unit is part of the Geoblock software, http://sourceforge.net/projects/geoblock
//-----------------------------------------------------------------------------
{
  The Form to edit parameters for Gemcom's Whittle file of blocks
}

unit fFileEditWhittlePars;

interface

uses
  Winapi.Messages,
  System.SysUtils, 
  System.Variants, 
  System.Classes,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs, 
  Vcl.StdCtrls,
  Vcl.ExtCtrls, 
  Vcl.ComCtrls,

  
  fInitialDialog,
  uWhittle;

type
  TfmFileEditWhittlePars = class(TfmInitialDialog)
    EditFileName: TEdit;
    MemoFileContents: TMemo;
    StatusBar: TStatusBar;
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EditFileNameChange(Sender: TObject);
    procedure MemoFileContentsChange(Sender: TObject);
    procedure MemoFileContentsKeyUp(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure MemoFileContentsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure MemoFileContentsMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
  private
    FComponent: Twt4XParametersFile;
    procedure SetComponent(const Value: Twt4XParametersFile);
    procedure ChangeParameters(Sender: TObject);

     
  public
     
    property Component: Twt4XParametersFile Read FComponent Write SetComponent;
  end;

var
  fmFileEditWhittlePars: TfmFileEditWhittlePars;

implementation

{$R *.dfm}

procedure TfmFileEditWhittlePars.ButtonCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
  Close;
end;

procedure TfmFileEditWhittlePars.FormCreate(Sender: TObject);
begin
  FComponent := Twt4XParametersFile.Create(Self);
  Component.onChange := ChangeParameters;
end;

procedure TfmFileEditWhittlePars.FormDestroy(Sender: TObject);
begin
  if ModalResult = mrOk then
    MemoFileContents.Lines.SaveToFile(Component.FileName);
  Component.Free;
end;

procedure TfmFileEditWhittlePars.EditFileNameChange(Sender: TObject);
begin
  Caption := EditFileName.Text;
end;

procedure TfmFileEditWhittlePars.MemoFileContentsChange(Sender: TObject);
var
  X, Y: integer;

begin
  X := MemoFileContents.SelStart + MemoFileContents.SelLength -
// it was earlier - MemoFileContents.Perform(EM_LINEINDEX, -1, 0) + 1;
    MemoFileContents.Perform(EM_LINEINDEX, 0, 0) + 1;

// it was earlier - Y := MemoFileContents.Perform(EM_LINEFROMCHAR, -1, 0) + 1;
  Y := MemoFileContents.Perform(EM_LINEFROMCHAR, 0, 0) + 1;

  StatusBar.SimpleText := 'X: ' + IntToStr(X) + '; Y: ' + IntToStr(Y);
  if MemoFileContents.SelLength <> 0 then
    StatusBar.SimpleText := StatusBar.SimpleText + '; <->: ' +
      IntToStr(MemoFileContents.SelLength);
end;

procedure TfmFileEditWhittlePars.SetComponent(const Value: Twt4XParametersFile);
begin
  if FComponent <> nil then
    FComponent.Assign(Value);
end;

procedure TfmFileEditWhittlePars.ChangeParameters(Sender: TObject);
var
  Stream: TStream;
  {  I : Integer;
    S : String;{}
begin
  EditFileName.Text := Component.FileName;
  try
    //{
    Stream := TMemoryStream.Create;
    try
      Component.SaveToStream(Stream);
      Stream.Seek(0, soFromBeginning);
      MemoFileContents.Lines.LoadFromStream(Stream);
      {      S:='';
            for I:=1 to 9 do S:=S+Format('%10s',[IntToStr(I)]);
            MemoFileContents.Lines.Insert(0,S);
            S:='';
            for I:=1 to 9 do S:=S+'1234567890';
            MemoFileContents.Lines.Insert(1,S);{}
    finally
      Stream.Free;
    end; //}MemoFileContents.Lines.LoadFromFile(Component.FileName);
  except
  end;
end;

procedure TfmFileEditWhittlePars.MemoFileContentsKeyUp(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  MemoFileContentsChange(nil);
end;

procedure TfmFileEditWhittlePars.MemoFileContentsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  MemoFileContentsChange(nil);
end;

procedure TfmFileEditWhittlePars.MemoFileContentsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
begin
  MemoFileContentsChange(nil);
end;

procedure TfmFileEditWhittlePars.ButtonOKClick(Sender: TObject);
begin
  MemoFileContents.Lines.SaveToFile(Component.FileName);
  ModalResult := mrOk;
  //  Close;
end;

end.
