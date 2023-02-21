//------------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//------------------------------------------------------------------------------
(* The Initial form to inherit all child Forms of Geoblock GUI *)

unit fInitialForm;


interface

uses
  System.Classes,
  System.ImageList,
  Vcl.Forms,
  Vcl.ImgList,
  Vcl.Controls,

  uCommon,
  GnuGetText;


type
  TfmInitialForm = class(TForm)
    ImageListInterface: TImageList;
    ImageListPictures: TImageList;
    procedure FormCreate(Sender: TObject);
  public
    procedure ReadIniFile; virtual;
    procedure WriteIniFile; virtual;
  protected
  end;

var
  fmInitialForm: TfmInitialForm;

//========================================================================
implementation
//========================================================================

{$R *.dfm}

//-------------------------------
// TfmInitialForm
//-------------------------------

//Here goes the translation of all component strings
procedure TfmInitialForm.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  inherited;
end;


procedure TfmInitialForm.ReadIniFile;
begin
  //
end;

procedure TfmInitialForm.WriteIniFile;
begin
  //
end;

end.
