//----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//----------------------------------------------------------------------------
{
  FileOpenModel  -  the dialog window to open model files
}


unit fFileOpenModel;

interface

uses
  Winapi.Windows, 
  Winapi.Messages,
  System.SysUtils, 
  System.Classes,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs,
  Vcl.ComCtrls, 
  Vcl.ImgList, 
  Vcl.ToolWin, 
  Vcl.StdCtrls, 
  Vcl.Buttons, 
  Vcl.ExtCtrls,

  Data.DB,
  Bde.DBTables,

  fFileOpenDialog;

type
  TfmFileOpenModel = class(TfmFileOpenDialog)
    TabSheetTins:      TTabSheet;
    TabSheetSolids:    TTabSheet;
    TabSheetGrids2D:   TTabSheet;
    TabSheetGrids3D:   TTabSheet;
    TabSheetMeshes2D:  TTabSheet;
    TabSheetMeshes3D:  TTabSheet;
    TabSheetPoints2D:  TTabSheet;
    TabSheetPoints3D:  TTabSheet;
    TabSheetHoles:     TTabSheet;
    TabSheetPolygons:  TTabSheet;
    procedure PageControlChange(Sender: TObject);
  protected
    function GetModelType: integer; override;
    function GetPathFromTag(ATabSheetTag: integer): string; override;
  end;

var
  fmFileOpenModel: TfmFileOpenModel;

implementation

uses
  uCommon,
  uGlobals;

{$R *.DFM}

function TfmFileOpenModel.GetPathFromTag(ATabSheetTag: integer): string;
begin
  case ATabSheetTag of
    0: Result  := ExpandPath(DirDholes);
    1: Result  := ExpandPath(DirPoints2D);
    2: Result  := ExpandPath(DirPoints3D);
    3: Result  := ExpandPath(DirPolygonPoly);
    4: Result  := ExpandPath(DirTinFaces);
    5: Result  := ExpandPath(DirSolids);
    6: Result  := ExpandPath(DirGrid2D);
    7: Result  := ExpandPath(DirGrid3D);
    8: Result  := ExpandPath(DirMesh2D);
    9: Result  := ExpandPath(DirMesh3D);
    else
      Result := ExpandPath(DirDataBase);
  end;
end;

procedure TfmFileOpenModel.PageControlChange(Sender: TObject);
begin
  inherited;
  GetPathFromTag(PageControl.ActivePage.Tag);
end;

function TfmFileOpenModel.GetModelType: integer;
begin
  Result := PageControl.ActivePage.Tag;
end;

end.
