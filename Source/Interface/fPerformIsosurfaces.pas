//----------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//----------------------------------------------------------------------------
(*!
  Features of isosorfaces dialog
*)


unit fPerformIsosurfaces;

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
  Vcl.Samples.Spin,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.Grids,

  GLS.Isosurface,

  fInitialDialog,
  cGlobals, System.ImageList, Vcl.ImgList, Vcl.Buttons, Vcl.ToolWin;


type
  TfmPerformIsosurfaces = class(TfmInitialDialog)
    GroupBoxCut:    TGroupBox;
    CheckBoxPlaneX: TCheckBox;
    SpinEditX:      TSpinEdit;
    CheckBoxPlaneY: TCheckBox;
    SpinEditY:      TSpinEdit;
    CheckBoxPlaneZ: TCheckBox;
    SpinEditZ:      TSpinEdit;
    GroupBox: TGroupBox;
    LabelNumberOfClasses: TLabel;
    StringGridClasses: TStringGrid;
    HeaderControlGrid: THeaderControl;
    SpinEditClasses: TSpinEdit;
    GroupBoxShow: TGroupBox;
    CheckBoxFaces: TCheckBox;
    CheckBoxEdges: TCheckBox;
    CheckBoxCups: TCheckBox;
    GroupBoxOutput: TGroupBox;
    ToolBarOutput: TToolBar;
    ToolButton3: TToolButton;
    ToolButtonPolygons: TToolButton;
    ToolButtonTINs: TToolButton;
    ToolButtonMeshes2D: TToolButton;
    PanelOutPath: TPanel;
    EditOutName: TEdit;
    SpeedButtonOutputBrowse: TSpeedButton;
    CheckBox1: TCheckBox;
    ImageList: TImageList;
    procedure CheckBoxPlaneXClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure SpinEditClassesChange(Sender: TObject);
  private
    procedure ReadIniFile;
    procedure WriteIniFile;
  public

  end;

var
  fmPerformIsosurfaces: TfmPerformIsosurfaces;

//===================================================================
implementation
//===================================================================

{$R *.DFM}

procedure TfmPerformIsosurfaces.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      CheckBoxFaces.Checked := ReadBool(Name, CheckBoxFaces.Name, True);
      CheckBoxEdges.Checked     := ReadBool(Name, CheckBoxEdges.Name, True);
      CheckBoxCups.Checked      := ReadBool(Name, CheckBoxCups.Name, True);

      CheckBoxPlaneX.Checked := ReadBool(Name, CheckBoxPlaneX.Name, True);
      CheckBoxPlaneY.Checked := ReadBool(Name, CheckBoxPlaneY.Name, True);
      CheckBoxPlaneZ.Checked := ReadBool(Name, CheckBoxPlaneZ.Name, True);

      SpinEditX.Value := ReadInteger(Name, SpinEditX.Name, 1);
      SpinEditY.Value := ReadInteger(Name, SpinEditY.Name, 1);
      SpinEditZ.Value := ReadInteger(Name, SpinEditZ.Name, 1);

    finally
      IniFile.Free;
    end;
end;

procedure TfmPerformIsosurfaces.SpinEditClassesChange(Sender: TObject);
begin
  StringGridClasses.RowCount := SpinEditClasses.Value;
end;

procedure TfmPerformIsosurfaces.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteBool(Name, CheckBoxFaces.Name, CheckBoxFaces.Checked);
      WriteBool(Name, CheckBoxEdges.Name, CheckBoxEdges.Checked);
      WriteBool(Name, CheckBoxCups.Name, CheckBoxCups.Checked);
      WriteBool(Name, CheckBoxPlaneX.Name, CheckBoxPlaneX.Checked);
      WriteBool(Name, CheckBoxPlaneY.Name, CheckBoxPlaneY.Checked);
      WriteBool(Name, CheckBoxPlaneZ.Name, CheckBoxPlaneZ.Checked);

      WriteInteger(Name, SpinEditX.Name, SpinEditX.Value);
      WriteInteger(Name, SpinEditY.Name, SpinEditY.Value);
      WriteInteger(Name, SpinEditZ.Name, SpinEditZ.Value);

    finally
      IniFile.Free;
    end;
end;

procedure TfmPerformIsosurfaces.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
end;

procedure TfmPerformIsosurfaces.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;

procedure TfmPerformIsosurfaces.CheckBoxPlaneXClick(Sender: TObject);
begin
  //  glFrustum(left: GLdouble; right: GLdouble; bottom: GLdouble;
  //            top: GLdouble; znear: GLdouble; zfar: GLdouble);
end;

end.
