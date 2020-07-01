//------------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//------------------------------------------------------------------------------
{! Grid generation dialog for selecting grid parameters }

unit fMethodGridGeneration;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.IniFiles,
  System.ImageList,
  Vcl.ImgList,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs,
  Vcl.StdCtrls, 
  Vcl.ExtCtrls, 
  Bde.DBTables, 
  Data.DB,
  Vcl.ComCtrls, 
  Vcl.ToolWin, 
  Vcl.Buttons, 
  Vcl.Samples.Spin,

  
  fMethodDialog,
  GBEditRange, 
  GBEditValue,
  dDialogs,
  dBase;

type
  TfmMethodGridGeneration = class(TfmMethodDialog)
    GroupBoxModelSize: TGroupBox;
    ImageBounds: TImage;
    ImageBlockNumbers: TImage;
    ImageBlockSize: TImage;
    GroupBoxOrigin: TGroupBox;
    GroupBoxLength: TGroupBox;
    GroupBoxEnd: TGroupBox;
    GroupBoxBlockNumbers: TGroupBox;
    GroupBoxBlockSizes: TGroupBox;
    PanelNX: TPanel;
    PanelNY: TPanel;
    PanelDX: TPanel;
    PanelDY: TPanel;
    StaticTextXO: TStaticText;
    StaticTextYO: TStaticText;
    StaticTextZO: TStaticText;
    StaticTextYE: TStaticText;
    StaticTextXE: TStaticText;
    StaticTextZE: TStaticText;
    StaticTextLX: TStaticText;
    StaticTextLY: TStaticText;
    StaticTextLZ: TStaticText;
    SpinEditNX: TSpinEdit;
    SpinEditNY: TSpinEdit;
    SpinEditNZ: TSpinEdit;
    StaticTextNZ: TStaticText;
    StaticTextDZ: TStaticText;
    GroupBoxMaterial: TGroupBox;
    SpinEditMaterialID: TSpinEdit;
    SpeedButtonOpenBounds: TSpeedButton;
    SpeedButtonReset: TSpeedButton;
    SpeedButtonSaveBounds: TSpeedButton;
    evLX: TEdit;
    evLY: TEdit;
    evLZ: TEdit;
    GBEditValueXO: TGBEditValue;
    GBEditValueYO: TGBEditValue;
    GBEditValueZO: TGBEditValue;
    GBEditValueXE: TGBEditValue;
    GBEditValueYE: TGBEditValue;
    GBEditValueZE: TGBEditValue;
    GBEditValueDX: TGBEditValue;
    GBEditValueDY: TGBEditValue;
    GBEditValueDZ: TGBEditValue;
    procedure ButtonOKClick(Sender: TObject);
    procedure SpeedButtonSaveBoundsClick(Sender: TObject);
    procedure ToolButtonInputClick(Sender: TObject);
    procedure ListBoxInputNamesClick(Sender: TObject);
    procedure SpeedButtonOutputBrowseClick(Sender: TObject);
    procedure SpeedButtonBoundsClick(Sender: TObject);
    procedure SpeedButtonResetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EditValueZOChange(Sender: TObject);
    procedure EditValueZEChange(Sender: TObject);
    procedure EditValueDZChange(Sender: TObject);
    procedure EditValueXOChange(Sender: TObject);
    procedure EditValueYOChange(Sender: TObject);
    procedure EditValueXEChange(Sender: TObject);
    procedure EditValueYEChange(Sender: TObject);
    procedure EditValueDXChange(Sender: TObject);
    procedure EditValueDYChange(Sender: TObject);
    procedure SpinEditNXChange(Sender: TObject);
    procedure SpinEditNYChange(Sender: TObject);
    procedure SpinEditNZChange(Sender: TObject);
  private
    XO, YO, ZO, LX, LY, LZ, DX, DY, DZ: double;
    NX, NY, NZ: integer;
    XE, YE, ZE: double;
    procedure InitControls;
    procedure DefaultValues;
    procedure InitModelSize(const XO, YO, ZO, DX, DY, DZ: double;
      const NX, NY, NZ: integer);
    procedure MakeGrid(AFileName: TFileName; const XO, YO, ZO, DX, DY, DZ: double;
      const NX, NY, NZ: integer);
    procedure UpdateBlockNumbers;
    procedure UpdateBlockSizes;
    procedure ReadIniFile;
    procedure WriteIniFile;
  private
    FUpdateCount: integer;
    function GetInUpdate: boolean;
    procedure SetInUpdate(Value: boolean);
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    property InUpdate: boolean Read GetInUpdate Write SetInUpdate;
  end;

var
  fmMethodGridGeneration: TfmMethodGridGeneration;

implementation

uses
  uFileCreator,
  uGlobals,
  uCommon,
  uProfuns,
  uResStrings;

{$R *.DFM}

{ TfmMethodGridGeneration }

procedure TfmMethodGridGeneration.DefaultValues;
begin
  XO := 0;
  YO := 0;
  ZO := 0;
  XE := 1000;
  YE := 1000;
  ZE := 1000;
  LX := 1000;
  LY := 1000;
  LZ := 1000;
  NX := 10;
  NY := 10;
  NZ := 10;
  DX := 100;
  DY := 100;
  DZ := 100;
end;


procedure TfmMethodGridGeneration.ListBoxInputNamesClick(Sender: TObject);
begin
  inherited;
  if ListBoxInputNames.Items[ListBoxInputNames.ItemIndex] <> '' then
  begin
    MinMaxXYZ(InModelName, XO, YO, ZO, XE, YE, ZE, ProgressBar);
    NX := 10;
    NY := 10; //Default numbers of grid lines
    if ToolButtonPoints2D.Down then
    begin
      NZ := 1;
      OutModelType := mtGrids2D;
      PanelOutPath.Caption := ExpandPath(DirGrid2D);
    end
    else
    begin
      OutModelType := mtGrids3D;
      NZ := 10;
      PanelOutPath.Caption := ExpandPath(DirGrid3D);
    end;
    PanelOutPath.Hint := PanelOutPath.Caption;
    OutModelName      := PanelOutPath.Caption + EditOutName.Text;

    DX := (XE - XO) / (NX - 1);
    DY := (YE - YO) / (NY - 1);
    if NZ <> 1 then
      DZ := (ZE - ZO) / (NZ - 1)
    else
      DZ := ZE - ZO;
    XO := XO - DX / 2;
    YO := YO - DY / 2;
    if NZ <> 1 then
      ZO := ZO - DZ / 2;
    XE := XE + DX / 2;
    YE := YE + DY / 2;
    if NZ <> 1 then
      ZE := ZE + DZ / 2;

    InitModelSize(XO, YO, ZO, DX, DY, DZ, NX, NY, NZ);
    Refresh;
  end;
  UpdateBlockSizes;
end;


procedure TfmMethodGridGeneration.SpeedButtonBoundsClick(Sender: TObject);
begin
  dmDialogs.OpenDialog.InitialDir := ExpandPath(DirExploring);
  if dmDialogs.OpenDialog.Execute then
  begin
    ReadFramework(dmDialogs.OpenDialog.FileName,
      XO, YO, ZO, DX, DY, DZ, NX, NY, NZ);
    InitModelSize(XO, YO, ZO, DX, DY, DZ, NX, NY, NZ);
  end;
  UpdateBlockSizes;
end;

procedure TfmMethodGridGeneration.ToolButtonInputClick(Sender: TObject);
begin
  inherited;
  InitControls;
  ListBoxInputNamesClick(Self);
end;

procedure TfmMethodGridGeneration.InitControls;
begin
  case InModelType of
    mtPoints2D:
    begin
      StaticTextZO.Enabled := False;
      GBEditValueZO.Enabled     := False;
      StaticTextZE.Enabled := False;
      GBEditValueZE.Enabled     := False;
      StaticTextDZ.Enabled := False;
      GBEditValueDZ.Enabled     := False;
      StaticTextNZ.Enabled := False;
      SpinEditNZ.Enabled := False;

      OutModelType      := mtGrids2D;
      PanelOutPath.Caption := ExpandPath(DirGrid2D);
      PanelOutPath.Hint := PanelOutPath.Caption;
    end;
    mtPoints3D:
    begin
      StaticTextZO.Enabled := True;
      GBEditValueZO.Enabled    := True;
      StaticTextZE.Enabled := True;
      GBEditValueZE.Enabled    := True;
      StaticTextNZ.Enabled := True;
      SpinEditNZ.Enabled := True;
      StaticTextDZ.Enabled := True;
      GBEditValueDZ.Enabled    := True;
      OutModelType      := mtGrids3D;
      PanelOutPath.Caption := ExpandPath(DirGrid3D);
      PanelOutPath.Hint := PanelOutPath.Caption;
    end;
  end;
  if NameOnly(InModelName) <> '' then
    ListBoxInputNamesClick(Self)
  else
  begin
    DefaultValues;
    if ToolButtonPoints2D.Down then
    begin
      PanelOutPath.Caption := ExpandPath(DirGrid2D);
      EditOutName.Text     := 'Grid2D';
    end
    else
    begin
      PanelOutPath.Caption := ExpandPath(DirGrid3D);
      EditOutName.Text     := 'Grid3D';
    end;
    OutModelName := PanelOutPath.Caption + EditOutName.Text;
  end;
  ButtonOK.Enabled := True;
end;

procedure TfmMethodGridGeneration.InitModelSize(const XO, YO, ZO, DX, DY, DZ: double;
  const NX, NY, NZ: integer);
begin
  GBEditValueXO.Text := FloatToStr(RoundTo(XO, Precision));
  GBEditValueYO.Text := FloatToStr(RoundTo(YO, Precision));
  GBEditValueZO.Text := FloatToStr(RoundTo(ZO, Precision));

  GBEditValueDX.Text      := FloatToStr(RoundTo(DX, Precision));
  GBEditValueDY.Text      := FloatToStr(RoundTo(DY, Precision));
  GBEditValueDZ.Text      := FloatToStr(RoundTo(DZ, Precision));
  SpinEditNX.Value := NX;
  SpinEditNY.Value := NY;
  if ToolButtonPoints2D.Down then
    SpinEditNZ.Value := 1
  else
    SpinEditNZ.Value := NZ;

  GBEditValueXE.Text := FloatToStr(RoundTo(XO + DX * NX, Precision));
  GBEditValueYE.Text := FloatToStr(RoundTo(YO + DY * NY, Precision));
  GBEditValueZE.Text := FloatToStr(RoundTo(ZO + DZ * NZ, Precision));
  evLX.Text := FloatToStr(RoundTo(XE - XO, Precision));
  evLY.Text := FloatToStr(RoundTo(YE - YO, Precision));
  evLZ.Text := FloatToStr(RoundTo(ZE - ZO, Precision));
end;


procedure TfmMethodGridGeneration.SpeedButtonResetClick(Sender: TObject);
begin
  DefaultValues;
  InitModelSize(XO, YO, ZO, DX, DY, DZ, NX, NY, NZ);
  UpdateBlockSizes;
  UpdateBlockNumbers;
  if ToolButtonPoints2D.Down then
    EditOutName.Text := 'Grid2D'
  else
    EditOutName.Text := 'Grid3D';
  Refresh;
end;



procedure TfmMethodGridGeneration.SpeedButtonOutputBrowseClick(Sender: TObject);
begin
  with dmDialogs do
  begin
    SaveDialog.HelpContext := HelpContext;
    if ToolButtonPoints2D.Down then
      SaveDialog.InitialDir := DataBasePath + DirGrid2D
    else
      SaveDialog.InitialDir := DataBasePath + DirGrid3D;
    if SaveDialog.Execute then
    begin
      OutModelName     := SaveDialog.FileName;
      PanelOutPath.Caption := ExtractFilePath(SaveDialog.FileName);
      EditOutName.Text := NameOnly(SaveDialog.FileName);
    end;
  end;
end;

procedure TfmMethodGridGeneration.SpeedButtonSaveBoundsClick(Sender: TObject);
begin
  dmDialogs.SaveDialog.InitialDir  := ExpandPath(DirExploring);
  dmDialogs.SaveDialog.FileName    := tblFramework + TableExt;
  dmDialogs.SaveDialog.HelpContext := HelpContext;
  if dmDialogs.SaveDialog.Execute then
  begin
    WriteFramework(dmDialogs.SaveDialog.FileName,
      XO, YO, ZO, DX, DY, DZ, NX, NY, NZ);
    CreatePolyBounds(dmDialogs.SaveDialog.FileName,
      XO, YO, ZO, DX, DY, DZ, NX, NY, NZ);
  end;
end;

procedure TfmMethodGridGeneration.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
  InitControls;
end;

procedure TfmMethodGridGeneration.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      SpinEditMaterialID.Value := ReadInteger(Name, SpinEditMaterialID.Name, 1);
      SpinEditNX.Value := ReadInteger(Name, SpinEditNX.Name, 10);
      SpinEditNY.Value := ReadInteger(Name, SpinEditNY.Name, 10);
      SpinEditNZ.Value := ReadInteger(Name, SpinEditNZ.Name, 10);
    finally
      IniFile.Free;
    end;
end;

procedure TfmMethodGridGeneration.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteInteger(Name, SpinEditMaterialID.Name, SpinEditMaterialID.Value);
      WriteInteger(Name, SpinEditNX.Name, SpinEditNX.Value);
      WriteInteger(Name, SpinEditNY.Name, SpinEditNY.Value);
      WriteInteger(Name, SpinEditNZ.Name, SpinEditNZ.Value);
    finally
      IniFile.Free;
    end;
end;

procedure TfmMethodGridGeneration.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;

procedure TfmMethodGridGeneration.EditValueXOChange(Sender: TObject);
begin
  if GBEditValueXO.Text = '' then
    GBEditValueXO.Text := '1';
  XO := StrToFloat(GBEditValueXO.Text);
  if XE < XO then
  begin
    XO := XE - LX;
    GBEditValueXO.Text := FloatToStr(XO);
  end;
  UpdateBlockSizes;
end;

procedure TfmMethodGridGeneration.EditValueYOChange(Sender: TObject);
begin
  if GBEditValueYO.Text = '' then
    GBEditValueYO.Text := '1';
  YO := StrToFloat(GBEditValueYO.Text);
  if YE < YO then
  begin
    YO := YE - LY;
    GBEditValueYO.Text := FloatToStr(YO);
  end;
  UpdateBlockSizes;
end;


procedure TfmMethodGridGeneration.EditValueZOChange(Sender: TObject);
begin
  if GBEditValueZO.Text = '' then
    GBEditValueZO.Text := '1';
  ZO := StrToFloat(GBEditValueZO.Text);
  if ZE < ZO then
  begin
    ZO := ZE - LZ;
    GBEditValueZO.Text := FloatToStr(ZO);
  end;
  UpdateBlockSizes;
end;

procedure TfmMethodGridGeneration.EditValueXEChange(Sender: TObject);
begin
  if GBEditValueXE.Text = '' then
    GBEditValueXE.Text := '1';
  XE := StrToFloat(GBEditValueXE.Text);
  if XE - XO < 0 then
  begin
    XE := LX + XO;    //Restore XE
    GBEditValueXE.Text := FloatToStr(XE);
  end;
  UpdateBlockSizes;
end;

procedure TfmMethodGridGeneration.EditValueYEChange(Sender: TObject);
begin
  if GBEditValueYE.Text = '' then
    GBEditValueYE.Text := '1';
  YE := StrToFloat(GBEditValueYE.Text);
  if YE - YO < 0 then
  begin
    YE := LY + YO;   //Restore YE
    GBEditValueYE.Text := FloatToStr(YE);
  end;
  UpdateBlockSizes;
end;


procedure TfmMethodGridGeneration.EditValueZEChange(Sender: TObject);
begin
  if GBEditValueZE.Text = '' then
    GBEditValueZE.Text := '1';
  ZE := StrToFloat(GBEditValueZE.Text);
  if ZE - ZO < 0 then
  begin
    ZE := LZ + ZO;   //Restore ZE
    GBEditValueZE.Text := FloatToStr(ZE);
  end;
  UpdateBlockSizes;
end;


procedure TfmMethodGridGeneration.EditValueDXChange(Sender: TObject);
begin
  if GBEditValueDX.Text = '' then
    GBEditValueDX.Text := '1';
  DX := StrToFloat(GBEditValueDX.Text);
  if DX < 0 then
  begin
    DX := LX / NX;      //Restore DX
    GBEditValueDX.Text := FloatToStr(DX);
  end;
  if not InUpdate then
  begin
    BeginUpdate;
    UpdateBlockNumbers;
    EndUpdate;
  end;
end;

procedure TfmMethodGridGeneration.EditValueDYChange(Sender: TObject);
begin
  if GBEditValueDY.Text = '' then
    GBEditValueDY.Text := '1';
  DY := StrToFloat(GBEditValueDY.Text);
  if DY < 0 then
  begin
    DY := LY / NY;      //Restore DY
    GBEditValueDY.Text := FloatToStr(DY);
  end;
  if not InUpdate then
  begin
    BeginUpdate;
    UpdateBlockNumbers;
    EndUpdate;
  end;
end;

procedure TfmMethodGridGeneration.EditValueDZChange(Sender: TObject);
begin
  if GBEditValueDZ.Text = '' then
    GBEditValueDZ.Text := '1';
  DZ := StrToFloat(GBEditValueDZ.Text);
  if DZ < 0 then
  begin
    DZ := LZ / NZ;        //Restore DZ
    GBEditValueDZ.Text := FloatToStr(DZ);
  end;
  if not InUpdate then
  begin
    BeginUpdate;
    UpdateBlockNumbers;
    EndUpdate;
  end;
end;

procedure TfmMethodGridGeneration.SpinEditNXChange(Sender: TObject);
begin
  NX := SpinEditNX.Value;
  if not InUpdate then
  begin
    BeginUpdate;
    UpdateBlockSizes;
    EndUpdate;
  end;
end;

procedure TfmMethodGridGeneration.SpinEditNYChange(Sender: TObject);
begin
  NY := SpinEditNX.Value;
  if not InUpdate then
  begin
    BeginUpdate;
    UpdateBlockSizes;
    EndUpdate;
  end;
end;

procedure TfmMethodGridGeneration.SpinEditNZChange(Sender: TObject);
begin
  if ToolButtonPoints2D.Down then
    NZ := 1
  else
    NZ := SpinEditNZ.Value;
  if not InUpdate then
  begin
    BeginUpdate;
    UpdateBlockSizes;
    EndUpdate;
  end;
end;

procedure TfmMethodGridGeneration.UpdateBlockNumbers;
begin
  SpinEditNX.Value := Trunc(StrToFloat(evLX.Text) / StrToFloat(GBEditValueDX.Text));
  SpinEditNY.Value := Trunc(StrToFloat(evLY.Text) / StrToFloat(GBEditValueDY.Text));
  SpinEditNZ.Value := Trunc(StrToFloat(evLZ.Text) / StrToFloat(GBEditValueDZ.Text));
end;

procedure TfmMethodGridGeneration.UpdateBlockSizes;
begin
  evLX.Text := FloatToStr(StrToFloat(GBEditValueXE.Text) - StrToFloat(GBEditValueXO.Text));
  evLY.Text := FloatToStr(StrToFloat(GBEditValueYE.Text) - StrToFloat(GBEditValueYO.Text));
  evLZ.Text := FloatToStr(StrToFloat(GBEditValueZE.Text) - StrToFloat(GBEditValueZO.Text));

  DX := (XE - XO) / NX;
  DY := (YE - YO) / NY;
  DZ := (ZE - ZO) / NZ;
  GBEditValueDX.Text :=
    FloatToStr(RoundTo(StrToFloat(evLX.Text) / SpinEditNX.Value, Precision));
  GBEditValueDY.Text :=
    FloatToStr(RoundTo(StrToFloat(evLY.Text) / SpinEditNY.Value, Precision));
  GBEditValueDZ.Text :=
    FloatToStr(RoundTo(StrToFloat(evLZ.Text) / SpinEditNZ.Value, Precision));
end;


procedure TfmMethodGridGeneration.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TfmMethodGridGeneration.EndUpdate;
begin
  FUpdateCount := Max(0, FUpdateCount - 1);
end;

function TfmMethodGridGeneration.GetInUpdate: boolean;
begin
  Result := boolean(FUpdateCount);
end;

procedure TfmMethodGridGeneration.SetInUpdate(Value: boolean);
begin
  case Value of
    False: EndUpdate;
    True: BeginUpdate;
  end;
end;


procedure TfmMethodGridGeneration.MakeGrid(AFileName: TFileName;
  const XO, YO, ZO, DX, DY, DZ: double; const NX, NY, NZ: integer);
var
  I, J, K: integer;
  ID:      integer;
  X, Y, Z: Float;
begin
  with dmBase do
  begin
    OutModelName := AFileName;
    CreateGridTables(OutModelName); // with default ID, X, Y, Z fields

    AddTableField(OutModelName, fldMATERIAL, ftInteger);

    TableOutput.TableName := OutModelName;
    TableOutput.Open;
    TableOutput.First;

    ProgressBar.Position := 0;
    ProgressBar.Min := 0;
    ProgressBar.Max := NX * NY * NZ;
    ProgressBar.Step := 1;
    ID := 0;
    //    X := XO + DX / 2;
    X  := XO;
    for I := 1 to NX do
    begin
      //      Y := YO + DY / 2;
      Y := YO;
      for J := 1 to NY do
      begin
        //        Z := ZO + DZ / 2;
        Z := ZO;
        for K := 1 to NZ do
        begin
          Inc(ID);
          ProgressBar.StepIt;
          TableOutput.Append;
          TableOutput.FieldByName(fldID).AsFloat := ID;
          TableOutput.FieldByName(fldX).AsFloat  := RoundTo(X, Precision);
          TableOutput.FieldByName(fldY).AsFloat  := RoundTo(Y, Precision);
          TableOutput.FieldByName(fldZ).AsFloat  := RoundTo(Z, Precision);
          TableOutput.FieldByName(fldMATERIAL).AsInteger := SpinEditMaterialID.Value;
          TableOutput.Post;
          Z := Z + DZ;
        end;
        Y := Y + DY;
      end;
      if ModalResult = mrCancel then
        Exit;
      X := X + DX;
    end;
    WriteParFile(TableOutput.TableName, XO, YO, ZO, DX, DY, DZ, NX, NY, NZ);
    ProgressBar.Position := ProgressBar.Max;
    TableOutput.Close;
  end;
end;

procedure TfmMethodGridGeneration.ButtonOKClick(Sender: TObject);
begin
  inherited;
  if ModalResult <> mrNone then
  begin
    Cursor := crHourGlass;
    try
      MakeGrid(OutModelName, XO, YO, ZO, DX, DY, DZ, NX, NY, NZ)
    finally
      Cursor := crDefault;
    end;
  end
  else
    Exit;
end;

end.
