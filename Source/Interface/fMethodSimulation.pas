//----------------------------------------------------------------------------
// The unit of Geoblock, http://sourceforge.net/projects/geoblock
//---------------------------------------------------------------------------
{!  Simulate a new point dataset }

unit fMethodSimulation;

interface

uses
  System.SysUtils, 
  System.Classes, 
  System.Math, 
  System.IniFiles,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs,
  Vcl.StdCtrls, 
  Vcl.ComCtrls, 
  Vcl.ExtCtrls, 
  Vcl.ImgList,
  Vcl.Grids, 
  Vcl.Buttons, 
  Vcl.ToolWin,
  Vcl.Samples.Spin,
  Vcl.FileCtrl,

  //DB
  Data.DB, 
  Bde.DBTables,

  dBase,
  GBEditValue,
  fInitialForm,
  fPageDialog,
  uGlobals,
  uProfuns,
  uFileCreator;

type
  TfmFileNew = class(TfmPageDialog)
    tsLines: TTabSheet;
    tsAreas: TTabSheet;
    tsSurfaces: TTabSheet;
    tsVolumes: TTabSheet;
    GroupBoxStart: TGroupBox;
    stXO: TStaticText;
    stYO: TStaticText;
    stZO: TStaticText;
    GroupBoxEnd: TGroupBox;
    stYE: TStaticText;
    stXE: TStaticText;
    stZE: TStaticText;
    seNPoints: TSpinEdit;
    StaticTextNumberOfPoints: TStaticText;
    GroupBoxOutput: TGroupBox;
    ToolBarShowAs: TToolBar;
    ToolButton3: TToolButton;
    ToolButtonMap: TToolButton;
    ToolButtonTable: TToolButton;
    ToolButtonGraph: TToolButton;
    PanelOutPath: TPanel;
    EditOutputName: TEdit;
    SpeedButtonOutputBrowse: TSpeedButton;
    ProgressBar: TProgressBar;
    stGO: TStaticText;
    stGE: TStaticText;
    rgVolumes: TRadioGroup;
    rgAreas: TRadioGroup;
    rgDistribution: TRadioGroup;
    rgSurfaces: TRadioGroup;
    GroupBox: TGroupBox;
    StringGridClasses: TStringGrid;
    HeaderControlGrid: THeaderControl;
    rgLines: TRadioGroup;
    SpinEditClasses: TSpinEdit;
    StaticTextNumberOfClasses: TStaticText;
    evXO: TEdit;
    evXE: TEdit;
    evYO: TEdit;
    evZO: TEdit;
    evGO: TEdit;
    evYE: TEdit;
    evZE: TEdit;
    evGE: TEdit;
    rgSpacing: TRadioGroup;
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PageControlChanging(Sender: TObject; var AllowChange: boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PageControlChange(Sender: TObject);
    procedure ButtonFunctionClick(Sender: TObject);
    procedure ToolButtonShowAsClick(Sender: TObject);
    procedure rgVolumesClick(Sender: TObject);
    procedure rgDistributionClick(Sender: TObject);
    procedure SpinEditClassesChange(Sender: TObject);
    procedure rgAreasClick(Sender: TObject);
    procedure rgLinesClick(Sender: TObject);
    procedure rgSurfacesClick(Sender: TObject);
  public
    OutModelType: integer;
    procedure SetSortClasses;
  private
    function CreateNewModelTable: boolean;
    procedure ReadIniFile;
    procedure WriteIniFile;
    procedure GenerateAndWritePoints(const FileName: TFileName);
  end;

var
  fmFileNew: TfmFileNew;

//===========================================================================
implementation
//===========================================================================

uses
  uCommon,
  uResStrings,
  fFileOpenText,
  uRandomAM;

{$R *.DFM}


procedure TfmFileNew.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;

procedure TfmFileNew.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
  SetSortClasses;
  PageControlChange(Self);
end;


procedure TfmFileNew.PageControlChanging(Sender: TObject; var AllowChange: boolean);
begin
  if ((Sender as TPageControl).ActivePage = tsLines) then
    AllowChange := (EditOutputName.Text <> '')
  else
    AllowChange := True;
end;


procedure TfmFileNew.GenerateAndWritePoints(const FileName: TFileName);
var
  I, J: Integer;
  TablePoints : TTable;

  Npts: Integer;   // Number of points
  Angle : Single;
  R, R1, H, Phi, Theta : Single;
  X, Y, Z, G : Single; // Coordinates and G value of points inside a volume

  XO, XE : Single;
  YO, YE : Single;
  ZO, ZE : Single;
  GO, GE : Single;
  Mean, StDev : Extended;

begin
   TablePoints := TTable.Create(nil);
   TablePoints.TableName := FileName;
   TablePoints.Open;
   TablePoints.First;
   Npts := seNPoints.Value;
   XO := StrToInt(evXO.Text);
   XE := StrToInt(evXE.Text);
   YO := StrToInt(evYO.Text);
   YE := StrToInt(evYE.Text);
   ZO := StrToInt(evZO.Text);
   ZE := StrToInt(evZE.Text);
   GO := StrToFloat(evGO.Text);
   GE := StrToFloat(evGE.Text);
   Mean := 100.00; StDev := 10.00;
   I := 0; J := 0;
   for I := 0 to Npts - 1 do
   begin
     try
       // Different types of containers
       case  PageControl.ActivePageIndex of
         0: //Points in Lines
             begin
             // Random dh collars on xy plane and random contacts in lines along z
               X := RandUniform(XO,XE);
               Y := RandUniform(YO,YE);
               Z := RandUniform(ZO,ZE);
             end;
         1: //Points in Areas
           case rgAreas.ItemIndex of
             0: // In Rectangle
             begin
               X := RandUniform(XO,XE);
               Y := RandUniform(YO,YE);
               Z := ZE;
             end;
             1: // In Circle
             begin
               R := RandUniform(XO,XE);
//             R := 100*Random_Normal;
//             R := 100*Random_Exponential; // or R := RandExponent(100);
//             R := Random_Poisson (Mean);   // The ring of Saturn
               Theta := RandUniform(0, 2*Pi);
               X := R*Cos(Theta);
               Y := R*Sin(Theta);
               Z := ZE;
             end;
             2: // In Triangle
             begin
               R := RandUniform(XO,XE);
               Theta := RandUniform(0, 2*Pi);
               Z := ZE;
             end;
         end;
         2: //Points on Surfaces
         case rgSurfaces.ItemIndex of
           0: // On Box
           begin
             if (J > 5) then
               J := 0;
             if J = 0 then
             begin
               X := XO;
               Y := RandUniform(YO,YE);
               Z := RandUniform(ZO,ZE);
             end;
             if J = 1  then
             begin
               X := XE;
               Y := RandUniform(YO,YE);
               Z := RandUniform(ZO,ZE);
             end;
             if J = 2  then
             begin
               X := RandUniform(XO,XE);
               Y := YO;
               Z := RandUniform(ZO,ZE);
             end;
             if J = 3  then
             begin
               X := RandUniform(XO,XE);
               Y := XE;
               Z := RandUniform(ZO,ZE);
             end;
             if J = 4  then
             begin
               X := RandUniform(XO,XE);
               Y := RandUniform(YO,YE);
               Z := ZO;
             end;
             if J = 5  then
             begin
               X := RandUniform(XO,XE);
               Y := RandUniform(YO,YE);
               Z := ZE;
             end;
             Inc(J)
           end;
           1: // On Sphere
           begin
             R := RandUniform(ZE,ZO);
             Theta := RandUniform(0, 2*Pi);
             R1 := Sqrt(abs(XO*XO-R*R));
             X := R1 * Cos(Theta);
             Y := R1 * Sin(Theta);
             Z := R;
           end;
           2: // On Cylinder
           begin
             R := RandUniform(ZO,ZE);
             Theta := RandUniform(0, 2*Pi);
             X := (XE-XO)*Sin(Theta)/2;
             Y := (YE-YO)*Cos(Theta)/2;
             Z := R;
           end;
           3: // On Cone
           begin
             R := RandUniform(XO,XE);
             R1 := Sqrt(abs((XE-XO) - R*R));
             Theta := RandUniform(0, 2*Pi);
             X := R1 *  Cos(Theta);
             Y := R1 *  Sin(Theta);
             Z := R;
           end;
         end; //case
         3: //Points in Volumes
         case rgVolumes.ItemIndex of
           0: // In Box
           begin
             X := RandUniform(XO,XE);
             Y := RandUniform(YO,XE);
             Z := RandUniform(ZO,ZE);
           end;
           1: // In Sphere
           begin
             R := RandUniform(XO,XE);
             R1 := Sqrt(abs(R));
             Phi := ArcCos(1 - 2* Random());
             Theta := RandUniform(0, 2*Pi);
             X := R1 *  Cos(Theta)*Sin(Phi);
             Y := R1 *  Sin(Theta)*Sin(Phi);
             Z := R1 *  Cos(Phi);
           end;
           2: // In Cylinder
           begin
             R := RandUniform(XO,XE);
             R1 := Sqrt(abs(R));
             Theta := RandUniform(0, 2*Pi);
             X := R1*Sin(Theta);
             Y := R1*Cos(Theta);
             Z := RandUniform(0, ZO - ZE);
           end;
           3: // In Cone
           begin
             //
           end;
         end; //case
       end;

       case rgDistribution.ItemIndex of
         0: G := RandUniform(GO,GE);
         1: G := 100*Random_Normal; //RandNorm(Mean, StDev: single);
         2: G := RandLogNorm(Mean, StDev);
         3: G := 100*Random_Exponential; // or R := RandExponent(256);
         4: G := Random_Poisson (Mean);
         5: G := 1;//RandEmpiric(X: array of single; Fx: array of single; NClass: integer);
       end;

       TablePoints.Append;
       TablePoints.FieldByName(fldID).AsInteger := I+1;
       TablePoints.FieldByName(fldX).AsFloat := RoundTo(X, Precision);
       TablePoints.FieldByName(fldY).AsFloat := RoundTo(Y, Precision);
       TablePoints.FieldByName(fldZ).AsFloat := RoundTo(Z, Precision);
       TablePoints.FieldByName(fldG).AsFloat := RoundTo(G, Precision);
     finally
       TablePoints.Post;
     end;
   end;
   TablePoints.Free;
end;

function TfmFileNew.CreateNewModelTable: boolean;
begin
  Result := True;
  with dmBase do
  begin
    case PageControl.ActivePageIndex of
    0: begin // Polylines
        OutModelType := mtDholes;
        TableOutput.TableName := DataBasePath + DirDholes + EditOutputName.Text;
        CreateHoleTables(TableOutput.TableName);
       end;
    1: begin // Area objects
         OutModelType := mtPoints2D;
         TableOutput.TableName := DataBasePath + DirPoints2D + EditOutputName.Text;
         CreatePoint2DTables(TableOutput.TableName);
        end;
    2: begin  // Surface objects
         OutModelType :=  mtPoints2D;
         TableOutput.TableName := DataBasePath + DirPoints2D + EditOutputName.Text;
         CreatePoint2DTables(TableOutput.TableName);
       end;
    3: begin // Volume objects
         OutModelType :=  mtPoints3D;
         TableOutput.TableName := DataBasePath + DirPoints3D + EditOutputName.Text;
         CreatePoint3DTables(TableOutput.TableName);
       end;
    end;
    GenerateAndWritePoints(TableOutput.TableName);
  end;
end;

procedure TfmFileNew.ToolButtonShowAsClick(Sender: TObject);
begin
  ToolBarShowAs.Tag := (Sender as TToolButton).Tag;
end;

procedure TfmFileNew.PageControlChange(Sender: TObject);
begin
  OutModelType := PageControl.ActivePageIndex;
  case PageControl.ActivePageIndex of
  0: rgLinesClick(Sender);
  1: rgAreasClick(Sender);
  2: rgSurfacesClick(Sender);
  3: rgVolumesClick(Sender);
  end;
  ButtonOK.Enabled := True;
end;

procedure TfmFileNew.rgLinesClick(Sender: TObject);
begin
   PanelOutPath.Caption := DataBasePath + DirDHoles;
   case rgLines.ItemIndex of
     0: EditOutputName.Text   := 'Segments';
     1: EditOutputName.Text   := 'Polylines';
     2: EditOutputName.Text   := 'Polygons';
   end;
   OutModelType := mtPoints2D;
end;

procedure TfmFileNew.rgAreasClick(Sender: TObject);
begin
  PanelOutPath.Caption := DataBasePath + DirPoints2D;
  case rgAreas.ItemIndex of
   0: EditOutputName.Text   := 'Rectangle';
   1: EditOutputName.Text   := 'Circle';
   2: EditOutputName.Text   := 'Triangle';
  end;
  OutModelType := mtPoints2D;
end;

procedure TfmFileNew.rgSurfacesClick(Sender: TObject);
begin
  PanelOutPath.Caption := DataBasePath + DirPoints3D;
  case rgSurfaces.ItemIndex of
     0: EditOutputName.Text   := 'Box';
     1: EditOutputName.Text   := 'Sphere';
     2: EditOutputName.Text   := 'Cylinder';
     3: EditOutputName.Text   := 'Cone';
  end;
  OutModelType := mtPoints3D;
end;

procedure TfmFileNew.rgVolumesClick(Sender: TObject);
begin
  PanelOutPath.Caption := DataBasePath + DirPoints3D;
  case rgVolumes.ItemIndex of
     0: EditOutputName.Text   := 'Box';
     1: EditOutputName.Text   := 'Sphere';
     2: EditOutputName.Text   := 'Cylinder';
     3: EditOutputName.Text   := 'Cone';
  end;
  OutModelType := mtPoints3D;
end;


procedure TfmFileNew.rgDistributionClick(Sender: TObject);
begin
  case rgDistribution.ItemIndex of
    0, 1, 2, 3, 4:
       begin
         StringGridClasses.Enabled := False;
         SpinEditClasses.Enabled := False;
         GroupBox.Enabled := False;
         StaticTextNumberOfClasses.Enabled := False;
       end;
    5: begin  //Empirical for N classes in [Gmin, Gmax] range
         StringGridClasses.Enabled := True;
         SpinEditClasses.Enabled := True;
         GroupBox.Enabled := True;
         StaticTextNumberOfClasses.Enabled := True;
       end;
  end;
end;


procedure TfmFileNew.SetSortClasses;
var
  I, Sort: integer;
  Lo, Hi:  double;

begin
  // Instead of 100 here should be Max Value for active attribute
  for I := 0 to SpinEditClasses.Value - 1 do
  begin
    StringGridClasses.Cells[0, I] := IntToStr(I + 1);
    Lo := I * round(100 / SpinEditClasses.Value);
    StringGridClasses.Cells[1, I] := FloatToStr(Lo);
    Hi := (I + 1) * round(100 / SpinEditClasses.Value);
    StringGridClasses.Cells[2, I] := FloatToStr(Hi);
    StringGridClasses.Cells[3, I] := IntToStr(I);
  end;
end;

procedure TfmFileNew.SpinEditClassesChange(Sender: TObject);
begin
  inherited;
  StringGridClasses.RowCount := SpinEditClasses.Value;
  SetSortClasses;
end;

procedure TfmFileNew.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteInteger(Name, PageControl.Name, PageControl.ActivePageIndex);
      WriteString(Name, seNPoints.Name, IntToStr(seNPoints.Value));
      WriteString(Name, evXO.Name, evXO.Text);
      WriteString(Name, evXE.Name, evXE.Text);
      WriteString(Name, evYO.Name, evYO.Text);
      WriteString(Name, evYE.Name, evYE.Text);
      WriteString(Name, evZO.Name, evZO.Text);
      WriteString(Name, evZE.Name, evZE.Text);
      WriteString(Name, evGO.Name, evGO.Text);
      WriteString(Name, evGE.Name, evGE.Text);
      WriteInteger(Name, rgLines.Name, rgLines.ItemIndex);
      WriteInteger(Name, rgAreas.Name, rgAreas.ItemIndex);
      WriteInteger(Name, rgSurfaces.Name, rgSurfaces.ItemIndex);
      WriteInteger(Name, rgVolumes.Name, rgVolumes.ItemIndex);
      WriteInteger(Name, rgDistribution.Name, rgDistribution.ItemIndex);
    finally
      IniFile.Free;
    end;
end;

procedure TfmFileNew.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      PageControl.ActivePageIndex := ReadInteger(Name, PageControl.Name, 0);
      seNPoints.Value := StrToInt(ReadString(Name, seNPoints.Name, seNPoints.Text));
      evXO.Text := ReadString(Name, evXO.Name, evXO.Text);
      evXE.Text := ReadString(Name, evXE.Name, evXE.Text);
      evYO.Text := ReadString(Name, evYO.Name, evYO.Text);
      evYE.Text := ReadString(Name, evYE.Name, evYE.Text);
      evZO.Text := ReadString(Name, evZO.Name, evZO.Text);
      evZE.Text := ReadString(Name, evZE.Name, evZE.Text);
      evGO.Text := ReadString(Name, evGO.Name, evGO.Text);
      evGE.Text := ReadString(Name, evGE.Name, evGE.Text);
      rgLines.ItemIndex := ReadInteger(Name, rgLines.Name, 0);
      rgAreas.ItemIndex := ReadInteger(Name, rgAreas.Name, 0);
      rgSurfaces.ItemIndex := ReadInteger(Name, rgSurfaces.Name, 0);
      rgVolumes.ItemIndex := ReadInteger(Name, rgVolumes.Name, 0);
      rgDistribution.ItemIndex := ReadInteger(Name, rgDistribution.Name, 0);
    finally
      IniFile.Free;
    end;
end;


procedure TfmFileNew.ButtonFunctionClick(Sender: TObject);
begin
  /// Defines a function to place points
end;

procedure TfmFileNew.ButtonOKClick(Sender: TObject);
begin
  inherited;
  if not CreateNewModelTable then
  begin
    ModalResult := mrNone;
    Exit;
  end;
end;

end.
