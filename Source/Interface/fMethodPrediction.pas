//----------------------------------------------------------------------------
// The unit of Geoblock, http://sourceforge.net/projects/geoblock
//---------------------------------------------------------------------------
{! The one dimensional prediction of mineral liberation using modal analysis of samples }

unit fMethodPrediction;

interface

uses
  Winapi.Windows, 
  Winapi.Messages, 
  System.SysUtils, 
  System.Variants, 
  System.Classes, 
  System.Math, 
  System.IniFiles,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms,
  Vcl.Dialogs, 
  Vcl.StdCtrls, 
  Vcl.ComCtrls, 
  Vcl.Buttons, 
  Vcl.ToolWin, 
  Vcl.ExtCtrls,
  Vcl.Grids,
  Vcl.Samples.Spin,

  Data.DB,
  Bde.DBTables,

  
  fMethodDialog, System.ImageList, Vcl.ImgList;

type
  TfmMethodPrediction = class(TfmMethodDialog)
    RadioGroupGrainSize: TRadioGroup;
    GroupBoxGranulometry: TGroupBox;
    PanelDistributionButtons: TPanel;
    SpeedButtonASTM:    TSpeedButton;
    SpeedButtonPower:   TSpeedButton;
    PanelDistribution:  TPanel;
    StringGridMillProduct: TStringGrid;
    GroupBoxRecovery:   TGroupBox;
    PanelRecoveryButtons: TPanel;
    SpeedButtonLognorm: TSpeedButton;
    SpeedButtonUniform: TSpeedButton;
    PanelFunction:      TPanel;
    StringGridRecoveryProduct: TStringGrid;
    SpinEditGrain:      TSpinEdit;
    HeaderControlSizes: THeaderControl;
    HeaderControlRecoveries: THeaderControl;
    procedure FormCreate(Sender: TObject);
    procedure SpeedButtonPowerClick(Sender: TObject);
    procedure SpeedButtonASTMClick(Sender: TObject);
    procedure SpeedButtonUniformClick(Sender: TObject);
    procedure SpeedButtonLognormClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    // Private declarations
    ActiveAttribute, Mineral, // Selected Mineral name field from the dialog list
    Mineral_Grain: string; // Mineral_Grain field in the Table
    Grain: integer; // Mean interception length for the target phase B
    Assay, // Content of target mineral phase B in raw ore, %
    Grade, // Content of B phase in concentrate, %
    Yield, // Portion of concentrate, %
    Recovery, // Recovery of B phase in concentrates, %
    Rest, // Content of B in waste or tailings, %
    Waste, // Portion of waste or tailings, %
    Loss:  double; // Recovery of B phase in waste or tailings, %
    procedure ValsFromDialog;
    procedure SetGradeClasses;
    procedure SetSizeClasses;
    function Prediction: boolean;
    procedure ReadIniFile;
    procedure WriteIniFile;
  public
     
  end;

var
  fmMethodPrediction: TfmMethodPrediction;

//============================================================
implementation
//============================================================

uses
  uGlobals,
  uResStrings,
  uCommon,
  uProfuns,
  dBase;

{$R *.dfm}

const
  MaxSizeClass  = 12; //The number of classes for size distribution
  MaxGradeClass = 12; //The number of classes for Grade distribution

  MaxGrain = 16 * 1024; // Default number of grains
  Seeds     = 1120;      // Seeds for random generation in 3D
  GenLine  = 32 * 1024; // Default length of GenLine in microns

type
  PGrain    = ^TGrain; //Pointer to TGrain type
  TGrain    = array[1..MaxGrain] of integer;
  TFloatVec = array[1..MaxSizeClass] of double;
  TIntMat   = array[1..MaxSizeClass, 1..MaxGradeClass] of integer;

var
  AGrains, BGrains: PGrain; // Pointers to grain arrays
  SizeVec:  TFloatVec;      // Mean Particle sizes in System.Classes, mu
  GrindVec: TFloatVec;      // Expected Mill Product, in %
  RecovVec: TFloatVec;      // Recovery Probability Function, in %

var
  DefString: string[20];

procedure TfmMethodPrediction.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
  SetSizeClasses;
  SetGradeClasses;
end;

procedure TfmMethodPrediction.SetGradeClasses;
var
  I: integer;
  Lev1, Lev2: integer;
begin
  with StringGridRecoveryProduct do
  begin
    //Default Grade Scale of Particles
    for I := 1 to 12 do
      Cells[0, I - 1] := IntToStr(I);
    if SpeedButtonUniform.Down = True then
    begin
      Cells[1, 0] := '0';
      for I := 2 to 11 do
        Cells[1, I - 1] := IntToStr((I - 2) * 10) + '-' + IntToStr((I - 1) * 10);
      Cells[1, 11] := '100';
    end;
    if SpeedButtonLognorm.Down = True then
    begin
      Cells[1, 0] := '0' + '-' + '1';
      for I := 2 to 7 do
      begin
        Lev1 := Round(IntPower(2, I - 2));
        Lev2 := Round(IntPower(2, I - 1));
        Cells[1, I - 1] := IntToStr(Lev1) + '-' + IntToStr(Lev2);
      end;
      Cells[1, 7] := IntToStr(64) + '-' + IntToStr(70);
    end;
    //Default Extraction Probabilities in Grade System.Classes, %
    Cells[2, 0]  := '0';
    Cells[2, 1]  := '2';
    Cells[2, 2]  := '5';
    Cells[2, 3]  := '20';
    Cells[2, 4]  := '66';
    Cells[2, 5]  := '75';
    Cells[2, 6]  := '82';
    Cells[2, 7]  := '86';
    Cells[2, 8]  := '90';
    Cells[2, 9]  := '95';
    Cells[2, 10] := '99';
    Cells[2, 11] := '99';
  end;
end;

procedure TfmMethodPrediction.SetSizeClasses;
var
  I: integer;
  Dp1, Dp2: integer; //Lower and Upper limits in classes

begin
  // Initialize StringGridGrindProduct visual component
  with StringGridMillProduct do
  begin
    for I := 1 to 12 do
      Cells[0, I - 1] := IntToStr(I); //Class Numbers
    if SpeedButtonPower.Down = True then
    begin //Geometrical Scale with base 2 in Microns
      Cells[1, 0] := '0' + '-' + '1';
      SizeVec[1]  := 1;
      for I := 2 to 11 do
      begin
        Dp1 := Round(IntPower(2, I - 2));
        Dp2 := Round(IntPower(2, I - 1));
        Cells[1, I - 1] := IntToStr(Dp1) + '-' + IntToStr(Dp2);
        SizeVec[I] := Dp2;
      end;
      Cells[1, 11] := '>' + '1024';
      SizeVec[12]  := 2048;
    end;
    if SpeedButtonASTM.Down = True then
    begin //ASTM (USA) or TC-24 (ISA) Sizes in Microns
      Cells[1, 0]  := '0' + '-' + '45';
      SizeVec[1]   := 45;
      Cells[1, 1]  := '45' + '-' + '63';
      SizeVec[2]   := 63;
      Cells[1, 2]  := '63' + '-' + '90';
      SizeVec[3]   := 90;
      Cells[1, 3]  := '90' + '-' + '125';
      SizeVec[4]   := 125;
      Cells[1, 4]  := '125' + '-' + '180';
      SizeVec[5]   := 180;
      Cells[1, 5]  := '180' + '-' + '250';
      SizeVec[6]   := 250;
      Cells[1, 6]  := '250' + '-' + '355';
      SizeVec[7]   := 355;
      Cells[1, 7]  := '355' + '-' + '500';
      SizeVec[8]   := 500;
      Cells[1, 8]  := '500' + '-' + '710';
      SizeVec[9]   := 710;
      Cells[1, 9]  := '710' + '-' + '1000';
      SizeVec[10]  := 1000;
      Cells[1, 10] := '1000' + '-' + '2000';
      SizeVec[11]  := 2000;
      Cells[1, 11] := '>' + '2000';
      SizeVec[12]  := 4000;
    end;
    //Default Yields by Size Classes in Percents
    Cells[2, 0]  := '0';
    Cells[2, 1]  := '2';
    Cells[2, 2]  := '6';
    Cells[2, 3]  := '22';
    Cells[2, 4]  := '50';
    Cells[2, 5]  := '10';
    Cells[2, 6]  := '6';
    Cells[2, 7]  := '4';
    Cells[2, 8]  := '0';
    Cells[2, 9]  := '0';
    Cells[2, 10] := '0';
    Cells[2, 11] := '0';
  end;
end;

procedure TfmMethodPrediction.SpeedButtonPowerClick(Sender: TObject);
begin
  SetSizeClasses;
end;

procedure TfmMethodPrediction.SpeedButtonASTMClick(Sender: TObject);
begin
  SetSizeClasses;
end;

procedure TfmMethodPrediction.SpeedButtonUniformClick(Sender: TObject);
begin
  SetGradeClasses;
end;

procedure TfmMethodPrediction.SpeedButtonLognormClick(Sender: TObject);
begin
  SetGradeClasses;
end;

procedure TfmMethodPrediction.ValsFromDialog;
var
  I: integer;
begin
  Grain := SpinEditGrain.Value; //Value for mean phase size
  for I := 1 to MaxSizeClass do //Values for Mill Product
    GrindVec[I] := StrToFloat(StringGridMillProduct.Cells[2, I - 1]);
  for I := 1 to MaxGradeClass do //Values for Extraction
    RecovVec[I] := StrToFloat(StringGridRecoveryProduct.Cells[2, I - 1]);
end;


{-------------------- ORE DRESSING PREDICTION -----------------
 Input:
   Assay     - Volume content of target mineral phase B
   Grain     - Mean size of B mineral phase in a sample
   GrindVec  - Density size distribution of the grinding product
   RecovVec  - Recovery function for B in concentrate
 Output:     (in volumes %)
    Grade    - Content of B in concentrate
    Yield    - Portion of concentrate from raw ore
    Recovery - Recovery of B in concentrate
    Rest     - Content of B in tailings
    Waste    - Portion of waste or tailings from raw ore
    Loss     - Losses of B in tailings
--------------------------------------------------------------}

function TfmMethodPrediction.Prediction: boolean;

var
  LibMat:  TIntMat; // Liberation Matrix of Texture
  InFrac:  TIntMat; // Input Fractions to Mill or Separator
  OutFrac: TIntMat; // Output Fractions from Mill or Separator

  //-------------------- PhaseGenerator ----------------------------
  // Generate a set of grain intervals for A and B phases
  // Input:
  //    Assay  - Linear, Areal or Volume portion of phases B
  //    Grain  - Mean specific interceipt length of the target B phase
  //             from linear microscopic analysis
  //          or Mean specific area of the B phase
  //             from areal microscopic analysis
  //          or Mean specific volume after stereological reconstruction

  // Output: AGrain and BGrain arrays along GenLine
  //-----------------------------------------------------------------
  {sub}
  procedure PhaseGenerator;
  var
    I:      integer;
    Period: integer; // The period for two-phase ore texture
    //  A,
    AB:     integer; // Sum Length of A and AB
    Da, Db: integer; // Grain A and B sizes, segments

  begin
    //  A:=0;
    AB     := 0;
    Period := Round(100 * Grain / Assay); // Sum of mean A and B lengths
    Da     := Period - Grain; // Mean length of the waste phase A
    Db     := Grain; // Mean length of the target phase B
    Randomize; // The Random number generator
    //RandSeed:= Seed;
    I := 0;
    repeat
      Inc(I);
      AGrains^[I] := Random(Da);          // New A grain length
      //    A:= A+AGrains^[I];            // Sum phase A on the line
      AB := AB + AGrains^[I];
      BGrains^[I] := Random(Db);          // New B grain length
      AB := AB + BGrains^[I];             // Entire line
    until AB >= GenLine;
    if AB > GenLine then // Removing extra intersections
    begin
      BGrains^[I] := BGrains^[I] - (AB - GenLine);
      if BGrains^[I] < 0 then
      begin
        AGrains^[I] := AGrains^[I] + BGrains^[I];
        BGrains^[I] := 0;
      end;
    end;
  end; // GenPhases

  //==================================================================
  // Description: Calculation matrix of liberation  LibMat
  // Input:    Set of A and B phases for generated line GenLine
  //           Size distribution of fragments (particles)
  // Output:   Matrix of liberation  [MaxSizeClass*MaxGradeClass]
  //           volume distribution of fragments
  //==================================================================
  procedure Liberator;
  var
    I, J, K, A, B, AB: integer; // Sum length of A, B and AB on GenLine
    La, Lb: integer; // Sum A or B  in a composite particle
    Lab:    integer; // Sum A and B in composite particle
    Dp:     integer; // Particle size
    After:  integer; // Rest A or B beyond the particle length
    Pure:   integer; // Free A or B segment
    Quota:  real;    // Content of A or B in locked particles
    Switch: boolean; // Toggler between phases A and B

    ChordA: integer;
    ChordB: integer; // Current A and B phase grain sizes

  begin
    FillChar(LibMat, SizeOf(LibMat), 0);
    for I := 1 to MaxSizeClass do
    begin
      Dp := Round(SizeVec[I]); // Particle sizes in classes
      if Dp > GenLine then
        Continue;
      Grade  := 0;
      After  := 0;
      Switch := False;
      K      := 0;
      La     := 0;
      Lb     := 0;
      A      := 0;
      B      := 0;
      //    AB:=0;
      repeat // GenLine
        Lab := After;
        while Lab < Dp do
        begin
          Switch := not Switch;
          if Switch = True then // Mineral A
          begin
            Inc(K);
            ChordA := AGrains^[K];
            La     := La + ChordA; // sum A in locked particles
            A      := A + ChordA;  // sum A on line
            Lab    := Lab + ChordA;
          end
          else
          begin
            ChordB := BGrains^[K];
            Lb     := Lb + ChordB; // sum B in locked
            B      := B + ChordB;  // sum B on line
            Lab    := Lab + ChordB;
          end;
          After := Lab - Dp;
        end; // While
        if Switch = True then // Mineral A
        begin
          La := La - After;
          if La = Dp then // Add one Free particle A
            LibMat[I, 1] := LibMat[I, 1] + 1
          else // Add one composite particle
          begin
            Quota := (Dp - La) / Dp;
            J     := Trunc(Quota * 10) + 2;
            LibMat[I, J] := LibMat[I, J] + 1;
          end;
        end
        else // Mineral B
        begin
          Lb := Lb - After;
          if Lb = Dp then // Add one Free particle B
            LibMat[I, MaxGradeClass] := LibMat[I, MaxGradeClass] + 1
          else // Add one composite particle
          begin
            Quota := Lb / Dp;
            J     := Trunc(Quota * 10) + 2;   // Define the current Grade class
            LibMat[I, J] := LibMat[I, J] + 1; // Add one locked particle
          end;
        end;
        if After >= Dp then
        begin
          Pure  := After div Dp; // Number of free particles
          After := After mod Dp; // After fragmentation
          if Switch = True then
          begin // Free A mineral phase
            LibMat[I, 1] := LibMat[I, 1] + Pure;
            La := After;
            Lb := 0;
          end
          else
          begin // Free B  Mineral phase
            LibMat[I, MaxGradeClass] := LibMat[I, MaxGradeClass] + Pure;
            La := 0;
            Lb := After;
          end;
        end
        else
        begin
          if Switch = True then
          begin
            La := After;
            Lb := 0;
          end
          else
          begin
            La := 0;
            Lb := After;
          end;
        end;
        AB := A + B - After;
      until AB >= GenLine;
    end;
    // Convert from numeric to volume distribution
    for I := 1 to MaxSizeClass do
      for J := 1 to MaxGradeClass do
        LibMat[I, J] := LibMat[I, J] * Round(SizeVec[I]);
  end; // Liberator

  //------------------- Comminutor ------------------------
  // Input:
  //     Fin - input size/Grade distribution
  //     LibMat  - liberation matrix of fragments
  //     GrindVec - size distribution of feed particles
  // OutPut:
  //     Fout - fractions of product particles
  //-------------------------------------------------------
  {sub}
  procedure Comminutor(var Fin, Fout: TIntMat);
  var
    I, J: integer;
  begin
    FillChar(Fout, SizeOf(Fout), 0);
    //Multiply matrix Fin and vector grindvec to calculate matrix Fout
    for I := 1 to MaxSizeClass do
      for J := 1 to MaxGradeClass do
      begin
        if Fin[I, J] > 0 then
          Fout[I, J] := Round(Fin[I, J] * GrindVec[I] * 0.01)
        else
          Fout[I, J] := 0;
      end;
  end; // Comminutor

  //------------------------- Separator ------------------------------
  // Input:
  //   Fin - input Size/Grade distribution before separation
  //   RecovVec - Separation function or recovery probabilities
  // OutPut:
  //   Fout - output Size/Grade distribution in concentrate
  //   Integral parameters:
  //       Grade     -  content of B in concentrate, vol.%
  //       Yield     -  portion of concentrate, vol.%
  //       Recovery  -  recovery of B in concentrate, vol.%
  //       Rest    -  content of B in tailings, vol.%
  //       Waste     -  portion of tailings, vol.%
  //       Loss      -  recovery of B in tailings, vol.%
  //------------------------------------------------------------------
  {sub}
  procedure Separator(var Fin, Fout: TIntMat);
  var
    I, J:  integer;
    SumAFin, SumBFin, // Sum A and B in raw product
    SumAFout, SumBFout, // Sum A and B in out product
    SumFin, SumFout: real; // Sums In fractions and Out fractions
    Alpha: real; // Content of B on GenLine

  begin
    Yield    := 0;
    Grade    := 0;
    Recovery := 0;
    Waste    := 0;
    Rest     := 0;
    Loss     := 0;
    FillChar(Fout, Sizeof(Fout), #0);
    // Define Sum of fragments for out matrix
    SumFin := 0;
    for I := MaxSizeClass downto 1 do
      for J := 1 to MaxGradeClass do
        if Fin[I, J] > 0 then
          SumFin := SumFin + Fin[I, J];
    // Calculation of input A and B in locked particles
    // in raw material
    SumAFin := 0;
    SumBFin := 0;
    for I := 1 to MaxSizeClass do
      for J := 2 to MaxGradeClass - 1 do
      begin
        SumAFin := SumAFin + Fin[I, J] * (1 - (J * 0.1 - 0.15));
        SumBFin := SumBFin + Fin[I, J] * (J * 0.1 - 0.15);
      end;
    // Add free fragments of A and B phases
    for I := 1 to MaxSizeClass do
    begin
      SumAFin := SumAFin + Fin[I, 1];
      SumBFin := SumBFin + Fin[I, MaxGradeClass];
    end;
    // Calculation Size/Grade out fractions using
    // recovery probability vector of concentrate
    SumFout := 0;
    for I := 1 to MaxSizeClass do
      for J := 1 to MaxGradeClass do
      begin // marginal sum of fragments
        Fout[I, J] := Round(Fin[I, J] * RecovVec[J] * 0.01);
        SumFout    := SumFout + Fout[I, J];
      end;
    // Calculating absolute inputs of A and B in locked particles
    // for final product
    SumAFout := 0;
    SumBFout := 0;
    for I := 1 to MaxSizeClass do
      for J := 2 to MaxGradeClass - 1 do
      begin
        SumAFout := SumAFout + Fout[I, J] * (1 - (J * 0.1 - 0.15));
        SumBFout := SumBFout + Fout[I, J] * (J * 0.1 - 0.15);
      end;
    // Add free fragments of A and B
    for I := 1 to MaxSizeClass do
    begin
      SumAFout := SumAFout + Fout[I, 1];
      SumBFout := SumBFout + Fout[I, MaxGradeClass];
    end;
    //-------- Calculation Yield, Grade and Recovery ---------
    Yield := SumFout / SumFin;
    if (SumFout <> 0) then
      Grade := SumBFout / SumFout
    else
      Grade := 0;

    if Grade >= 1 then
      Grade := 1;
    if Yield >= 1 then
      Yield := 1;

    Waste := 1 - Yield;
    if (SumFout <> SumFin) then
      Rest := SumAFout / (SumFin - SumFout)
    else
      Rest := 0;

    Alpha := SumBFin / SumFin;
    if Alpha <> 0 then
    begin
      Recovery := Yield * Grade / Alpha;
      Loss     := Waste * Rest / Alpha;
    end
    else
    begin
      Recovery := 0;
      Loss     := 0;
    end;
    // Concentrate parameters in percentage
    Yield    := 100 * Yield;
    Grade    := 100 * Grade;
    Recovery := 100 * Recovery;
    // Tailings parameters in percentage
    Waste    := 100 * Waste;
    Rest     := 100 * Rest;
    Loss     := 100 * Loss;
  end; //Separator

  //-------------------------
  // Main body of Prediction
  //-------------------------
begin
  // Mineral Phase Size Generation for A and B minerals
  PhaseGenerator;
  // Mineral Liberation Matrix calculation
  Liberator;
  // Size and Grade distribution after comminution
  InFrac := LibMat;
  Comminutor(InFrac, OutFrac);
  // Size and Grade distribution after separation
  InFrac := OutFrac;
  Separator(InFrac, OutFrac);
  Result := True;
end;

procedure TfmMethodPrediction.ButtonOKClick(Sender: TObject);

var
  I: integer;
  Grade_Mineral, Yield_Mineral, Recovery_Mineral: string;

begin
  inherited;
  with dmBase do
    try
      //Mineral := ActiveAttribute; not work
      Mineral := GetCurrentItem(ListBoxRealAttribute);
      TableInput.TableName := InModelName;
      if RadioGroupGrainSize.ItemIndex = 1 then // Read Rhythm from Table
      begin
        TableInput.Open; // Table with attribute fields
        Mineral_Grain := Mineral + '_' + 'RHYTHM'; // There should be such field
        if TableInput.FindField(Mineral_Grain) = nil then
        begin
          MessageDlg(Mineral_Grain + ' ???', mtError, [mbOK], 0);
          ModalResult := mrNone;
          TableInput.Close;
          Exit;
        end;
        TableInput.Close;
      end;
      // Open Table and Insert Grade_G, Yield_G and Recovery_G fields
      // with G as the SelectedMineral
      Grade_Mineral    := Mineral + '_' + 'GRADE';
      Yield_Mineral    := Mineral + '_' + 'YIELD' ;
      Recovery_Mineral := Mineral + '_' + 'RECOVERY';
      // If current table has the fields then delete it
      ProgressBar.Position := 0;
      ProgressBar.Min  := 0;
      ProgressBar.Max  := 2;
      with QueryInput do
      begin
        //Open;
        if FindField(Grade_Mineral) <> nil then
        begin
          //Close;
          SQL.Clear;
          SQL.Add('ALTER TABLE "' + InModelName + '"' + ' DROP "' +
            InModelName + '"."' + Grade_Mineral + '"');
          try
            TableInput.Close;
            ExecSQL;
          except
            on E: EDBEngineError do
              if E.Errors[0].ErrorCode <> 10009 then
              begin
                ShowMessage(LoadResString(@rsErrorWithTable));
                ModalResult := mrNone;
                Show;
                Exit;
              end;
            else
              ;
          end;
        end;
        QueryInput.Close;
        SQL.Clear;
        SQL.Add('ALTER TABLE "' + InModelName + '"' + ' ADD "' +
          InModelName + '"."' + Grade_Mineral + '" FLOAT(32,2)');
        try
          TableInput.Close;
          ExecSQL;
        except
          on E: EDBEngineError do
            if E.Errors[0].ErrorCode <> 10009 then
            begin
              ShowMessage(LoadResString(@rsErrorWithTable));
              ModalResult := mrNone;
              Show;
              Exit;
            end;
          else
            ;
        end;
        TableInput.Open;
        if TableInput.FindField(Yield_Mineral) <> nil then
        begin
          QueryInput.Close;
          SQL.Clear;
          SQL.Add('ALTER TABLE "' + InModelName + '"' + ' ADD "' +
            InModelName + '"."' + Yield_Mineral + '" FLOAT(32,2)');
          try
            TableInput.Close;
            ExecSQL;
          except
            on E: EDBEngineError do
              if E.Errors[0].ErrorCode <> 10009 then
              begin
                ShowMessage(LoadResString(@rsErrorWithTable));
                ModalResult := mrNone;
                Show;
                Exit;
              end;
            else
              ;
          end;
        end;
        QueryInput.Close;
        SQL.Clear;
        SQL.Add('ALTER TABLE "' + InModelName + '"' + ' ADD "' +
          InModelName + '"."' + Yield_Mineral + '" FLOAT(32,2)');
        try
          TableInput.Close;
          ExecSQL;
        except
          on E: EDBEngineError do
            if E.Errors[0].ErrorCode <> 10009 then
            begin
              ShowMessage(LoadResString(@rsErrorWithTable));
              ModalResult := mrNone;
              Show;
              Exit;
            end;
          else
            ;
        end;
        TableInput.Open;
        if TableInput.FindField(Recovery_Mineral) <> nil then
        begin
          QueryInput.Close;
          SQL.Clear;
          SQL.Add('ALTER TABLE "' + InModelName + '"' + ' ADD "' +
            InModelName + '"."' + Recovery_Mineral + '" FLOAT(32,2)');
          try
            TableInput.Close;
            ExecSQL;
          except
            on E: EDBEngineError do
              if E.Errors[0].ErrorCode <> 10009 then
              begin
                ShowMessage(LoadResString(@rsErrorWithTable));
                ModalResult := mrNone;
                Show;
                Exit;
              end;
            else
              ;
          end;
        end;
        QueryInput.Close;
        SQL.Clear;
        SQL.Add('ALTER TABLE "' + InModelName + '"' + ' ADD "' +
          InModelName + '"."' + Recovery_Mineral + '" FLOAT(32,2)');
        try
          TableInput.Close;
          ExecSQL;
        except
          on E: EDBEngineError do
            if E.Errors[0].ErrorCode <> 10009 then
            begin
              ShowMessage(LoadResString(@rsErrorWithTable));
              ModalResult := mrNone;
              Show;
              Exit;
            end;
          else
            ;
        end;
        QueryInput.Close;
      end;
      TableInput.Open;
      TableInput.First;
      ProgressBar.StepIt;
      ProgressBar.Max      := TableInput.RecordCount;
      ProgressBar.Position := 0;

      New(AGrains);
      New(BGrains);
      // Convert from Dialog Strings to Real Values
      ValsFromDialog;
      // Calculation ore dressing parameters for every record
      for I := 1 to TableInput.RecordCount do
      begin
        ProgressBar.Position := TableInput.RecNo;
        if TableInput.FieldByName(Mineral).IsNull = False then // Not blank
        begin // Calculate and write ore dressing parameters
          Assay := TableInput.FieldByName(Mineral).AsFloat;
          if Assay > 0 then
          begin
            if RadioGroupGrainSize.ItemIndex = 1 then
              Grain := TableInput.FieldByName(Mineral_Grain).AsInteger;
            try
              Prediction;
            except
              beep
            end;
            TableInput.Edit;
            TableInput.FieldByName(Grade_Mineral).AsFloat :=
              RoundTo(Grade, Precision);
            TableInput.FieldByName(Yield_Mineral).AsFloat :=
              RoundTo(Yield, Precision);
            TableInput.FieldByName(Recovery_Mineral).AsFloat :=
              RoundTo(Recovery, Precision);
            TableInput.Post;
          end;
        end;
        TableInput.Next;
        if ModalResult = mrCancel then
        begin
          QueryInput.Close;
          Exit;
        end;
      end;
      Dispose(AGrains);
      Dispose(BGrains);
    finally
      TableInput.Close;
    end;
end;

procedure TfmMethodPrediction.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      RadioGroupGrainSize.ItemIndex := ReadInteger(Name, RadioGroupGrainSize.Name, 0);
      SpinEditGrain.Value := ReadInteger(Name, SpinEditGrain.Name, 50);
    finally
      IniFile.Free;
    end;
end;

procedure TfmMethodPrediction.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteInteger(Name, RadioGroupGrainSize.Name, RadioGroupGrainSize.ItemIndex);
      WriteInteger(Name, SpinEditGrain.Name, SpinEditGrain.Value);
    finally
      IniFile.Free;
    end;
end;

procedure TfmMethodPrediction.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;

end.
