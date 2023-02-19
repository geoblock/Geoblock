//-----------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//----------------------------------------------------------------------------
(*
  The routines to play sounds in TerraScene
*)

unit uTerraSound;

interface

uses
  Winapi.Windows, 
  System.SysUtils,

  
  GLS.SoundManager,
  Sounds.BASS,
  GLS.Keyboard,
  Bass.Import,
  uTerraObjects,

  
  uGlobals;

var
  //SOUND VARIABLES
  CarState:     0..9;
  CurSoundName: string;
  PlayingNow:   boolean;
  IsGoingCar:   boolean;
  CarStateChanged: boolean;
  LastCarState: 0..9;
  bStream:      cardinal;
  theCurCar: TGeoSceneCar;


procedure InitSound(aCar: TGeoSceneCar; SndLib: TGLSoundLibrary);
procedure PlaySound;
procedure PlayCurSound;
procedure GetCarStateBySpeed;

//=======================================================================
implementation
//=======================================================================

procedure InitSound(aCar: TGeoSceneCar; SndLib: TGLSoundLibrary);
begin
  theCurCar := aCar;

  SndLib.Samples.AddFile(DataAssetsPath + 'Sound\start.wav');
  SndLib.Samples.AddFile(DataAssetsPath + 'Sound\idle.wav');
  SndLib.Samples.AddFile(DataAssetsPath + 'Sound\rev.wav');
  SndLib.Samples.AddFile(DataAssetsPath + 'Sound\first.wav');
  SndLib.Samples.AddFile(DataAssetsPath + 'Sound\second.wav');
  SndLib.Samples.AddFile(DataAssetsPath + 'Sound\third.wav');
  SndLib.Samples.AddFile(DataAssetsPath + 'Sound\fourth.wav');
  SndLib.Samples.AddFile(DataAssetsPath + 'Sound\stop.wav');

(*
  with TGLBSoundEmitter.Create(theCurCar.Behaviours) do
    //ox - theCurCar.Frame.Behaviours
  begin
    Source.SoundLibrary := SndLib;
    Playing := False;
  end;
  CarState     := 2;
  LastCarState := 2;
  CurSoundName := '';
  PlayingNow   := False;
  IsGoingCar   := False;
  //ox -theCurCar.MotorRunning := true;
*)
end;

procedure GetCurSound;
begin
  case CarState of
    1: CurSoundName := 'start.wav';
    2: CurSoundName := 'idle.wav';
    3: CurSoundName := 'rev.wav';
    4: CurSoundName := 'first.wav';
    5: CurSoundName := 'second.wav';
    6: CurSoundName := 'third.wav';
    7: CurSoundName := 'fourth.wav';
    8: CurSoundName := 'stop.wav';
  end;
end;

procedure PlayCurSound;
begin
  CarStateChanged := (CarState <> LastCarState);
  if (CarState <> 0) then
  begin
    with TGLBSoundEmitter(theCurCar.Behaviours[0]) do //ox Frame.Behaviours[0]
    begin
      if ((not CarStateChanged) and (not PlayingNow) and (CarState > 1) and
        (CarState <> 8)) then
      begin
        Playing := False;
        Source.SoundName := CurSoundName;
        Playing := True;
      end;
      if (CarStateChanged) then
      begin
        Playing := False;
        Source.SoundName := CurSoundName;
        Playing := True;
      end;
    end;
  end;
  LastCarState := CarState;
end;

procedure GetCarStateBySpeed;
begin
  case round(theCurCar.StartFrame) of  //theCurCar.Speed
    MIN_SPEED:
    begin
      if theCurCar.isSwitchingAnimation then //ox - MotorRunning
      begin
        if CarState > 1 then
          CarState := 2;
      end;
      IsGoingCar := False;
    end;
    1..50:
    begin
      CarState   := 4;
      IsGoingCar := True;
    end;
    51..100:
    begin
      CarState   := 5;
      IsGoingCar := True;
    end;
    101..120:
    begin
      CarState   := 6;
      IsGoingCar := True;
    end;
    121..MAX_SPEED:
    begin
      CarState   := 7;
      IsGoingCar := True;
    end;
  end;
end;

procedure PlaySound;
begin
  PlayingNow := TGLBSoundEmitter(theCurCar.Behaviours[0]).Playing;

  //GO FORWARD
  if (isKeyDown(VK_UP)) then
  begin
    if ((CarState <> 8) and (CarState <> 2)) then
      GetCarStateBySpeed;
    if (CarState = 2) then
      CarState := 3;
    GetCurSound;
    PlayCurSound;
  end;


  if (not PlayingNow) then
      //SWITCH ON ENGINE
  begin
      if (isKeyDown('s')) then
      begin
         if (CarState = 0) then
            CarState := 1
         else
            exit;
         theCurCar.MotorRunning := true; //theCurCar.MotorRunning
         GetCurSound;
         PlayCurSound;
      end
      else      //SWITCH OFF ENGINE
      if (isKeyDown(VK_END)) then
      begin
         CarState := 8;
         theCurCar.MotorRunning := false;
         GetCurSound;
      end
      else
      if (CarState = 1) then
      begin
        CarState := 2;
      end
      else
      if (CarState = 8) then
         CarState := 0;

  GetCarStateBySpeed;
  GetCurSound;
  PlayCurSound;
  end;
end;

end.
