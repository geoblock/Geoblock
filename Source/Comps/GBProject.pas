//
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//
(* GBFileNameProperty *)

unit GBProject;

interface

uses
  Winapi.Windows, 
  Winapi.Messages, 
  System.SysUtils, 
  System.Classes,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs;

type
  TGBProject = class(TComponent)
  private
    FProjectNameUpdated: boolean;
    FProjectName: TFileName;

    //events
    FonProjectNameChange: TNotifyEvent;
    FonChange: TNotifyEvent;

    procedure CallChange;
    procedure CallProjectNameChange;
    procedure SetProjectName(const Value: TFileName);
    function GetProjectExist: boolean;
  protected
     
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    FileName : TFileName;
    OpenDialog: TOpenDialog;
    property ProjectName: TFileName read FProjectName write SetProjectName;
    property ProjectExist: boolean read GetProjectExist;
    procedure CreateProject;
    procedure Save;
    procedure SaveAs;
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(FileName: TFileName);
    procedure ProjectNameUpdate;
  published
    property onChange: TNotifyEvent read FonChange write FonChange;
    property onProjectNameChange: TNotifyEvent
      Read FonProjectNameChange write FonProjectNameChange;
  end;

var
  Project: TGBProject;

implementation

{ TGBProject }

procedure TGBProject.CallChange;
begin
  if Assigned(FonChange) then;
end;

procedure TGBProject.CallProjectNameChange;
begin

end;

constructor TGBProject.Create(AOwner: TComponent);
begin
  inherited;
  FProjectName := FileName;
end;

procedure TGBProject.CreateProject;
begin

end;

destructor TGBProject.Destroy;
begin
  FProjectName := '';
  inherited;
end;

function TGBProject.GetProjectExist: boolean;
begin
  Result := FileExists(ProjectName);
end;

procedure TGBProject.ProjectNameUpdate;
begin
  Save;
  FProjectNameUpdated := True;
end;

procedure TGBProject.Save;
begin
  SaveToFile(ProjectName);
end;

procedure TGBProject.SaveAs;
begin
  if OpenDialog.Execute then
  begin
    Save;
  end;
end;

procedure TGBProject.SaveToFile(FileName: TFileName);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenWrite or fmShareExclusive);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TGBProject.SaveToStream(Stream: TStream);
begin

end;

procedure TGBProject.SetProjectName(const Value: TFileName);
begin
  FProjectName := Value;
  CallProjectNameChange;
  CallChange;
end;


end.
