//-----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//-----------------------------------------------------------------------------
{
  ! Plugin Registry
}

unit uPluginReg;

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  Vcl.Graphics,
  Vcl.Forms,
  uResStrings;

type
  TCustomPlugin = class(TComponent)
  private
    FDescription: string;
    FCaption:  string;
    FHint:     string;
    FHelpFile: string;
    FIDString: string;
    FAuthor:   string;
    FBitmap:   TBitmap;
    FHelpContext: THelpContext;
    FPageName: string;
    FTopicIndex: integer;
    function GetGlyph: HBITMAP;
    procedure SetGlyph(const Value: HBitMap);
    procedure SetPageName(const Value: string);
    procedure SetTopicIndex(const Value: integer);
  protected
    procedure SetAuthor(const Value: string); virtual;
    procedure SetCaption(const Value: string); virtual;
    procedure SetDescription(const Value: string); virtual;
    procedure SetHelpContext(const Value: THelpContext); virtual;
    procedure SetHelpFile(const Value: string); virtual;
    procedure SetHint(const Value: string); virtual;
    procedure SetIDString(const Value: string); virtual;
  public
    constructor Create(Owner: TComponent); override;
    procedure Execute; virtual;

    property Caption: string read FCaption write SetCaption;
    property Hint: string read FHint write SetHint;
    property HelpFile: string read FHelpFile write SetHelpFile;
    property HelpContext: THelpContext read FHelpContext write SetHelpContext;
    property Glyph: HBitMap read GetGlyph write SetGlyph;
    property Author: string read FAuthor write SetAuthor;
    property Description: string read FDescription write SetDescription;
    property IDString: string read FIDString write SetIDString;
    property PageName: string read FPageName write SetPageName;
    property TopicIndex: integer read FTopicIndex write SetTopicIndex;
  end;

  TPluginClass = class of TCustomPlugin;

type
  TPluginArray = array of TPluginClass;

  TInitLibraryProc = procedure(AHandle: THandle); stdcall;
  TRegisterPluginsProc = procedure(Page: string; Plugins: TPluginArray); stdcall;
  TSetRegisterPluginsProc = procedure(Proc: TRegisterPluginsProc); stdcall;
  TRegisterProc = procedure; stdcall;


procedure RegisterPlugins(Page: string; Plugins: TPluginArray);
  stdcall;

//==========================================================================
implementation
//==========================================================================

var
  RegisterPluginsProc: TRegisterPluginsProc = nil;


{ TCustomPlugin }

constructor TCustomPlugin.Create;
begin
  inherited;
  FDescription := ' ';
  FHelpFile := '';
  FHelpContext := 0;
  FAuthor   := ' ';
  FHint     := ' ';
  FBitmap   := TBitmap.Create;
  FCaption  := ClassName;
  FIDString := rsGeoblock + '.' + FCaption;
  FPageName := LoadResString(@rsMiscellany); //Default
  FTopicIndex := 0;
end;

procedure TCustomPlugin.Execute;
begin
  raise Exception.Create(LoadResString(@rsNotImplemented));
end;

function TCustomPlugin.GetGlyph: HBITMAP;
begin
  Result := FBitmap.Handle;
end;

procedure TCustomPlugin.SetAuthor(const Value: string);
begin
  FAuthor := Value;
end;

procedure TCustomPlugin.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TCustomPlugin.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TCustomPlugin.SetGlyph(const Value: HBitMap);
begin
  FBitmap.Handle := Value;
end;

procedure TCustomPlugin.SetHelpContext(const Value: THelpContext);
begin
  FHelpContext := Value;
end;

procedure TCustomPlugin.SetHelpFile(const Value: string);
begin
  FHelpFile := Value;
end;

procedure TCustomPlugin.SetHint(const Value: string);
begin
  FHint := Value;
end;

procedure TCustomPlugin.SetIDString(const Value: string);
begin
  FIDString := Value;
end;

procedure TCustomPlugin.SetPageName(const Value: string);
begin
  FPageName := Value;
end;

procedure TCustomPlugin.SetTopicIndex(const Value: integer);
begin
  FTopicIndex := Value;
end;


//================================================================\\
procedure SetRegisterPluginProc(Proc: TRegisterPluginsProc); stdcall;
begin
  RegisterPluginsProc := Proc;
end;

procedure RegisterPlugins(Page: string; Plugins: TPluginArray);
  stdcall;
begin
  if Assigned(RegisterPluginsProc) then
    RegisterPluginsProc(Page, Plugins);
end;

procedure InitLibrary(AHandle: THandle); stdcall;
begin
  Application.Handle := AHandle;
end;

exports
  SetRegisterPluginProc,
  InitLibrary;

end.
