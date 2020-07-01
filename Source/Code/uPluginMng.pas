//
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//
{
  Plugin Manager
}

unit uPluginMng;

interface

uses
  Winapi.Windows,
  System.Classes, 
  System.Contnrs, 
  System.SysUtils,
  Vcl.Dialogs, 
  Vcl.Forms, 
  uPluginReg;

type
  TPluginEvent = procedure(Sender: TObject; PluginFileName: TFileName) of object;

type
  TPluginManager = class(TComponent)
  private
    FPluginLibraryList: TList;
    FPluginsList:  TObjectList;
    FPluginExt:    string;
    FOnErrorLoadPlugin: TPluginEvent;
    FOnLoadPlugin: TPluginEvent;
    FPageList:     TStringList;

    procedure AddPlugin(FileName: TFileName);
    procedure DefaulErrorLoadPlugin(Sender: TObject; PluginFileName: TFileName);
    procedure SetPluginExt(const Value: string);

    procedure RegisterPluginsLibrary(Inst: HINST);
    procedure RegisterPlugins(Page: string; Plugins: array of TPluginClass);
    function GetPluginCount: integer;
    function GetPlugins(Index: integer): TCustomPlugin;
    function GetPages(Index: integer): string;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  public
    property PluginExt: string Read FPluginExt Write SetPluginExt;

    property OnLoadPlugin: TPluginEvent Read FOnLoadPlugin Write FOnLoadPlugin;
    property OnErrorLoadPlugin: TPluginEvent
      Read FOnErrorLoadPlugin Write FOnErrorLoadPlugin;

    procedure ClearPlugins;
    procedure LoadPlugins(DirPlugins: TFileName);

    property PluginCount: integer Read GetPluginCount;
    property Plugins[Index: integer]: TCustomPlugin Read GetPlugins; default;
    property Pages[Index: integer]: string Read GetPages;
  end;

var
  PluginManager: TPluginManager = nil;

//===========================================================================
implementation
//===========================================================================

uses
  uGlobals,
  uCommon,
  uProfuns,
  uResStrings;

procedure RegisterPlugins(Page: string; Plugins: TPluginArray);
  stdcall;
begin
  if Assigned(PluginManager) then
    PluginManager.RegisterPlugins(Page, Plugins);
end;

{ TPluginManager }

procedure TPluginManager.LoadPlugins(DirPlugins: TFileName);
var
  SearchRec: TSearchRec;
  FileName:  TFileName;
begin
  FileName := DirPlugins + FPluginExt;
  if FindFirst(FileName, faAnyFile and not faDirectory, SearchRec) = 0 then
    repeat
      try
        AddPlugin(SlashSep(DirPlugins, SearchRec.Name));
      except
        Application.HandleException(Self);
      end;
    until FindNext(SearchRec) <> 0;
  FindClose(SearchRec);
end;


procedure TPluginManager.AddPlugin(FileName: TFileName);
var
  InitLibraryProc: TInitLibraryProc;
  RegisterProc: TRegisterProc;
  SetRegisterPluginProc: TSetRegisterPluginsProc;
  plgHandle: HINST;

begin
  //search directory to load all plugins
  plgHandle := LoadLibrary(PChar(FileName));
  if plgHandle <> 0 then
    try
      RegisterPluginsLibrary(plgHandle);
      if Assigned(onLoadPlugin) then
        onLoadPlugin(Self, ExtractFileName(FileName));

      InitLibraryProc := GetProcAddress(plgHandle, 'InitLibrary');
      SetRegisterPluginProc := GetProcAddress(plgHandle, 'SetRegisterPluginProc');
      RegisterProc    := GetProcAddress(plgHandle, 'Register');

      InitLibraryProc(Application.Handle);
      SetRegisterPluginProc(uPluginMng.RegisterPlugins);
      RegisterProc;
    except
      if Assigned(onErrorLoadPlugin) then
        onErrorLoadPlugin(Self,
          ExtractFileName(FileName));
    end;
end;

procedure TPluginManager.ClearPlugins;
var
  I: integer;
begin
  FPluginsList.Clear;
  for I := 0 to FPluginLibraryList.Count - 1 do
    try
      FreeLibrary(HINST(FPluginLibraryList.Items[I]));
    except
    end;
  FPluginLibraryList.Clear;
end;

constructor TPluginManager.Create;
begin
  inherited;
  FPageList    := TStringList.Create;
  FPluginsList := TObjectList.Create;
  FPluginExt   := PlugExt; //Geoblock Plugins
  FPluginLibraryList := TList.Create;
  FOnErrorLoadPlugin := DefaulErrorLoadPlugin;
end;

procedure TPluginManager.DefaulErrorLoadPlugin(Sender: TObject;
  PluginFileName: TFileName);
begin
  ShowMessage(LoadResString(@rsErrorLoadingFile));
end;

destructor TPluginManager.Destroy;
begin
  ClearPlugins;
  FPluginLibraryList.Free;
  FPluginsList.Free;
  FPageList.Free;
  inherited;
end;

function TPluginManager.GetPages(Index: integer): string;
begin
  try
    Result := FPageList[Index];
  except
    Result := LoadResString(@rsOthers);
  end;
end;

function TPluginManager.GetPluginCount: integer;
begin
  Result := FPluginsList.Count;
end;

function TPluginManager.GetPlugins(Index: integer): TCustomPlugin;
begin
  Result := TCustomPlugin(FPluginsList.Items[Index]);
end;

procedure TPluginManager.RegisterPlugins(Page: string; Plugins: array of TPluginClass);
var
  I:      integer;
  Plugin: TCustomPlugin;
begin
  for I := Low(Plugins) to High(Plugins) do
  begin
    Plugin := Plugins[I].Create(Self);
    FPluginsList.Add(Plugin);
    FPageList.Add(Plugin.PageName);
  end;
end;

procedure TPluginManager.RegisterPluginsLibrary(Inst: HINST);
begin
  if FPluginLibraryList.IndexOf(Pointer(Inst)) < 0 then
    FPluginLibraryList.Add(Pointer(Inst));
end;

procedure TPluginManager.SetPluginExt(const Value: string);
begin
  FPluginExt := Value;
end;

end.
