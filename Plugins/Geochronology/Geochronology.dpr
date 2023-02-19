// ----------------------------------------------------------------------------
// Geochronology plugin for Geoblock, http://sourceforge.net/projects/geoblock
// ----------------------------------------------------------------------------
//
(* ! The plugin for problem book *)

library Geochronology;



{$R 'Geochronos.res' 'Geochronos.rc'}

uses
  Winapi.Windows,
  System.ShareMem,
  System.Classes,
  GNUgettext,
  uPluginReg,
  fInitialForm in '..\..\Source\Interface\fInitialForm.pas' {fmInitialForm},
  fInitialDialog in '..\..\Source\Interface\fInitialDialog.pas' {fmInitialDialog},
  fGeochronology in 'fGeochronology.pas' {fmMainForm};

type
  TGeochronologyPlugin = class(TCustomPlugin)
  public
    constructor Create(Owner: TComponent); override;
    procedure Execute; override;
    destructor Destroy; override;
  private
  end;

constructor TGeochronologyPlugin.Create(Owner: TComponent);
begin
  inherited;
  if fmMainForm = nil then
    fmMainForm := TfmMainForm.Create(Self);
  IDString := 'Geoblock.Geochronology';
  Glyph := LoadBitmap(HInstance, 'Geochronos');      // bitmap name from rc
  PageName := _('Geology');
  Caption := _('Geochronology');
end;

destructor TGeochronologyPlugin.Destroy;
begin
  inherited Destroy;
end;

procedure TGeochronologyPlugin.Execute;
begin
  fmMainForm.ShowModal;
end;

procedure Register;
var
  arr: TPluginArray;
begin
  SetLength(arr, 1);
  arr[0] := TGeochronologyPlugin;
  RegisterPlugins('', arr);
end;

// ========================================================================
exports
// ========================================================================

  Register;

begin

end.
