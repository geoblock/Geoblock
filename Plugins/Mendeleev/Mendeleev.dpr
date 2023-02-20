// ---------------------------------------------------------------------------
// This is plugin for the Geoblock, http://sourceforge.net/projects/geoblock
// ----------------------------------------------------------------------------

library Mendeleev;

(* Periodic table of Mendeleev *)

{$R 'Mendeleev.res' 'Mendeleev.rc'}
{$E gpl}

uses
  System.ShareMem,
  Winapi.Windows,
  System.Classes,
  gnuGettext,
  uPluginReg,
  fMendeleev in 'fMendeleev.pas' {fmMendeleev};

type
  TPeriodicTablePlugin = Class(TCustomPlugin)
  public
    constructor Create(Owner: TComponent); override;
    procedure Execute; override;
  end;

constructor TPeriodicTablePlugin.Create(Owner: TComponent);
begin
  inherited;
  if fmMendeleev = nil then
    fmMendeleev := TfmMendeleev.Create(Self);
  IDString := 'Geoblock.MendeleevTable';
  Glyph := LoadBitmap(HInstance, 'Mendeleev');
  PageName := _('Geology');
  Caption := _('Periodic Table');
end;

procedure TPeriodicTablePlugin.Execute;
begin
  fmMendeleev.ShowModal;
end;

procedure Register;
var
  arr: TPluginArray;
begin
  SetLength(arr, 1);
  arr[0] := TPeriodicTablePlugin;
  RegisterPlugins('', arr);
end;

exports
  Register;

begin

end.
