//---------------------------------------------------------------------------
//This plugin is part of the Geoblock, http://sourceforge.net/projects/geoblock
//
//  C++ adaptaton of PeriodicTable.dpr plugin
//
//---------------------------------------------------------------------------
#include <vcl.h>
#include <windows.h>
#include <registry.hpp>
#include "uPluginReg.h"
#include "fmMendeleev.h"
//---------------------------------------------------------------------------

#pragma hdrstop

class TPeriodicTable : public TCustomPlugin {
 __fastcall TPeriodicTable (TComponent *Owner);
 void __fastcall Execute();
};

//---------------------------------------------------------------------------
TfmMain *fmMain;

DynamicArray<TClass> arr;

//---------------------------------------------------------------------------
__fastcall TPeriodicTable::TPeriodicTable (TComponent *Owner): TCustomPlugin(Owner)
{
  if (fmMain == NULL)
   fmMain = new TfmMain(this);

  IDString = "Geoblock.Mendeleev";
  Glyph = LoadBitmap(HInstance,"Mendeleev");
///  PageName = _("Geology");
  PageName = "Geology";
///  Caption = _("Mendeleev");
  Caption = "Mendeleev";
}
//---------------------------------------------------------------------------
void __fastcall TPeriodicTable::Execute() {
  fmMain->ShowModal();
};
//---------------------------------------------------------------------------
extern "C" __declspec(dllexport) __stdcall void Register()
{
 arr.Length = 10;
 arr[0] = __classid(TPeriodicTable);
 RegisterPlugins2("",arr);
}
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void* lpReserved)
{
		return 1;
}
//---------------------------------------------------------------------------
