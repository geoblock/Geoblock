//---------------------------------------------------------------------------

#include "uPluginReg.h"
#include "uConsts.hpp"

//---------------------------------------------------------------------------

__fastcall TCustomPlugin::TCustomPlugin(TComponent *AOwner): TComponent(AOwner)
{
  FDescription = " ";
  FHelpFile = " ";
  FHelpContext = 0;
  FAuthor = " ";
  FHint = " ";
  FBitmap = new Vcl.CheckLst::TBitmap;
  FCaption = ClassName();
//  FIDString = uConsts_SgbGeology + "." + FCaption;
  FIDString = "Geology." + FCaption;
//  FPageName = uConsts_SgbMiscellany;      // Default
  FPageName = "Miscellany";      // Default
  FTopicIndex = 0;
}

void __fastcall TCustomPlugin::Execute()
{
// throw Exception(uConsts_SgbNotImplemented);
 throw Exception("Not Implemented");
}

void TCustomPlugin::SetAuthor(AnsiString Value){ FAuthor = Value; }

void TCustomPlugin::SetCaption(AnsiString Value) { FCaption = Value; }

void TCustomPlugin::SetDescription(AnsiString Value) { FDescription = Value; }

HBITMAP TCustomPlugin::GetGlyph() { return FBitmap->Handle; }

void TCustomPlugin::SetGlyph(HBITMAP const Value) { FBitmap->Handle = Value; }

void TCustomPlugin::SetHelpContext(THelpContext const Value) { FHelpContext = Value; }

void TCustomPlugin::SetHelpFile(AnsiString Value) { FHelpFile = Value; }

void TCustomPlugin::SetHint(AnsiString Value ) { FHint = Value; }

void TCustomPlugin::SetIDString(AnsiString Value) { FIDString = Value; }

void TCustomPlugin::SetPageName(AnsiString Value) { FPageName = Value; }

void TCustomPlugin::SetTopicIndex(int const Value) { FTopicIndex = Value; }

//---------------------------------------------------------------------------

TRegisterPluginsProc RegisterPluginsProc = NULL;

extern "C" __declspec(dllexport) __stdcall void SetRegisterPluginProc(TRegisterPluginsProc Proc)
{
  RegisterPluginsProc = Proc;
}

void __stdcall RegisterPlugins2(AnsiString Page, DynamicArray<TClass> Plugins)
{
  if (RegisterPluginsProc != NULL) RegisterPluginsProc(Page, Plugins);
}

extern "C" __declspec(dllexport) __stdcall void InitLibrary(HWND AHandle)
{
 Application->Handle = AHandle;
}

#pragma package(smart_init)








