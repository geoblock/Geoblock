//---------------------------------------------------------------------------
//   This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//
//   Base class for creation user plugins with Borland C++ for Geoblock
//
//
//---------------------------------------------------------------------------


//---------------------------------------------------------------------------
#ifndef PluginRegH
#define PluginRegH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.CheckLst.hpp>
#include <dstring.h>
#include <sysdyn.h>
//---------------------------------------------------------------------------

typedef void __stdcall (*TRegisterPluginsProc) (AnsiString Page, DynamicArray<TClass> Plugins);

typedef void __stdcall (*TSetRegisterPluginsProc) (TRegisterPluginsProc Proc);

void __stdcall RegisterPlugins2(AnsiString Page, DynamicArray<TClass> Plugins);

//---------------------------------------------------------------------------

class TCustomPlugin : public TComponent
{
  private:
	AnsiString FDescription;
        AnsiString FCaption;
	AnsiString FHint;
	AnsiString FHelpFile;
	AnsiString FIDString;
	AnsiString FAuthor;
	CheckLst::TBitmap* FBitmap;
	THelpContext FHelpContext;
	AnsiString FPageName;
	int FTopicIndex;
        HBITMAP GetGlyph();
	void SetGlyph(HBITMAP Value);
	void SetPageName(AnsiString Value);
	void SetTopicIndex(int Value);
  protected:
	virtual void SetAuthor(AnsiString Value);
	virtual void SetCaption(AnsiString Value);
	virtual void SetDescription(AnsiString Value);
	virtual void SetHelpContext(THelpContext Value);
	virtual void SetHelpFile(AnsiString Value);
	virtual void SetHint(AnsiString Value);
	virtual void SetIDString(AnsiString Value);
  public:
	__fastcall TCustomPlugin (TComponent *AOwner);
	__fastcall void virtual Execute();
	__property AnsiString Caption = {read = FCaption, write = SetCaption};
	__property AnsiString Hint = {read=FHint, write=SetHint};
	__property AnsiString HelpFile = {read=FHelpFile, write=SetHelpFile};
	__property THelpContext HelpContext = {read=FHelpContext, write=SetHelpContext};
	__property HBITMAP Glyph = {read=GetGlyph, write=SetGlyph};
	__property AnsiString Author = {read=FAuthor, write=SetAuthor};
	__property AnsiString Description = {read=FDescription, write=SetDescription};
	__property AnsiString IDString = {read=FIDString, write=SetIDString};
	__property AnsiString PageName = {read=FPageName, write=SetPageName};
	__property int TopicIndex = {read=FTopicIndex, write=SetTopicIndex};
};

#endif
