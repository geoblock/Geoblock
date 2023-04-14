//$$---- Form CPP ----
//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include "fmMendeleev.h"
//---------------------------------------------------------------------------
//#pragma link "cResStrings"     for localization
//#pragma link "gnugettext"

#pragma package(smart_init)
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TfmMain::TfmMain(TComponent* Owner) : TForm(Owner) {}
//---------------------------------------------------------------------------
void __fastcall TfmMain::UpdateMainMinerals()
{
    AnsiString fldELEMENT = "ELEMENT";
    AnsiString fldSYMBOL = "SYMBOL";
    AnsiString S = "";
    //  try
    DBListBoxMainMinerals->Items->BeginUpdate();
    //	try
    DBListBoxMainMinerals->Items->Clear();
    TableMainMinerals->First();

    while (!TableMainMinerals->Eof) {
        if (TableMainMinerals->FieldByName(fldELEMENT)->AsString ==
            TableElements->FieldByName(fldSYMBOL)->AsString)
        {
            S = TableMainMinerals->FieldByName(DBListBoxMainMinerals->DataField)
                    ->AsString;
            DBListBoxMainMinerals->Items->Add(S);
        }
		TableMainMinerals->Next();
	}
	//	finally
	DBListBoxMainMinerals->Items->EndUpdate();
	//	end;
	//  except
	DBListBoxMainMinerals->Items->Add("");
	//  end;
};
//---------------------------------------------------------------------------

void __fastcall TfmMain::SetLanguage()
{
	AnsiString GeneralSection = "\\SOFTWARE\\Geoblock\\General";
	RegIni = new TRegistryIniFile(GeneralSection);
	int Language = RegIni->ReadInteger(GeneralSection, "Language", 9);
	AnsiString AppPath = RegIni->ReadString(GeneralSection, "AppPath", "");
	if (Language != LANG_ENGLISH) {
		///	textdomain("geology");
		///	BindTextDomain ("geology", AppPath + "Locale"+ PathDelim);
	}

	Language = LANG_ENGLISH; //without translation
	switch (Language) {
		case LANG_ENGLISH:
			///	UseLanguage("en");

			TableElements->TableName =
				UpperCase(AppPath + "Data\\Reference\\en\\Elements");
			TableMainMinerals->TableName = UpperCase(
				AppPath + "Data\\Reference\\en\\MainMinerals");
			break;

		case LANG_RUSSIAN:
			///	  UseLanguage("ru");
			TableElements->TableName =
				UpperCase(AppPath + "Data\\Reference\\ru\\Elements");
			TableMainMinerals->TableName = UpperCase(
				AppPath + "Data\\Reference\\ru\\MainMinerals");
			break;
	}

 /*
  TP_GlobalIgnoreClass(__classid(TTable));
  TP_GlobalIgnoreClass(__classid(TFields));
  TP_GlobalIgnoreClass(__classid(TDBEdit));
  TP_GlobalIgnoreClass(__classid(TDBRichEdit));
*/
    DBEditState->DataField = "STATE";
    DBRichEditDescription->DataField = "DESCRIPTION";
    DBEditName->DataField = "NAME";
    DBListBoxMainMinerals->DataField = "MINERAL";

    ///  TranslateComponent(this);

    RegIni->Free();
};

//---------------------------------------------------------------------------
void __fastcall TfmMain::FormCreate(TObject* Sender)
{
    SetLanguage();
    TableElements->Open();
    TableMainMinerals->Open();
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::FormActivate(TObject* Sender)
{
	SpeedButtonFe->Down = true;
    SpeedButtonFe->Click();
    TableElements->RecNo = dynamic_cast<TComponent*>(Sender)->Tag;
    UpdateMainMinerals();
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::SpeedButtonElementClick(TObject* Sender)
{
    TableElements->RecNo = dynamic_cast<TComponent*>(Sender)->Tag;
    UpdateMainMinerals();
};
//---------------------------------------------------------------------------
void __fastcall TfmMain::PageControlLegendChange(TObject* Sender)
{
    if (PageControlLegend->ActivePage == TabSheetMetallurgy) {
        SpeedButtonTi->Font->Color = LabelFerrousMetals->Font->Color;
        SpeedButtonV->Font->Color = LabelFerrousMetals->Font->Color;
        SpeedButtonFe->Font->Color = LabelFerrousMetals->Font->Color;
        SpeedButtonMn->Font->Color = LabelFerrousMetals->Font->Color;
        SpeedButtonCr->Font->Color = LabelFerrousMetals->Font->Color;

        SpeedButtonNi->Font->Color = LabelNonFerrousMetals->Font->Color;
        SpeedButtonCu->Font->Color = LabelNonFerrousMetals->Font->Color;
        SpeedButtonZn->Font->Color = LabelNonFerrousMetals->Font->Color;
        SpeedButtonAl->Font->Color = LabelNonFerrousMetals->Font->Color;

        SpeedButtonAu->Font->Color = LabelPreciousMetals->Font->Color;
        SpeedButtonAg->Font->Color = LabelPreciousMetals->Font->Color;
        SpeedButtonPt->Font->Color = LabelPreciousMetals->Font->Color;

        SpeedButtonZr->Font->Color = LabelRareElements->Font->Color;

        SpeedButtonRa->Font->Color = LabelRadioactiveElements->Font->Color;
        SpeedButtonU->Font->Color = LabelRadioactiveElements->Font->Color;

        SpeedButtonSi->Font->Color = LabelNonmetallicElements->Font->Color;
        SpeedButtonP->Font->Color = LabelNonmetallicElements->Font->Color;
        SpeedButtonS->Font->Color = LabelNonmetallicElements->Font->Color;
    }
    if (PageControlLegend->ActivePage == TabSheetChemistry) {
        SpeedButtonTi->Font->Color = LabelTransitionMetals->Font->Color;
        SpeedButtonV->Font->Color = LabelTransitionMetals->Font->Color;
        SpeedButtonFe->Font->Color = LabelTransitionMetals->Font->Color;
        SpeedButtonMn->Font->Color = LabelTransitionMetals->Font->Color;
        SpeedButtonCr->Font->Color = LabelTransitionMetals->Font->Color;

        SpeedButtonNi->Font->Color = LabelTransitionMetals->Font->Color;
        SpeedButtonCu->Font->Color = LabelTransitionMetals->Font->Color;
        SpeedButtonZn->Font->Color = LabelTransitionMetals->Font->Color;
        SpeedButtonAl->Font->Color = LabelOtherMetals->Font->Color;

        SpeedButtonAu->Font->Color = LabelTransitionMetals->Font->Color;
        SpeedButtonAg->Font->Color = LabelTransitionMetals->Font->Color;
        SpeedButtonPt->Font->Color = LabelTransitionMetals->Font->Color;
        SpeedButtonZr->Font->Color = LabelTransitionMetals->Font->Color;

        SpeedButtonRa->Font->Color = LabelRareEarthMetals->Font->Color;
        SpeedButtonU->Font->Color = LabelRareEarthMetals->Font->Color;

        SpeedButtonSi->Font->Color = LabelOtherNonmetals->Font->Color;
        SpeedButtonP->Font->Color = LabelOtherNonmetals->Font->Color;
        SpeedButtonS->Font->Color = LabelOtherNonmetals->Font->Color;
    }
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::ButtonHelpClick(TObject* Sender)
{
    Application->HelpContext(HelpContext);
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::ButtonPrintClick(TObject* Sender)
{
    if (MessageDlg(ButtonPrint->Caption + "?", mtConfirmation,
            TMsgDlgButtons() << mbYes << mbNo, 0) == mrYes)
        Print();
}
//---------------------------------------------------------------------------

