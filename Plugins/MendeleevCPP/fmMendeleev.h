//$$---- Form HDR ----
//---------------------------------------------------------------------------

#ifndef fmMendeleevH
#define fmMendeleevH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <DBCtrls.hpp>
#include <Mask.hpp>
#include <DB.hpp>
#include <DBTables.hpp>
#include <Registry.hpp>
#include "uConsts.hpp"
#include "gnugettext.hpp"


//---------------------------------------------------------------------------
class TfmMain : public TForm
{
__published:	// IDE-managed Components
	TDataSource *DataSourceElements;
	TDataSource *DataSourceMinerals;
	TTable *TableElements;
	TTable *TableMainMinerals;
	TPanel *PanelTable;
	TSpeedButton *SpeedButtonH;
	TLabel *LabelIA;
	TLabel *LabelIIA;
	TLabel *LabelIIIB;
	TLabel *LabelIIIA;
	TLabel *LabelVIIIA;
	TLabel *LabelVIIA;
	TLabel *LabelLanthanide;
	TLabel *LabelActinide;
	TLabel *LabelIVB;
	TLabel *LabelVB;
	TLabel *Label1VIB;
	TLabel *LabelVIIB;
	TLabel *LabelIIB;
	TLabel *Label1IB;
	TLabel *LabelIVA;
	TLabel *LabelVA;
	TLabel *LabelVIA;
	TSpeedButton *SpeedButtonLi;
	TSpeedButton *SpeedButtonNa;
	TSpeedButton *SpeedButtonK;
	TSpeedButton *SpeedButtonRb;
	TSpeedButton *SpeedButtonCs;
	TSpeedButton *SpeedButtonFr;
	TSpeedButton *SpeedButtonBe;
	TSpeedButton *SpeedButtonMg;
	TSpeedButton *SpeedButtonCa;
	TSpeedButton *SpeedButtonSr;
	TSpeedButton *SpeedButtonBa;
	TSpeedButton *SpeedButtonRa;
	TSpeedButton *SpeedButtonSc;
	TSpeedButton *SpeedButtonY;
	TSpeedButton *SpeedButtonLa;
	TSpeedButton *SpeedButtonAc;
	TSpeedButton *SpeedButtonTi;
	TSpeedButton *SpeedButtonZr;
	TSpeedButton *SpeedButtonHf;
	TSpeedButton *SpeedButtonDb;
	TSpeedButton *SpeedButtonV;
	TSpeedButton *SpeedButtonNb;
	TSpeedButton *SpeedButtonTa;
	TSpeedButton *SpeedButtonJl;
	TSpeedButton *SpeedButtonCr;
	TSpeedButton *SpeedButtonMo;
	TSpeedButton *SpeedButtonW;
	TSpeedButton *SpeedButtonMn;
	TSpeedButton *SpeedButtonTc;
	TSpeedButton *SpeedButtonRe;
	TSpeedButton *SpeedButtonFe;
	TSpeedButton *SpeedButtonRu;
	TSpeedButton *SpeedButtonOs;
	TSpeedButton *SpeedButtonCo;
	TSpeedButton *SpeedButtonRh;
	TSpeedButton *SpeedButtonIr;
	TSpeedButton *SpeedButtonNi;
	TSpeedButton *SpeedButtonPd;
	TSpeedButton *SpeedButtonPt;
	TSpeedButton *SpeedButtonCu;
	TSpeedButton *SpeedButtonAg;
	TSpeedButton *SpeedButtonAu;
	TSpeedButton *SpeedButtonZn;
	TSpeedButton *SpeedButtonCd;
	TSpeedButton *SpeedButtonHg;
	TSpeedButton *SpeedButtonB;
	TSpeedButton *SpeedButtonAl;
	TSpeedButton *SpeedButtonGa;
	TSpeedButton *SpeedButtonIn;
	TSpeedButton *SpeedButtonTl;
	TSpeedButton *SpeedButtonC;
	TSpeedButton *SpeedButtonSi;
	TSpeedButton *SpeedButtonGe;
	TSpeedButton *SpeedButtonSn;
	TSpeedButton *SpeedButtonPb;
	TSpeedButton *SpeedButtonN;
	TSpeedButton *SpeedButtonP;
	TSpeedButton *SpeedButtonAs;
	TSpeedButton *SpeedButtonSb;
	TSpeedButton *SpeedButtonBi;
	TSpeedButton *SpeedButtonO;
	TSpeedButton *SpeedButtonS;
	TSpeedButton *SpeedButtonSe;
	TSpeedButton *SpeedButtonTe;
	TSpeedButton *SpeedButtonPo;
	TSpeedButton *SpeedButtonF;
	TSpeedButton *SpeedButtonCl;
	TSpeedButton *SpeedButtonBr;
	TSpeedButton *SpeedButtonI;
	TSpeedButton *SpeedButtonAt;
	TSpeedButton *SpeedButtonHe;
	TSpeedButton *SpeedButtonNe;
	TSpeedButton *SpeedButtonAr;
	TSpeedButton *SpeedButtonKr;
	TSpeedButton *SpeedButtonXe;
	TSpeedButton *SpeedButtonRn;
	TSpeedButton *SpeedButtonCe;
	TSpeedButton *SpeedButtonTh;
	TSpeedButton *SpeedButtonPr;
	TSpeedButton *SpeedButtonPa;
	TSpeedButton *SpeedButtonNd;
	TSpeedButton *SpeedButtonU;
	TSpeedButton *SpeedButtonPm;
	TSpeedButton *SpeedButtonNp;
	TSpeedButton *SpeedButtonSm;
	TSpeedButton *SpeedButtonPu;
	TSpeedButton *SpeedButtonEu;
	TSpeedButton *SpeedButtonAm;
	TSpeedButton *SpeedButtonGd;
	TSpeedButton *SpeedButtonCm;
	TSpeedButton *SpeedButtonTb;
	TSpeedButton *SpeedButtonBk;
	TSpeedButton *SpeedButtonDy;
	TSpeedButton *SpeedButtonCf;
	TSpeedButton *SpeedButtonHo;
	TSpeedButton *SpeedButtonEs;
	TSpeedButton *SpeedButtonEr;
	TSpeedButton *SpeedButtonFm;
	TSpeedButton *SpeedButtonTm;
	TSpeedButton *SpeedButtonMd;
	TSpeedButton *SpeedButtonYb;
	TSpeedButton *SpeedButtonNo;
	TSpeedButton *SpeedButtonLu;
	TSpeedButton *SpeedButtonLr;
	TSpeedButton *SpeedButtonRf;
	TSpeedButton *SpeedButtonBh;
	TSpeedButton *SpeedButtonHn;
	TSpeedButton *SpeedButtonMt;
	TSpeedButton *SpeedButtonUn;
	TGroupBox *GroupBoxVIII;
	TPanel *PanelBottom;
	TButton *ButtonCancel;
	TButton *ButtonOK;
	TButton *ButtonPrint;
	TPageControl *PageControlLegend;
	TTabSheet *TabSheetChemistry;
	TLabel *LabelAlkaliMetals;
	TLabel *LabelRareEarthMetals;
	TLabel *LabelTransitionMetals;
	TLabel *LabelAlkaliEarthMetals;
	TLabel *LabelOtherMetals;
	TLabel *LabelOtherNonmetals;
	TLabel *LabelHalogens;
	TLabel *LabelNobleGases;
	TTabSheet *TabSheetMetallurgy;
	TLabel *LabelFerrousMetals;
	TLabel *LabelNonFerrousMetals;
	TLabel *LabelRareElements;
	TLabel *LabelPreciousMetals;
	TLabel *LabelRadioactiveElements;
	TLabel *LabelNonmetallicElements;
	TPanel *PanelMendeleevTable;
	TPanel *Panel1;
	TLabel *LabelDensity;
	TLabel *LabelBoilingPoint;
	TLabel *LabelMeltingPoint;
	TLabel *LabelState;
	TLabel *LabelValence;
	TLabel *LabelAtomicWeight;
	TLabel *LabelAtomicNumber;
	TLabel *LabelName;
	TLabel *LabelDescription;
	TLabel *LabelMainMinerals;
	TDBEdit *DBEditName;
	TDBEdit *DBEditAtomicNumber;
	TDBEdit *DBEditWeight;
	TDBEdit *DBEditValence;
	TDBEdit *DBEditState;
	TDBEdit *DBEditMeltingPoint;
	TDBEdit *DBEditBoilingPoint;
	TDBEdit *DBEditDensity;
	TDBListBox *DBListBoxMainMinerals;
	TDBRichEdit *DBRichEditDescription;
	TButton *ButtonHelp;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall SpeedButtonElementClick(TObject* Sender);
	void __fastcall FormActivate(TObject *Sender);
	void __fastcall PageControlLegendChange(TObject *Sender);
	void __fastcall ButtonHelpClick(TObject *Sender);
	void __fastcall ButtonPrintClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TfmMain(TComponent* Owner);
	TRegistryIniFile* RegIni;
	void __fastcall UpdateMainMinerals();
	void __fastcall SetLanguage();
};
//---------------------------------------------------------------------------
extern PACKAGE TfmMain *fmMain;
//---------------------------------------------------------------------------
#endif
