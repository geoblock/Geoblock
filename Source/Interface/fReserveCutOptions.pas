//----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//----------------------------------------------------------------------------
{! The dialog for cut options }

unit fReserveCutOptions;

interface

uses
  System.SysUtils, 
  System.Classes,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs,
  Vcl.StdCtrls, 
  Vcl.ExtCtrls,
  //DB
  Data.DB, 
  Bde.DBTables,
  
  fInitialDialog;

type
  TfmReserveCutOptions = class(TfmInitialDialog)
    GroupBoxMode: TGroupBox;
    ListBoxOreTypes: TListBox;
    ListBoxOreSorts: TListBox;
    Label1: TLabel;
    CheckBoxOreTypeAndSort: TCheckBox;
  private
    function GetCalcBySort: boolean;
    function GetfldOreSortName: string;
    function GetfldOreTypeName: string;
     
  public
     
    procedure SetTableInput(const TableInput: TTable);
    property CalcBySort: boolean Read GetCalcBySort;
    property fldOreTypeName: string Read GetfldOreTypeName;
    property fldOreSortName: string Read GetfldOreSortName;
  end;

var
  fmReserveCutOptions: TfmReserveCutOptions;

//==========================================================================
implementation
//==========================================================================

uses
  uGlobals,
  uProfuns,
  uCommon;

{$R *.DFM}

{ TfmReserveCuttingOptions }

function TfmReserveCutOptions.GetCalcBySort: boolean;
begin
  Result := CheckBoxOreTypeAndSort.Checked;
end;

function TfmReserveCutOptions.GetfldOreSortName: string;
begin
  Result := ListBoxOreSorts.Items[ListBoxOreSorts.ItemIndex];
end;

function TfmReserveCutOptions.GetfldOreTypeName: string;
begin
  Result := ListBoxOreTypes.Items[ListBoxOreTypes.ItemIndex];
end;

procedure TfmReserveCutOptions.SetTableInput(const TableInput: TTable);
var
  I: integer;
begin
  ListBoxOreTypes.Items.BeginUpdate;
  ListBoxOreSorts.Items.BeginUpdate;
  try
    ListBoxOreTypes.Items.Clear;
    ListBoxOreSorts.Items.Clear;
    try
      TableInput.Open;
      for I := 0 to TableInput.FieldCount - 1 do
        with TableInput, Fields[I] do
          if (Fields[I] is TIntegerField) and
            (Pos(fldID + '_', UpperCase(FieldName)) <> 1) and
            (fldID <> UpperCase(FieldName)) then
          begin
            ListBoxOreTypes.Items.Add(FieldName);
            ListBoxOreSorts.Items.Add(FieldName);
          end;
    except
    end;
    TableInput.Close;
    ListBoxOreTypes.ItemIndex := ListBoxOreTypes.Items.IndexOf(fldORETYPE);
    ListBoxOreSorts.ItemIndex := ListBoxOreSorts.Items.IndexOf(fldORESORT);
    try
      CheckItemIndex(ListBoxOreTypes);
      CheckItemIndex(ListBoxOreSorts);
    except
    end;
    ButtonOK.Enabled := ListBoxOreTypes.Items.Count > 0;
  finally
    ListBoxOreTypes.Items.EndUpdate;
    ListBoxOreSorts.Items.EndUpdate;
  end;
end;

end.
