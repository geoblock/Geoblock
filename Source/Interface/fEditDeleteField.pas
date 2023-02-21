//------------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//------------------------------------------------------------------------------
(*  The dialog to delete fields *)

unit fEditDeleteField;

interface

uses
  Winapi.Windows, 
  Winapi.Messages, 
  System.SysUtils, 
  System.Classes, 
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs,
  Vcl.StdCtrls, 
  Vcl.CheckLst, 
  Vcl.ExtCtrls,
  
  fInitialDialog;

type
  TfmEditDeleteField = class(TfmInitialDialog)
    CheckListBoxFields: TCheckListBox;
  private
     
  public
     
  end;

var
  fmEditDeleteField: TfmEditDeleteField;

implementation

{$R *.DFM}

end.
