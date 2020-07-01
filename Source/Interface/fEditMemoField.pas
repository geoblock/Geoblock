//------------------------------------------------------------------------------

// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock

//------------------------------------------------------------------------------
{!  The form for memo fields }

unit fEditMemoField;

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
  Vcl.DBCtrls, 
  Vcl.ExtCtrls,
  
  fInitialDialog;

type
  TfmEditMemoField = class(TfmInitialDialog)
    DBMemo: TDBMemo;
  private
     
  public
     
  end;

var
  fmEditMemoField: TfmEditMemoField;

implementation

{$R *.DFM}

end.
