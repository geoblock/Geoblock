//-----------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//-----------------------------------------------------------------------------
(* Options for viewing one layer or horizon in 3d grid model *)


unit fViewHorizon;

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
  Vcl.ExtCtrls, 
  
  Vcl.DBCtrls, 
  Vcl.Grids, 
  Vcl.DBGrids, 
  Vcl.Buttons,
  
  GBEditValue, Data.DB;

type
  TfmViewHorizon = class(TForm)
    PanelOKCancelHelp: TPanel;
    OKBtn:      TButton;
    ButtonCancel: TButton;
    ButtonHelp: TButton;
    DBGrid:     TDBGrid;
    PanelNavigator: TPanel;
    DBNavigator1: TDBNavigator;
    Panel1:     TPanel;
  private
  end;

var
  fmViewHorizon: TfmViewHorizon;

implementation

{$R *.DFM}


end.
