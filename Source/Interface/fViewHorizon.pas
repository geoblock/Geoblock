 //----------------------------------------------------------------------------
 // Geoblock is a program for modeling and visualization of geoscience datasets.

 // The contents of this file are subject to the Mozilla Public License
 // Version 1.1 (the "License"); you may not use this file except in compliance
 // with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/

 // Software distributed under the License is distributed on an "AS IS" basis,
 // WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 // the specific language governing rights and limitations under the License.

 // The initial developer of the original code and contributors are documented
 // in the accompanying help file Geoblock.chm. Portions created by these
 // individuals are Copyright (C) of these individuals. All Rights Reserved.
 //---------------------------------------------------------------------------

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
