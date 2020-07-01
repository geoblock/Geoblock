 //------------------------------------------------------------------------------
 // This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
 //------------------------------------------------------------------------------

 //---------------------------------------------------------------------------
 // The contents of this file are subject to the Mozilla Public License
 // Version 2.0 (the "License"); you may not use this file except in compliance
 // with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/

 // Software distributed under the License is distributed on an "AS IS" basis,
 // WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 // the specific language governing rights and limitations under the License.

 // The initial developer of the original code and contributors are documented
 // in the accompanying help file Geoblock.chm. Portions created by these
 // individuals are Copyright (C) of these individuals. All Rights Reserved.
 //---------------------------------------------------------------------------

unit fInterLinear;

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
  Vcl.Samples.Spin, 
  Vcl.ExtCtrls,

  
  fInitialDialog;

type
  TfmInterLinear = class(TfmInitialDialog)
    GroupBoxAnisotropy: TGroupBox;
    GroupBox1:    TGroupBox;
    LabelRatio1:  TLabel;
    LabelRatio2:  TLabel;
    SpinEdit1:    TSpinEdit;
    SpinEdit2:    TSpinEdit;
    GroupBox2:    TGroupBox;
    LabelAzimuth: TLabel;
    LabelDip:     TLabel;
    LabelStrike:  TLabel;
    EditAzimuth:  TEdit;
    EditDip:      TEdit;
    EditStrike:   TEdit;
  private
     
  public
     
  end;

var
  fmInterLinear: TfmInterLinear;

implementation

uses
  uLinearByTIN;

{$R *.DFM}

end.
