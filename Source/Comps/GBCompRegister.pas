//
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//

unit GBCompRegister;

(* Numeric property *)

interface

uses 
  System.Classes, 
  DesignIntf;

procedure Register;

//===============================================================
implementation
//===============================================================

uses
  GBEditRange,
  GBHelpContext,
  GBProject,
  GBGraphics;

procedure Register;
begin
  RegisterComponents('Geoblock', [TGBEditRange, TGBEditScaleInteger,
  TGBEditScaleDouble, TGBCanvas, TGBProject]);

  RegisterPropertyEditor(TypeInfo(THelpContext), TComponent,
    'HelpContext', THelpContextProperty);
  RegisterPropertyEditor(TypeInfo(string), TComponent,
    'HelpFile', THelpContextProperty);
end;

end.
