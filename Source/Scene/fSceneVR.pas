 //------------------------------------------------------------------------------
 // This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
 //------------------------------------------------------------------------------

unit fSceneVR;

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
  Vcl.ExtCtrls,
  
  GLScene, 
  GLObjects, 
  GLCrossPlatform, 
  GLBaseClasses,
  GLSceneViewer;

type
  TfmSceneVR = class(TForm)
    PanelTop:     TPanel;
    GLScene:      TGLScene;
    PanelTopRight: TPanel;
    PanelTopLeft: TPanel;
    glsViewerLeft: TGLSceneViewer;
    glsViewerRight: TGLSceneViewer;
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
     
  public
     
  end;

var
  fmSceneVR: TfmSceneVR;

//=====================================================================
implementation
//=====================================================================

{$R *.DFM}

procedure TfmSceneVR.FormResize(Sender: TObject);
begin
  PanelTopLeft.Width    := ClientWidth div 2;
  PanelTopLeft.Height    := ClientHeight div 2;
end;

procedure TfmSceneVR.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
