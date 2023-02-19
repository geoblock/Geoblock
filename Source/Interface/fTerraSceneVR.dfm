object fmSceneVR: TfmSceneVR
  Left = 298
  Top = 121
  Caption = 'VR Scene'
  ClientHeight = 414
  ClientWidth = 613
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnResize = FormResize
  TextHeight = 13
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 613
    Height = 414
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alClient
    BevelOuter = bvNone
    Caption = 'PanelTop'
    DockSite = True
    TabOrder = 0
    object PanelTopRight: TPanel
      Left = 305
      Top = 0
      Width = 308
      Height = 414
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alClient
      DragKind = dkDock
      DragMode = dmAutomatic
      TabOrder = 0
      object glsViewerRight: TGLSceneViewer
        Left = 1
        Top = 1
        Width = 306
        Height = 412
        PenAsTouch = False
        Align = alClient
        TabOrder = 0
      end
    end
    object PanelTopLeft: TPanel
      Left = 0
      Top = 0
      Width = 305
      Height = 414
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alLeft
      DragKind = dkDock
      DragMode = dmAutomatic
      Locked = True
      TabOrder = 1
      object glsViewerLeft: TGLSceneViewer
        Left = 1
        Top = 1
        Width = 303
        Height = 412
        PenAsTouch = False
        Align = alClient
        TabOrder = 0
      end
    end
  end
  object GLScene: TGLScene
    Left = 16
    Top = 16
  end
end
