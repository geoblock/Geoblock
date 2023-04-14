inherited fmMapWindow: TfmMapWindow
  Left = 214
  Top = 182
  HelpContext = 20
  Caption = 'Loading...'
  ClientHeight = 306
  ClientWidth = 626
  Font.Height = -11
  FormStyle = fsMDIChild
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnKeyDown = FormKeyDown
  OnPaint = FormPaint
  OnResize = FormResize
  ExplicitWidth = 644
  ExplicitHeight = 353
  TextHeight = 13
  object GBCanvas: TGBCanvas [0]
    Left = 0
    Top = 0
    Width = 626
    Height = 306
    Align = alClient
    Caption = 'Map canvas'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    Visible = False
    OnDblClick = GBCanvasDblClick
    OnMouseDown = GBCanvasMouseDown
    OnMouseMove = GBCanvasMouseMove
    ViewMode = vmXYZ
    ViewAngle = 30.000000000000000000
    NearZ = 20.000000000000000000
    FarZ = 100000.000000000000000000
    PickWidth = 3.000000000000000000
    PickHeight = 3.000000000000000000
    DrawCursor = crDefault
    Material.MaterialType = mtCustom
    Material.AmbientColor.Color = 926006
    Material.DiffuseColor.Color = 3042742
    Material.SpecularColor.Color = 2835812
    Material.Shininess = 0.217948719859123200
    Lighting.SpotCutoff = 180.000000000000000000
    Lighting.ConstantAttenuation = 1.000000000000000000
    Lighting.AmbientColor.Color = clSilver
    Lighting.AmbientColor.Alpha = 1.000000000000000000
    Lighting.DiffuseColor.Color = clSilver
    Lighting.DiffuseColor.Alpha = 1.000000000000000000
    Lighting.SpecularColor.Color = clSilver
    Lighting.SpecularColor.Alpha = 1.000000000000000000
    Lighting.Position.Y = 1000.000000000000000000
    Lighting.Position.Z = 1000.000000000000000000
    Lighting.Position.W = 1.000000000000000000
    Lighting.SpotDirection.Z = -1.000000000000000000
    Lighting.LightModel.LocalViewer = False
    Lighting.LightModel.TwoSide = False
    Lighting.LightModel.Ambient.Color = 3355443
    Lighting.LightModel.Ambient.Alpha = 1.000000000000000000
    Axes.PitchAxisColor = clWhite
    Axes.YawAxisColor = clYellow
    Axes.AxisRadius = 0.250000000000000000
    Axes.AxisLength = 30.000000000000000000
    Axes.Visible = False
    kX = 1.000000000000000000
    kY = 1.000000000000000000
    kZ = 1.000000000000000000
    MouseWheelStep = 1.000000000000000000
    MinX = -100.000000000000000000
    MaxX = 100.000000000000000000
    MinY = -100.000000000000000000
    MaxY = 100.000000000000000000
    MinZ = -100.000000000000000000
    MaxZ = 100.000000000000000000
    Lx = 200.000000000000000000
    Ly = 200.000000000000000000
    Lz = 200.000000000000000000
    InitialRotation.Pitch = 90.000000000000000000
    RotationSequence = RPY
    OnCallLists = GBCanvasCallLists
    OnSelectPrimitive = GBCanvasSelectPrimitive
    object StatusBar: TStatusBar
      Left = 1
      Top = 286
      Width = 624
      Height = 19
      Panels = <
        item
          Text = 'FPS:'
          Width = 100
        end
        item
          Text = 'X:'
          Width = 100
        end
        item
          Text = 'Y:'
          Width = 100
        end
        item
          Text = 'Z:'
          Width = 100
        end>
      ExplicitLeft = 232
      ExplicitTop = 272
      ExplicitWidth = 0
    end
  end
  object TableMap: TTable
    Left = 24
    Top = 8
  end
  object PopupMenuMap: TPopupMenu
    AutoPopup = False
    Left = 120
    Top = 8
    object MenuItemMapSavePolygon: TMenuItem
      Caption = 'Save Polygon'
      OnClick = MenuItemMapSavePolygonClick
    end
    object MenuItemMapDeletePolygon: TMenuItem
      Caption = 'Delete Polygon'
    end
  end
end
