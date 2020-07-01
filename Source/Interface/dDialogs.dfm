object dmDialogs: TdmDialogs
  OldCreateOrder = False
  Height = 545
  Width = 795
  object PrintDialog: TPrintDialog
    HelpContext = 140
    Options = [poHelp]
    Left = 173
    Top = 282
  end
  object PrintSetupDialog: TPrinterSetupDialog
    Left = 49
    Top = 282
  end
  object SaveDialog: TSaveDialog
    HelpContext = 120
    DefaultExt = '*.DB'
    Filter = 'Data Tables (*.DB)|*.DB'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofShowHelp, ofEnableSizing]
    Left = 51
    Top = 86
  end
  object SavePictureDialog: TSavePictureDialog
    DefaultExt = '*.bmp'
    Filter = 
      'All (*.jpg;*.jpeg;*.bmp;*.ico)|*.jpg;*.jpeg;*.bmp;*.ico|JPEG Ima' +
      'ge File (*.jpg)|*.jpg|JPEG Image File (*.jpeg)|*.jpeg|Bitmaps (*' +
      '.bmp)|*.bmp|Icons (*.ico)|*.ico'
    Left = 696
    Top = 89
  end
  object ColorDialog: TColorDialog
    Color = clGray
    Options = [cdFullOpen, cdShowHelp, cdAnyColor]
    Left = 408
    Top = 164
  end
  object OpenPictureDialog: TOpenPictureDialog
    Filter = 
      'All (*.gif;*.png;*.jpg;*.jpeg;*.bmp;*.ico;*.emf;*.wmf)|*.gif;*.p' +
      'ng;*.jpg;*.jpeg;*.bmp;*.ico;*.emf;*.wmf|GIF Image (*.gif)|*.gif|' +
      'Portable Network Vcl.Vcl.CheckLst, (*.png)|*.png|JPEG Image File' +
      ' (*.jpg)|*.jpg|JPEG Image File (*.jpeg)|*.jpeg|Bitmaps (*.bmp)|*' +
      '.bmp|Icons (*.ico)|*.ico|Enhanced Metafiles (*.emf)|*.emf|Metafi' +
      'les (*.wmf)|*.wmf'
    Left = 688
    Top = 16
  end
  object FindDialog: TFindDialog
    Left = 56
    Top = 166
  end
  object ReplaceDialog: TReplaceDialog
    Options = [frDown, frShowHelp]
    Left = 176
    Top = 166
  end
  object OpenDialog: TOpenDialog
    HelpContext = 110
    DefaultExt = 'db'
    Filter = 'Data tables (*.DB)|*.DB|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofShowHelp, ofEnableSizing]
    Left = 52
    Top = 18
  end
  object OpenDialogProject: TOpenDialog
    HelpContext = 102
    DefaultExt = 'prj'
    Filter = 
      'Project as Text (*.prj)|*.prj|Project as map (*.prj)|*.prj|All F' +
      'iles (*.*)|*.*'
    FilterIndex = 2
    Options = [ofHideReadOnly, ofShowHelp, ofEnableSizing]
    Left = 156
    Top = 18
  end
  object OpenDialogSQL: TOpenDialog
    HelpContext = 210
    DefaultExt = 'sql'
    Filter = 'SQL (*.sql)|*.sql|All Files (*.*)|*.*'
    Left = 280
    Top = 16
  end
  object SaveDialogSQL: TSaveDialog
    HelpContext = 210
    DefaultExt = 'sql'
    Filter = 'SQL (*.sql)|*.sql|All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 296
    Top = 88
  end
  object SaveDialogExport: TSaveDialog
    HelpContext = 131
    Left = 160
    Top = 88
  end
  object OpenDialogImport: TOpenDialog
    HelpContext = 130
    DefaultExt = 'kdr'
    Filter = 
      'MapInfo Format  (*.MIF)|*.MIF|Surfer Data (*.DAT)|*.DAT|ArcInfo ' +
      'or Surfer Grid (*.GRD)|*.GRD|Three-D Economic Blocks  (*.ECO)|*.' +
      'ECO|Three-D Results  (*.RES)|*.RES|Four-X Model  (*.MOD)|*.MOD|F' +
      'our-X Sequence  (*.MSQ)|*.MSQ|AutoCad File (*.DXF)|*.DXF|Strings' +
      ' and Polygons (*.KDR)|*.KDR|Mesh3D(*.INP)|*.INP|Geo-EAS (*.EAS)|' +
      '*.EAS|Shape files ArcView (*.SHP)|*.SHP|All (*.*)|*.*'
    FilterIndex = 0
    Options = [ofPathMustExist, ofFileMustExist]
    Left = 408
    Top = 16
  end
  object SaveDialogText: TSaveDialog
    HelpContext = 121
    Filter = 'Text (*.txt)|*.txt|Project (*.prj)|*.prj'
    Left = 536
    Top = 80
  end
  object OpenDialogText: TOpenDialog
    Tag = 1
    DefaultExt = '*.TXT'
    Filter = 
      'Text  (*.TXT)|*.TXT|ASCII (*.ASC)|*.ASC|Project(*.PRJ)|*.PRJ|All' +
      ' Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 537
    Top = 12
  end
  object FontDialog: TFontDialog
    HelpContext = 611
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Options = [fdEffects, fdShowHelp, fdApplyButton]
    Left = 296
    Top = 164
  end
end
