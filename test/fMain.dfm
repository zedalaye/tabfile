object frmMain: TfrmMain
  Left = 222
  Top = 128
  Width = 533
  Height = 438
  Caption = 'Dump'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object grdDump: TDrawGrid
    Left = 0
    Top = 0
    Width = 525
    Height = 177
    Align = alTop
    ColCount = 3
    Ctl3D = True
    DefaultRowHeight = 18
    RowCount = 2
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goHorzLine, goRangeSelect, goThumbTracking]
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 0
    OnDrawCell = grdDumpDrawCell
  end
  object odMain: TOpenDialog
    Left = 252
    Top = 4
  end
  object MainMenu1: TMainMenu
    Left = 284
    Top = 4
    object Fichier1: TMenuItem
      Caption = '&Fichier'
      object Ouvrir1: TMenuItem
        Caption = '&Ouvrir...'
        ShortCut = 16463
        OnClick = Ouvrir1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Quitter1: TMenuItem
        Caption = '&Quitter'
        ShortCut = 16465
        OnClick = Quitter1Click
      end
    end
  end
end
