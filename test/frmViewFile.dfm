object Form1: TForm1
  Left = 506
  Top = 253
  Width = 568
  Height = 367
  Caption = 'ViewFile'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ListView1: TListView
    Left = 0
    Top = 0
    Width = 552
    Height = 311
    Align = alClient
    Columns = <
      item
        Caption = 'N'#176' Ligne'
        Width = 70
      end
      item
        Caption = 'Texte'
        Width = 1000
      end>
    OwnerData = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnCustomDrawItem = ListView1CustomDrawItem
    OnData = ListView1Data
  end
  object MainMenu1: TMainMenu
    Left = 292
    Top = 28
    object Fichier1: TMenuItem
      Caption = 'Fichier'
      object Ouvrir1: TMenuItem
        Caption = 'Ouvrir...'
        ShortCut = 16463
        OnClick = Ouvrir1Click
      end
      object Quitter1: TMenuItem
        Caption = 'Quitter'
        OnClick = Quitter1Click
      end
    end
    object Edition1: TMenuItem
      Caption = 'Edition'
      object Rechercher1: TMenuItem
        Caption = 'Rechercher'
        ShortCut = 16454
        OnClick = Rechercher1Click
      end
      object OccurrenceSuivante: TMenuItem
        Caption = 'Occurrence Suivante'
        Enabled = False
        ShortCut = 114
        OnClick = Rechercher1Click
      end
    end
  end
end
