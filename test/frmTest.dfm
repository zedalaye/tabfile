object frmMain: TfrmMain
  Left = 246
  Top = 174
  BorderStyle = bsDialog
  Caption = 'Test TabFile'
  ClientHeight = 424
  ClientWidth = 496
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 496
    Height = 405
    ActivePage = tsTabFile
    Align = alClient
    TabOrder = 1
    object tsTabFile: TTabSheet
      Caption = 'TabFile'
      object Label1: TLabel
        Left = 196
        Top = 8
        Width = 74
        Height = 13
        Caption = 'Nom du fichier :'
      end
      object btCreation: TButton
        Left = 196
        Top = 52
        Width = 97
        Height = 25
        Caption = 'Création fichier'
        TabOrder = 0
        OnClick = btCreationClick
      end
      object rgTaille: TRadioGroup
        Left = 4
        Top = 4
        Width = 185
        Height = 89
        Caption = 'Taille du fichier'
        ItemIndex = 1
        Items.Strings = (
          '1 Ko'
          '1 Mo'
          '10 Mo'
          '100 Mo')
        TabOrder = 1
      end
      object eFichier: TEdit
        Left = 196
        Top = 24
        Width = 121
        Height = 21
        Enabled = False
        TabOrder = 2
        Text = 'test.dat'
      end
      object bgTest: TGroupBox
        Left = 4
        Top = 95
        Width = 477
        Height = 278
        Caption = 'Tests'
        TabOrder = 3
        object btInverse: TButton
          Left = 8
          Top = 128
          Width = 165
          Height = 25
          Caption = 'Inversion début / fin'
          TabOrder = 0
          OnClick = btInverseClick
        end
        object btTaille50: TButton
          Left = 8
          Top = 16
          Width = 165
          Height = 25
          Hint = 'aze|aze aze aze'
          Caption = 'Réduire la taille à 50 octets'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = btTaille50Click
        end
        object btAleat: TButton
          Left = 8
          Top = 44
          Width = 165
          Height = 25
          Caption = 'Contenu aléatoire'
          TabOrder = 2
          OnClick = btAleatClick
        end
        object bt1moAleat: TButton
          Left = 8
          Top = 72
          Width = 165
          Height = 25
          Caption = '1 000 000 valeurs aléatoire'
          TabOrder = 3
          OnClick = bt1moAleatClick
        end
        object btRaz: TButton
          Left = 8
          Top = 100
          Width = 165
          Height = 25
          Caption = 'Remise à la valeur 0'
          TabOrder = 4
          OnClick = btRazClick
        end
        object gbChaine: TGroupBox
          Left = 188
          Top = 16
          Width = 281
          Height = 149
          Caption = 'Chaîne de caractères'
          TabOrder = 5
          object Label2: TLabel
            Left = 12
            Top = 24
            Width = 76
            Height = 13
            Caption = 'Chaîne de test :'
          end
          object btRecherche: TButton
            Left = 180
            Top = 116
            Width = 89
            Height = 25
            Caption = 'Recherche'
            TabOrder = 0
            OnClick = btRechercheClick
          end
          object eChaine: TEdit
            Left = 100
            Top = 20
            Width = 169
            Height = 21
            TabOrder = 1
            Text = 'TEST'
          end
          object btCopie10: TButton
            Left = 12
            Top = 52
            Width = 165
            Height = 25
            Caption = 'Copie à 10 positions aléatoires'
            TabOrder = 2
            OnClick = btCopie10Click
          end
          object btCopieFin: TButton
            Left = 12
            Top = 80
            Width = 165
            Height = 25
            Caption = 'Copie en fin de fichier'
            TabOrder = 3
            OnClick = btCopieFinClick
          end
        end
        object Button1: TButton
          Left = 8
          Top = 168
          Width = 165
          Height = 25
          Caption = 'Insérer 1000 octets au début'
          TabOrder = 6
          OnClick = Button1Click
        end
        object Button2: TButton
          Left = 8
          Top = 196
          Width = 165
          Height = 25
          Caption = 'Supprimer 500 octets au début'
          TabOrder = 7
          OnClick = Button2Click
        end
        object Button4: TButton
          Left = 8
          Top = 224
          Width = 165
          Height = 25
          Caption = 'Ajouter 10 # à la fin'
          TabOrder = 8
          OnClick = Button4Click
        end
      end
    end
    object tsTabStringsFile: TTabSheet
      Caption = 'TabStringsFile'
      ImageIndex = 1
      object Label3: TLabel
        Left = 196
        Top = 8
        Width = 74
        Height = 13
        Caption = 'Nom du fichier :'
      end
      object btStringVisualiser: TButton
        Left = 400
        Top = 8
        Width = 75
        Height = 25
        Caption = 'Visualiser'
        TabOrder = 0
        OnClick = btStringVisualiserClick
      end
      object rgTailleFichierTexte: TRadioGroup
        Left = 4
        Top = 4
        Width = 133
        Height = 109
        Caption = 'Taille du fichier texte'
        ItemIndex = 1
        Items.Strings = (
          '0'
          '1 Ko'
          '1 Mo'
          '10 Mo'
          '100 Mo')
        TabOrder = 1
      end
      object btCreationFichierTexte: TButton
        Left = 196
        Top = 52
        Width = 97
        Height = 25
        Caption = 'Création fichier'
        TabOrder = 2
        OnClick = btCreationFichierTexteClick
      end
      object eFichierTexte: TEdit
        Left = 196
        Top = 24
        Width = 121
        Height = 21
        Enabled = False
        TabOrder = 3
        Text = 'fichier.txt'
      end
      object rgSeparateur: TRadioGroup
        Left = 4
        Top = 116
        Width = 133
        Height = 73
        Caption = 'Séparateur de lignes'
        ItemIndex = 2
        Items.Strings = (
          'CR'
          'LF'
          'CR/LF')
        TabOrder = 4
      end
      object Button6: TButton
        Left = 4
        Top = 196
        Width = 173
        Height = 25
        Caption = 'Modification de la première ligne'
        TabOrder = 5
        OnClick = Button6Click
      end
      object Button7: TButton
        Left = 4
        Top = 224
        Width = 173
        Height = 25
        Caption = 'Insertion d'#39'une ligne au début'
        TabOrder = 6
        OnClick = Button7Click
      end
      object Button3: TButton
        Left = 4
        Top = 340
        Width = 193
        Height = 25
        Caption = 'Remplacer par le contenu d'#39'un fichier'
        TabOrder = 7
        OnClick = Button3Click
      end
      object Button8: TButton
        Left = 4
        Top = 280
        Width = 173
        Height = 25
        Caption = 'Ajout d'#39'une ligne vide'
        TabOrder = 8
        OnClick = Button8Click
      end
      object Button5: TButton
        Left = 4
        Top = 308
        Width = 173
        Height = 25
        Caption = 'Insertion d'#39'une ligne vide'
        TabOrder = 9
        OnClick = Button5Click
      end
      object btStringRecherche: TButton
        Left = 232
        Top = 196
        Width = 75
        Height = 25
        Caption = 'Recherche'
        TabOrder = 10
        OnClick = btStringRechercheClick
      end
      object eChaineRecherche: TEdit
        Left = 316
        Top = 196
        Width = 121
        Height = 21
        TabOrder = 11
        Text = 'texte'
      end
      object Button10: TButton
        Left = 4
        Top = 252
        Width = 173
        Height = 25
        Caption = 'Suppression de la première ligne'
        TabOrder = 12
        OnClick = Button10Click
      end
      object btCompterLignes: TButton
        Left = 232
        Top = 232
        Width = 125
        Height = 25
        Caption = 'Compter les lignes'
        TabOrder = 13
        OnClick = btCompterLignesClick
      end
      object Button9: TButton
        Left = 236
        Top = 276
        Width = 75
        Height = 25
        Caption = 'Button9'
        TabOrder = 14
        OnClick = Button9Click
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 405
    Width = 496
    Height = 19
    Panels = <>
    SimplePanel = False
  end
  object OpenDialog1: TOpenDialog
    Left = 20
    Top = 388
  end
end
