object frmVisu: TfrmVisu
  Left = 439
  Top = 298
  Width = 434
  Height = 385
  Caption = 'frmVisu'
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
  object DrawGrid1: TDrawGrid
    Left = 20
    Top = 56
    Width = 349
    Height = 269
    ColCount = 2
    DefaultColWidth = 45
    DefaultRowHeight = 17
    RowCount = 1
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goRangeSelect, goThumbTracking]
    TabOrder = 0
    OnDrawCell = DrawGrid1DrawCell
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 426
    Height = 29
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 12
      Top = 4
      Width = 32
      Height = 13
      Caption = 'Label1'
    end
  end
end
