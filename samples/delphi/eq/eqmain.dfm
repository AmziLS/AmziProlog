object MF: TMF
  Left = 544
  Top = 126
  Width = 535
  Height = 488
  ActiveControl = Draw
  Caption = 'Equation Test'
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object eqPaintBox: TPaintBox
    Left = 48
    Top = 264
    Width = 433
    Height = 169
    Color = clWhite
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Terminal'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    OnPaint = eqPaintBoxPaint
  end
  object Label2: TLabel
    Left = 48
    Top = 240
    Width = 82
    Height = 16
    Caption = 'Text Display'
  end
  object Label1: TLabel
    Left = 48
    Top = 168
    Width = 165
    Height = 16
    Caption = 'Mathematical Expression'
  end
  object eqEditBox: TEdit
    Left = 48
    Top = 192
    Width = 433
    Height = 24
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'System'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object Draw: TBitBtn
    Left = 400
    Top = 128
    Width = 81
    Height = 33
    Caption = 'Draw'
    TabOrder = 1
    OnClick = DrawClick
  end
  object Memo1: TMemo
    Left = 48
    Top = 8
    Width = 337
    Height = 153
    Lines.Strings = (
      'Enter a mathematical expression as it would'
      'be written in a programming language (using'
      'lower case letters)  and press '#39'Draw'#39' to see'
      'it displayed in text book format.  Try:'
      ''
      'c = sqrt(a**2 + b**2)'
      '(apples + oranges)/(pears + cherries)'
      'tree = 1/1/1/1/1/1/1')
    TabOrder = 2
  end
  object HelpButton: TButton
    Left = 400
    Top = 80
    Width = 81
    Height = 33
    Caption = 'Help'
    TabOrder = 3
    OnClick = HelpButtonClick
  end
  object CloseButton: TButton
    Left = 400
    Top = 8
    Width = 81
    Height = 33
    Caption = 'Close'
    TabOrder = 4
    OnClick = CloseButtonClick
  end
  object LSEng: TLSEngine
  end
end