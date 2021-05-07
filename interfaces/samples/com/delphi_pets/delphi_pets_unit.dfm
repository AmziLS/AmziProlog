object MainForm: TMainForm
  Left = 192
  Top = 107
  Width = 182
  Height = 195
  Caption = 'Delphi Pets'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 65
    Height = 13
    Caption = 'Pick a sound:'
  end
  object Label2: TLabel
    Left = 8
    Top = 120
    Width = 63
    Height = 13
    Caption = 'Your Pet is a:'
  end
  object SoundListBox: TListBox
    Left = 80
    Top = 8
    Width = 65
    Height = 57
    ItemHeight = 13
    Items.Strings = (
      'woof'
      'quack'
      'meow')
    TabOrder = 0
  end
  object PetEdit: TEdit
    Left = 80
    Top = 120
    Width = 65
    Height = 21
    TabOrder = 1
  end
  object GetPetButton: TButton
    Left = 80
    Top = 80
    Width = 65
    Height = 25
    Caption = 'Get Pet'
    TabOrder = 2
    OnClick = GetPetButtonClick
  end
  object LS: TComLogicServer
    AutoConnect = False
    ConnectKind = ckNewInstance
    Left = 16
    Top = 32
  end
end
