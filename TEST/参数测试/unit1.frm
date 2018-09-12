object Form1: TForm1
  Left = 341
  Height = 405
  Top = 186
  Width = 625
  Caption = 'Form1'
  ClientHeight = 405
  ClientWidth = 625
  OnShow = FormShow
  LCLVersion = '6.5'
  object Button1: TButton
    Left = 16
    Height = 25
    Top = 24
    Width = 75
    Caption = 'Button1'
    OnClick = Button1Click
    TabOrder = 0
  end
  object Memo1: TMemo
    Left = 152
    Height = 405
    Top = 0
    Width = 473
    Align = alRight
    Lines.Strings = (
      'Memo1'
    )
    TabOrder = 1
  end
  object Button2: TButton
    Left = 16
    Height = 25
    Top = 56
    Width = 75
    Caption = 'Button2'
    OnClick = Button2Click
    TabOrder = 2
  end
  object Button3: TButton
    Left = 16
    Height = 25
    Top = 104
    Width = 75
    Caption = 'Button3'
    OnClick = Button3Click
    TabOrder = 3
  end
  object Button4: TButton
    Left = 16
    Height = 25
    Top = 184
    Width = 75
    Caption = 'Button4'
    OnClick = Button4Click
    TabOrder = 4
  end
end
