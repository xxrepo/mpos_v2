object Form1: TForm1
  Left = 308
  Height = 329
  Top = 186
  Width = 785
  Caption = 'Form1'
  ClientHeight = 329
  ClientWidth = 785
  OnCreate = FormCreate
  LCLVersion = '6.5'
  object Button1: TButton
    Left = 8
    Height = 25
    Top = 64
    Width = 75
    Caption = 'add list'
    OnClick = Button1Click
    TabOrder = 0
  end
  object Memo1: TMemo
    Left = 280
    Height = 329
    Top = 0
    Width = 505
    Align = alRight
    Lines.Strings = (
      'Memo1'
    )
    TabOrder = 1
  end
  object Button2: TButton
    Left = 8
    Height = 25
    Top = 96
    Width = 75
    Caption = 'Button2'
    OnClick = Button2Click
    TabOrder = 2
  end
  object Button3: TButton
    Left = 88
    Height = 25
    Top = 224
    Width = 75
    Caption = 'Button3'
    TabOrder = 3
  end
end
