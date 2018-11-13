object Form2: TForm2
  Left = 308
  Height = 376
  Top = 186
  Width = 750
  Caption = 'Form2'
  ClientHeight = 376
  ClientWidth = 750
  OnCreate = FormCreate
  LCLVersion = '6.5'
  object Button4: TButton
    Left = 16
    Height = 25
    Top = 48
    Width = 88
    Caption = 'add hashlist'
    OnClick = Button4Click
    TabOrder = 0
  end
  object Button5: TButton
    Left = 16
    Height = 25
    Top = 208
    Width = 75
    Caption = 'Button5'
    OnClick = Button5Click
    TabOrder = 1
  end
  object Memo1: TMemo
    Left = 245
    Height = 376
    Top = 0
    Width = 505
    Align = alRight
    Lines.Strings = (
      'Memo1'
    )
    TabOrder = 2
  end
  object Button1: TButton
    Left = 112
    Height = 25
    Top = 48
    Width = 75
    Caption = 'get'
    OnClick = Button1Click
    TabOrder = 3
  end
  object Button2: TButton
    Left = 112
    Height = 25
    Top = 80
    Width = 75
    Caption = 'delete'
    OnClick = Button2Click
    TabOrder = 4
  end
  object Button3: TButton
    Left = 112
    Height = 25
    Top = 112
    Width = 75
    Caption = 'remove'
    OnClick = Button3Click
    TabOrder = 5
  end
  object Button6: TButton
    Left = 16
    Height = 25
    Top = 79
    Width = 75
    Caption = 'add 全局'
    OnClick = Button6Click
    TabOrder = 6
  end
  object Button7: TButton
    Left = 16
    Height = 25
    Top = 112
    Width = 75
    Caption = '全局 = nil'
    OnClick = Button7Click
    TabOrder = 7
  end
end
