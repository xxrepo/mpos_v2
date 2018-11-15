object Form1: TForm1
  Left = 308
  Height = 356
  Top = 186
  Width = 711
  Caption = 'Form1'
  ClientHeight = 356
  ClientWidth = 711
  OnCreate = FormCreate
  LCLVersion = '6.5'
  object Memo1: TMemo
    Left = 312
    Height = 356
    Top = 0
    Width = 399
    Align = alRight
    Lines.Strings = (
      'Memo1'
    )
    TabOrder = 0
  end
  object Button1: TButton
    Left = 16
    Height = 25
    Top = 16
    Width = 75
    Caption = 'add'
    OnClick = Button1Click
    TabOrder = 1
  end
  object Button2: TButton
    Left = 16
    Height = 25
    Top = 56
    Width = 75
    Caption = 'clone'
    OnClick = Button2Click
    TabOrder = 2
  end
  object Button3: TButton
    Left = 128
    Height = 25
    Top = 16
    Width = 75
    Caption = 'Clear'
    OnClick = Button3Click
    TabOrder = 3
  end
  object Button4: TButton
    Left = 128
    Height = 25
    Top = 56
    Width = 75
    Caption = 'Clear'
    OnClick = Button4Click
    TabOrder = 4
  end
  object Button5: TButton
    Left = 224
    Height = 25
    Top = 56
    Width = 75
    Caption = 'Button5'
    OnClick = Button5Click
    TabOrder = 5
  end
end
