object Form1: TForm1
  Left = 308
  Height = 419
  Top = 186
  Width = 629
  Caption = 'Form1'
  ClientHeight = 419
  ClientWidth = 629
  OnShow = FormShow
  LCLVersion = '6.5'
  object Button2: TButton
    Left = 408
    Height = 25
    Top = 48
    Width = 75
    Caption = 'URL test'
    OnClick = Button2Click
    TabOrder = 0
  end
  object Memo1: TMemo
    Left = 0
    Height = 419
    Top = 0
    Width = 400
    Align = alLeft
    Lines.Strings = (
      'Memo1'
    )
    TabOrder = 1
  end
  object Button3: TButton
    Left = 488
    Height = 25
    Top = 48
    Width = 75
    Caption = 'PosR'
    OnClick = Button3Click
    TabOrder = 2
  end
  object Button4: TButton
    Left = 408
    Height = 25
    Top = 80
    Width = 75
    Caption = 'RCutStr'
    OnClick = Button4Click
    TabOrder = 3
  end
  object Button1: TButton
    Left = 488
    Height = 25
    Top = 80
    Width = 75
    Caption = '字符串内存'
    OnClick = Button1Click
    TabOrder = 4
  end
  object Button5: TButton
    Left = 408
    Height = 25
    Top = 112
    Width = 120
    Caption = 'tomcat server start'
    OnClick = Button5Click
    TabOrder = 5
  end
  object Button7: TButton
    Left = 408
    Height = 25
    Top = 176
    Width = 75
    Caption = 'cmstp'
    OnClick = Button7Click
    TabOrder = 6
  end
end
