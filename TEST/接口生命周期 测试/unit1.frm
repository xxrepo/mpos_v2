object Form1: TForm1
  Left = 308
  Height = 399
  Top = 186
  Width = 670
  Caption = 'Form1'
  ClientHeight = 399
  ClientWidth = 670
  OnCreate = FormCreate
  LCLVersion = '6.5'
  object Button1: TButton
    Left = 8
    Height = 25
    Top = 8
    Width = 160
    Caption = '局部类声明，指定接口实例'
    OnClick = Button1Click
    TabOrder = 0
  end
  object Memo1: TMemo
    Left = 240
    Height = 399
    Top = 0
    Width = 430
    Align = alRight
    Lines.Strings = (
      'Memo1'
    )
    TabOrder = 1
  end
  object Button2: TButton
    Left = 8
    Height = 25
    Top = 48
    Width = 75
    Caption = '= nil'
    OnClick = Button2Click
    TabOrder = 2
  end
  object Button3: TButton
    Left = 8
    Height = 25
    Top = 80
    Width = 75
    Caption = '调接口方法'
    OnClick = Button3Click
    TabOrder = 3
  end
  object Button4: TButton
    Left = 8
    Height = 25
    Top = 120
    Width = 160
    Caption = '直接指定全局接口实例'
    OnClick = Button4Click
    TabOrder = 4
  end
  object Button5: TButton
    Left = 8
    Height = 25
    Top = 152
    Width = 160
    Caption = '指定局部接口实例'
    OnClick = Button5Click
    TabOrder = 5
  end
  object Button6: TButton
    Left = 8
    Height = 25
    Top = 216
    Width = 99
    Caption = '加接口加入List'
    OnClick = Button6Click
    TabOrder = 6
  end
  object Button7: TButton
    Left = 8
    Height = 25
    Top = 248
    Width = 128
    Caption = '加接口加入泛型List'
    OnClick = Button7Click
    TabOrder = 7
  end
  object Button8: TButton
    Left = 142
    Height = 25
    Top = 248
    Width = 98
    Caption = '泛型List clear'
    OnClick = Button8Click
    TabOrder = 8
  end
end
