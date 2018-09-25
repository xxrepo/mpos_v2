object Form1: TForm1
  Left = 325
  Height = 462
  Top = 187
  Width = 994
  Caption = 'Form1'
  ClientHeight = 442
  ClientWidth = 994
  KeyPreview = True
  Menu = MainMenu1
  OnKeyDown = FormKeyDown
  LCLVersion = '6.5'
  object Button1: TButton
    Left = 720
    Height = 25
    Top = 8
    Width = 75
    Caption = 'Button1'
    OnClick = Button1Click
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 616
    Height = 25
    Top = 8
    Width = 96
    TabOrder = 1
    Text = 'Edit1'
  end
  object Panel1: TPanel
    Left = 0
    Height = 442
    Top = 0
    Width = 594
    Align = alLeft
    Caption = 'Panel1'
    TabOrder = 2
    OnMouseMove = Panel1MouseMove
  end
  object Button2: TButton
    Left = 720
    Height = 25
    Top = 43
    Width = 75
    Caption = 'Button2'
    OnClick = Button2Click
    TabOrder = 3
  end
  object Edit2: TEdit
    Left = 616
    Height = 25
    Top = 43
    Width = 96
    TabOrder = 4
    Text = 'Edit2'
  end
  object Button4: TButton
    Left = 616
    Height = 25
    Top = 115
    Width = 75
    Caption = 'flow layout'
    OnClick = Button4Click
    TabOrder = 5
  end
  object Button5: TButton
    Left = 712
    Height = 25
    Top = 115
    Width = 75
    Caption = 'Button5'
    OnClick = Button5Click
    TabOrder = 6
  end
  object Button6: TButton
    Left = 616
    Height = 25
    Top = 264
    Width = 75
    Caption = 'memu'
    OnClick = Button6Click
    TabOrder = 7
  end
  object Button7: TButton
    Left = 616
    Height = 25
    Top = 152
    Width = 75
    Caption = 'grid layout'
    OnClick = Button7Click
    TabOrder = 8
  end
  object Button8: TButton
    Left = 641
    Height = 25
    Top = 179
    Width = 75
    Caption = 'Button8'
    OnClick = Button8Click
    TabOrder = 9
  end
  object Button9: TButton
    Left = 641
    Height = 25
    Top = 208
    Width = 75
    Caption = 'Button9'
    OnClick = Button9Click
    TabOrder = 10
  end
  object Button10: TButton
    Left = 641
    Height = 25
    Top = 234
    Width = 75
    Caption = 'Button10'
    OnClick = Button10Click
    TabOrder = 11
  end
  object Button11: TButton
    Left = 720
    Height = 25
    Top = 179
    Width = 75
    Caption = 'Button11'
    OnClick = Button11Click
    TabOrder = 12
  end
  object Memo1: TMemo
    Left = 808
    Height = 258
    Top = 0
    Width = 190
    Lines.Strings = (
      'Memo1'
    )
    TabOrder = 13
  end
  object MainMenu1: TMainMenu
    Left = 88
    Top = 8
    object MenuItem1: TMenuItem
      Caption = 'MenuItem1'
    end
  end
end
