object FormTest: TFormTest
  Left = 264
  Height = 370
  Top = 183
  Width = 836
  Caption = 'FormTest'
  ClientHeight = 370
  ClientWidth = 836
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  LCLVersion = '6.5'
  object Button1: TButton
    Left = 23
    Height = 25
    Top = 30
    Width = 75
    Caption = 'Button1'
    OnClick = Button1Click
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 136
    Height = 25
    Top = 32
    Width = 80
    TabOrder = 1
    Text = 'Edit1'
  end
  object Memo1: TMemo
    Left = 246
    Height = 370
    Top = 0
    Width = 590
    Align = alRight
    Lines.Strings = (
      'Memo1'
    )
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object Button2: TButton
    Left = 23
    Height = 25
    Top = 79
    Width = 75
    Caption = 'Button2'
    OnClick = Button2Click
    TabOrder = 3
  end
  object Button3: TButton
    Left = 23
    Height = 25
    Top = 144
    Width = 75
    Caption = 'Button3'
    OnClick = Button3Click
    TabOrder = 4
  end
end
