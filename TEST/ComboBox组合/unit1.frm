object Form1: TForm1
  Left = 308
  Height = 425
  Top = 186
  Width = 819
  Caption = 'Form1'
  ClientHeight = 425
  ClientWidth = 819
  OnCreate = FormCreate
  LCLVersion = '6.5'
  object Edit1: TEdit
    Left = 16
    Height = 25
    Top = 56
    Width = 176
    TabOrder = 0
    Text = 'Edit1'
  end
  object ListBox1: TListBox
    Left = 16
    Height = 136
    Top = 80
    Width = 176
    ItemHeight = 0
    TabOrder = 1
  end
  object ComboBox1: TComboBox
    Left = 16
    Height = 25
    Top = 8
    Width = 180
    ItemHeight = 17
    Items.Strings = (
      'aa'
      'bb'
      'cc'
      'dd'
    )
    OnDropDown = ComboBox1DropDown
    OnSelect = ComboBox1Select
    TabOrder = 2
    Text = 'ComboBox1'
  end
  object Button2: TButton
    Left = 280
    Height = 25
    Top = 232
    Width = 75
    Caption = 'create'
    OnClick = Button2Click
    TabOrder = 3
  end
  object Panel1: TPanel
    Left = 16
    Height = 186
    Top = 232
    Width = 250
    Caption = 'Panel1'
    Color = clMoneyGreen
    ParentColor = False
    TabOrder = 4
  end
  object Button3: TButton
    Left = 280
    Height = 25
    Top = 264
    Width = 75
    Caption = 'set width'
    OnClick = Button3Click
    TabOrder = 5
  end
  object Button4: TButton
    Left = 216
    Height = 25
    Top = 8
    Width = 75
    Caption = 'Button4'
    OnClick = Button4Click
    TabOrder = 6
  end
  object ComboBoxEx1: TComboBoxEx
    Left = 328
    Height = 24
    Top = 56
    Width = 120
    ItemHeight = 16
    ItemsEx = <>
    TabOrder = 7
  end
  object CheckComboBox1: TCheckComboBox
    Left = 328
    Height = 24
    Top = 97
    Width = 120
    ItemHeight = 16
    TabOrder = 8
  end
  object Memo1: TMemo
    Left = 469
    Height = 425
    Top = 0
    Width = 350
    Align = alRight
    Lines.Strings = (
      'Memo1'
    )
    TabOrder = 9
  end
  object Button5: TButton
    Left = 216
    Height = 25
    Top = 55
    Width = 75
    Caption = 'Button5'
    OnClick = Button5Click
    TabOrder = 10
  end
  object Button1: TButton
    Left = 280
    Height = 25
    Top = 296
    Width = 75
    Caption = 'add item'
    OnClick = Button1Click
    TabOrder = 11
  end
  object Button6: TButton
    Left = 280
    Height = 25
    Top = 328
    Width = 75
    Caption = 'select only'
    OnClick = Button6Click
    TabOrder = 12
  end
end
