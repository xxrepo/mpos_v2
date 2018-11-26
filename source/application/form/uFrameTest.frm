object TestFrame: TTestFrame
  Left = 0
  Height = 436
  Top = 0
  Width = 569
  ClientHeight = 436
  ClientWidth = 569
  OnClick = FrameClick
  TabOrder = 0
  DesignLeft = 308
  DesignTop = 186
  object Panel7: TPanel
    Left = 104
    Height = 24
    Top = 8
    Width = 82
    Caption = '显示1商品'
    Color = clMoneyGreen
    ParentColor = False
    TabOrder = 0
    OnClick = Panel7Click
  end
  object Panel6: TPanel
    Left = 191
    Height = 24
    Top = 8
    Width = 82
    Caption = '显示800商品'
    Color = clMoneyGreen
    ParentColor = False
    TabOrder = 1
    OnClick = Panel6Click
  end
  object Panel1: TPanel
    Left = 8
    Height = 24
    Top = 8
    Width = 88
    Caption = '切换主题'
    Color = clSkyBlue
    ParentColor = False
    TabOrder = 2
    OnClick = Panel1Click
  end
  object Button1: TButton
    Left = 64
    Height = 25
    Top = 48
    Width = 75
    Caption = 'Button1'
    OnClick = Button1Click
    TabOrder = 3
  end
  object Label1: TLabel
    Left = 8
    Height = 12
    Top = 61
    Width = 36
    Caption = 'Label1'
    ParentColor = False
  end
  object Button2: TButton
    Left = 64
    Height = 25
    Top = 80
    Width = 75
    Caption = 'Button2'
    OnClick = Button2Click
    TabOrder = 4
  end
  object ListBox1: TListBox
    Left = 8
    Height = 80
    Top = 120
    Width = 265
    ItemHeight = 0
    TabOrder = 5
  end
  object ComboBox1: TComboBox
    Left = 8
    Height = 25
    Top = 224
    Width = 100
    ItemHeight = 17
    TabOrder = 6
    Text = 'ComboBox1'
  end
  object Button3: TButton
    Left = 145
    Height = 25
    Top = 267
    Width = 75
    Caption = 'Button3'
    OnClick = Button3Click
    TabOrder = 7
  end
end
