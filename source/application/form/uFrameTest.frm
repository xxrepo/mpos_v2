object TestFrame: TTestFrame
  Left = 0
  Height = 436
  Top = 0
  Width = 280
  ClientHeight = 436
  ClientWidth = 280
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
  object Panel2: TPanel
    Left = 8
    Height = 26
    Top = 104
    Width = 88
    Caption = 'DB Init'
    TabOrder = 3
    OnClick = Panel2Click
  end
  object edtShopCode: TEdit
    Left = 8
    Height = 25
    Top = 39
    Width = 88
    Enabled = False
    TabOrder = 4
  end
  object edtTermCode: TEdit
    Left = 104
    Height = 25
    Top = 39
    Width = 82
    Enabled = False
    TabOrder = 5
  end
  object edtTermUUID: TEdit
    Left = 8
    Height = 25
    Top = 72
    Width = 264
    Enabled = False
    TabOrder = 6
  end
  object Panel5: TPanel
    Left = 192
    Height = 25
    Top = 39
    Width = 26
    Caption = '...'
    Color = clMoneyGreen
    ParentColor = False
    TabOrder = 7
    OnClick = Panel5Click
  end
  object Panel3: TPanel
    Left = 8
    Height = 24
    Top = 160
    Width = 124
    Caption = 'Panel3'
    TabOrder = 8
    OnClick = Panel3Click
  end
  object Panel4: TPanel
    Left = 8
    Height = 24
    Top = 192
    Width = 124
    Caption = 'Panel4'
    TabOrder = 9
    OnClick = Panel4Click
  end
end
