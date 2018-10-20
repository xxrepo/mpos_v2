object TestFrame: TTestFrame
  Left = 0
  Height = 436
  Top = 0
  Width = 240
  ClientHeight = 436
  ClientWidth = 240
  TabOrder = 0
  DesignLeft = 308
  DesignTop = 186
  object Panel7: TPanel
    Left = 64
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
    Left = 152
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
    Left = 0
    Height = 24
    Top = 8
    Width = 60
    Caption = '切换主题'
    Color = clSkyBlue
    ParentColor = False
    TabOrder = 2
    OnClick = Panel1Click
  end
  object Panel2: TPanel
    Left = 48
    Height = 50
    Top = 122
    Width = 170
    Caption = 'Panel2'
    TabOrder = 3
    OnClick = Panel2Click
  end
end
