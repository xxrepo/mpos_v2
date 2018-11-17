object TestFrame: TTestFrame
  Left = 0
  Height = 436
  Top = 0
  Width = 280
  ClientHeight = 436
  ClientWidth = 280
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
    Left = 77
    Height = 25
    Top = 123
    Width = 75
    Caption = 'Button1'
    OnClick = Button1Click
    TabOrder = 3
  end
end
