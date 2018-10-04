inherited NavigatorFrame: TNavigatorFrame
  Height = 363
  Width = 160
  ClientHeight = 363
  ClientWidth = 160
  OnResize = FrameResize
  object PanelTop: TPanel[0]
    Left = 0
    Height = 38
    Top = 0
    Width = 160
    Align = alTop
    BevelOuter = bvNone
    Color = clMoneyGreen
    ParentColor = False
    TabOrder = 0
  end
  object PanelBottom: TPanel[1]
    Left = 0
    Height = 55
    Top = 308
    Width = 160
    Align = alBottom
    BevelOuter = bvNone
    Color = clMoneyGreen
    ParentColor = False
    TabOrder = 1
  end
  object PanelClient: TPanel[2]
    Left = 0
    Height = 270
    Top = 38
    Width = 160
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
  end
end
