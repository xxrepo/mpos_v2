object SaleFrame: TSaleFrame
  Left = 0
  Height = 375
  Top = 0
  Width = 706
  AutoSize = True
  ClientHeight = 375
  ClientWidth = 706
  OnResize = FrameResize
  TabOrder = 0
  DesignLeft = 341
  DesignTop = 186
  object PanelSale: TPanel
    Left = 0
    Height = 375
    Top = 0
    Width = 706
    Align = alClient
    BevelOuter = bvNone
    ClientHeight = 375
    ClientWidth = 706
    TabOrder = 0
    object PanelBottom: TPanel
      Left = 0
      Height = 55
      Top = 320
      Width = 706
      Align = alBottom
      BevelOuter = bvNone
      ClientHeight = 55
      ClientWidth = 706
      TabOrder = 0
      object Edit1: TEdit
        Left = 64
        Height = 27
        Top = 17
        Width = 328
        BorderStyle = bsNone
        Font.Height = -27
        Font.Name = '黑体'
        OnKeyDown = Edit1KeyDown
        ParentFont = False
        TabOrder = 0
      end
      object Panel3: TPanel
        Left = 8
        Height = 27
        Top = 17
        Width = 50
        BevelOuter = bvNone
        Caption = '扫码'
        Color = 15640885
        ParentColor = False
        TabOrder = 1
      end
      object Label1: TLabel
        Left = 512
        Height = 12
        Top = 31
        Width = 36
        Caption = '数量：'
        ParentColor = False
      end
      object Label2: TLabel
        Left = 584
        Height = 12
        Top = 31
        Width = 36
        Caption = '合计：'
        ParentColor = False
      end
      object Lab_sumCost: TLabel
        Left = 624
        Height = 27
        Top = 17
        Width = 70
        Caption = '76.50'
        Font.Color = 155368
        Font.Height = -27
        ParentColor = False
        ParentFont = False
      end
      object Lab_count: TLabel
        Left = 548
        Height = 12
        Top = 31
        Width = 6
        Caption = '3'
        ParentColor = False
      end
    end
    object PanelItems: TPanel
      Left = 6
      Height = 280
      Top = 40
      Width = 694
      Align = alClient
      BorderSpacing.Left = 6
      BorderSpacing.Right = 6
      BevelOuter = bvNone
      Color = clWhite
      ParentColor = False
      TabOrder = 1
      OnMouseWheel = PanelItemsMouseWheel
    end
    object PanelTitle: TPanel
      Left = 6
      Height = 38
      Top = 0
      Width = 694
      Align = alTop
      BorderSpacing.Left = 6
      BorderSpacing.Right = 6
      BevelOuter = bvNone
      ClientHeight = 38
      ClientWidth = 694
      TabOrder = 2
      object LabelCommodity: TLabel
        Left = 14
        Height = 12
        Top = 16
        Width = 24
        Caption = '品名'
        ParentColor = False
      end
      object LabelPrice: TLabel
        Left = 264
        Height = 12
        Top = 16
        Width = 24
        Caption = '单价'
        ParentColor = False
      end
      object LabelQuantity: TLabel
        Left = 350
        Height = 12
        Top = 16
        Width = 24
        Caption = '数量'
        ParentColor = False
      end
      object LabelCost: TLabel
        Left = 432
        Height = 12
        Top = 16
        Width = 24
        Caption = '小计'
        ParentColor = False
      end
    end
    object PanelAdjustment: TPanel
      Left = 6
      Height = 2
      Top = 38
      Width = 694
      Align = alTop
      BorderSpacing.Left = 6
      BorderSpacing.Right = 6
      BevelOuter = bvNone
      Color = clWhite
      ParentColor = False
      TabOrder = 3
    end
  end
end
