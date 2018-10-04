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
  DesignLeft = 308
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
        Width = 300
        BorderStyle = bsNone
        Font.Height = -27
        Font.Name = '黑体'
        OnKeyDown = Edit1KeyDown
        ParentFont = False
        TabOrder = 0
      end
      object PanelSum: TPanel
        Left = 448
        Height = 55
        Top = 0
        Width = 258
        Align = alRight
        BevelOuter = bvNone
        ClientHeight = 55
        ClientWidth = 258
        TabOrder = 1
        object Label1: TLabel
          Left = 8
          Height = 12
          Top = 32
          Width = 36
          Caption = '数量：'
          ParentColor = False
        end
        object Label2: TLabel
          Left = 96
          Height = 12
          Top = 32
          Width = 36
          Caption = '合计：'
          ParentColor = False
        end
        object Lab_sumCost: TLabel
          Left = 136
          Height = 24
          Top = 20
          Width = 48
          Caption = '0.00'
          Font.Color = 155368
          Font.Height = -24
          Font.Name = '黑体'
          ParentColor = False
          ParentFont = False
        end
        object Lab_count: TLabel
          Left = 48
          Height = 12
          Top = 32
          Width = 6
          Caption = '0'
          ParentColor = False
        end
      end
      object LabelScan: TLabel
        Left = 16
        Height = 27
        Top = 17
        Width = 40
        Alignment = taCenter
        AutoSize = False
        Caption = '扫码'
        Color = 15640885
        Layout = tlCenter
        ParentColor = False
        Transparent = False
      end
    end
    object PanelItems: TPanel
      Left = 6
      Height = 246
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
        Left = 44
        Height = 36
        Top = 2
        Width = 380
        Align = alClient
        Alignment = taCenter
        AutoSize = False
        BorderSpacing.Top = 2
        Caption = '品名'
        Layout = tlCenter
        ParentColor = False
      end
      object LabelPrice: TLabel
        Left = 424
        Height = 36
        Top = 2
        Width = 80
        Align = alRight
        Alignment = taRightJustify
        AutoSize = False
        BorderSpacing.Top = 2
        Caption = '单价'
        Layout = tlCenter
        ParentColor = False
      end
      object LabelQuantity: TLabel
        Left = 504
        Height = 36
        Top = 2
        Width = 60
        Align = alRight
        Alignment = taRightJustify
        AutoSize = False
        BorderSpacing.Top = 2
        Caption = '数量'
        Layout = tlCenter
        ParentColor = False
      end
      object LabelCost: TLabel
        Left = 564
        Height = 36
        Top = 2
        Width = 100
        Align = alRight
        Alignment = taRightJustify
        AutoSize = False
        BorderSpacing.Top = 2
        BorderSpacing.Right = 30
        Caption = '小计'
        Layout = tlCenter
        ParentColor = False
      end
      object LabelNO: TLabel
        Left = 10
        Height = 36
        Top = 2
        Width = 34
        Align = alLeft
        Alignment = taRightJustify
        AutoSize = False
        BorderSpacing.Left = 10
        BorderSpacing.Top = 2
        Caption = '序号'
        Layout = tlCenter
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
    object PanelMsg: TPanel
      Left = 10
      Height = 34
      Top = 286
      Width = 686
      Align = alBottom
      Alignment = taLeftJustify
      BorderSpacing.Left = 10
      BorderSpacing.Right = 10
      BevelOuter = bvNone
      Caption = '提示信息'
      Font.Height = -16
      Font.Name = '黑体'
      ParentFont = False
      TabOrder = 4
      Visible = False
      Wordwrap = True
    end
  end
end
