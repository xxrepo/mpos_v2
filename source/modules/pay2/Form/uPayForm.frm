inherited PayForm: TPayForm
  Left = 373
  Height = 431
  Top = 169
  Width = 885
  Caption = 'PayForm'
  ClientHeight = 431
  ClientWidth = 885
  KeyPreview = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  object PayPanel: TPanel[0]
    Left = 0
    Height = 431
    Top = 0
    Width = 885
    Align = alClient
    ClientHeight = 431
    ClientWidth = 885
    TabOrder = 0
    object Label1: TLabel
      Left = 26
      Height = 12
      Top = 21
      Width = 60
      Caption = '单据金额: '
      ParentColor = False
    end
    object Bevel1: TBevel
      Left = 26
      Height = 3
      Top = 53
      Width = 390
      Shape = bsBottomLine
    end
    object labNeedAmount: TLabel
      Left = 104
      Height = 21
      Top = 15
      Width = 78
      Alignment = taRightJustify
      AutoSize = False
      Caption = '8888.80'
      Font.CharSet = GB2312_CHARSET
      Font.Color = clRed
      Font.Height = -21
      Font.Name = '黑体'
      Font.Pitch = fpFixed
      Font.Quality = fqDraft
      ParentColor = False
      ParentFont = False
    end
    object Label3: TLabel
      Left = 184
      Height = 12
      Top = 21
      Width = 12
      Caption = '元'
      ParentColor = False
    end
    object Bevel2: TBevel
      Left = 26
      Height = 11
      Top = 117
      Width = 390
      Shape = bsBottomLine
    end
    object Label4: TLabel
      Left = 26
      Height = 12
      Top = 70
      Width = 60
      Caption = '优惠金额: '
      ParentColor = False
    end
    object Label5: TLabel
      Left = 104
      Height = 21
      Top = 64
      Width = 78
      Alignment = taRightJustify
      AutoSize = False
      Caption = '0.00'
      Font.CharSet = GB2312_CHARSET
      Font.Color = clRed
      Font.Height = -21
      Font.Name = '黑体'
      Font.Pitch = fpFixed
      Font.Quality = fqDraft
      ParentColor = False
      ParentFont = False
    end
    object Label6: TLabel
      Left = 184
      Height = 12
      Top = 70
      Width = 12
      Caption = '元'
      ParentColor = False
    end
    object Label7: TLabel
      Left = 256
      Height = 12
      Top = 67
      Width = 48
      Caption = '券优惠: '
      ParentColor = False
    end
    object Label8: TLabel
      Left = 312
      Height = 21
      Top = 61
      Width = 68
      Alignment = taRightJustify
      AutoSize = False
      Caption = '0.00'
      Font.CharSet = GB2312_CHARSET
      Font.Color = clRed
      Font.Height = -21
      Font.Name = '黑体'
      Font.Pitch = fpFixed
      Font.Quality = fqDraft
      ParentColor = False
      ParentFont = False
    end
    object Label9: TLabel
      Left = 392
      Height = 12
      Top = 67
      Width = 12
      Caption = '元'
      ParentColor = False
    end
    object Label10: TLabel
      Left = 26
      Height = 12
      Top = 96
      Width = 60
      Caption = '优惠信息: '
      ParentColor = False
    end
    object Label11: TLabel
      Left = 104
      Height = 16
      Top = 94
      Width = 128
      Caption = '没有可用的优惠券'
      Font.CharSet = GB2312_CHARSET
      Font.Color = clRed
      Font.Height = -16
      Font.Name = '黑体'
      Font.Pitch = fpFixed
      Font.Quality = fqDraft
      ParentColor = False
      ParentFont = False
    end
    object Bevel3: TBevel
      Left = 26
      Height = 5
      Top = 200
      Width = 638
      Shape = bsBottomLine
    end
    object Label13: TLabel
      Left = 26
      Height = 12
      Top = 224
      Width = 96
      Caption = '请选择支付方式: '
      ParentColor = False
    end
    object Panel1: TPanel
      Tag = 1
      Left = 24
      Height = 138
      Top = 256
      Width = 96
      BevelColor = clMoneyGreen
      BevelInner = bvLowered
      BevelWidth = 2
      BorderStyle = bsSingle
      Caption = '现金'
      TabOrder = 0
      OnClick = Panel1Click
    end
    object Panel2: TPanel
      Tag = 2
      Left = 168
      Height = 138
      Top = 256
      Width = 96
      BevelOuter = bvNone
      BevelWidth = 6
      BorderStyle = bsSingle
      Caption = '通码支付'
      TabOrder = 1
      OnClick = Panel1Click
    end
    object Panel3: TPanel
      Tag = 3
      Left = 304
      Height = 138
      Top = 256
      Width = 96
      BevelInner = bvLowered
      Caption = '银联支付'
      TabOrder = 2
      OnClick = Panel1Click
    end
    object Panel7: TPanel
      Left = 568
      Height = 138
      Top = 256
      Width = 96
      BevelInner = bvLowered
      Caption = '其他支付'
      TabOrder = 3
    end
    object FPromptPanel: TPanel
      Left = 1
      Height = 25
      Top = 405
      Width = 883
      Align = alBottom
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Font.Height = -21
      Font.Name = '黑体'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 4
    end
    object Label14: TLabel
      Left = 256
      Height = 12
      Top = 18
      Width = 60
      Caption = '还需支付: '
      ParentColor = False
    end
    object Label15: TLabel
      Left = 312
      Height = 21
      Top = 12
      Width = 68
      Alignment = taRightJustify
      AutoSize = False
      Caption = '0.00'
      Font.CharSet = GB2312_CHARSET
      Font.Color = clGreen
      Font.Height = -21
      Font.Name = '黑体'
      Font.Pitch = fpFixed
      Font.Quality = fqDraft
      ParentColor = False
      ParentFont = False
    end
    object Label16: TLabel
      Left = 392
      Height = 12
      Top = 18
      Width = 12
      Caption = '元'
      ParentColor = False
    end
    object Memo1: TMemo
      Left = 424
      Height = 178
      Top = 8
      Width = 352
      Lines.Strings = (
        'Memo1'
      )
      TabOrder = 5
    end
    object Panel5: TPanel
      Tag = 4
      Left = 440
      Height = 138
      Top = 256
      Width = 96
      BevelInner = bvLowered
      Caption = '积分支付'
      TabOrder = 6
      OnClick = Panel1Click
    end
  end
end
