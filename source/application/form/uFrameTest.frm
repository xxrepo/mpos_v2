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
    Left = 46
    Height = 12
    Top = 87
    Width = 36
    Caption = 'Label1'
    ParentColor = False
  end
  object Memo1: TMemo
    Left = 8
    Height = 90
    Top = 104
    Width = 88
    Lines.Strings = (
      'Memo1'
    )
    TabOrder = 4
  end
  object DateTimePicker1: TDateTimePicker
    Left = 280
    Height = 48
    Top = 51
    Width = 240
    CenturyFrom = 1941
    MaxDate = 2958465
    MinDate = -53780
    AutoSize = False
    Font.Height = -19
    ParentFont = False
    TabOrder = 5
    TrailingSeparator = False
    TextForNullDate = 'NULL'
    LeadingZeros = True
    Kind = dtkDateTime
    TimeFormat = tf24
    TimeDisplay = tdHMS
    DateMode = dmUpDown
    Date = 43423
    Time = 0.659917974538985
    UseDefaultSeparators = True
    HideDateTimeParts = []
    MonthNames = 'Long'
  end
  object Button2: TButton
    Left = 104
    Height = 25
    Top = 104
    Width = 75
    Caption = 'Button2'
    OnClick = Button2Click
    TabOrder = 6
  end
  object StringGrid1: TStringGrid
    Left = 0
    Height = 184
    Top = 136
    Width = 272
    ColCount = 4
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goSmoothScroll]
    TabOrder = 7
  end
  object DBGrid1: TDBGrid
    Left = 310
    Height = 100
    Top = 173
    Width = 200
    Color = clWindow
    Columns = <>
    TabOrder = 8
  end
  object DrawGrid1: TDrawGrid
    Left = 0
    Height = 116
    Top = 320
    Width = 272
    ExtendedSelect = False
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goSmoothScroll]
    TabOrder = 9
  end
end
