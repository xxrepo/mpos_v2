object Form1: TForm1
  Left = 308
  Height = 373
  Top = 186
  Width = 820
  Caption = 'Form1'
  ClientHeight = 373
  ClientWidth = 820
  LCLVersion = '6.5'
  object Panel1: TPanel
    Left = 24
    Height = 200
    Top = 48
    Width = 226
    Caption = 'Panel1'
    Color = clMoneyGreen
    ParentColor = False
    TabOrder = 0
    OnMouseDown = Panel1MouseDown
    OnMouseEnter = Panel1MouseEnter
    OnMouseLeave = Panel1MouseLeave
    OnMouseMove = Panel1MouseMove
    OnMouseUp = Panel1MouseUp
    OnMouseWheel = Panel1MouseWheel
    OnMouseWheelDown = Panel1MouseWheelDown
    OnMouseWheelUp = Panel1MouseWheelUp
  end
  object Memo1: TMemo
    Left = 574
    Height = 373
    Top = 0
    Width = 246
    Align = alRight
    Lines.Strings = (
      'Memo1'
    )
    TabOrder = 1
  end
  object Button1: TButton
    Left = 312
    Height = 25
    Top = 22
    Width = 107
    Caption = 'get mouse pos'
    OnClick = Button1Click
    TabOrder = 2
  end
  object Button2: TButton
    Left = 312
    Height = 25
    Top = 67
    Width = 107
    Caption = 'set mouse pos'
    OnClick = Button2Click
    TabOrder = 3
  end
  object Button3: TButton
    Left = 24
    Height = 25
    Top = 264
    Width = 75
    Caption = 'Button3'
    OnClick = Button3Click
    TabOrder = 4
  end
  object StringGrid1: TStringGrid
    Left = 272
    Height = 240
    Top = 120
    Width = 240
    ColCount = 3
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect, goSmoothScroll]
    RowCount = 10
    TabOrder = 5
    OnMouseWheelUp = StringGrid1MouseWheelUp
  end
  object Button4: TButton
    Left = 104
    Height = 25
    Top = 296
    Width = 75
    Caption = 'Button4'
    OnClick = Button4Click
    TabOrder = 6
  end
end
