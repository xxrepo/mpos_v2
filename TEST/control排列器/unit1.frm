object Form1: TForm1
  Left = 341
  Height = 474
  Top = 186
  Width = 742
  Caption = 'Form1'
  ClientHeight = 474
  ClientWidth = 742
  KeyPreview = True
  OnKeyDown = FormKeyDown
  LCLVersion = '6.5'
  object Panel1: TPanel
    Left = 16
    Height = 448
    Top = 16
    Width = 386
    Caption = 'Panel1'
    Color = clMoneyGreen
    ParentColor = False
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 460
    Height = 474
    Top = 0
    Width = 282
    Align = alRight
    Caption = 'Panel2'
    ClientHeight = 474
    ClientWidth = 282
    TabOrder = 1
    object Button1: TButton
      Left = 16
      Height = 25
      Top = 16
      Width = 75
      Caption = 'Button1'
      TabOrder = 0
    end
    object StringGrid1: TStringGrid
      Left = 40
      Height = 100
      Top = 248
      Width = 200
      TabOrder = 1
    end
  end
end
