object Form1: TForm1
  Left = 308
  Height = 358
  Top = 186
  Width = 723
  Caption = 'Form1'
  ClientHeight = 358
  ClientWidth = 723
  LCLVersion = '6.5'
  object Panel1: TPanel
    Left = 0
    Height = 358
    Top = 0
    Width = 210
    Align = alLeft
    Caption = 'Panel1'
    ClientHeight = 358
    ClientWidth = 210
    TabOrder = 0
    object Button1: TButton
      Left = 16
      Height = 25
      Top = 72
      Width = 75
      Caption = 'Button1'
      OnClick = Button1Click
      TabOrder = 0
    end
  end
  object Memo1: TMemo
    Left = 210
    Height = 358
    Top = 0
    Width = 513
    Align = alClient
    Lines.Strings = (
      'Memo1'
    )
    TabOrder = 1
  end
end
