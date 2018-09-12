object Form1: TForm1
  Left = 341
  Height = 402
  Top = 186
  Width = 750
  Caption = 'Form1'
  ClientHeight = 402
  ClientWidth = 750
  OnCreate = FormCreate
  LCLVersion = '6.5'
  object BarcodeQR1: TBarcodeQR
    Left = 0
    Height = 176
    Top = 8
    Width = 176
    StrictSize = True
    Text = '123'
  end
  object Image1: TImage
    Left = 184
    Height = 178
    Top = 8
    Width = 170
  end
  object Button1: TButton
    Left = 376
    Height = 25
    Top = 16
    Width = 75
    Caption = 'Button1'
    OnClick = Button1Click
    TabOrder = 0
  end
  object Button2: TButton
    Left = 376
    Height = 25
    Top = 48
    Width = 75
    Caption = 'Button2'
    OnClick = Button2Click
    TabOrder = 1
  end
  object Button3: TButton
    Left = 376
    Height = 25
    Top = 80
    Width = 75
    Caption = 'Button3'
    OnClick = Button3Click
    TabOrder = 2
  end
  object Panel1: TPanel
    Left = 56
    Height = 150
    Top = 208
    Width = 150
    Caption = 'Panel1'
    Color = clMoneyGreen
    ParentColor = False
    TabOrder = 3
  end
  object Button4: TButton
    Left = 464
    Height = 25
    Top = 48
    Width = 75
    Caption = 'Button4'
    OnClick = Button4Click
    TabOrder = 4
  end
  object Image2: TImage
    Left = 272
    Height = 90
    Top = 208
    Width = 90
  end
  object frReport1: TfrReport
    InitialZoom = pzDefault
    Options = []
    PreviewButtons = [pbZoom, pbLoad, pbSave, pbPrint, pbFind, pbHelp, pbExit]
    DataType = dtDataSet
    Left = 416
    Top = 208
  end
end
