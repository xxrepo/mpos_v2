

  IAFontPeer = interface(IAPeer)
    ['{3F71ACAE-FB45-4D03-B19D-11D9F9904F28}']
    function GetColor: TAColor;
    function GetHeight: Integer;
    function GetName: string;
    function GetSize: Integer;
    procedure SetColor(AValue: TAColor);
    procedure SetHeight(AValue: Integer);
    procedure SetName(AValue: string);
    procedure SetSize(AValue: Integer);
  end;

  IAGraphicPeer = interface(IAPeer)
    ['{D7F5DC6F-F764-4FA6-B593-FA9A99D07B94}']
    function GetEmpty: Boolean;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(AValue: Integer);
    procedure SetWidth(AValue: Integer);
    procedure Clear;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream);
  end;

  IARasterImagePeer = interface(IAGraphicPeer)
    ['{44C0847A-2F28-4132-898F-C690AA5FF40F}']
    function GetCanvas: TACanvas;
    procedure FreeImage;
  end;

  IACustomBitmapPeer = interface(IARasterImagePeer)
    ['{CF38C118-3B93-40CA-B7E1-BFCEC82019B6}']
    function GetMonochrome: Boolean;
    procedure SetMonochrome(AValue: Boolean);
    procedure SetSize(AWidth, AHeight: Integer);
  end;

  {  ---- 这一部分通过快捷实现 ----
  IABitmapPeer = interface(IACustomBitmapPeer)
  end;

  IAJPEGImagePeer = interface(IACustomBitmapPeer)
  end;

  IAGIFImagePeer = interface(IACustomBitmapPeer)
  end;

  IAPortableNetworkGraphicPeer = interface(IACustomBitmapPeer)
  end;
  }

  IABrushPeer = interface(IAPeer)
    ['{23F27BBB-4BA5-45D3-84BF-6E050819EF21}']
    function GetBitmap: TACustomBitmap;
    function GetColor: TAColor;
    procedure SetBitmap(AValue: TACustomBitmap);
    procedure SetColor(AValue: TAColor);
  end;

  IACanvasPeer = interface(IAPeer)
    ['{19E1D2D4-25BD-417F-8179-D626BFF43B7D}']
    function GetBrush: TABrush;
    function GetFont: TAFont;
    procedure SetBrush(AValue: TABrush);
    procedure SetFont(AValue: TAFont);
    procedure FillRect(X1,Y1,X2,Y2: Integer);
    procedure TextOut(X,Y: Integer; const Text: string);
  end;


