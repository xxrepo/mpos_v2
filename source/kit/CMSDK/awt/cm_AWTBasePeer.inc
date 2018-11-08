

  { IAPeer
    // The peer interface for . This is the top level peer interface for widgets and defines the
    // bulk of methods for AWT component peers. Most component peers have to implement this
    // interface (via one of the subinterfaces).
    // <br/>
    // The peer interfaces are intended only for use in porting the AWT. They are not intended for
    // use by application developers, and developers should not implement peers nor invoke any of
    // the peer methods directly on the peer instances.
  }

  IAPeer = interface
    ['{1ED8E4BF-2896-4971-8485-FA93466075FD}']
    function GetDelegate: TObject;
  end;

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

  IACanvasPeer = interface(IAPeer)
    ['{19E1D2D4-25BD-417F-8179-D626BFF43B7D}']
    procedure TextOut(X,Y: Integer; const Text: string);
  end;



