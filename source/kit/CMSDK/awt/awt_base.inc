
type

  // 前置声明
  TAFont = class;
  TACustomBitmap = class;
  TABrush = class;
  TACanvas = class;
  TAControlBorderSpacing = class;
  TAControl = class;
  TAWinControl = class;
  TACustomControl = class;
  TAForm = class;
  IAToolkit = interface;

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
    function GetHashCode: PtrInt;
  end;



