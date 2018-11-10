

  IAControlBorderSpacingPeer = interface(IAPeer)
    ['{CD41CE07-9DBA-4785-8FF5-E05D716ACEF6}']
    function GetAround: Integer;
    function GetBottom: Integer;
    function GetLeft: Integer;
    function GetRight: Integer;
    function GetTop: Integer;
    procedure SetAround(AValue: Integer);
    procedure SetBottom(AValue: Integer);
    procedure SetLeft(AValue: Integer);
    procedure SetRight(AValue: Integer);
    procedure SetTop(AValue: Integer);
  end;

  IAComponentPeer = interface(IAPeer)
    ['{F5955633-8025-47AE-87EB-A53E240AC20C}']
    function GetName: TComponentName;
    procedure SetName(AValue: TComponentName);
    function GetTag: PtrInt;
    procedure SetTag(AValue: PtrInt);
  end;

  IAControlPeer = interface(IAComponentPeer)
    ['{E1192DCA-5D61-4D56-AB24-0D3CA216FE54}']
    function GetAlign: TAAlign;
    function GetBorderSpacing: TAControlBorderSpacing;
    function GetBoundsRect: TRect;
    function GetColor: TAColor;
    function GetEnabled: Boolean;
    function GetFont: TAFont;
    function GetHeight: Integer;
    function GetLeft: Integer;
    function GetText: TACaption;
    function GetTop: Integer;
    function GetWidth: Integer;
    procedure SetAlign(AValue: TAAlign);
    procedure SetBorderSpacing(AValue: TAControlBorderSpacing);
    procedure SetBoundsRect(AValue: TRect);
    procedure SetColor(AValue: TAColor);
    procedure SetEnabled(AValue: Boolean);
    procedure SetFont(AValue: TAFont);
    procedure SetHeight(AValue: Integer);
    procedure SetLeft(AValue: Integer);
    procedure SetText(AValue: TACaption);
    procedure SetTop(AValue: Integer);
    procedure SetWidth(AValue: Integer);
    //
    function GetParent: TAWinControl;
    procedure SetParent(AValue: TAWinControl);
  end;

  IAGraphicControlPeer = interface(IAControlPeer)
    ['{6245A0A6-5DAA-4B97-A3BB-BB24C97F88AB}']
    //property Canvas: TCanvas read FCanvas;
  end;

  IAWinControlPeer = interface(IAControlPeer)
    ['{F34E9CA5-AFE6-4AA0-8D35-FD81DC1A6956}']
    function GetControl(AIndex: Integer): TAControl;
    function GetControlCount: Integer;
    procedure InsertControl(AControl: TAControl);
    procedure RemoveControl(AControl: TAControl);
    property ControlCount: Integer read GetControlCount;
    //
    function CanFocus: Boolean;
    function CanSetFocus: Boolean;
    procedure SetFocus;
    procedure AddKeyListener(l: IKeyListener);
  end;

  IACustomControlPeer = interface(IAWinControlPeer)
    ['{78B50173-8CA7-4176-A6BD-404570BDD6DE}']
    function GetBorderStyle: TABorderStyle;
    function GetCanvas: TACanvas;
    procedure SetBorderStyle(AValue: TABorderStyle);
    procedure SetCanvas(AValue: TACanvas);
    //property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

  IALabelPeer = interface(IAGraphicControlPeer)
    ['{234D8271-1395-4DC3-B018-B783E0F4A76F}']
  end;

  IAPanelPeer = interface(IACustomControlPeer)
    ['{4BA1BB04-559F-46AF-BD05-D5CB2D2DA227}']

  end;

  IAEditPeer = interface(IAWinControlPeer)
    ['{3A9FCF75-9370-4F05-B737-B5A08266C40F}']
    procedure Clear;
    procedure SelectAll;
  end;

  IAFormPeer = interface(IAWinControlPeer)
    ['{5A8A620E-272E-4724-B339-282802CE878E}']
    function GetFormBorderStyle: TAFormBorderStyle;
    procedure SetFormBorderStyle(AValue: TAFormBorderStyle);
    function ShowModal: Integer;
  end;



