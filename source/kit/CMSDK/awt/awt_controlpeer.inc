

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
    function GetAlign: TAlign;
    function GetAutoSize: Boolean;
    function GetBorderSpacing: TAControlBorderSpacing;
    function GetBoundsRect: TRect;
    function GetColor: TColor;
    function GetEnabled: Boolean;
    function GetFont: TAFont;
    function GetHeight: Integer;
    function GetLeft: Integer;
    function GetText: TCaption;
    function GetTop: Integer;
    function GetVisible: Boolean;
    function GetWidth: Integer;
    procedure SetAlign(AValue: TAlign);
    procedure SetAutoSize(AValue: Boolean);
    procedure SetBorderSpacing(AValue: TAControlBorderSpacing);
    procedure SetBoundsRect(AValue: TRect);
    procedure SetColor(AValue: TColor);
    procedure SetEnabled(AValue: Boolean);
    procedure SetFont(AValue: TAFont);
    procedure SetHeight(AValue: Integer);
    procedure SetLeft(AValue: Integer);
    procedure SetText(AValue: TCaption);
    procedure SetTop(AValue: Integer);
    procedure SetVisible(AValue: Boolean);
    procedure SetWidth(AValue: Integer);
    //
    function GetParent: TAWinControl;
    procedure SetParent(AValue: TAWinControl);
    //
    procedure AdjustSize;
    procedure InvalidatePreferredSize;
    procedure BringToFront;
    procedure Hide;
    procedure Invalidate;
    procedure SendToBack;
    procedure Show;
    procedure Update;
    //
    procedure AddControlListener(l: IControlListener);
    procedure RemoveControlListener(l: IControlListener);
    function GetControlListeners: TControlListenerList;
  end;

  IAGraphicControlPeer = interface(IAControlPeer)
    ['{6245A0A6-5DAA-4B97-A3BB-BB24C97F88AB}']
    //property Canvas: TCanvas read FCanvas;
  end;

  IAWinControlPeer = interface(IAControlPeer)
    ['{5E8453F9-0B30-4ECC-B465-B682826E392F}']
    function GetControl(AIndex: Integer): TAControl;
    function GetControlCount: Integer;
    function GetShowing: Boolean;
    function GetTabOrder: TTabOrder;
    procedure SetTabOrder(AValue: TTabOrder);
    procedure InsertControl(AControl: TAControl);
    procedure RemoveControl(AControl: TAControl);
    //
    function CanFocus: Boolean;
    function CanSetFocus: Boolean;
    function Focused: Boolean;
    procedure SetFocus;
    procedure AddWinControlListener(l: IWinControlListener);
    procedure RemoveWinControlListener(l: IWinControlListener);
    function GetWinControlListeners: TWinControlListenerList;
    procedure AddKeyListener(l: IKeyListener);
    procedure RemoveKeyListener(l: IKeyListener);
    function GetKeyListeners: TKeyListenerList;
  end;

  IACustomControlPeer = interface(IAWinControlPeer)
    ['{59304415-85DA-463B-9335-625787C6DEC8}']
    function GetBorderStyle: TBorderStyle;
    function GetCanvas: TACanvas;
    procedure SetBorderStyle(AValue: TBorderStyle);
    procedure SetCanvas(AValue: TACanvas);
    procedure AddCustomControlListener(l: ICustomControlListener);
    procedure RemoveCustomControlListener(l: ICustomControlListener);
    function GetCustomControlListeners: TCustomControlListenerList;
  end;

  IALabelPeer = interface(IAGraphicControlPeer)
    ['{234D8271-1395-4DC3-B018-B783E0F4A76F}']
    function GetAlignment: TAlignment;
    function GetLayout: TTextLayout;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetLayout(AValue: TTextLayout);
  end;

  IAPanelPeer = interface(IACustomControlPeer)
    ['{4BA1BB04-559F-46AF-BD05-D5CB2D2DA227}']
    function GetAlignment: TAlignment;
    function GetBevelColor: TColor;
    function GetBevelInner: TPanelBevel;
    function GetBevelOuter: TPanelBevel;
    function GetBevelWidth: TBevelWidth;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetBevelColor(AValue: TColor);
    procedure SetBevelInner(AValue: TPanelBevel);
    procedure SetBevelOuter(AValue: TPanelBevel);
    procedure SetBevelWidth(AValue: TBevelWidth);
  end;

  IAEditPeer = interface(IAWinControlPeer)
    ['{3A9FCF75-9370-4F05-B737-B5A08266C40F}']
    procedure Clear;
    procedure SelectAll;
  end;

  IAFormPeer = interface(IAWinControlPeer)
    ['{C5BAADBB-C30B-4EE5-BA3B-53200EFC0729}']
    function GetFormBorderStyle: TFormBorderStyle;
    procedure SetFormBorderStyle(AValue: TFormBorderStyle);
    procedure Close;
    function ShowModal: Integer;
    procedure AddFormListener(l: IFormListener);
    procedure RemoveFormListener(l: IFormListener);
    function GetFormListeners: TFormListenerList;
  end;



