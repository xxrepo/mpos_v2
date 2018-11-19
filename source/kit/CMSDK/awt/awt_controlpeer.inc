

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
    ['{D89E2EB1-E7CF-4B6B-B172-D25D755E2C44}']
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
    procedure AddMouseListener(l: IMouseListener);
    procedure RemoveMouseListener(l: IMouseListener);
    function GetMouseListeners: TMouseListenerList;
  end;

  IAGraphicControlPeer = interface(IAControlPeer)
    ['{6245A0A6-5DAA-4B97-A3BB-BB24C97F88AB}']
    //property Canvas: TCanvas read FCanvas;
  end;

  IAWinControlPeer = interface(IAControlPeer)
    ['{5E8453F9-0B30-4ECC-B465-B682826E392F}']
    function GetBorderStyle: TBorderStyle;
    function GetControl(AIndex: Integer): TAControl;
    function GetControlCount: Integer;
    function GetShowing: Boolean;
    function GetTabOrder: TTabOrder;
    procedure SetBorderStyle(AValue: TBorderStyle);
    procedure SetTabOrder(AValue: TTabOrder);
    //
    procedure InsertControl(AControl: TAControl);
    procedure RemoveControl(AControl: TAControl);
    function CanFocus: Boolean;
    function CanSetFocus: Boolean;
    function Focused: Boolean;
    procedure SetFocus;
    //
    procedure AddWinControlListener(l: IWinControlListener);
    procedure RemoveWinControlListener(l: IWinControlListener);
    function GetWinControlListeners: TWinControlListenerList;
    procedure AddKeyListener(l: IKeyListener);
    procedure RemoveKeyListener(l: IKeyListener);
    function GetKeyListeners: TKeyListenerList;
  end;

  IACustomControlPeer = interface(IAWinControlPeer)
    ['{0B94070A-4213-45E4-BF07-86824295F02E}']
    function GetCanvas: TACanvas;
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
    ['{683F4DD2-AAAF-4D53-930A-8D237AC6DF54}']
    procedure Clear;
    procedure SelectAll;
    function GetMaxLength: Integer;
    function GetNumbersOnly: Boolean;
    function GetPasswordChar: Char;
    function GetReadOnly: Boolean;
    function GetSelLength: integer;
    function GetSelStart: integer;
    function GetSelText: String;
    procedure SetMaxLength(AValue: Integer);
    procedure SetNumbersOnly(AValue: Boolean);
    procedure SetPasswordChar(AValue: Char);
    procedure SetReadOnly(AValue: Boolean);
    procedure SetSelLength(AValue: integer);
    procedure SetSelStart(AValue: integer);
    procedure SetSelText(AValue: String);
    //
    procedure AddEditListener(l: IEditListener);
    procedure RemoveEditListener(l: IEditListener);
    function GetEditListeners: TEditListenerList;
  end;

  IAMemoPeer = interface(IAEditPeer)
    ['{4A02B20C-73D0-433D-9A67-9C72D09FF50B}']
    function GetLines: TStrings;
    function GetScrollBars: TScrollStyle;
    procedure SetLines(AValue: TStrings);
    procedure SetScrollBars(AValue: TScrollStyle);
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

  IADateTimePickerPeer = interface(IAWinControlPeer)
    ['{4CBDC79F-746D-4809-B247-2D89B3D1C5B1}']
    function GetDate: TDate;
    function GetDateTime: TDateTime;
    function GetMaxDate: TDate;
    function GetMinDate: TDate;
    function GetTime: TTime;
    procedure SetDate(AValue: TDate);
    procedure SetDateTime(AValue: TDateTime);
    procedure SetMaxDate(AValue: TDate);
    procedure SetMinDate(AValue: TDate);
    procedure SetTime(AValue: TTime);
  end;





