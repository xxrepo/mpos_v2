{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_AWT

    This is not a complete unit, for testing

    Abstract Window Toolkit
    依赖于具体解决方案，否则抛出异常。
    不建议不分次编译中操作同一对象，你可能遇到类型操作失误的状况。
    建议在使用时捕获相应的异常。

    这一部分都是线程不安全的。

 **********************************************************************}

unit cm_AWT;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces, cm_freegenerics;

type

  { EAWTException }

  EAWTException = class(Exception);

  {$i awt_type.inc}
  {$i awt_base.inc}
  {$i awt_graphicspeer.inc}

  { TAObject }

  TAObject = class abstract(TCMBasePersistent)
  protected
    FPeer: IAPeer;
  public
    constructor Create;
    constructor Create(APeer: IAPeer); overload;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetPeer: IAPeer;
  end;

  { TAFont }

  TAFont = class(TAObject)
  private
    function GetColor: TColor;
    function GetHeight: Integer;
    function GetName: string;
    function GetSize: Integer;
    procedure SetColor(AValue: TColor);
    procedure SetHeight(AValue: Integer);
    procedure SetName(AValue: string);
    procedure SetSize(AValue: Integer);
  public
    constructor Create;
    constructor Create(APeer: IAFontPeer); overload;
    function GetPeer: IAFontPeer;
  public
    property Color: TColor read GetColor write SetColor;
    property Height: Integer read GetHeight write SetHeight;
    property Name: string read GetName write SetName;
    property Size: Integer read GetSize write SetSize;
  end;

  { TAGraphic }

  TAGraphic = class abstract(TAObject)
  private
    function GetEmpty: Boolean;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(AValue: Integer);
    procedure SetWidth(AValue: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    function GetPeer: IAGraphicPeer;
  public
    procedure Clear;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream);
    property Empty: Boolean read GetEmpty;
    property Height: Integer read GetHeight write SetHeight;
    property Width: Integer read GetWidth write SetWidth;
  end;

  { TARasterImage }

  TARasterImage = class abstract(TAGraphic)
  private
    function GetCanvas: TACanvas;
  public
    function GetPeer: IARasterImagePeer;
  public
    procedure FreeImage;
    property Canvas: TACanvas read GetCanvas;
  end;

  { TACustomBitmap }

  TACustomBitmap = class(TARasterImage)
  private
    function GetMonochrome: Boolean;
    procedure SetMonochrome(AValue: Boolean);
  public
    // 这里映射的是一个抽象类，这个构造方法仅用于取出映射对象多态属性。
    constructor Create(APeer: IACustomBitmapPeer);
    function GetPeer: IACustomBitmapPeer;
  public
    procedure SetSize(AWidth, AHeight: Integer);
    property Monochrome: Boolean read GetMonochrome write SetMonochrome;
  end;

  { TABitmap }

  TABitmap = class(TACustomBitmap)
  public
    constructor Create;
  end;

  { TAJPEGImage }

  TAJPEGImage = class(TACustomBitmap)
  public
    constructor Create;
  end;

  { TAGIFImage }

  TAGIFImage = class(TACustomBitmap)
  public
    constructor Create;
  end;

  { TAPortableNetworkGraphic }

  TAPortableNetworkGraphic = class(TACustomBitmap)
  public
    constructor Create;
  end;

  { TABrush }

  TABrush = class(TAObject)
  private
    function GetBitmap: TACustomBitmap;
    function GetColor: TColor;
    procedure SetBitmap(AValue: TACustomBitmap);
    procedure SetColor(AValue: TColor);
  public
    constructor Create(APeer: IABrushPeer);
    function GetPeer: IABrushPeer;
  public
    property Bitmap: TACustomBitmap read GetBitmap write SetBitmap;
    property Color: TColor read GetColor write SetColor;
  end;

  { TACanvas }

  TACanvas = class(TAObject)
  private
    function GetBrush: TABrush;
    function GetFont: TAFont;
    procedure SetBrush(AValue: TABrush);
    procedure SetFont(AValue: TAFont);
  public
    constructor Create;
    constructor Create(APeer: IACanvasPeer); overload;
    function GetPeer: IACanvasPeer;
  public
    procedure FillRect(X1,Y1,X2,Y2: Integer);
    procedure TextOut(X,Y: Integer; const Text: string);
    property Brush: TABrush read GetBrush write SetBrush;
    property Font: TAFont read GetFont write SetFont;
  end;

  {$i awt_event.inc}
  {$i awt_controlpeer.inc}

  { TAControlBorderSpacing }

  TAControlBorderSpacing = class(TAObject)
  private
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
  public
    constructor Create(OwnerControl: TAControl);
    constructor Create(APeer: IAControlBorderSpacingPeer); overload;
    function GetPeer: IAControlBorderSpacingPeer;
  public
    property Left: Integer read GetLeft write SetLeft;
    property Top: Integer read GetTop write SetTop;
    property Right: Integer read GetRight write SetRight;
    property Bottom: Integer read GetBottom write SetBottom;
    property Around: Integer read GetAround write SetAround;
  end;

  { TAComponent }

  TAComponent = class abstract(TAObject)
  private
    FOwner: TAComponent;
    FComponents: TFPList; // TODO 是否应该如 WinControl controls 一样代理？
    function GetComponent(AIndex: Integer): TAComponent;
    function GetComponentCount: Integer;
  public
    constructor Create(AOwner: TAComponent); virtual;
    destructor Destroy; override;
    procedure DestroyComponents;
    procedure InsertComponent(AComponent: TAComponent);
    procedure RemoveComponent(AComponent: TAComponent);
    property Components[AIndex: Integer]: TAComponent read GetComponent;
    property ComponentCount: Integer read GetComponentCount;
    property Owner: TAComponent read FOwner;
    function GetPeer: IAComponentPeer;
  private
    function GetName: TComponentName;
    procedure SetName(AValue: TComponentName);
    function GetTag: PtrInt;
    procedure SetTag(AValue: PtrInt);
  public
    property Name: TComponentName read GetName write SetName;
    property Tag: PtrInt read GetTag write SetTag;
  end;

  { TAControl }

  TAControl = class abstract(TAComponent)
  public
    function GetPeer: IAControlPeer;
  private
    function GetParent: TAWinControl;
    procedure SetParent(AValue: TAWinControl);
  private
    function GetAlign: TAlign;
    function GetAutoSize: Boolean;
    function GetBorderSpacing: TAControlBorderSpacing;
    function GetBoundsRect: TRect;
    function GetColor: TColor;
    function GetEnabled: Boolean;
    function GetFont: TAFont;
    function GetHeight: Integer;
    function GetHint: TCaption;
    function GetLeft: Integer;
    function GetMark: TCaption;
    function GetParentColor: Boolean;
    function GetParentFont: Boolean;
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
    procedure SetHint(AValue: TCaption);
    procedure SetLeft(AValue: Integer);
    procedure SetMark(AValue: TCaption);
    procedure SetParentColor(AValue: Boolean);
    procedure SetParentFont(AValue: Boolean);
    procedure SetText(AValue: TCaption);
    procedure SetTop(AValue: Integer);
    procedure SetVisible(AValue: Boolean);
    procedure SetWidth(AValue: Integer);
  protected //control 未公开
    procedure Click; virtual;
    procedure DblClick; virtual;
    property ParentColor: Boolean read GetParentColor write SetParentColor;
    property ParentFont: Boolean  read GetParentFont write SetParentFont;
    property Text: TCaption read GetText write SetText; //LCL 中 Text 和 Caption 内容一致
  public
    procedure AdjustSize;
    procedure InvalidatePreferredSize;
  public
    procedure BringToFront;
    procedure Hide;
    procedure Invalidate;
    procedure SendToBack;
    procedure Show;
    procedure Update;
  public
    property Align: TAlign read GetAlign write SetAlign;
    property AutoSize: Boolean read GetAutoSize write SetAutoSize;
    property BoundsRect: TRect read GetBoundsRect write SetBoundsRect;
    property BorderSpacing: TAControlBorderSpacing read GetBorderSpacing write SetBorderSpacing;
    property Caption: TCaption read GetText write SetText; //LCL 中 Text 和 Caption 内容一致
    property Color: TColor read GetColor write SetColor;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Font: TAFont read GetFont write SetFont;
  public
    property Left: Integer read GetLeft write SetLeft;
    property Height: Integer read GetHeight write SetHeight;
    property Hint: TCaption read GetHint write SetHint;
    property Top: Integer read GetTop write SetTop;
    property Width: Integer read GetWidth write SetWidth;
    property Parent: TAWinControl read GetParent write SetParent;
    property Visible: Boolean read GetVisible write SetVisible;
  public
    procedure AddControlListener(l: IControlListener);
    procedure RemoveControlListener(l: IControlListener);
    function GetControlListeners: TControlListenerList;
    procedure AddMouseListener(l: IMouseListener);
    procedure RemoveMouseListener(l: IMouseListener);
    function GetMouseListeners: TMouseListenerList;
  public // AWT 特有的
    property Mark: TCaption read GetMark write SetMark;
  end;

  TAGraphicControl = class abstract(TAControl)
  end;

  { TAWinControl }

  TAWinControl = class abstract(TAControl)
  private
    function GetBorderStyle: TBorderStyle;
    function GetControl(AIndex: Integer): TAControl;
    function GetControlCount: Integer;
    function GetShowing: Boolean;
    function GetTabOrder: TTabOrder;
    procedure SetBorderStyle(AValue: TBorderStyle);
    procedure SetTabOrder(AValue: TTabOrder);
  public
    function GetPeer: IAWinControlPeer;
  protected
    property BorderStyle: TBorderStyle read GetBorderStyle write SetBorderStyle; //start at TWinControl
  public
    procedure InsertControl(AControl: TAControl);
    procedure RemoveControl(AControl: TAControl);
    property ControlCount: Integer read GetControlCount;
    property Controls[AIndex: Integer]: TAControl read GetControl;
    property TabOrder: TTabOrder read GetTabOrder write SetTabOrder;
    property Showing: Boolean read GetShowing;
  public
    function CanFocus: Boolean;
    function CanSetFocus: Boolean;
    function Focused: Boolean;
    procedure SetFocus;
  public
    procedure AddWinControlListener(l: IWinControlListener);
    procedure RemoveWinControlListener(l: IWinControlListener);
    function GetWinControlListeners: TWinControlListenerList;
    procedure AddKeyListener(l: IKeyListener); //添加指定的按键侦听器，以接收发自此 WinControl 的按键事件。
    procedure RemoveKeyListener(l: IKeyListener);
    function GetKeyListeners: TKeyListenerList;
  end;

  { TACustomControl }

  TACustomControl = class abstract(TAWinControl)
  private
    function GetCanvas: TACanvas;
    procedure SetCanvas(AValue: TACanvas);
  public
    function GetPeer: IACustomControlPeer;
  public
    property BorderStyle;
    property Canvas: TACanvas read GetCanvas write SetCanvas;
  public
    procedure AddCustomControlListener(l: ICustomControlListener);
    procedure RemoveCustomControlListener(l: ICustomControlListener);
    function GetCustomControlListeners: TCustomControlListenerList;
  end;

  { TAListBox }

  TAListBox = class(TAWinControl)
  private
    function GetCanvas: TACanvas;
    function GetCount: Integer;
    function GetItemHeight: Integer;
    function GetItemIndex: Integer;
    function GetItems: TStrings;
    function GetSelected(Index: Integer): Boolean;
    function GetSorted: Boolean;
    procedure SetItemHeight(AValue: Integer);
    procedure SetItemIndex(AValue: Integer);
    procedure SetItems(AValue: TStrings);
    procedure SetSelected(Index: Integer; AValue: Boolean);
    procedure SetSorted(AValue: Boolean);
  public
    constructor Create(AOwner: TAComponent); override;
    function GetPeer: IAListBoxPeer;
  public
    procedure Click; override;
    procedure Clear;
    function GetSelectedText: string;
    function ItemRect(Index: Integer): TRect;
    property Canvas: TACanvas read GetCanvas;
    property Count: Integer read GetCount;
    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property Items: TStrings read GetItems write SetItems;
    property Selected[Index: Integer]: Boolean read GetSelected write SetSelected;
    property Sorted: Boolean read GetSorted write SetSorted;
  public
    procedure AddListBoxListener(l: IListBoxListener);
    procedure RemoveListBoxListener(l: IListBoxListener);
    function GetListBoxListeners: TListBoxListenerList;
  end;

  { TAComboBox }

  TAComboBox = class(TAWinControl)
  private
    function GetCanvas: TACanvas;
    function GetDropDownCount: Integer;
    function GetItemIndex: Integer;
    function GetItems: TStrings;
    function GetMaxLength: Integer;
    function GetSelLength: Integer;
    function GetSelStart: Integer;
    function GetSelText: string;
    function GetSorted: Boolean;
    function GetStyle: TComboBoxStyle;
    procedure SetDropDownCount(AValue: Integer);
    procedure SetItemIndex(AValue: Integer);
    procedure SetItems(AValue: TStrings);
    procedure SetMaxLength(AValue: Integer);
    procedure SetSelLength(AValue: Integer);
    procedure SetSelStart(AValue: Integer);
    procedure SetSelText(AValue: string);
    procedure SetSorted(AValue: Boolean);
    procedure SetStyle(AValue: TComboBoxStyle);
  public
    constructor Create(AOwner: TAComponent); override;
    function GetPeer: IAComboBoxPeer;
  public //CustomComboBox 未公开
    property MaxLength: Integer read GetMaxLength write SetMaxLength;
    property Sorted: Boolean read GetSorted write SetSorted;
  public //CustomComboBox 未公开
    procedure AddComboBoxListener(l: IComboBoxListener);
    procedure RemoveComboBoxListener(l: IComboBoxListener);
    function GetComboBoxListeners: TComboBoxListenerList;
  public
    procedure Clear;
    procedure SelectAll;
    property Canvas: TACanvas read GetCanvas;
    property DropDownCount: Integer read GetDropDownCount write SetDropDownCount;
    property Items: TStrings read GetItems write SetItems;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelText: string read GetSelText write SetSelText;
    property Style: TComboBoxStyle read GetStyle write SetStyle;
    property Text;
  end;

  { TALabel }

  TALabel = class(TAGraphicControl)
  private
    function GetAlignment: TAlignment;
    function GetLayout: TTextLayout;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetLayout(AValue: TTextLayout);
  public
    constructor Create(AOwner: TAComponent); override;
    function GetPeer: IALabelPeer;
  public
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property Layout: TTextLayout read GetLayout write SetLayout;
  end;

  { TAPanel }

  TAPanel = class(TACustomControl)
  private
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
  public
    constructor Create(AOwner: TAComponent); override;
    function GetPeer: IAPanelPeer;
  published
    property ParentColor;
    property ParentFont;
  public
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property BevelColor: TColor read GetBevelColor write SetBevelColor;
    property BevelInner: TPanelBevel read GetBevelInner write SetBevelInner;
    property BevelOuter: TPanelBevel read GetBevelOuter write SetBevelOuter;
    property BevelWidth: TBevelWidth read GetBevelWidth write SetBevelWidth;
  end;

  { TACustomEdit }

  TACustomEdit = class abstract(TAWinControl)
  private
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
  public
    function GetPeer: IACustomEditPeer;
  public
    procedure Clear;
    procedure SelectAll;
    property MaxLength: Integer read GetMaxLength write SetMaxLength;
    property NumbersOnly: Boolean read GetNumbersOnly write SetNumbersOnly;
    property PasswordChar: Char read GetPasswordChar write SetPasswordChar;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property SelLength: integer read GetSelLength write SetSelLength;
    property SelStart: integer read GetSelStart write SetSelStart;
    property SelText: String read GetSelText write SetSelText;
  public
    procedure AddEditListener(l: IEditListener);
    procedure RemoveEditListener(l: IEditListener);
    function GetEditListeners: TEditListenerList;
  end;

  { TAEdit }

  TAEdit = class(TACustomEdit)
  public
    constructor Create(AOwner: TAComponent); override;
    function GetPeer: IAEditPeer;
  published
    property ParentColor;
    property ParentFont;
    property Text;
  end;

  { TAMemo }

  TAMemo = class(TACustomEdit)
  private
    function GetLines: TStrings;
    function GetScrollBars: TScrollStyle;
    procedure SetLines(AValue: TStrings);
    procedure SetScrollBars(AValue: TScrollStyle);
  public
    constructor Create(AOwner: TAComponent); override;
    function GetPeer: IAMemoPeer;
  public
    property Lines: TStrings read GetLines write SetLines;
    property ScrollBars: TScrollStyle read GetScrollBars write SetScrollBars;
  published
    property ParentColor;
    property ParentFont;
    property Text;
  end;

  { TAForm }

  TAForm = class(TACustomControl)
  private
    function GetFormBorderStyle: TFormBorderStyle;
    procedure SetFormBorderStyle(AValue: TFormBorderStyle);
  public
    constructor Create(AOwner: TAComponent); override;
    function GetPeer: IAFormPeer;
  public
    property BorderStyle: TFormBorderStyle read GetFormBorderStyle write SetFormBorderStyle;
    procedure Close;
    function ShowModal: Integer;
  public
    procedure AddFormListener(l: IFormListener);
    procedure RemoveFormListener(l: IFormListener);
    function GetFormListeners: TFormListenerList;
  published
    property ParentColor;
    property ParentFont;
  end;

  { TAFrame }

  TAFrame = class(TACustomControl)
  public
    constructor Create(AOwner: TAComponent); override;
    function GetPeer: IAFramePeer;
  published
    property ParentColor;
    property ParentFont;
  end;

  {$i awt_extctrls.inc}
  {$i awt_toolkit.inc}

implementation

{ TAObject }

constructor TAObject.Create;
begin
  if not Assigned(TAWTManager.DefaultToolkit) then
    raise EAWTException.Create(NoToolKitError);
  FPeer := nil;
end;

constructor TAObject.Create(APeer: IAPeer);
begin
  if not Assigned(APeer) then
    raise EAWTException.Create(PeerNilError);
  FPeer := APeer;
end;

destructor TAObject.Destroy;
begin
  FPeer := nil;
  inherited Destroy;
end;

procedure TAObject.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source.UnitName = 'cm_AWT') and (Source.FieldAddress('GetPeer') <> nil) then
    TAObject(Source).FPeer := Self.FPeer;
end;

function TAObject.GetPeer: IAPeer;
begin
  Result := FPeer;
end;

{$i awt_impl.inc}


initialization
  TAWTManager.DefaultToolkit := nil;



end.

