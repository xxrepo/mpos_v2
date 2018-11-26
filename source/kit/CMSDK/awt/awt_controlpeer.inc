

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
    ['{98DA4F74-E24A-4167-86BF-08CF841A4E7C}']
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
    //
    function GetParent: TAWinControl;
    procedure SetParent(AValue: TAWinControl);
    //
    procedure Click;
    procedure DblClick;
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

  IAListBoxPeer = interface(IAWinControlPeer)
    ['{199CECD4-7ED2-4E84-B1AC-AED8CEB824E9}']
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
    procedure Clear;
    function GetSelectedText: string;
    function ItemRect(Index: Integer): TRect;
    procedure AddListBoxListener(l: IListBoxListener);
    procedure RemoveListBoxListener(l: IListBoxListener);
    function GetListBoxListeners: TListBoxListenerList;
  end;

  IAComboBoxPeer = interface(IAWinControlPeer)
    ['{06106264-E9AD-48ED-B8AD-303CDE2B1854}']
    function GetMaxLength: Integer;
    function GetSorted: Boolean;
    procedure SetMaxLength(AValue: Integer);
    procedure SetSorted(AValue: Boolean);
    procedure AddComboBoxListener(l: IComboBoxListener);
    procedure RemoveComboBoxListener(l: IComboBoxListener);
    function GetComboBoxListeners: TComboBoxListenerList;
    //
    function GetCanvas: TACanvas;
    function GetDropDownCount: Integer;
    function GetItemIndex: Integer;
    function GetItems: TStrings;
    function GetSelLength: Integer;
    function GetSelStart: Integer;
    function GetSelText: string;
    function GetStyle: TComboBoxStyle;
    procedure SetDropDownCount(AValue: Integer);
    procedure SetItemIndex(AValue: Integer);
    procedure SetItems(AValue: TStrings);
    procedure SetSelLength(AValue: Integer);
    procedure SetSelStart(AValue: Integer);
    procedure SetSelText(AValue: string);
    procedure SetStyle(AValue: TComboBoxStyle);
    //
    procedure Clear;
    procedure SelectAll;
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

  IACustomEditPeer = interface(IAWinControlPeer)
    ['{F7A19E1B-6D51-418E-806F-F4FD6F24B0AE}']
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

  IAEditPeer = interface(IACustomEditPeer)
    ['{2CEAB4FD-826C-43D1-B195-8FE16EC0CE25}']
  end;

  IAMemoPeer = interface(IACustomEditPeer)
    ['{4A213FBE-DC13-4B85-B718-97C1309B3F8C}']
    function GetLines: TStrings;
    function GetScrollBars: TScrollStyle;
    procedure SetLines(AValue: TStrings);
    procedure SetScrollBars(AValue: TScrollStyle);
  end;

  IAFormPeer = interface(IACustomControlPeer)
    ['{4970C5E8-A665-4553-8E83-FAC3D0A52643}']
    function GetFormBorderStyle: TFormBorderStyle;
    procedure SetFormBorderStyle(AValue: TFormBorderStyle);
    procedure Close;
    function ShowModal: Integer;
    procedure AddFormListener(l: IFormListener);
    procedure RemoveFormListener(l: IFormListener);
    function GetFormListeners: TFormListenerList;
  end;

  IAFramePeer = interface(IACustomControlPeer)
    ['{ECAF3794-1637-46DB-9231-B0DE1A79CF70}']
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

  IACustomGridPeer = interface(IACustomControlPeer)
    ['{3AA80C10-953C-4B0F-B6E5-4557DCEC0913}']
    function GetBorderColor: TColor;
    function GetCol: Integer;
    function GetColCount: Integer;
    function GetColWidths(ACol: Integer): Integer;
    function GetDefColWidth: Integer;
    function GetDefRowHeight: Integer;
    function GetFixedColor: TColor;
    function GetFixedCols: Integer;
    function GetFixedRows: Integer;
    function GetGridBorderStyle: TBorderStyle;
    function GetOptions: TGridOptions;
    function GetRow: Integer;
    function GetRowCount: Integer;
    function GetRowHeights(ARow: Integer): Integer;
    function GetScrollBars: TScrollStyle;
    function GetTitleFont: TAFont;
    procedure SetBorderColor(AValue: TColor);
    procedure SetCol(AValue: Integer);
    procedure SetColCount(AValue: Integer);
    procedure SetColWidths(ACol: Integer; AValue: Integer);
    procedure SetDefColWidth(AValue: Integer);
    procedure SetDefRowHeight(AValue: Integer);
    procedure SetFixedcolor(AValue: TColor);
    procedure SetFixedCols(AValue: Integer);
    procedure SetFixedRows(AValue: Integer);
    procedure SetGridBorderStyle(AValue: TBorderStyle);
    procedure SetOptions(AValue: TGridOptions);
    procedure SetRow(AValue: Integer);
    procedure SetRowCount(AValue: Integer);
    procedure SetRowHeights(aRow: Integer; AValue: Integer);
    procedure SetScrollBars(AValue: TScrollStyle);
    procedure SetTitleFont(AValue: TAFont);
    //
    procedure BeginUpdate;
    function  CellRect(ACol, ARow: Integer): TRect;
    procedure Clear;
    procedure EndUpdate(ARefresh: Boolean=True);
    procedure InvalidateCell(ACol, ARow: Integer);
    procedure InvalidateCol(ACol: Integer);
    procedure InvalidateRow(ARow: Integer);
    // ???
    //procedure AddGridListener(l: IGridListener);
    //procedure RemoveGridListener(l: IGridListener);
    //function GetGridListeners: TGridListenerList;
  end;

  IACustomDrawGridPeer = interface(IACustomGridPeer)
    ['{A6430878-8297-4FAC-BE74-D8C6CA3A169A}']
    procedure AddDrawGridListener(l: IDrawGridListener);
    procedure RemoveDrawGridListener(l: IDrawGridListener);
    function GetDrawGridListeners: TDrawGridListenerList;
  end;

  IAStringGridPeer = interface(IACustomDrawGridPeer)
    ['{B71FEAD1-9A0A-4650-8277-ADB9FF98FEB2}']
    function GetCells(ACol, ARow: Integer): string;
    function GetCols(Index: Integer): TStrings;
    function GetRows(Index: Integer): TStrings;
    procedure SetCells(ACol, ARow: Integer; AValue: string);
    procedure SetCols(Index: Integer; AValue: TStrings);
    procedure SetRows(Index: Integer; AValue: TStrings);
    procedure AutoSizeColumn(ACol: Integer);
    procedure AutoSizeColumns;
  end;



