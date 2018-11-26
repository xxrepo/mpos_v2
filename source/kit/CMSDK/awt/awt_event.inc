
type

  {-----------------------------------------------------------

       LCL 的事件模式是对类指定回调函数地址，这可能在不同编译中存在类型强制转换、
   代码把控不严等而造成内存地址错乱的问题。同时其模式是耦合在一起的，且只能指定一个回调函数。
   这里抛弃了 LCL 的这一模式。

  -----------------------------------------------------------}

  { IControlEvent }

  IControlEvent = interface(ICMEvent)
    ['{50ABB64A-498C-4364-9E55-F4B5EE6860F0}']
    function GetAControl: TAControl;
  end;

  IControlListener = interface(ICMListener)
    ['{9AB1F294-33E8-43F1-800F-D3F3D2452713}']
    procedure ControlClick(e: IControlEvent);
    procedure ControlDblClick(e: IControlEvent); //Control 未公开，在公开的子类中使用。
    procedure ControlResize(e: IControlEvent);
  end;

  TControlListenerList = specialize TFGInterfaceList<IControlListener>;

  { IWinControlEvent }

  IWinControlEvent = interface(IControlEvent)
    ['{50ABB64A-498C-4364-9E55-F4B5EE6860F0}']
    function GetAWinControl: TAWinControl;
  end;

  IWinControlListener = interface(IControlListener)
    ['{D44601BB-6A97-4C39-8545-182B27224DAE}']
    procedure ControlEnter(e: IWinControlEvent);
    procedure ControlExit(e: IWinControlEvent);
  end;

  TWinControlListenerList = specialize TFGInterfaceList<IWinControlListener>;

  { IEditEvent }

  IEditEvent = interface(IWinControlEvent)
    ['{EF1F1598-848E-4727-8A92-F002D4AA1F5F}']
    function GetAEdit: TACustomEdit;
  end;

  IEditListener = interface(IWinControlListener)
    ['{9C805BD9-620F-44E4-B208-A6F59D15797D}']
    procedure EditChanged(e: IEditEvent);
  end;

  TEditListenerList = specialize TFGInterfaceList<IEditListener>;

  { ICustomControlEvent }

  ICustomControlEvent = interface(IWinControlEvent)
    ['{C633D6A0-484D-48A4-BA6C-9A968EB336D8}']
    function GetACustomControl: TACustomControl;
  end;

  ICustomControlListener = interface(IWinControlListener)
    ['{E27DCA5F-6472-486D-AB59-8339AECE3CCE}']
    procedure ControlPaint(e: ICustomControlEvent);
  end;

  TCustomControlListenerList = specialize TFGInterfaceList<ICustomControlListener>;

  { IListBoxEvent }

  IListBoxEvent = interface(IWinControlEvent)
    ['{155E7F11-B262-4404-B3A8-27B06D0504B8}']
    function GetAListBox: TAListBox;
  end;

  IListBoxListener = interface(IWinControlListener)
    ['{E27DCA5F-6472-486D-AB59-8339AECE3CCE}']
    procedure ListBoxSelectionChange(e: IListBoxEvent);
  end;

  TListBoxListenerList = specialize TFGInterfaceList<IListBoxListener>;

  { IComboBoxEvent }

  IComboBoxEvent = interface(IWinControlEvent)
    ['{37DB41FB-359D-4EE4-BCE2-67247ED2F191}']
    function GetAComboBox: TAComboBox;
  end;

  IComboBoxListener = interface(IWinControlListener)
    ['{FC082D0A-A67E-4869-BC50-C37318AB1FB5}']
    procedure ComboBoxChange(e: IComboBoxEvent);
    procedure ComboBoxDropDown(e: IComboBoxEvent);
    procedure ComboBoxSelect(e: IComboBoxEvent);
  end;

  TComboBoxListenerList = specialize TFGInterfaceList<IComboBoxListener>;

  { IFormEvent }

  IFormEvent = interface(ICustomControlEvent)
    ['{9BF85BE1-7BB4-4A08-AAE2-3C8C16B878C5}']
    function GetAForm: TAForm;
  end;

  IFormListener = interface(ICustomControlListener)
    ['{7F6E577F-6FF0-47CA-AB16-8DAD96DFCB6F}']
    procedure FormActivate(e: IFormEvent);
    procedure FormClose(e: IFormEvent);
    procedure FormDeactivate(e: IFormEvent);
    procedure FormHide(e: IFormEvent);
    procedure FormShow(e: IFormEvent);
  end;

  TFormListenerList = specialize TFGInterfaceList<IFormListener>;

  { IGridEvent }

  IGridEvent = interface(ICustomControlEvent)
    ['{360A315C-4D82-4D4B-A97C-E4C4632563FD}']
    function GetAGrid: TACustomGrid;
    function GetCol: Integer;
    function GetRow: Integer;
  end;

  ICustomGridListener = interface(ICustomControlListener)
    ['{A3FA1B1C-0AB6-42AD-914F-75E6DEF342CC}']
    procedure GridDrawCell(e: IGridEvent);
    procedure GridSelection(e: IGridEvent);
  end;

  IDrawGridListener = interface(ICustomGridListener)
    ['{09A21DCA-9082-46CC-A673-78FB5592A08A}']
    procedure GridSelectCell(e: IGridEvent);
  end;

  TDrawGridListenerList = specialize TFGInterfaceList<IDrawGridListener>;

  // --------------------------------- 以上是控件的 -----------------------------------------

  { IKeyEvent
    // 表示组件中发生键击的事件。
  }

  IKeyEvent = interface(IControlEvent)
    ['{935AB8D2-7383-4B1E-933D-C0D8AA30F377}']
    function GetKeyChar: Char; //返回与此事件中的键关联的字符。
    function GetKeyCode: Word; //返回与此事件中的键关联的整数 keyCode。
    procedure SetKeyChar(AKeyChar: Char); //设置 keyCode 值，以表示某个逻辑字符。
    procedure SetKeyCode(AKeyCode: Word); //设置 keyCode 值，以表示某个物理键。
    function GetKeyText: string; //返回按键的描述。
    function GetShiftState: TShiftState;
  end;

  { IKeyListener
    //  用于接收键盘事件（击键）的侦听器接口。
    旨在处理键盘事件的类要么实现此接口（及其包含的所有方法），要么扩展抽象 KeyAdapter 类（仅重写有用的方法）。
    然后使用组件的 addKeyListener 方法将从该类所创建的侦听器对象向该组件注册。按下、释放或键入键时生成键盘事件。
    然后调用侦听器对象中的相关方法并将该 KeyEvent 传递给它。
  }

  IKeyListener = interface(ICMListener)
    ['{81FC98A1-681E-453D-BDCD-7F373453CAA9}']
    procedure KeyPressed(e: IKeyEvent); //按下某个键时调用此方法。
    procedure KeyReleased(e: IKeyEvent); //释放某个键时调用此方法。
    procedure KeyTyped(e: IKeyEvent); //键入某个键时调用此方法。
  end;

  TKeyListenerList = specialize TFGInterfaceList<IKeyListener>;

  { IMouseEvent }

  IMouseEvent = interface(IControlEvent)
    ['{D2F43C65-C8BC-476B-94B6-AFA44848575A}']
    function GetPoint: TPoint;
    function GetX: Integer;
    function GetY: Integer;
    function GetXOnScreen: Integer;
    function GetYOnScreen: Integer;
    function GetWheelDelta: Integer;
    function GetButton: TMouseButton;
    function GetShiftState: TShiftState;
  end;

  IMouseListener = interface(ICMListener)
    ['{E3D23FD3-D93F-49D0-B8BE-2806C5F08CE6}']
    procedure MouseEntered(e: IMouseEvent); //鼠标进入到组件上时调用。
    procedure MouseExited(e: IMouseEvent); //鼠标离开组件时调用。
    procedure MousePressed(e: IMouseEvent);  //鼠标按键在组件上按下时调用。
    procedure MouseReleased(e: IMouseEvent); //鼠标按钮在组件上释放时调用。
    procedure MouseMoved(e: IMouseEvent); //鼠标在组件上移动时调用。
    procedure MouseWheeled(e: IMouseEvent); //鼠标在组件上滚动时调用。
  end;

  TMouseListenerList = specialize TFGInterfaceList<IMouseListener>;



