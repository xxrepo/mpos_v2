
type

  {-----------------------------------------------------------

       LCL 的事件模式是对类指定回调函数地址，这可能在不同编译中存在类型强制转换、
   代码把控不严等而造成内存地址错乱的问题。同时其模式是耦合在一起的，且只能指定一个回调函数。
   这里抛弃了 LCL 的这一模式。

  -----------------------------------------------------------}

  IControlEvent = interface(ICMEvent)
    ['{50ABB64A-498C-4364-9E55-F4B5EE6860F0}']
    function GetAControl: TAControl;
  end;

  { IControlListener }

  IControlListener = interface(ICMListener)
    ['{9AB1F294-33E8-43F1-800F-D3F3D2452713}']
    procedure ControlClick(e: IControlEvent);
    procedure ControlDblClick(e: IControlEvent); //Control 未公开，在公开的子类中使用。
    procedure ControlResize(e: IControlEvent);
  end;

  TControlListenerList = specialize TFGInterfaceList<IControlListener>;

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

  ICustomControlEvent = interface(IWinControlEvent)
    ['{C633D6A0-484D-48A4-BA6C-9A968EB336D8}']
    function GetACustomControl: TACustomControl;
  end;

  ICustomControlListener = interface(IWinControlListener)
    ['{E27DCA5F-6472-486D-AB59-8339AECE3CCE}']
    procedure ControlPaint(e: ICustomControlEvent);
  end;

  TCustomControlListenerList = specialize TFGInterfaceList<ICustomControlListener>;

  IFormEvent = interface(ICustomControlEvent)
    ['{9BF85BE1-7BB4-4A08-AAE2-3C8C16B878C5}']
    function GetAForm: TAForm;
  end;

  IFormListener = interface(ICustomControlListener)
    ['{199F7D95-13C8-4BCB-995C-9D05F9A27240}']
    procedure FormActivate(e: IFormEvent);
    procedure FormClose(e: IFormEvent);
    procedure FormCreate(e: IFormEvent);
    procedure FormHide(e: IFormEvent);
    procedure FormShow(e: IFormEvent);
  end;

  TFormListenerList = specialize TFGInterfaceList<IFormListener>;

  { IKeyEvent
    // 表示组件中发生键击的事件。
  }

  IKeyEvent = interface(ICMEvent)
    ['{38F3CC3F-7786-4EBB-B372-872558527C1D}']
    function GetKeyChar: Char; //返回与此事件中的键关联的字符。
    function GetKeyCode: Word; //返回与此事件中的键关联的整数 keyCode。
    procedure SetKeyChar(AKeyChar: Char); //设置 keyCode 值，以表示某个逻辑字符。
    procedure SetKeyCode(AKeyCode: Word); //设置 keyCode 值，以表示某个物理键。
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


