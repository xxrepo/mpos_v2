unit cm_AWTEvent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces;

type

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

  { TKeyAdapter }

  TKeyAdapter = class(TCMBase, IKeyListener)
  public
    procedure KeyPressed(e: IKeyEvent); virtual;
    procedure KeyReleased(e: IKeyEvent); virtual;
    procedure KeyTyped(e: IKeyEvent); virtual;
  end;

implementation

{ TKeyAdapter }

procedure TKeyAdapter.KeyPressed(e: IKeyEvent);
begin
  //There's nothing to do here.
end;

procedure TKeyAdapter.KeyReleased(e: IKeyEvent);
begin
  //There's nothing to do here.
end;

procedure TKeyAdapter.KeyTyped(e: IKeyEvent);
begin
  //There's nothing to do here.
end;

end.

