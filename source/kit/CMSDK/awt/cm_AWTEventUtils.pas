unit cm_AWTEventUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, //Generics.Defaults,
  cm_interfaces,
  cm_AWT;

type

  { TControlAdapter }

  TControlAdapter = class(TCMBase, IControlListener)
  public
    procedure ControlClick(e: IControlEvent); virtual;
    procedure ControlDblClick(e: IControlEvent); virtual;
    procedure ControlResize(e: IControlEvent); virtual;
  end;

  { TWinControlAdapter }

  TWinControlAdapter = class(TControlAdapter, IWinControlListener)
  public
    procedure ControlEnter(e: IWinControlEvent); virtual;
    procedure ControlExit(e: IWinControlEvent); virtual;
  end;

  { TKeyAdapter
    // 接收键盘事件的抽象适配器类。此类中的方法为空。此类存在的目的是方便创建侦听器对象。
    //扩展此类即可创建 KeyEvent 侦听器并重写所需事件的方法。（如果要实现 KeyListener 接口，则必须
    //定义该接口内的所有方法。此抽象类将所有方法都定义为 null，所以只需针对关心的事件定义方法。）
  }

  TKeyAdapter = class(TCMBase, IKeyListener)
  public
    procedure KeyPressed(e: IKeyEvent); virtual;
    procedure KeyReleased(e: IKeyEvent); virtual;
    procedure KeyTyped(e: IKeyEvent); virtual;
  end;

  { TMouseAdapter }

  TMouseAdapter = class(TCMBase, IMouseListener)
  public
    procedure MouseEntered(e: IMouseEvent); virtual;
    procedure MouseExited(e: IMouseEvent); virtual;
    procedure MousePressed(e: IMouseEvent); virtual;
    procedure MouseReleased(e: IMouseEvent); virtual;
    procedure MouseMoved(e: IMouseEvent); virtual;
    procedure MouseWheeled(e: IMouseEvent); virtual;
  end;

implementation

{ TControlAdapter }

procedure TControlAdapter.ControlClick(e: IControlEvent);
begin
  //There's nothing to do here.
end;

procedure TControlAdapter.ControlDblClick(e: IControlEvent);
begin

end;

procedure TControlAdapter.ControlResize(e: IControlEvent);
begin

end;

{ TWinControlAdapter }

procedure TWinControlAdapter.ControlEnter(e: IWinControlEvent);
begin

end;

procedure TWinControlAdapter.ControlExit(e: IWinControlEvent);
begin

end;

{ TKeyAdapter }

procedure TKeyAdapter.KeyPressed(e: IKeyEvent);
begin

end;

procedure TKeyAdapter.KeyReleased(e: IKeyEvent);
begin

end;

procedure TKeyAdapter.KeyTyped(e: IKeyEvent);
begin

end;

{ TMouseAdapter }

procedure TMouseAdapter.MouseEntered(e: IMouseEvent);
begin

end;

procedure TMouseAdapter.MouseExited(e: IMouseEvent);
begin

end;

procedure TMouseAdapter.MousePressed(e: IMouseEvent);
begin

end;

procedure TMouseAdapter.MouseReleased(e: IMouseEvent);
begin

end;

procedure TMouseAdapter.MouseMoved(e: IMouseEvent);
begin

end;

procedure TMouseAdapter.MouseWheeled(e: IMouseEvent);
begin

end;


end.

