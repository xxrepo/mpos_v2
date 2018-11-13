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
    procedure ControlResize(e: IControlEvent); virtual;
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

implementation

{ TControlAdapter }

procedure TControlAdapter.ControlClick(e: IControlEvent);
begin
  //There's nothing to do here.
end;

procedure TControlAdapter.ControlResize(e: IControlEvent);
begin
  //There's nothing to do here.
end;

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

