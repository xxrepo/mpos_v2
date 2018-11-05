unit cm_awt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TAWColor = -$7FFFFFFF-1..$7FFFFFFF;

  TAWMessage = class
  private
    FMsg: string;
    FRInt: Integer;
  public
    property Msg: string read FMsg write FMsg;
    property RInt: Integer read FRInt write FRInt;
  end;

  { TWPObject
    // 窗口代理对象
  }

  TWPObject = class
  public
    procedure AWMessage(var Message: TAWMessage); virtual;
  end;

  { TAWObject
    // 抽象窗口对象
  }

  TAWObject = class
  private
    FPeer: TWPObject;
  protected
    property WPObject: TWPObject read FPeer write FPeer;
    function GetWPInteger(const APropertyName: string): Integer;
    procedure SetWPInteger(const APropertyName: string; AValue: Integer);
    //procedure firePropertyChange(String propertyName, Object oldValue, Object newValue);
  public
    constructor Create;
    //property ControlCount: Integer read GetControlCount;
    //property Controls[Index: Integer]: TControl read GetControl;
  end;

  TAWObjectClass = class of TAWObject;

  IAWWidgetSet = interface
    ['{0BD4F0EE-9C6F-4882-A92E-C4B34D866777}']
    function GetWPObject(const AAWClass: TAWObjectClass): TWPObject;
  end;

  IAWMessageSender = interface
    ['{0624BC78-EF7F-4B58-8F3E-63AA706D26B6}']
    procedure AWMessage(var Message: TAWMessage);
  end;

var
  AWMessageSender: IAWMessageSender;
  AWWidgetSet: IAWWidgetSet;

implementation

{ TWPObject }

procedure TWPObject.AWMessage(var Message: TAWMessage);
begin
  //
end;

{ TAWObject }

function TAWObject.GetWPInteger(const APropertyName: string): Integer;
var
  m: TAWMessage;
begin
  m := TAWMessage.Create;
  m.Msg := APropertyName;
  WPObject.AWMessage(m);
  Result := m.RInt;
  m.Free;
end;

procedure TAWObject.SetWPInteger(const APropertyName: string; AValue: Integer);
var
  m: TAWMessage;
begin
  m := TAWMessage.Create;
  m.Msg := APropertyName;
  m.RInt := AValue;
  WPObject.AWMessage(m);
  m.Free;
end;

constructor TAWObject.Create;
begin
  FPeer := AWWidgetSet.GetWPObject(TAWObjectClass(Self.ClassType));
end;


end.

