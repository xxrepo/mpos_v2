{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_interfaces

    This is not a complete unit, for testing

 **********************************************************************}

unit cm_interfaces;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  ICMBase = interface(IUnknown)
    ['{B024BC87-6FFF-4CDE-BA0D-CA90269454A0}']
    function GetImplementorName: string;
    function GetHashCode: PtrInt;
  end;

  { TCMBase }

  TCMBase = class(TInterfacedObject, ICMBase)
  public
    function GetImplementorName: string; virtual;
  end;

  { TCMUnfetteredBase }

  TCMUnfetteredBase = class(TInterfacedObject, ICMBase)
  protected
    function _Release: LongInt;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  public
    function GetImplementorName: string; virtual;
  end;

  { TCMBasePersistent }

  TCMBasePersistent = class(TInterfacedPersistent, ICMBase)
  public
    function GetImplementorName: string; virtual;
  end;

  { TCMBaseComponent }

  TCMBaseComponent = class(TComponent, ICMBase)
  protected
    FControlFree: Boolean;
    FRefCount: LongInt;
    FDestroyCount : LongInt;
    function _AddRef: LongInt;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release: LongInt;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  public
    destructor Destroy; override;
    procedure AfterConstruction;override;
    function GetImplementorName: string; virtual;
  end;

  { ICMLog } (************* 一个简单的日志接口。  *************
              //使用TEventType作为日志的级别，顺序定义如下：
                  custom (the least serious)
                  debug
                  info
                  warning
                  error (the most serious)
              *************************************************)
  ICMLog = interface(ICMBase)
    ['{3A9A32E6-C416-4738-8C8F-227D974C27B6}']
    procedure Custom(const AMsg: string);
    procedure Debug(const AMsg: string);
    procedure Info(const AMsg: string);
    procedure Warning(const AMsg: string);
    procedure Error(const AMsg: string);
    procedure Error(const AMsg: string; Ex: Exception);
  end;

  { IListener
    //A  tagging interface that all event listener interfaces must extend.
  }
  ICMListener = interface(ICMBase)
    ['{E167DECC-876B-4A66-851B-C72415FFB49C}']
  end;

  { ICMEvent
    // 所有事件状态对象都将从其拓展的根接口。
  }
  ICMEvent = interface(ICMBase)
    ['{A08AEE7C-6C02-4F1B-8CB3-97C52350D687}']
    function GetSource: TObject;
  end;

  TCMListener = class(TCMBase, ICMListener)
  end;

  { TCMEvent
    // 所有 Event 在构造时都引用了对象 "source"，在逻辑上认为该对象是最初发生有关 Event 的对象。
  }
  TCMEvent = class(TCMBase, ICMEvent)
  private
    FSource: TObject;
  public
    constructor Create(ASource: TObject);
    function GetSource: TObject;
  end;

  //------------------------------------------------------------------------------------------------
  // 以下不从 ICMBase 拓展
  //------------------------------------------------------------------------------------------------

  { IRunnable
    //  Runnable 接口应该由那些打算通过某一线程执行其实例的类来实现。类必须定义一个称为 run 的无参
    数方法。也可用于作为一个单纯的执行体。
        设计该接口的目的是为希望在活动时执行代码的对象提供一个公共协议。例如，CMThread 类实现了
    Runnable。激活的意思是说某个线程已启动并且尚未停止。
        此外，Runnable 为非 CMThread 子类的类提供了一种激活方式。通过实例化某个 Thread 实例并将自身
    作为运行目标，就可以运行实现 Runnable 的类而无需创建 CMThread 的子类。大多数情况下，如果只想重写
    run() 方法，而不重写其他 CMThread 方法，那么应使用 Runnable 接口。这很重要，因为除非程序员打算修
    改或增强类的基本行为，否则不应为该类创建子类。
  }

  IRunnable = interface
    ['{E75DDBD5-E3D7-41C5-9209-71FFCC9EACA9}']
    procedure Run;
  end;

  { IExecutor
    //  执行已提交的 Runnable 任务的对象。此接口提供一种将任务提交与每个任务将如何运行的机制（包括
    线程使用的细节、调度等）分离开来的方法。通常使用 Executor 而不是显式地创建线程。
  }

  IExecutor = interface
    ['{FC949ADB-BA89-4698-B113-9C1CF7BB2DFB}']
    procedure Execute(Aommand: IRunnable);
  end;


implementation

{TCMBase}

function TCMBase.GetImplementorName: string;
begin
  Result := Self.UnitName + '.' + Self.ClassName;
end;

{ TCMUnfetteredBase }

function TCMUnfetteredBase._Release: LongInt;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  _Release:=InterlockedDecrement(FRefCount);
end;

function TCMUnfetteredBase.GetImplementorName: string;
begin
  Result := Self.UnitName + '.' + Self.ClassName;
end;

{ TCMBasePersistent }

function TCMBasePersistent.GetImplementorName: string;
begin
  Result := Self.UnitName + '.' + Self.ClassName;
end;

{TCMBaseComponent}

destructor TCMBaseComponent.Destroy;
begin
  // We must explicitly reset.
  FRefCount:=0;
  FDestroyCount:=0;
  inherited Destroy;
end;

procedure TCMBaseComponent.AfterConstruction;
begin
  inherited AfterConstruction;
  FControlFree := False;
end;

function TCMBaseComponent._AddRef: LongInt;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if FControlFree then
    _AddRef := InterlockedIncrement(FRefCount)
  else
    Result := inherited _AddRef;
end;

function TCMBaseComponent._Release: LongInt;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if FControlFree then
    begin
      _Release:=InterlockedDecrement(FRefCount);
      if _Release=0 then
        begin
          if InterlockedIncrement(FDestroyCount)=1 then
            Self.Destroy;
        end;
    end
  else
    Result := inherited _Release;
end;

function TCMBaseComponent.GetImplementorName: string;
begin
  Result := Self.UnitName + '.' + Self.ClassName;
end;

{ TCMEvent }

constructor TCMEvent.Create(ASource: TObject);
begin
  inherited Create;
  if ASource = nil then
    raise EArgumentNilException.Create('nil source');
  FSource := ASource;
end;

function TCMEvent.GetSource: TObject;
begin
  Result := FSource;
end;


end.

