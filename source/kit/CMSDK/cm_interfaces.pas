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

  { TCMEvent
    // 所有 Event 在构造时都引用了对象 "source"，在逻辑上认为该对象是最初发生有关 Event 的对象。
  }
  TCMEvent = class(TCMBase, ICMEvent)
  private
    FSource: TObject;
  public
    constructor Create(ASource: TObject); virtual;
    function GetSource: TObject;
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

