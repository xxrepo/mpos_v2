{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_messager

    This is not a complete unit, for testing

 **********************************************************************}

unit cm_messager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs,
  cm_classes, cm_interfaces;

type

  ICMMessageRecord = interface
    ['{A2FCD237-59C2-44FD-BFE5-F26F558C2F87}']
    function GetMessagerName: string;
    function GetEventType: TEventType;
    function GetSource: string;
    function GetTitle: string;
    function GetMessage: string;     //获取本地化或格式化之前的“原始”信息。
    function GetDateTime: TDateTime;
  end;

  { TCMMessageRecord }

  TCMMessageRecord = class(TInterfacedObject, ICMMessageRecord)
  private
    FMessagerName: string;
    FEventType: TEventType;
    FSource: string;
    FTitle: string;
    FMessage: string;
    FDateTime: TDateTime;
  protected
    procedure setTitle(AValue: string); virtual;
    procedure setMessage(AValue: string); virtual;
  public
    property MessagerName: string read FMessagerName write FMessagerName;
    property EventType: TEventType read FEventType write FEventType;
    property Source: string read FSource write FSource;
    property Title: string read FTitle write setTitle;
    property Message: string read FMessage write setMessage;
    property DateTime: TDateTime read FDateTime write FDateTime;
  public
    function GetMessagerName: string;
    function GetEventType: TEventType;
    function GetSource: string;
    function GetTitle: string; virtual;
    function GetMessage: string; virtual;
    function GetDateTime: TDateTime;
  end;

  TCMMessageRecordClass = class of TCMMessageRecord;

  ICMMessageFilter = interface
    ['{611FFDC6-612F-431C-92ED-3C120CFC1124}']
    function IsAble(ARecord: ICMMessageRecord): Boolean;
  end;

  ICMMessageFormatter = interface
    ['{0E418A61-3FAE-4643-AE80-0D5DC466FFB8}']
    function Format(ARecord: ICMMessageRecord): string;
  end;

  ICMMessageHandler = interface(ICMBase)
    ['{85083853-1DD4-42EF-8E57-8A5478B503C2}']
    procedure Publish(ARecord: ICMMessageRecord);
    procedure SetLevel(Etl: TEventTypeLevel);
    function GetLevel: TEventTypeLevel;
    procedure SetFilter(AFilter: ICMMessageFilter);
    procedure SetFormatter(AFormatter: ICMMessageFormatter);
  end;

  TCMMessageHandlers = array of ICMMessageHandler;

  ICMMessager = interface(ICMBase)
    ['{A960A38C-A099-4A66-8413-BB97C126E1C4}']
    procedure AddMessageHandler(AHandler: ICMMessageHandler);
    procedure RemoveMessageHandler(AHandler: ICMMessageHandler);
    procedure RemoveAllMessageHandler;
    function GetMessageHandlers: TCMMessageHandlers;
    procedure SetLevel(Et: TEventTypeLevel);
    function GetLevel: TEventTypeLevel;
    function GetMessagerName: shortstring;
    procedure Message(ARecord: ICMMessageRecord);
    procedure Message(Et: TEventType; const ATitle, AMsg: string);
    procedure Custom(const AMsg: string);
    procedure Custom(const AMsgFmt: string; Args: array of const);
    procedure Debug(const AMsg: string);
    procedure Debug(const AMsgFmt: string; Args: array of const);
    procedure Debug(const ATitle, AMsg: string);
    procedure Debug(const ATitle, AMsgFmt: string; Args: array of const);
    procedure Info(const AMsg: string);
    procedure Info(const AMsgFmt: string; Args: array of const);
    procedure Warning(const AMsg: string);
    procedure Warning(const AMsgFmt : string; Args: array of const);
    procedure Error(const AMsg: string);
    procedure Error(const AMsgFmt: string; Args: array of const);
    procedure Error(const ATitle, AMsg: string);
    procedure Error(const ATitle, AMsgFmt: string; Args: array of const);
    procedure Error(const ATitleHead: string; Ex: Exception);
    procedure Error(const ATitleHeadFmt: string; Args: array of const; Ex: Exception);
  end;

  { TCMSimpleFormatter }

  TCMSimpleFormatter = class(TCMBase, ICMMessageFormatter)
  public
    function Format(ARecord: ICMMessageRecord): string; virtual;
  end;

  { TCMMessageHandler }

  TCMMessageHandler = class abstract(TCMBase, ICMMessageHandler)
  private
    FEventTypeLevel: TEventTypeLevel;
    FFilter: ICMMessageFilter;
    FFormatter: ICMMessageFormatter;
  protected
    procedure doPublish(ARecord: ICMMessageRecord); virtual; abstract;
    property Filter: ICMMessageFilter read FFilter;
    property Formatter: ICMMessageFormatter read FFormatter;
  public
    constructor Create;
    procedure Publish(ARecord: ICMMessageRecord);
    procedure SetLevel(Etl: TEventTypeLevel); virtual;
    function GetLevel: TEventTypeLevel;
    procedure SetFilter(AFilter: ICMMessageFilter); virtual;
    procedure SetFormatter(AFormatter: ICMMessageFormatter); virtual;
  end;

  { TCMMessager }

  TCMMessager = class(TCMUnfetteredBase, ICMMessager)
  private
    FHandlerList: TThreadList;
    tempHandler: ICMMessageHandler;
    FEventTypeLevel: TEventTypeLevel;
    FMessagerName: shortstring;
    FBundleName: shortstring;
    FMessageRecordClass: TCMMessageRecordClass;
  public
    constructor Create(AMessagerName: shortstring);
    destructor Destroy; override;
    property MessagerName: shortstring read FMessagerName write FMessagerName;
    property BundleName: shortstring read FBundleName write FBundleName;
    procedure SetMessageRecordClass(AClass: TCMMessageRecordClass);
    procedure FromMessageHandlers(ACMMessager: TCMMessager); virtual;
    function GetMessageHandlerCount: Integer;
    procedure DoMessage(ARec: TCMMessageRecord); virtual;
  public
    procedure AddMessageHandler(AHandler: ICMMessageHandler); virtual;
    procedure RemoveMessageHandler(AHandler: ICMMessageHandler); virtual;
    procedure RemoveAllMessageHandler;
    function GetMessageHandlers: TCMMessageHandlers;
    procedure SetLevel(Etl: TEventTypeLevel);
    function GetLevel: TEventTypeLevel;
    function GetMessagerName: shortstring;
    procedure Message(ARecord: ICMMessageRecord);
    procedure Message(Et: TEventType; const ATitle, AMsg: string);
    procedure Custom(const AMsg: string);
    procedure Custom(const AMsgFmt: string; Args: array of const);
    procedure Debug(const AMsg: string);
    procedure Debug(const AMsgFmt: string; Args: array of const);
    procedure Debug(const ATitle, AMsg: string);
    procedure Debug(const ATitle, AMsgFmt: string; Args: array of const);
    procedure Info(const AMsg: string);
    procedure Info(const AMsgFmt: string; Args: array of const);
    procedure Warning(const AMsg: string);
    procedure Warning(const AMsgFmt : string; Args: array of const);
    procedure Error(const AMsg: string);
    procedure Error(const AMsgFmt: string; Args: array of const);
    procedure Error(const ATitle, AMsg: string);
    procedure Error(const ATitle, AMsgFmt: string; Args: array of const);
    procedure Error(const ATitleHead: string; Ex: Exception);
    procedure Error(const ATitleHeadFmt: string; Args: array of const; Ex: Exception);
  end;

  { ICMMessageable }

  ICMMessageable = interface(ICMBase)
    ['{8CEC1F24-5696-40F4-80DF-7243B70F3167}']
    function Messager: TCMMessager;
  end;

  { TCMMessageManager }

  TCMMessageManager = class
  private
    class var FManager: TCMMessageManager;
    class var FDefaultHandler: ICMMessageHandler;
  private
    FMessagerList: TFPHashObjectList;
    //{$WARNINGS OFF}
    constructor Create; //Please do not use this constructor
  public
    destructor Destroy; override;
    class function GetInstance: TCMMessageManager;
    class property DefaultHandler: ICMMessageHandler read FDefaultHandler write FDefaultHandler;
    procedure AddMessager(AMessager: TCMMessager);
    function GetMessager(const ABundleName: string): TCMMessager; overload;
    function GetMessager(const ABundleName: string; AHandler: ICMMessageHandler): TCMMessager; overload;
    function GetMessager(AGetter: TObject): TCMMessager; overload;
    function GetMessager(AGetter: TObject; AHandler: ICMMessageHandler): TCMMessager; overload;
    procedure ReleaseMessager(const ABundleName: string);
    procedure Reset;
  end;

  { TCMMessageable }

  TCMMessageable = class(TCMBase, ICMMessageable)
  private
    FMessager: TCMMessager;
  public
    constructor Create;
    constructor Create(AHandler: ICMMessageHandler); overload;
    function Messager: TCMMessager;
  end;

  { TCMMessageablePersistent }

  TCMMessageablePersistent = class(TCMBasePersistent, ICMMessageable)
  private
    FMessager: TCMMessager;
  public
    constructor Create;
    constructor Create(AHandler: ICMMessageHandler); overload;
    function Messager: TCMMessager;
  end;

  { TCMMessageableComponent }

  TCMMessageableComponent = class(TCMBaseComponent, ICMMessageable)
  private
    FMessager: TCMMessager;
  public
    constructor Create(AOwner: TComponent); override; overload;
    constructor Create(AOwner: TComponent; AHandler: ICMMessageHandler); overload;
    function Messager: TCMMessager;
  end;

  function GetEventTypeLevelValue(Et: TEventType): Integer;
  function GetEventTypeLevelValue(Etl: TEventTypeLevel): Integer;

const
  DefaultMessageHandlerCode: string = 'DEFAULT';

var
  DistributionMessagerNameIncludeUnitName: Boolean = False;

implementation

//(etCustom 1, etInfo 2, etDebug 3, etWarning 4, etError 5)
function GetEventTypeLevelValue(Et: TEventType): Integer;
begin
  Result := 0;
  case Et of
  etCustom: Result := 1;
  etDebug: Result := 2;
  etInfo: Result := 3;
  etWarning: Result := 4;
  etError: Result := 5;
  end;
end;

function GetEventTypeLevelValue(Etl: TEventTypeLevel): Integer;
begin
  case Etl of
  etlAll: Result := 0;
  etlCustom: Result := 1;
  etlDebug: Result := 2;
  etlInfo: Result := 3;
  etlWarning: Result := 4;
  etlError: Result := 5;
  etlOff: Result := 99;
  end;
end;

{ TCMMessageRecord }

procedure TCMMessageRecord.setTitle(AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
end;

procedure TCMMessageRecord.setMessage(AValue: string);
begin
  if FMessage = AValue then
    Exit;
  FMessage := AValue;
end;

function TCMMessageRecord.GetMessagerName: string;
begin
  Result := FMessagerName;
end;

function TCMMessageRecord.GetEventType: TEventType;
begin
  Result := FEventType;
end;

function TCMMessageRecord.GetSource: string;
begin
  Result := FSource;
end;

function TCMMessageRecord.GetTitle: string;
begin
  Result := FTitle;
end;

function TCMMessageRecord.GetMessage: string;
begin
  Result := FMessage;
end;

function TCMMessageRecord.GetDateTime: TDateTime;
begin
  Result := FDateTime;
end;

{ TCMMessageManager }

constructor TCMMessageManager.Create;
begin
  FMessagerList := TFPHashObjectList.Create(True);
end;

destructor TCMMessageManager.Destroy;
begin
  FMessagerList.Free;
  inherited Destroy;
end;

class function TCMMessageManager.GetInstance: TCMMessageManager;
begin
  Result := nil;
  if not Assigned(TCMMessageManager.FManager) then
    TCMMessageManager.FManager := TCMMessageManager.Create;
  Result := TCMMessageManager.FManager;
end;

procedure TCMMessageManager.AddMessager(AMessager: TCMMessager);
begin
  if Assigned(AMessager) then
    FMessagerList.Add(AMessager.BundleName, AMessager);
end;

function TCMMessageManager.GetMessager(const ABundleName: string): TCMMessager;
begin
  Result := GetMessager(ABundleName, TCMMessageManager.FDefaultHandler);
end;

function TCMMessageManager.GetMessager(const ABundleName: string; AHandler: ICMMessageHandler): TCMMessager;
begin
  Result := TCMMessager(FMessagerList.Find(ABundleName));
  if not Assigned(Result) then
    Result := TCMMessager.Create(ABundleName + 'Messager');
  Result.BundleName := ABundleName;
  if Assigned(AHandler) then
    Result.AddMessageHandler(AHandler);
end;

function TCMMessageManager.GetMessager(AGetter: TObject): TCMMessager;
begin
  if DistributionMessagerNameIncludeUnitName then
    Result := GetMessager(AGetter.UnitName + '.' + AGetter.ClassName)
  else
    Result := GetMessager(AGetter.ClassName);
end;

function TCMMessageManager.GetMessager(AGetter: TObject; AHandler: ICMMessageHandler): TCMMessager;
begin
  if DistributionMessagerNameIncludeUnitName then
    Result := GetMessager(AGetter.UnitName + '.' + AGetter.ClassName, AHandler)
  else
    Result := GetMessager(AGetter.ClassName, AHandler);
end;

procedure TCMMessageManager.ReleaseMessager(const ABundleName: string);
var
  er: TCMMessager;
begin
  er := TCMMessager(FMessagerList.Find(ABundleName));
  if not Assigned(er) then
    FreeAndNil(er);
end;

procedure TCMMessageManager.Reset;
var
  i: Integer;
  er: TCMMessager;
begin
  for i:=0 to FMessagerList.Count-1 do
    begin
      er := TCMMessager(FMessagerList[i]);
      if Assigned(er) then
        er.RemoveAllMessageHandler;
    end;
end;

{ TCMMessageable }

constructor TCMMessageable.Create;
begin
  FMessager := TCMMessageManager.GetInstance.GetMessager(Self);
end;

constructor TCMMessageable.Create(AHandler: ICMMessageHandler);
begin
  FMessager := TCMMessageManager.GetInstance.GetMessager(Self, AHandler);
end;

function TCMMessageable.Messager: TCMMessager;
begin
  Result := FMessager;
end;

{ TCMMessageablePersistent }

constructor TCMMessageablePersistent.Create;
begin
  FMessager := TCMMessageManager.GetInstance.GetMessager(Self);
end;

constructor TCMMessageablePersistent.Create(AHandler: ICMMessageHandler);
begin
  FMessager := TCMMessageManager.GetInstance.GetMessager(Self, AHandler);
end;

function TCMMessageablePersistent.Messager: TCMMessager;
begin
  Result := FMessager;
end;

{ TCMMessageableComponent }

constructor TCMMessageableComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMessager := TCMMessageManager.GetInstance.GetMessager(Self);
end;

constructor TCMMessageableComponent.Create(AOwner: TComponent; AHandler: ICMMessageHandler);
begin
  Self.Create(AOwner); //考虑重载
  FMessager := TCMMessageManager.GetInstance.GetMessager(Self, AHandler);
end;

function TCMMessageableComponent.Messager: TCMMessager;
begin
  Result := FMessager;
end;

{ TCMSimpleFormatter }

function TCMSimpleFormatter.Format(ARecord: ICMMessageRecord): string;
begin
  Result := ARecord.GetSource + ': ' + ARecord.GetTitle + ' ' + ARecord.GetMessage;
end;

{ TCMMessageHandler }

constructor TCMMessageHandler.Create;
begin
  inherited Create;
  FEventTypeLevel := etlAll;
  FFilter := nil;
  FFormatter := nil;
end;

procedure TCMMessageHandler.Publish(ARecord: ICMMessageRecord);
begin
  if GetEventTypeLevelValue(ARecord.GetEventType) < GetEventTypeLevelValue(FEventTypeLevel) then
    Exit;
  if Assigned(FFilter) and (not FFilter.IsAble(ARecord)) then
    Exit;
  doPublish(ARecord);
end;

procedure TCMMessageHandler.SetLevel(Etl: TEventTypeLevel);
begin
  FEventTypeLevel := Etl;
end;

function TCMMessageHandler.GetLevel: TEventTypeLevel;
begin
  Result := FEventTypeLevel;
end;

procedure TCMMessageHandler.SetFilter(AFilter: ICMMessageFilter);
begin
  FFilter := AFilter;
end;

procedure TCMMessageHandler.SetFormatter(AFormatter: ICMMessageFormatter);
begin
  FFormatter := AFormatter;
end;

{TCMMessager}

constructor TCMMessager.Create(AMessagerName: shortstring);
begin
  inherited Create;
  FHandlerList := TThreadList.Create;
  FEventTypeLevel := etlAll;
  FMessagerName := AMessagerName;
  FBundleName := '';
  TCMMessageManager.GetInstance.AddMessager(Self);
  FMessageRecordClass := TCMMessageRecord;
end;

destructor TCMMessager.Destroy;
begin
  FHandlerList.Free;
  inherited Destroy;
end;

procedure TCMMessager.SetMessageRecordClass(AClass: TCMMessageRecordClass);
begin
  FMessageRecordClass := AClass;
end;

procedure TCMMessager.AddMessageHandler(AHandler: ICMMessageHandler);
begin
  if Assigned(AHandler) then
    begin
      tempHandler := AHandler; //解决handler以局部变量创建的问题
      FHandlerList.Add(tempHandler);
    end;
end;

procedure TCMMessager.RemoveMessageHandler(AHandler: ICMMessageHandler);
begin
  FHandlerList.Remove(AHandler);
end;

procedure TCMMessager.RemoveAllMessageHandler;
begin
  FHandlerList.Clear;
end;

procedure TCMMessager.FromMessageHandlers(ACMMessager: TCMMessager);
var
  //handlers: TCMMessageHandlers;
  i: Integer;
  list: TList;
begin
  if not Assigned(ACMMessager) then
    Exit;
  //standard
  {
  handlers := ACMMessager.GetMessageHandlers;
  for i:=Low(handlers) to High(handlers) do
    Self.AddMessageHandler(handlers[i]);
  }
  //fast
  list := ACMMessager.FHandlerList.LockList;
  try
    for i:=0 to list.Count-1 do
      Self.FHandlerList.Add(list[i]);
  finally
    ACMMessager.FHandlerList.UnlockList;
  end;
end;

function TCMMessager.GetMessageHandlers: TCMMessageHandlers;
var
  list: TList;
  i: Integer;
begin
  list := FHandlerList.LockList;
  try
    SetLength(Result, list.Count);
    for i:=0 to list.Count-1 do
      begin
        Result[i] := ICMMessageHandler(list[i]);
      end;
  finally
    FHandlerList.UnlockList;
  end;
end;

function TCMMessager.GetMessageHandlerCount: Integer;
begin
  Result := 0;
  Result := FHandlerList.LockList.Count;
  FHandlerList.UnlockList;
end;

procedure TCMMessager.SetLevel(Etl: TEventTypeLevel);
begin
  FEventTypeLevel := Etl;
end;

function TCMMessager.GetLevel: TEventTypeLevel;
begin
  Result := FEventTypeLevel;
end;

function TCMMessager.GetMessagerName: shortstring;
begin
  Result := FMessagerName;
end;

procedure TCMMessager.Message(ARecord: ICMMessageRecord);
var
  list: TList;
  i: Integer;
begin
  if GetEventTypeLevelValue(ARecord.GetEventType) < GetEventTypeLevelValue(FEventTypeLevel) then
    Exit;
  list := FHandlerList.LockList;
  try
    for i:=0 to list.Count-1 do
      begin
        ICMMessageHandler(list[i]).Publish(ARecord);
      end;
  finally
    FHandlerList.UnlockList;
  end;
end;

procedure TCMMessager.DoMessage(ARec: TCMMessageRecord);
begin
  ARec.MessagerName := GetMessagerName;
  ARec.Source := BundleName;      //源应是
  ARec.DateTime := now;
  Message(ARec);
end;

procedure TCMMessager.Message(Et: TEventType; const ATitle, AMsg: string);
var
  rec: TCMMessageRecord;
begin
  rec := FMessageRecordClass.Create;
  rec.EventType := Et;
  rec.Title := ATitle;
  rec.Message := AMsg;
  DoMessage(rec);
end;

procedure TCMMessager.Custom(const AMsg: string);
begin
  Message(etCustom, '', AMsg);
end;

procedure TCMMessager.Custom(const AMsgFmt: string; Args: array of const);
begin
  Custom(Format(AMsgFmt, Args));
end;

procedure TCMMessager.Debug(const AMsg: string);
begin
  Message(etDebug, '', AMsg);
end;

procedure TCMMessager.Debug(const AMsgFmt: string; Args: array of const);
begin
  Debug(Format(AMsgFmt, Args));
end;

procedure TCMMessager.Debug(const ATitle, AMsg: string);
begin
  Message(etDebug, ATitle, AMsg);
end;

procedure TCMMessager.Debug(const ATitle, AMsgFmt: string; Args: array of const);
begin
  Debug(ATitle, Format(AMsgFmt, Args));
end;

procedure TCMMessager.Info(const AMsg: string);
begin
  Message(etInfo, '', AMsg);
end;

procedure TCMMessager.Info(const AMsgFmt: string; Args: array of const);
begin
  Info(Format(AMsgFmt, Args));
end;

procedure TCMMessager.Warning(const AMsg: string);
begin
  Message(etWarning, '', AMsg);
end;

procedure TCMMessager.Warning(const AMsgFmt: string; Args: array of const);
begin
  Warning(Format(AMsgFmt, Args));
end;

procedure TCMMessager.Error(const AMsg: string);
begin
  Message(etError, '', AMsg);
end;

procedure TCMMessager.Error(const AMsgFmt: string; Args: array of const);
begin
  Error(Format(AMsgFmt, Args));
end;

procedure TCMMessager.Error(const ATitle, AMsg: string);
begin
  Message(etError, ATitle, AMsg);
end;

procedure TCMMessager.Error(const ATitle, AMsgFmt: string; Args: array of const);
begin
  Error(ATitle, Format(AMsgFmt, Args));
end;

procedure TCMMessager.Error(const ATitleHead: string; Ex: Exception);
begin
  Error(ATitleHead + ' ' + Ex.ClassName, Ex.Message);
end;

procedure TCMMessager.Error(const ATitleHeadFmt: string; Args: array of const; Ex: Exception);
begin
  Error(Format(ATitleHeadFmt, Args), Ex);
end;


initialization
  TCMMessageManager.FManager := nil;
  TCMMessageManager.FDefaultHandler := nil;

finalization
  if Assigned(TCMMessageManager.FManager) then
    TCMMessageManager.FManager.Free;

end.

