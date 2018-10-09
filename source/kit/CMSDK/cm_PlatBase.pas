{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_PlatBase

    This is not a complete unit, for testing

 **********************************************************************}

unit cm_PlatBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_messager, cm_logutils;

type

  { TCMLogMessageHandler }

  TCMLogMessageHandler = class(TCMMessageHandler, ICMMessageHandler)
  private
    FLogger: TCMFileLogger;
  protected
    procedure doPublish(ARecord: ICMMessageRecord); override;
  public
    constructor Create(ALogger: TCMFileLogger);
  end;

  TMessageMethod = procedure(Et: TEventType; const ASource, ATitle, AMsg: string; Dt: TDateTime) of object;

  { TCMSimpleMessageHandler }

  TCMSimpleMessageHandler = class(TCMMessageHandler, ICMMessageHandler)
  private
    FMessageMethod: TMessageMethod;
  protected
    procedure doPublish(ARecord: ICMMessageRecord); override;
  public
    constructor Create(AMessageMethod: TMessageMethod);
  end;

implementation

{ TCMLogMessageHandler }

constructor TCMLogMessageHandler.Create(ALogger: TCMFileLogger);
begin
  inherited Create;
  FLogger := ALogger;
end;

procedure TCMLogMessageHandler.doPublish(ARecord: ICMMessageRecord);
begin
  if Assigned(Self.Formatter) then
    FLogger.Log(ARecord.GetEventType, Self.Formatter.Format(ARecord), ARecord.GetDateTime)
  else if ARecord.GetTitle = '' then
    FLogger.Log(ARecord.GetEventType, ARecord.GetSource + ':' + ARecord.GetMessage, ARecord.GetDateTime)
  else
    FLogger.Log(ARecord.GetEventType, ARecord.GetSource + ':' + ARecord.GetTitle + ' ' + ARecord.GetMessage, ARecord.GetDateTime);
end;

{TCMSimpleMessageHandler}

constructor TCMSimpleMessageHandler.Create(AMessageMethod: TMessageMethod);
begin
  inherited Create;
  FMessageMethod := AMessageMethod;
end;

procedure TCMSimpleMessageHandler.doPublish(ARecord: ICMMessageRecord);
begin
  FMessageMethod(ARecord.GetEventType, ARecord.GetSource, ARecord.GetTitle, ARecord.GetMessage, ARecord.GetDateTime);
end;

end.

