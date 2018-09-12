{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_classes

    This is not a complete unit, for testing

 **********************************************************************}

unit cm_classes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cm_interfaces;

type

  TStringArray = array of string;

  TEventTypeLevel = (etlAll, etlCustom, etlDebug, etlInfo, etlWarning, etlError, etlOff);

  EIllegalArgumentException = class(Exception);

  { TCMEvent }

  TCMEvent = class(TCMBase, ICMEvent)
  private
    FSource: TObject;
  public
    constructor Create(ASource: TObject);
    function GetSource: TObject;
  end;

implementation


{ TCMEvent }

constructor TCMEvent.Create(ASource: TObject);
begin
  if FSource = nil then
    raise EIllegalArgumentException.Create('null source');
  FSource := ASource;
end;

function TCMEvent.GetSource: TObject;
begin
  Result := FSource;
end;

end.

