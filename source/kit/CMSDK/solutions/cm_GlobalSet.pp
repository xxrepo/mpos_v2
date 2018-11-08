unit cm_GlobalSet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces, cm_type;

type

  { TCMGlobalSet }

  TCMGlobalSet = class(TCMBase, IGlobalSet)
  public
    function GetMainThreadID: TThreadID;
    function GetCurrentThreadVar: TThread;
    function GetGlobalNameSpace: IReadWriteSync;
  end;

implementation

{ TCMGlobalSet }

function TCMGlobalSet.GetMainThreadID: TThreadID;
begin
  Result := Classes.MainThreadID;
end;

function TCMGlobalSet.GetCurrentThreadVar: TThread;
begin
  Result := Classes.TThread.CurrentThread;
end;

function TCMGlobalSet.GetGlobalNameSpace: IReadWriteSync;
begin
  Result := Classes.GlobalNameSpace;
end;


end.

