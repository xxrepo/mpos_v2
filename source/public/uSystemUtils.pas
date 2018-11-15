unit uSystemUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces,
  uSystem;

type

  { TSystemEvent }

  TSystemEvent = class(TCMEvent, ISystemEvent)
  private
    FSystem: IAppSystem;
  public
    constructor Create(ASource: TObject; ASystem: IAppSystem); reintroduce;
    function GetSystem: IAppSystem;
  end;

  { TSystemAdapter }

  TSystemAdapter = class(TCMBase, ISystemListener)
  public
    procedure Loaded(e: ISystemEvent); virtual;
    procedure Prepared(e: ISystemEvent); virtual;
    procedure Logined(e: ISystemEvent); virtual;
    procedure Logoutting(e: ISystemEvent); virtual;
    procedure Closing(e: ISystemEvent); virtual;
  end;


implementation

{ TSystemEvent }

constructor TSystemEvent.Create(ASource: TObject; ASystem: IAppSystem);
begin
  inherited Create(ASource);
  FSystem := ASystem;
end;

function TSystemEvent.GetSystem: IAppSystem;
begin
  Result := FSystem;
end;

{ TSystemAdapter }

procedure TSystemAdapter.Loaded(e: ISystemEvent);
begin

end;

procedure TSystemAdapter.Prepared(e: ISystemEvent);
begin

end;

procedure TSystemAdapter.Logined(e: ISystemEvent);
begin

end;

procedure TSystemAdapter.Logoutting(e: ISystemEvent);
begin

end;

procedure TSystemAdapter.Closing(e: ISystemEvent);
begin

end;



end.

