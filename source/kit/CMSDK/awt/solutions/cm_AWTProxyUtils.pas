unit cm_AWTProxyUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, ExtCtrls,
  cm_messager,
  cm_AWT, cm_AWTProxy;

type

  { TProxyConverter }

  TProxyConverter = class(TCMMessageable)
  public
    function ConvertPanel(ALCLPanel: TPanel): TAPanel;
  end;

  { TExtAPanel }

  TExtAPanel = class(TAPanel)
  public
    constructor Create(AOwner: TAComponent; ALCLPanel: TPanel); overload;
  end;

  { TExtProxyPanelPeer }

  TExtProxyPanelPeer = class(TProxyPanelPeer)
  public
    constructor Create(TheTarget: TAComponent; ALCLPanel: TPanel); overload;
  end;

implementation

{ TProxyConverter }

function TProxyConverter.ConvertPanel(ALCLPanel: TPanel): TAPanel;
begin
  Result := TExtAPanel.Create(nil, ALCLPanel);
end;

{ TExtAPanel }

constructor TExtAPanel.Create(AOwner: TAComponent; ALCLPanel: TPanel);
begin
  inherited Create(AOwner);
  FPeer := nil;
  FPeer := TExtProxyPanelPeer.Create(Self, ALCLPanel);
end;

{ TExtProxyPanelPeer }

constructor TExtProxyPanelPeer.Create(TheTarget: TAComponent; ALCLPanel: TPanel);
begin
  inherited Create(TheTarget, nil);
  FDelegateObj.Free;
  FDelegateObj := ALCLPanel;
end;

end.

