{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_AToolkit

    This is not a complete unit, for testing

    Toolkit 的一种实现方案，通过在运转的 LCL 环境下使用代理的方式。

 **********************************************************************}

unit cm_AToolkit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, Graphics,
  cm_messager,
  cm_awt;

type

  { TAToolkit }

  TAToolkit = class(TCMMessageable, IAToolkit)
  public
    function CreatePanel(ATarget: TAPanel): IAPanelPeer;
    function CreateEdit(ATarget: TAEdit): IAEditPeer;
    function CreateForm(ATarget: TAForm): IAFormPeer;
  end;

implementation

uses cm_AProxy;

{ TAToolkit }

function TAToolkit.CreatePanel(ATarget: TAPanel): IAPanelPeer;
begin
  Result := TAPanelPeer.Create(nil);
end;

function TAToolkit.CreateEdit(ATarget: TAEdit): IAEditPeer;
begin
  Result := TAEditPeer.Create(nil);
end;

function TAToolkit.CreateForm(ATarget: TAForm): IAFormPeer;
begin
  Result := TAFormPeer.Create(nil);
end;


initialization
  Toolkit := TAToolkit.Create;

end.

