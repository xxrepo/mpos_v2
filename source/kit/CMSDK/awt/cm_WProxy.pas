{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_WProxy

    This is not a complete unit, for testing

 **********************************************************************}

unit cm_WProxy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, Graphics,
  cm_LCL,
  cm_awt,
  cm_messager,
  cm_Plat;

type

  { TWPControl }

  TWPControl = class(TWPObject)
  private
    FProxyObj: TControl;
    function GetColor: TColor;
    function GetHeight: Integer;
    function GetLeft: Integer;
    function GetTop: Integer;
    function GetWidth: Integer;
    procedure SetColor(AValue: TColor);
    procedure SetHeight(AValue: Integer);
    procedure SetLeft(AValue: Integer);
    procedure SetTop(AValue: Integer);
    procedure SetWidth(AValue: Integer);
  public
    constructor Create;
    procedure AWMessage(var Message: TAWMessage); override;
 public
    property Color: TColor read GetColor write SetColor;
    property Left: Integer read GetLeft write SetLeft;
    property Height: Integer read GetHeight write SetHeight;
    property Top: Integer read GetTop write SetTop;
    property Width: Integer read GetWidth write SetWidth;
  end;

  { TWPForm }

  TWPForm = class(TWPControl)
  private

  public
    constructor Create;
    procedure AWMessage(var Message: TAWMessage); override;
  public
    function ShowModal: Integer;
  end;

  { TAWWidgetSet }

  TAWWidgetSet = class(TCMMessageable, IAWWidgetSet)
  public
    function GetWPObject(const AAWClass: TAWObjectClass): TWPObject;
  end;

var
  LCLGenerator: ICMLCLGenerator = nil;
  LCLPropertyReaderWriter: ICMLCLPropertyReaderWriter = nil;

implementation

{ TWPControl }

function TWPControl.GetColor: TColor;
begin
  Result := FProxyObj.Color;
end;

function TWPControl.GetHeight: Integer;
begin
  Result := FProxyObj.Height;
end;

function TWPControl.GetLeft: Integer;
begin
  Result := FProxyObj.Left;
end;

function TWPControl.GetTop: Integer;
begin
  Result := FProxyObj.Top;
end;

function TWPControl.GetWidth: Integer;
begin
  Result := FProxyObj.Width;
end;

procedure TWPControl.SetColor(AValue: TColor);
begin
  InterfaceRegister.OutInterface(ICMLCLPropertyReaderWriter, LCLPropertyReaderWriter);
  //FProxyObj.Color := AValue;
  LCLPropertyReaderWriter.SetOrdProp(FProxyObj, 'Color', AValue);
end;

procedure TWPControl.SetHeight(AValue: Integer);
begin
  FProxyObj.Height := AValue;
end;

procedure TWPControl.SetLeft(AValue: Integer);
begin
  FProxyObj.Left := AValue;
end;

procedure TWPControl.SetTop(AValue: Integer);
begin
  FProxyObj.Top := AValue;
end;

procedure TWPControl.SetWidth(AValue: Integer);
begin
  FProxyObj.Width := AValue;
end;

constructor TWPControl.Create;
begin
  InterfaceRegister.OutInterface(ICMLCLGenerator, LCLGenerator);
  FProxyObj := TControl(LCLGenerator.NewComponent('TControl', nil));
end;

procedure TWPControl.AWMessage(var Message: TAWMessage);
begin
  inherited AWMessage(Message);
  if Message.Msg = 'Color' then
    begin
      Self.Color := Message.RInt;
    end
  else if Message.Msg = 'Left' then
    begin
      Self.Left := Message.RInt;
    end
  else if Message.Msg = 'Top' then
    begin
      Self.Top := Message.RInt;
    end
  else if Message.Msg = 'Width' then
    begin
      Self.Width := Message.RInt;
    end;
end;

{ TWPForm }

constructor TWPForm.Create;
begin
  InterfaceRegister.OutInterface(ICMLCLGenerator, LCLGenerator);
  FProxyObj := TForm(LCLGenerator.NewComponent('TForm', nil));
end;

procedure TWPForm.AWMessage(var Message: TAWMessage);
begin
  inherited AWMessage(Message);
  if Message.Msg = 'ShowModal' then
    begin
      Message.RInt := ShowModal;
    end;
end;

function TWPForm.ShowModal: Integer;
begin
  Result := TForm(FProxyObj).ShowModal;
end;

{ TAWWidgetSet }

function TAWWidgetSet.GetWPObject(const AAWClass: TAWObjectClass): TWPObject;
begin
  Messager.Info('aaaaaaaaaaaaaaaaaaaaaa');
  if AAWClass.ClassName = 'TAWForm' then
    Result := TWPForm.Create
  else
    Result := TWPObject.Create;
end;

initialization
  AWWidgetSet := TAWWidgetSet.Create;




end.

