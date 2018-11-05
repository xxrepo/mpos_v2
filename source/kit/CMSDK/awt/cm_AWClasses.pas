unit cm_AWClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_awt;

type

  { TAWControl }

  TAWControl = class(TAWObject)
  private
    function GetColor: TAWColor;
    function GetHeight: Integer;
    function GetLeft: Integer;
    function GetTop: Integer;
    function GetWidth: Integer;
    procedure SetColor(AValue: TAWColor);
    procedure SetHeight(AValue: Integer);
    procedure SetLeft(AValue: Integer);
    procedure SetTop(AValue: Integer);
    procedure SetWidth(AValue: Integer);
  public
    property Color: TAWColor read GetColor write SetColor;
    property Left: Integer read GetLeft write SetLeft;
    property Height: Integer read GetHeight write SetHeight;
    property Top: Integer read GetTop write SetTop;
    property Width: Integer read GetWidth write SetWidth;
  end;

  { TAWForm }

  TAWForm = class(TAWControl)
  public
    function ShowModal: Integer;
  end;

implementation

{ TAWControl }

function TAWControl.GetColor: TAWColor;
begin
  Result := GetWPInteger('Color');
end;

function TAWControl.GetHeight: Integer;
begin
  Result := GetWPInteger('Height');
end;

function TAWControl.GetLeft: Integer;
begin
  Result := GetWPInteger('Left');
end;

function TAWControl.GetTop: Integer;
begin
  Result := GetWPInteger('Top');
end;

function TAWControl.GetWidth: Integer;
begin
  Result := GetWPInteger('Width');
end;

procedure TAWControl.SetColor(AValue: TAWColor);
begin
  SetWPInteger('Color', AValue);
end;

procedure TAWControl.SetHeight(AValue: Integer);
begin
  SetWPInteger('Height', AValue);
end;

procedure TAWControl.SetLeft(AValue: Integer);
begin
  SetWPInteger('Left', AValue);
end;

procedure TAWControl.SetTop(AValue: Integer);
begin
  SetWPInteger('Top', AValue);
end;

procedure TAWControl.SetWidth(AValue: Integer);
begin
  SetWPInteger('Width', AValue);
end;

{ TAWForm }

function TAWForm.ShowModal: Integer;
var
  m: TAWMessage;
begin
  m := TAWMessage.Create;
  m.Msg := 'ShowModal';
  WPObject.AWMessage(m);
  Result := m.RInt;
  m.Free;
end;



end.

