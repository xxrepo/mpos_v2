{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_forms

    This is not a complete unit, for testing

 **********************************************************************}

unit cm_forms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms,
  cm_interfaces,
  cm_theme;

type

  { TCMForm }

  TCMForm = class(TForm, ICMBase)
  public
    function GetImplementorName: string; virtual;
  end;

  { TCMThemeForm }

  TCMThemeForm = class(TCMForm, IThemeable)
  protected
    FTheme: ITheme;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetTheme(ATheme: ITheme); virtual;
  end;

implementation

{ TCMForm }

function TCMForm.GetImplementorName: string;
begin
  Result := Self.UnitName + '.' + Self.ClassName;
end;

{ TCMThemeForm }

constructor TCMThemeForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  TThemeableManager.GetInstance.AddThemeable(Self);
  FTheme := nil;
end;

destructor TCMThemeForm.Destroy;
begin
  TThemeableManager.GetInstance.RemoveThemeable(Self);
  FTheme := nil;
  inherited Destroy;
end;

procedure TCMThemeForm.SetTheme(ATheme: ITheme);
begin
  FTheme := ATheme;
end;


end.

