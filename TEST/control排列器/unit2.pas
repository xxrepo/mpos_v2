unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Contnrs, ExtCtrls;

type

  { TCMRowArrangeManager }

  TCMRowArrangeManager = class(TComponent)
  private
    FContainer: TWinControl;
    FRowClass: TControlClass;
    FControlList: TFPHashObjectList;
    procedure setContainer(AValue: TWinControl);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Container: TWinControl read FContainer write setContainer;
    //property ShowCount: Integer;
    function AddRow: Integer;
  end;

implementation

{ TCMRowArrangeManager }

procedure TCMRowArrangeManager.setContainer(AValue: TWinControl);
begin
  if FContainer = AValue then
    Exit;
  FContainer := AValue;
end;

constructor TCMRowArrangeManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FContainer := nil;
  FRowClass := TPanel;
  FControlList := TFPHashObjectList.Create(True);
end;

destructor TCMRowArrangeManager.Destroy;
begin
  FControlList.Free;
  inherited Destroy;
end;

function TCMRowArrangeManager.AddRow: Integer;
var
  c: TControl;
begin
  Result := -1;
  if Assigned(FRowClass) then
    begin
      c := FRowClass.Create(Self);
      if Assigned(Container) then
        begin
          c.Parent := Container;
          c.Left := 0;
          c.Width := Container.Width;
        end;
      Result := FControlList.Add(IntToStr(c.GetHashCode), c);
    end;
end;

end.

