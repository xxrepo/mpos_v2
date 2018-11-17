unit cm_AWTLayoutUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs,
  cm_AWT;

type

  { TALayoutManager }

  TALayoutManager = class abstract(TComponent)
  private
    FContainer: TAControl;
    FBorderSpacing: Integer;
    FLeftSpacing: Integer;
    FTopSpacing: Integer;
    procedure SetBorderSpacing(AValue: Integer);
    procedure SetContainer(AValue: TAControl);
    procedure SetLeftSpacing(AValue: Integer);
    procedure SetTopSpacing(AValue: Integer);
  protected
    FItems: TFPHashObjectList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Container: TAControl read FContainer write SetContainer; //布置指定容器
  protected type
    TLayoutItem = class
      FControl: TAControl;
    end;
    TLayoutItemClass = class of TLayoutItem;
  protected
    FItemClass: TLayoutItemClass;
    function CreateItem(AControl: TAControl): TLayoutItem;
    function AddItem(AItem: TLayoutItem): Integer;
  protected
    property BorderSpacing: Integer read FBorderSpacing write SetBorderSpacing;
    property LeftSpacing: Integer read FLeftSpacing write SetLeftSpacing;
    property TopSpacing: Integer read FTopSpacing write SetTopSpacing;
  public
    function AddLayoutControl(AControl: TAControl): Boolean;
    function PutLayoutControl(AControl: TAControl): Boolean;
    procedure RemoveLayoutControl(AControl: TAControl); virtual;
    function Count: Integer;
    procedure Clear; virtual;
    procedure ReLayout; virtual; abstract;
  end;

  TControlOrientation = (coLeftToRight, coRightToLeft, coLeftToBottom, coRightToBottom);

  { TAFlowLayout
    // 流布局用于安排有向流中的组件，这非常类似于段落中的文本行。流的方向取决于容器的 ControlOrientation 属性
  }

  TAFlowLayout = class(TALayoutManager)
  private
    FControlOrientation: TControlOrientation;
    FLineMaxCount: Integer;
    procedure SetControlOrientation(AValue: TControlOrientation);
    procedure SetLineMaxCount(AValue: Integer);
  public
    constructor Create(AOwner: TComponent); override;
  protected type
    TFlowLayoutItem = class(TLayoutItem)
      FNewLine: Boolean; //用于指定新行
      constructor Create;
    end;
  public
    property BorderSpacing;
    property LeftSpacing;
    property TopSpacing;
    property ControlOrientation: TControlOrientation read FControlOrientation write SetControlOrientation;
    property LineMaxCount: Integer read FLineMaxCount write SetLineMaxCount; //流动线中的最大布局数量
    function PutLayoutControl(AControl: TAControl; IsNewLine: Boolean): Boolean; overload;
    procedure ReLayout; override;
  end;

implementation

{ TALayoutManager }

procedure TALayoutManager.SetContainer(AValue: TAControl);
begin
  if FContainer = AValue then
    Exit;
  FContainer := AValue;
  ReLayout;
end;

procedure TALayoutManager.SetBorderSpacing(AValue: Integer);
begin
  if FBorderSpacing = AValue then
    Exit;
  FBorderSpacing := AValue;
  ReLayout;
end;

procedure TALayoutManager.SetLeftSpacing(AValue: Integer);
begin
  if FLeftSpacing = AValue then
    Exit;
  FLeftSpacing := AValue;
  ReLayout;
end;

procedure TALayoutManager.SetTopSpacing(AValue: Integer);
begin
  if FTopSpacing = AValue then
    Exit;
  FTopSpacing := AValue;
  ReLayout;
end;

constructor TALayoutManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FContainer := nil;
  FItemClass := TLayoutItem;
  FItems := TFPHashObjectList.Create(True);
  FBorderSpacing := 10;
  FLeftSpacing := 10;
  FTopSpacing := 10;
end;

destructor TALayoutManager.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TALayoutManager.CreateItem(AControl: TAControl): TLayoutItem;
begin
  Result := nil;
  if FItems.FindIndexOf(IntToStr(AControl.GetHashCode)) >= 0 then
    Exit;
  Result := FItemClass.Create;
  Result.FControl := AControl;
end;

function TALayoutManager.AddItem(AItem: TLayoutItem): Integer;
begin
  Result := FItems.Add(IntToStr(AItem.FControl.GetHashCode), AItem);
end;

function TALayoutManager.AddLayoutControl(AControl: TAControl): Boolean;
begin
  Result := PutLayoutControl(AControl);
  if Result then
    ReLayout;
end;

function TALayoutManager.PutLayoutControl(AControl: TAControl): Boolean;
var
  item: TLayoutItem;
begin
  Result := False;
  if Assigned(AControl) then
    begin
      item := Self.CreateItem(AControl);
      if Assigned(item) then
        Self.AddItem(item);
      Result := True;
    end;
end;

procedure TALayoutManager.RemoveLayoutControl(AControl: TAControl);
var
  i: Integer;
begin
  i := FItems.FindIndexOf(IntToStr(AControl.GetHashCode));
  if i >= 0 then
    FItems.Delete(i);
end;

function TALayoutManager.Count: Integer;
begin
  Result := FItems.Count;
end;

procedure TALayoutManager.Clear;
begin
  FItems.Clear;
  ReLayout;
end;

{ TAFlowLayout.TFlowLayoutItem }

constructor TAFlowLayout.TFlowLayoutItem.Create;
begin
  FNewLine := False;
end;

{ TAFlowLayout }

procedure TAFlowLayout.SetControlOrientation(AValue: TControlOrientation);
begin
  if FControlOrientation = AValue then
    Exit;
  FControlOrientation := AValue;
  ReLayout;
end;

procedure TAFlowLayout.SetLineMaxCount(AValue: Integer);
begin
  if FLineMaxCount = AValue then
    Exit;
  FLineMaxCount := AValue;
  ReLayout;
end;

constructor TAFlowLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemClass := TFlowLayoutItem;
  FControlOrientation := coLeftToRight;
  FLineMaxCount := 256;
end;

function TAFlowLayout.PutLayoutControl(AControl: TAControl; IsNewLine: Boolean): Boolean;
var
  item: TFlowLayoutItem;
begin
  Result := False;
  if Assigned(AControl) then
    begin
      item := TFlowLayoutItem(Self.CreateItem(AControl));
      if Assigned(item) then
        begin
          Self.AddItem(item);
          item.FNewLine := IsNewLine;
          Result := True;
        end;
    end;
end;

procedure TAFlowLayout.ReLayout;
var
  i: Integer;
  theItem: TFlowLayoutItem;
  theControl: TAControl;
  tabOrderInt: Integer;
  //位置相关
  nextLineX, nextLineY: Integer;
  x, y: Integer;
  //
  currMaxLineMark: Integer;
  lineControlCount: Integer;
begin
  if not Assigned(FContainer) then
    Exit;
  if (FContainer is TAWinControl) and (not TAWinControl(FContainer).Showing) then
    Exit;
  tabOrderInt := 0;
  currMaxLineMark := 0;
  lineControlCount := 0;
  //
  for i:=0 to FItems.Count-1 do
    begin
      theItem := TFlowLayoutItem(FItems[i]);
      theControl := theItem.FControl;
      if Assigned(theControl) then
        begin
          if theControl is TAWinControl then
            begin
              TAWinControl(theControl).TabOrder := tabOrderInt;
              tabOrderInt := tabOrderInt + 1;
            end;
          //
          if ControlOrientation = coLeftToRight then
            begin
              if i = 0 then
                begin
                  nextLineX := BorderSpacing;
                  nextLineY := BorderSpacing;
                end
              else if theItem.FNewLine or (nextLineX + theControl.Width + BorderSpacing > FContainer.Width) or (lineControlCount mod LineMaxCount = 0) then
                begin
                  nextLineX := BorderSpacing;
                  nextLineY := nextLineY + currMaxLineMark + TopSpacing;
                  currMaxLineMark := 0;
                  lineControlCount := 0;
                end;
              x := nextLineX;
              nextLineX := x + theControl.Width + LeftSpacing;
              y := nextLineY;
              //
              if theControl.Height > currMaxLineMark then
                currMaxLineMark := theControl.Height;
            end
          else if ControlOrientation = coRightToLeft then
            begin
              if i = 0 then
                begin
                  nextLineX := FContainer.Width - BorderSpacing;
                  nextLineY := BorderSpacing;
                end
              else if theItem.FNewLine or (nextLineX < theControl.Width + BorderSpacing) or (lineControlCount mod LineMaxCount = 0) then
                begin
                  nextLineX := FContainer.Width - BorderSpacing;
                  nextLineY := nextLineY + currMaxLineMark + TopSpacing;
                  currMaxLineMark := 0;
                  lineControlCount := 0;
                end;
              x := nextLineX - theControl.Width;
              nextLineX := x - LeftSpacing;
              y := nextLineY;
              //
              if theControl.Height > currMaxLineMark then
                currMaxLineMark := theControl.Height;
            end
          else if ControlOrientation = coLeftToBottom then
            begin
              if i = 0 then
                begin
                  nextLineX := BorderSpacing;
                  nextLineY := BorderSpacing;
                end
              else if theItem.FNewLine or (nextLineY + theControl.Height + BorderSpacing > FContainer.Height) or (lineControlCount mod LineMaxCount = 0) then
                begin
                  nextLineX := nextLineX + currMaxLineMark + LeftSpacing;
                  nextLineY := BorderSpacing;
                  currMaxLineMark := 0;
                  lineControlCount := 0;
                end;
              x := nextLineX;
              y := nextLineY;
              nextLineY := y + theControl.Height + TopSpacing;
              //
              if theControl.Width > currMaxLineMark then
                currMaxLineMark := theControl.Width;
            end
          else if ControlOrientation = coRightToBottom then
            begin
              if i = 0 then
                begin
                  nextLineX := FContainer.Width - BorderSpacing;
                  nextLineY := BorderSpacing;
                end
              else if theItem.FNewLine or (nextLineY + theControl.Height + BorderSpacing > FContainer.Height) or (lineControlCount mod LineMaxCount = 0) then
                begin
                  nextLineX := nextLineX - currMaxLineMark - LeftSpacing;
                  nextLineY := BorderSpacing;
                  currMaxLineMark := 0;
                  lineControlCount := 0;
                end;
              x := nextLineX - theControl.Width;
              y := nextLineY;
              nextLineY := y + theControl.Height + TopSpacing;
              //
              if theControl.Width > currMaxLineMark then
                currMaxLineMark := theControl.Width;
            end;
          theControl.Left := x;
          theControl.Top := y;
          lineControlCount := lineControlCount + 1;
        end;
    end;
end;

end.

