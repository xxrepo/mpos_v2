unit cm_AWTProxyToolkit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics,
  cm_messager,
  cm_AWT, cm_AWTProxy;

type

  { TProxyToolkit }

  TProxyToolkit = class(TCMMessageable, IAToolkit)
  public
    function CreateCustomBitmap(ATarget: TACustomBitmap): IACustomBitmapPeer;
    function CreateFont(ATarget: TAFont): IAFontPeer;
    function CreateCanvas(ATarget: TACanvas): IACanvasPeer;
    function CreateBorderSpacing(ATarget: TAControlBorderSpacing; OwnerControl: TAControl): IAControlBorderSpacingPeer;
    //
    function CreateLabel(ATarget: TALabel): IALabelPeer;
    function CreatePanel(ATarget: TAPanel): IAPanelPeer;
    function CreateEdit(ATarget: TAEdit): IAEditPeer;
    function CreateMemo(ATarget: TAMemo): IAMemoPeer;
    function CreateForm(ATarget: TAForm): IAFormPeer;
    function CreateFrame(ATarget: TAFrame): IAFramePeer;
    function CreateDateTimePicker(ATarget: TADateTimePicker): IADateTimePickerPeer;
    function CreateStringGrid(ATarget: TAStringGrid): IAStringGridPeer;
  end;

implementation

uses FPImage;

{ TProxyToolkit }

type
  // TGIFImage 未实现抽象方法，为消除警告，声明此私有类型
  TGIFImageEx = class(TGIFImage)
  protected
    class function GetWriterClass: TFPCustomImageWriterClass; override;
  end;
  class function TGIFImageEx.GetWriterClass: TFPCustomImageWriterClass;
  begin
    Result := nil;
  end;

function TProxyToolkit.CreateCustomBitmap(ATarget: TACustomBitmap): IACustomBitmapPeer;
var
  classStr: string;
begin
  Result := nil;
  // 无法判断类型信息
  classStr := Format('%s.%s', [ATarget.UnitName, ATarget.ClassName]);
  if classStr = 'cm_AWT.TABitmap' then
    Result := TProxyCustomBitmapPeer.Create(TBitmap.Create)
  else if classStr = 'cm_AWT.TAJPEGImage' then
    Result := TProxyCustomBitmapPeer.Create(TJPEGImage.Create)
  else if classStr = 'cm_AWT.TAGIFImage' then
    Result := TProxyCustomBitmapPeer.Create(TGIFImageEx.Create)
  else if classStr = 'cm_AWT.TAPortableNetworkGraphic' then
    Result := TProxyCustomBitmapPeer.Create(TPortableNetworkGraphic.Create);
end;

function TProxyToolkit.CreateFont(ATarget: TAFont): IAFontPeer;
begin
  Result := TProxyFontPeer.Create;
end;

function TProxyToolkit.CreateCanvas(ATarget: TACanvas): IACanvasPeer;
begin
  Result := TProxyCanvasPeer.Create;
end;

function TProxyToolkit.CreateBorderSpacing(ATarget: TAControlBorderSpacing; OwnerControl: TAControl): IAControlBorderSpacingPeer;
var
  c: TObject;
begin
  Result := nil;
  c := OwnerControl.GetPeer.GetDelegate;
  if c is TControl then
    Result := TProxyControlBorderSpacingPeer.Create(TControl(c));
end;

function TProxyToolkit.CreateLabel(ATarget: TALabel): IALabelPeer;
begin
  Result := TProxyLabelPeer.Create(ATarget, nil);
end;

function TProxyToolkit.CreatePanel(ATarget: TAPanel): IAPanelPeer;
begin
  Result := TProxyPanelPeer.Create(ATarget, nil);
end;

function TProxyToolkit.CreateEdit(ATarget: TAEdit): IAEditPeer;
begin
  Result := TProxyEditPeer.Create(ATarget, nil);
end;

function TProxyToolkit.CreateMemo(ATarget: TAMemo): IAMemoPeer;
begin
  Result := TProxyMemoPeer.Create(ATarget, nil);
end;

function TProxyToolkit.CreateForm(ATarget: TAForm): IAFormPeer;
begin
  Result := TProxyFormPeer.Create(ATarget, nil);
end;

function TProxyToolkit.CreateFrame(ATarget: TAFrame): IAFramePeer;
begin
  Result := TProxyFramePeer.Create(ATarget, nil);
end;

function TProxyToolkit.CreateDateTimePicker(ATarget: TADateTimePicker): IADateTimePickerPeer;
begin
  Result := TProxyDateTimePickerPeer.Create(ATarget, nil);
end;

function TProxyToolkit.CreateStringGrid(ATarget: TAStringGrid): IAStringGridPeer;
begin
  Result := TProxyStringGridPeer.Create(ATarget, nil);
end;

end.

