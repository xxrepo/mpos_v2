unit uSocketArrayofObject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Contnrs,typinfo;

Type
  TBaseDefine=Class(TObject)

  end;

  TBaseDefineClass=Class of TBaseDefine;


  { TBaseArrayOfObject }
  TBaseArrayOfObject=Class;
  TBaseArrayOfObjectClass=Class of TBaseArrayOfObject;

  TBaseArrayOfObject=Class
  Protected
    FItemList:TObjectList;
    function getLength():integer;virtual;
    function GetItem(index:integer):TObject;virtual;
  Public
    Constructor Create();
    Destructor Destroy; override;

    class function GetItemClass():TClass;virtual;abstract;
    class function GetItemTypeInfo():PTypeInfo;virtual;abstract;

    function Add(): TObject; virtual;
    function AddAt(const APosition: integer): TObject; virtual;
    property Length:Integer read getLength;
    property Item[AIndex: integer]: TObject read GetItem;
  end;

  {下面三个类是使用的例子,分别是: TBaseRequest,ArrayOfBaseRequest,GetCommandRequest }

  { TBaseRequest }
  TBaseRequest = class(TBaseDefine)
  private
    FRequestId: UnicodeString;
    FCompanyCode: UnicodeString;
    FShopCode: UnicodeString;
    FMacAddr: UnicodeString;
    FTerminalNo: UnicodeString;
    FAuthCode: UnicodeString;
  published
    property PartnerCode: UnicodeString read FRequestId write FRequestId ;//stored wstHas_RequestId;
    property Timestamp: UnicodeString read FCompanyCode write FCompanyCode;// stored wstHas_CompanyCode;
    property StoreCode: UnicodeString read FShopCode write FShopCode;// stored wstHas_ShopCode;
    property MacAddr: UnicodeString read FMacAddr write FMacAddr;// stored wstHas_MacAddr;
    property Nonce: UnicodeString read FTerminalNo write FTerminalNo;// stored wstHas_TerminalNo;
    property Sign: UnicodeString read FAuthCode write FAuthCode;// stored wstHas_AuthCode;
  end;


  { ArrayOfBaseRequest }

  ArrayOfBaseRequest = class(TBaseArrayOfObject)
  private
    function GetItem(AIndex: integer): TBaseRequest; override;
  public
    class function GetItemClass(): TClass; override;
    class function GetItemTypeInfo():PTypeInfo;override;
    function Add(): TBaseRequest; override;// {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition: integer): TBaseRequest; override;//{$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex: integer]: TBaseRequest read GetItem;
  end;
  TArrayOfBaseRequest=Class of ArrayOfBaseRequest;


  { GetCommandRequest }

  GetCommandRequest = class(TBaseRequest)
  Private
    FRequest:TBaseRequest;
    FTask: ArrayOfBaseRequest;
  Public
    Constructor Create();
    Destructor Destroy; override;
  Published
    Property Request:TBaseRequest read FRequest write FRequest;
    Property Task:ArrayOfBaseRequest read FTask write FTask;
  end;



implementation

{ ArrayOfBaseRequest }

function ArrayOfBaseRequest.GetItem(AIndex: integer): TBaseRequest;
begin
  Result:=TBaseRequest(inherited GetItem(AIndex));
end;

class function ArrayOfBaseRequest.GetItemClass: TClass;
begin
  Result:= TBaseRequest;
end;

class function ArrayOfBaseRequest.GetItemTypeInfo: PTypeInfo;
begin
  Result:= TBaseRequest.ClassInfo;
end;

function ArrayOfBaseRequest.Add: TBaseRequest;
begin
  Result:=TBaseRequest(inherited Add);
end;

function ArrayOfBaseRequest.AddAt(const APosition: integer): TBaseRequest;
begin
  Result:=TBaseRequest(inherited AddAt(APosition));
end;

{ TBaseArrayOfObject }

function TBaseArrayOfObject.getLength: integer;
begin
 Result:=FItemList.Count;
end;

function TBaseArrayOfObject.GetItem(index: integer): TObject;
begin
  Result:=nil;
  if (index>=FItemList.Count) or (index<0) then

  else
    Result:=FItemList.Items[index];
end;

constructor TBaseArrayOfObject.Create;
begin
  inherited;
  FItemList:=TObjectList.Create(True);
end;

destructor TBaseArrayOfObject.Destroy;
begin
  FItemList.Free;
  inherited Destroy;
end;


function TBaseArrayOfObject.Add: TObject;
begin
  Result:= GetItemClass().Create();
  FItemList.Add(Result);
end;

function TBaseArrayOfObject.AddAt(const APosition: integer): TObject;
begin
  Result:= GetItemClass().Create();
  FItemList.Insert(APosition,Result);
end;

{ GetCommandRequest }

constructor GetCommandRequest.Create;
begin
  Inherited;
  FTask:= ArrayOfBaseRequest.Create();
  FRequest:=TBaseRequest.Create;
end;

destructor GetCommandRequest.Destroy;
begin
  FRequest.Free;
  FTask.Free;
  inherited Destroy;
end;

end.

