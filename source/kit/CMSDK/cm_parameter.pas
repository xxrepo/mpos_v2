{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_parameter

    This is not a complete unit, for testing

 **********************************************************************}

unit cm_parameter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB,
  cm_interfaces, cm_DOM;

type

  EParameterError = class(Exception);

  TParameterDataType = (pdtUnknown, pdtNumber, pdtBoolean, pdtCurrency, pdtDateTime, pdtFloat, pdtInteger, pdtLargeInt, pdtString, pdtObject, pdtInterface, pdtPointer);

  { ICMParameterBaseData }

  ICMParameterBaseData = interface(ICMBase)
    ['{0BB761D1-25DC-47BD-9711-1367DDCA931C}']
    procedure Clear;
    function DataType: TParameterDataType;
    function IsNull: Boolean;
    function AsBoolean: Boolean;
    function AsDateTime: TDateTime;
    function AsCurrency: Currency;
    function AsFloat: Double;
    function AsInteger: Integer;
    function AsLargeInt: Int64;
    function AsString: string;
  end;

  { ICMParameterData }

  ICMParameterData = interface(ICMParameterBaseData)
    ['{BD20A499-00A0-4FBF-A6D0-10DA3B112D57}']
    function AsObject: TObject;
    function AsInterface: IUnknown;
    function AsPointer: Pointer;
  end;

  { ICMParameter }

  ICMParameter = interface(ICMParameterData)
    ['{00D5E38D-9A00-4AC3-8C3C-24F25689B95B}']
    function Id: Integer;
    function ParentId: Integer;
    function Level: Integer;
    function Name: string;
    function Clue: string;
    function ItemCount: Integer;
    function ItemIndex(const AName: string): Integer;
    function GetItem(AIndex: Integer): ICMParameter;
    procedure RemoveItem(const AName: string);
    procedure RemoveItems;
    function Get(const AParameterName: string): ICMParameter;
    //
    function AddBoolean(const AName: string; AValue: Boolean): ICMParameter;
    function AddDateTime(const AName: string; AValue: TDateTime): ICMParameter;
    function AddCurrency(const AName: string; AValue: Currency): ICMParameter;
    function AddFloat(const AName: string; AValue: Double): ICMParameter;
    function AddInteger(const AName: string; AValue: Integer): ICMParameter;
    function AddLargeInt(const AName: string; AValue: Int64): ICMParameter;
    function AddString(const AName, AValue: string): ICMParameter;
    function AddObject(const AName: string; AValue: TObject): ICMParameter;
    function AddInterface(const AName: string; AValue: IUnknown): ICMParameter;
    function AddPointer(const AName: string; AValue: Pointer): ICMParameter;
    //
    function ReBoolean(AValue: Boolean): ICMParameter;
    function ReDateTime(AValue: TDateTime): ICMParameter;
    function ReCurrency(AValue: Currency): ICMParameter;
    function ReFloat(AValue: Double): ICMParameter;
    function ReInteger(AValue: Integer): ICMParameter;
    function ReLargeInt(AValue: Int64): ICMParameter;
    function ReString(const AValue: string): ICMParameter;
    function ReObject(AValue: TObject): ICMParameter;
    function ReInterface(AValue: IUnknown): ICMParameter;
    function RePointer(AValue: Pointer): ICMParameter;
  end;

  { ICMParameterLoader }

  ICMParameterLoader = interface(ICMBase)
    ['{7D0D5A93-8810-4A4A-BF2A-34E4C432CB10}']
    function AddParameters(ABase: ICMParameter; ADataSet: TDataSet): Integer; overload; //约定参数 id 为增序, id 不大于 80'000'000. dataset 以 id 正排序
    function AddParameters(ABase: ICMParameter; ANode: TCMDOMNode): Integer; overload;
  end;

  { ICMConstantParameterDataList }

  ICMConstantParameterDataList = interface(ICMBase)
    ['{CB6EA79D-C00C-47A6-BE0E-0E6A535C948B}']
    function Count: Integer;
    function Get(AIndex: Integer): ICMParameterData; overload;
    function Get(const AName: string): ICMParameterData; overload;
    function GetName(AIndex: Integer): string;
    function GetNames: TStrings;
  end;

  { ICMParameterDataList }

  ICMParameterDataList = interface(ICMConstantParameterDataList)
    ['{CC0630F9-2DD4-4664-9EF8-54DE59629B00}']
    procedure Remove(AIndex: Integer); overload;
    procedure Remove(const AName: string); overload;
    procedure Clear;
    //不存在则增加
    function SetData(const AName: string; AData: ICMParameterData): ICMParameterData;
    function SetBoolean(const AName: string; AValue: Boolean): ICMParameterData;
    function SetDateTime(const AName: string; AValue: TDateTime): ICMParameterData;
    function SetCurrency(const AName: string; AValue: Currency): ICMParameterData;
    function SetFloat(const AName: string; AValue: Double): ICMParameterData;
    function SetInteger(const AName: string; AValue: Integer): ICMParameterData;
    function SetLargeInt(const AName: string; AValue: Int64): ICMParameterData;
    function SetString(const AName, AValue: string): ICMParameterData;
    function SetObject(const AName: string; AValue: TObject): ICMParameterData;
    function SetInterface(const AName: string; AValue: IUnknown): ICMParameterData;
    function SetPointer(const AName: string; AValue: Pointer): ICMParameterData;
  end;

implementation


end.

