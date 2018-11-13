


unit Unit1;

{$mode objfpc}{$H+}
{$inline on}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  cm_freegenerics;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure println(msg: string);
  end;

  IA = interface
    ['{53E8396C-0D35-457A-A107-5598EE237B5E}']
    procedure aa;
  end;

  { TA }

  TA = class(TInterfacedObject, IA)
  public
    constructor Create;
    destructor Destroy; override;
    procedure aa;
  end;

  TXList = class(specialize TGFPList<IA>);
  //TXList = class(specialize TGInterfaceList<IA>);
  //TXList = class(TInterfaceList);



var
  Form1: TForm1;

  fpList: TXList;



  //s: specialize TCMFPList<TObject>;

implementation

{$R *.frm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  fpList := TXList.Create;
  //hsList := THList.Create;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  a: IA;
begin
  a := TA.Create;
  a.aa;
  fpList.Add(a);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  fpList.Clear;
end;

procedure TForm1.println(msg: string);
begin
  Memo1.Lines.Add(msg);
end;

{ TA }

constructor TA.Create;
begin
  Form1.println('TA.Create');
end;

destructor TA.Destroy;
begin
  if Assigned(Form1) then
    if Form1.Showing then
      Form1.println('TA.Destroy');
  inherited Destroy;
end;

procedure TA.aa;
begin
  Form1.println('TA.aa');
end;

end.

