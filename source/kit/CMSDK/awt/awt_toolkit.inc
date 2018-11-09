

  { IAToolkit
    // 此接口是所有 Abstract Window Toolkit 实际实现的声明。
    // Toolkit 的实现被用于将各种组件绑定到特定工具包实现。
    // <br/>
    // 大多数应用程序不应直接调用该接口中的任何方法。Toolkit 定义的方法是一种“胶水”，将 awt 中
    // 与实现无关的类与 peer 中的对应物连接起来。Toolkit 定义的一些方法能直接查询本机操作系统。
    // <br/>
    // 更多思路请参阅 jdk api。
  }

  IAToolkit = interface
    ['{39A81EE2-2BBF-48D4-8389-2085F826707A}']
    function CreateCustomBitmap(ATarget: TACustomBitmap): IACustomBitmapPeer;
    function CreateCanvas(ATarget: TACanvas): IACanvasPeer;
    function CreateFont(ATarget: TAFont): IAFontPeer;
    //
    function CreateLabel(ATarget: TALabel): IALabelPeer;
    function CreatePanel(ATarget: TAPanel): IAPanelPeer;
    function CreateEdit(ATarget: TAEdit): IAEditPeer;
    function CreateForm(ATarget: TAForm): IAFormPeer;
  end;

  { TAWTManager
    // TODO 更多管理功能待加入
  }
  TAWTManager = class
  private
    //class var FManager: TAWTManager;
    class var FDefaultToolkit: IAToolkit;
    //constructor Create; //Please do not use this constructor
  public
    //destructor Destroy; override;
    //class function GetInstance: TAWTManager;
    class property DefaultToolkit: IAToolkit read FDefaultToolkit write FDefaultToolkit;
  end;

const
  DefaultToolkitCode: string = 'DEFAULT';

ResourceString
  NoToolKitError = 'No ToolKit solution specified';




