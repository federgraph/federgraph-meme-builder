unit RiggVar.App.Main0;

(*
-
-     F
-    * * *
-   *   *   G
-  *     * *   *
- E - - - H - - - I
-  *     * *         *
-   *   *   *           *
-    * *     *             *
-     D-------A---------------B
-              *
-              (C) federgraph.de
-
*)

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.UIConsts,
  FMX.Layouts,
  RiggVar.FB.Action,
  RiggVar.FB.ActionConst,
  RiggVar.FB.ActionGroups,
  RiggVar.FB.ActionTable,
  RiggVar.FB.ActionKeys,
  RiggVar.FB.ActionMap,
  RiggVar.FB.ActionTest,
  RiggVar.FB.TextBase,
  RiggVar.FederModel.Action,
  RiggVar.FederModel.Binding,
  RiggVar.FederModel.TouchBase,
  RiggVar.FederModel.Touch,
  RiggVar.FederModel.TouchPhone,
  RiggVar.Util.Logger;

type
  TMain0 =  class
  private
    FTouch: Integer;
    procedure InitRaster;
    function GetIsLandscape: Boolean;
    function GetIsPortrait: Boolean;
    function GetColorScheme: Integer;
    procedure SetColorScheme(const Value: Integer);
    procedure InitFederText(ft: TFederTouch0);
    function GetIsPhone: Boolean;
    procedure SetTouch(const Value: Integer);
    function GetFederText: TFederTouchBase;
    function GetFLText: string;
  protected
    FL: TStringList;
    procedure CopyText;
  public
    ActionMap1: TActionMap;
    ActionMap2: TActionMap;
    ActionHandler: IFederActionHandler;
    ActionHelper: TActionHelper;

    IsUp: Boolean;

    Layout: TLayout;

    FederText1: TFederTouch;
    FederText2: TFederTouchPhone;

    FederKeyboard: TFederKeyboard;
    BackgroundLock: Boolean;

    Logger: TLogger;

    ActionGroupList: TActionGroupList;
    ActionTest: TActionTest;
    FederBinding: TFederBinding;

    constructor Create;
    destructor Destroy; override;

    procedure ExecuteAction(fa: Integer);
    function GetChecked(fa: TFederAction): Boolean;

    procedure Init;
    procedure InitText;

    procedure DoTouchbarLeft(Delta: single);
    procedure DoTouchbarRight(Delta: single);
    procedure DoTouchbarBottom(Delta: single);
    procedure DoTouchbarTop(Delta: single);

    procedure CycleToolSet(i: Integer); virtual;
    procedure CycleColorSchemeM; virtual;
    procedure CycleColorSchemeP; virtual;

    procedure BlackText;
    procedure GrayText;
    procedure WhiteText;

    procedure InitTouch;
    procedure UpdateTouch;

    procedure UpdateText(ClearFlash: Boolean = False);

    property IsPhone: Boolean read GetIsPhone;
    property IsLandscape: Boolean read GetIsLandscape;
    property IsPortrait: Boolean read GetIsPortrait;

    property ColorScheme: Integer read GetColorScheme write SetColorScheme;
    property Touch: Integer read FTouch write SetTouch;

    property ActionMapTablet: TActionMap read ActionMap1;
    property ActionMapPhone: TActionMap read ActionMap2;

    property Keyboard: TFederKeyboard read FederKeyboard;
    property FederText: TFederTouchBase read GetFederText;
    property FLText: string read GetFLText;
  end;

implementation

uses
  FrmMeme,
  FMX.Platform,
  RiggVar.App.Main,
  RiggVar.MB.Def,
  RiggVar.FB.ActionShort,
  RiggVar.FB.ActionLong,
  RiggVar.FederModel.Keyboard01,
  RiggVar.FederModel.ActionMapPhone,
  RiggVar.FederModel.ActionMapTablet;

{ TMain0 }

constructor TMain0.Create;
begin
  Main := self;

  FL := TStringList.Create;
  Logger := TLogger.Create;

  ActionGroupList := TActionGroupList.Create;
  ActionTest := TActionTest.Create;

  ActionMap1 := TActionMapTablet.Create;
  ActionMap2 := TActionMapPhone.Create;

  InitRaster;

  TTouchBtn.WantHint := True;
  FederText1 := TFederTouch.Create(nil);
  FederText2 := TFederTouchPhone.Create(nil);
  FederKeyboard := TFederKeyboard01.Create;
  FederBinding := TFederBinding.Create;

  ActionHandler := TFederActionHandler.Create;
  ActionHandler.CheckForDuplicates(FL);
  ActionHelper := TActionHelper.Create(ActionHandler);
end;

destructor TMain0.Destroy;
begin
  ActionHelper.Free;
  ActionMap1.Free;
  ActionMap2.Free;

  FederKeyboard.Free;
  FederText1.Free;
  FederText2.Free;

  Logger.Free;
  FL.Free;

  ActionGroupList.Free;
  ActionTest.Free;
  FederBinding.Free;

  inherited;
end;

procedure TMain0.CopyText;
var
  cbs: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, IInterface(cbs)) then
  begin
    cbs.SetClipboard(FL.Text);
    Logger.Info('in CopyText ( check clipboard )');
  end;
end;

procedure TMain0.BlackText;
begin
  MainVar.ColorScheme.BlackText;
  FederText.UpdateColorScheme;
end;

procedure TMain0.WhiteText;
begin
  MainVar.ColorScheme.WhiteText;
  FederText.UpdateColorScheme;
end;

procedure TMain0.GrayText;
begin
  MainVar.ColorScheme.GrayText;
  FederText.UpdateColorScheme;
end;

procedure TMain0.Init;
begin
  InitText;
end;

procedure TMain0.InitFederText(ft: TFederTouch0);
begin
  if ft is TLayout then
  begin
    ft.Parent := FormMeme;
    TFederTouchBase.OwnerComponent := ft;
    TFederTouchBase.ParentObject := ft;
  end
  else
  begin
    TFederTouchBase.OwnerComponent := FormMeme;
    TFederTouchBase.ParentObject := FormMeme;
  end;

  ft.Position.X := 0;
  ft.Position.Y := 0;
  ft.Width := MainVar.ClientWidth;
  ft.Height := MainVar.ClientHeight;
  ft.Init;
end;

procedure TMain0.InitRaster;
begin
  MainVar.ClientWidth := FormMeme.ClientWidth;
  MainVar.ClientHeight := FormMeme.ClientHeight;
end;

procedure TMain0.InitText;
begin
  MainVar.ClientWidth := FormMeme.ClientWidth;
  MainVar.ClientHeight := FormMeme.ClientHeight;
  InitFederText(FederText1);
  InitFederText(FederText2);
  Touch := faTouchDesk;
end;

procedure TMain0.InitTouch;
begin
  InitRaster;
  FederText1.Visible := not IsPhone;
  FederText2.Visible := IsPhone;
end;

procedure TMain0.UpdateText;
begin
  FederText.UpdateText;
end;

procedure TMain0.UpdateTouch;
begin
  if Assigned(FederText) and FederText.InitOK then
  begin
    MainVar.ClientWidth := FormMeme.ClientWidth;
    MainVar.ClientHeight := FormMeme.ClientHeight;
    InitTouch;
    FederText.UpdateShape;
  end;
end;

function TMain0.GetIsLandscape: Boolean;
begin
  result := FormMeme.ClientWidth >= FormMeme.ClientHeight;
end;

function TMain0.GetIsPhone: Boolean;
var
  MinCount, MaxCount: Integer;
begin
  case FTouch of
    faTouchPhone: result := True;
    faTouchTablet: result := False;
    else
    begin
      MinCount := Min(FormMeme.ClientHeight, FormMeme.ClientWidth) div MainVar.Raster;
      MaxCount := Max(FormMeme.ClientHeight, FormMeme.ClientWidth) div MainVar.Raster;
      result  := (MinCount < 8) or (MaxCount < 12);
    end;
  end;
end;

function TMain0.GetIsPortrait: Boolean;
begin
  result := FormMeme.ClientWidth < FormMeme.ClientHeight;
end;

procedure TMain0.SetColorScheme(const Value: Integer);
begin
  if not BackgroundLock then
  begin
    MainVar.ColorScheme.Scheme := Value;
    MainVar.ColorScheme.Init(Value);
    if MainVar.ColorScheme.claBackground = claNull then
      BlackText;
    FormMeme.UpdateBackgroundColor(MainVar.ColorScheme.claBackground);
    FederText.UpdateColorScheme;
  end;
end;

procedure TMain0.SetTouch(const Value: Integer);
begin
  FTouch := Value;

  if IsPhone then
    FederText1.Visible := False
  else case FTouch of
    faTouchTablet: FederText1.Visible := True;
    faTouchPhone: FederText1.Visible := False;
    else
      FederText1.Visible := not IsPhone;
  end;
  FederText2.Visible := not FederText1.Visible;

  FederText.UpdateShape;
end;

procedure TMain0.CycleColorSchemeM;
var
  i: Integer;
  l: Boolean;
begin
  l := BackgroundLock;
  BackgroundLock := false;
  i := ColorScheme;
  Dec(i);
  if (i < 1) then
    i := ColorSchemeCount;
  if i > ColorSchemeCount then
    i := 1;

  MainVar.ColorScheme.SchemeDefault := i;
  ColorScheme := i;
  BackgroundLock := l;
end;

procedure TMain0.CycleColorSchemeP;
var
  i: Integer;
  l: Boolean;
begin
  l := BackgroundLock;
  BackgroundLock := false;
  i := ColorScheme;
  Inc(i);
  if (i < 1) then
    i := ColorSchemeCount;
  if i > ColorSchemeCount then
    i := 1;

  MainVar.ColorScheme.SchemeDefault := i;
  ColorScheme := i;
  BackgroundLock := l;
end;

procedure TMain0.CycleToolSet(i: Integer);
begin
  FederText.UpdateToolSet(i);
  FormMeme.UpdateReport;
end;

function TMain0.GetColorScheme: Integer;
begin
  result := MainVar.ColorScheme.Scheme;
end;

function TMain0.GetFederText: TFederTouchBase;
begin
  case FTouch of
    faTouchTablet: result := FederText1;
    faTouchPhone: result := FederText2;
    faTouchDesk:
    begin
      if IsPhone then
        result := FederText2
      else
        result := FederText1;
    end;
    else
      result := FederText1;
  end;
end;

function TMain0.GetFLText: string;
begin
  result := FL.Text;
end;

procedure TMain0.ExecuteAction(fa: Integer);
begin
  if IsUp then
  case fa of
    faToggleTouchFrame: FederText.ToggleTouchFrame;

    faActionPageM: CycleToolSet(-1);
    faActionPageP: CycleToolSet(1);

    faCycleColorSchemeM: CycleColorSchemeM;
    faCycleColorSchemeP: CycleColorSchemeP;

    else
      FormMeme.HandleAction(fa);
  end;
end;

function TMain0.GetChecked(fa: TFederAction): Boolean;
var
  F: TFormMeme;
begin
  F := FormMeme;
  result := false;
  if not IsUp then
    Exit;

  case fa of
    faToggleTouchFrame: result := FederText.FrameVisible;

    faMemeParamTopSize: result := F.Param = fpTopSize;
    faMemeParamBottomSize: result := F.Param = fpBottomSize;

    faMemeParamTopGlow: result := F.Param = fpTopGlow;
    faMemeParamBottomGlow: result := F.Param = fpBottomGlow;

    faMemeParamTopMargin: result := F.Param = fpTopMargin;
    faMemeParamBottomMargin: result := F.Param = fpBottomMargin;

    faMemeSelectTop: result := F.SelectedText = TSelectedText.stTop;
    faMemeSelectBottom: result := F.SelectedText = TSelectedText.stBottom;

    faMemeSample00: result := F.ActiveSampleManagerID = 0;
    faMemeSample01: result := F.ActiveSampleManagerID = 1;
    faMemeSample02: result := F.ActiveSampleManagerID = 2;

    faMemeSampleT: result := F.SampleIndex = 1;

    faMemeToggleEdits: result := F.TopEdit.Visible;
    faMemeToggleHelp: result := F.HelpText.Visible;
    faMemeToggleReport: result := F.ReportText.Visible;
    faButtonFrameReport: result := F.WantButtonFrameReport;
    faMemeToggleDropTarget: result := F.IsDropTargetVisible;
    else
      result := F.GetChecked(fa);
  end;

end;

procedure TMain0.DoTouchbarLeft(Delta: single);
begin
end;

procedure TMain0.DoTouchbarTop(Delta: single);
begin
end;

procedure TMain0.DoTouchbarRight(Delta: single);
begin
  FormMeme.HandleWheel(Round(Sign(Delta)));
end;

procedure TMain0.DoTouchbarBottom(Delta: single);
begin
end;

end.
