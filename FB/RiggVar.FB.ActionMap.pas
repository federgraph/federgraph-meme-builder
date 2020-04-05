unit RiggVar.FB.ActionMap;

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
  System.UITypes;

type
  TCornerLocation = (TopLeft, TopRight, BottomRight, BottomLeft);
  TInitActionProc = procedure (BtnID: Integer; Action: Integer) of object;
  TInitActionColorProc = procedure (BtnID: Integer; Action: Integer; cla: TAlphaColor) of object;

  TActionMapBase = class
  private
    FActionProc: TInitActionProc;
    FActionColorProc: TInitActionColorProc;
    procedure SetActionColorProc(const Value: TInitActionColorProc);
    procedure SetActionProc(const Value: TInitActionProc);
    procedure SetPageCount(const Value: Integer);
  protected
    FPageCount: Integer;
    TestName: string;
    procedure InitAction(BtnID: Integer; Action: Integer);
    procedure IAC(BtnID, Action: Integer; cla: TAlphaColor); virtual;
  public
    class var CurrentPageCaption: string;
    constructor Create;
    procedure InitActions(Layout: Integer); virtual;
    property ActionProc: TInitActionProc read FActionProc write SetActionProc;
    property ActionColorProc: TInitActionColorProc read FActionColorProc write SetActionColorProc;
    property PageCount: Integer read FPageCount write SetPageCount;
  end;

  TCollectibleActionMap = class(TActionMapBase)
  private
    procedure TestProcAll(BtnID, Action: Integer);
    procedure TestProcOne(BtnID, Action: Integer);
    procedure DoCollectMappings;
    procedure TestProcOneIAC(BtnID, Action: Integer; cla: TAlphaColor);
    procedure TestProcAllIAC(BtnID, Action: Integer; cla: TAlphaColor);
  protected
    TestAction: Integer;
    TestPage: Integer;
    TestList: TStrings;
    TestProc: TInitActionProc;
    TestProcIAC: TInitActionColorProc;
  public
    procedure CollectOne(fa: Integer; ML: TStrings);
    procedure CollectAll(ML: TStrings);
  end;

  TEscapableActionMap = class(TActionMapBase)
  private
    function GetEscapeIndex: Integer;
    procedure SetEscapeIndex(const Value: Integer);
    procedure SetMaxIndex(const Value: Integer);
    function GetMaxIndex: Integer;
  protected
    FMaxIndex: Integer;
    FEscapeIndex: Integer;
  public
    constructor Create;
    property MaxIndex: Integer read GetMaxIndex write SetMaxIndex;
    property EscapeIndex: Integer read GetEscapeIndex write SetEscapeIndex;
  end;

  TActionMap = TCollectibleActionMap;

implementation

uses
  RiggVar.FB.ActionShort,
  RiggVar.FB.ActionLong;

{ TActionMap }

procedure TActionMapBase.InitActions(Layout: Integer);
begin
  //virtual
end;

constructor TActionMapBase.Create;
begin
  FPageCount := 1;
end;

procedure TActionMapBase.InitAction(BtnID, Action: Integer);
begin
  if Assigned(ActionProc) then
    ActionProc(BtnID, Action);
end;

procedure TActionMapBase.IAC(BtnID, Action: Integer; cla: TAlphaColor);
begin
  if Assigned(ActionColorProc) then
    ActionColorProc(BtnID, Action, cla);
end;

procedure TActionMapBase.SetActionColorProc(const Value: TInitActionColorProc);
begin
  FActionColorProc := Value;
end;

procedure TActionMapBase.SetActionProc(const Value: TInitActionProc);
begin
  FActionProc := Value;
end;

procedure TActionMapBase.SetPageCount(const Value: Integer);
begin
  FPageCount := Value;
end;

{ TEscapableActionMap }

constructor TEscapableActionMap.Create;
begin
  inherited;
  FEscapeIndex := 2;
end;

function TEscapableActionMap.GetEscapeIndex: Integer;
begin
  if FEscapeIndex <= 0 then
    result := FPageCount + 1
  else if FEscapeIndex > FPageCount + 1 then
    result := FPageCount + 1
  else
    result := FEscapeIndex;
end;

procedure TEscapableActionMap.SetEscapeIndex(const Value: Integer);
begin
  FEscapeIndex := Value;
end;

function TEscapableActionMap.GetMaxIndex: Integer;
begin
  if (FMaxIndex > 0) and (FMaxIndex <= PageCount) then
    result := FMaxIndex
  else
    result := FPageCount;
end;

procedure TEscapableActionMap.SetMaxIndex(const Value: Integer);
begin
  FMaxIndex := Value;
end;

{ TCollectibleActionMap }

procedure TCollectibleActionMap.DoCollectMappings;
var
  p: Integer;
  iap: TInitActionProc;
  iacp: TInitActionColorProc;
  savedPageCaption: string;
begin
  savedPageCaption := CurrentPageCaption;

  iap := ActionProc;
  iacp := ActionColorProc;

  ActionProc := TestProc;
  ActionColorProc := TestProcIAC;
  { ActionMap 0 starts with 0, normal Maps start with 1 }
  for p := 0 to PageCount do
  begin
    TestPage := p;
    InitActions(p);
  end;

  ActionProc := iap;
  ActionColorProc := iacp;

  CurrentPageCaption := savedPageCaption;
end;

procedure TCollectibleActionMap.CollectAll(ML: TStrings);
begin
  TestList := ML;
  TestProc := TestProcAll;
  TestProcIAC := TestProcAllIAC;
  DoCollectMappings;
end;

procedure TCollectibleActionMap.CollectOne(fa: Integer; ML: TStrings);
begin
  TestAction := fa;
  TestList := ML;
  TestProc := TestProcOne;
  TestProcIAC := TestProcOneIAC;
  DoCollectMappings;
end;

procedure TCollectibleActionMap.TestProcOne(BtnID, Action: Integer);
begin
  if Action = TestAction then
    TestList.Add(Format('%s %d/%d', [TestName, TestPage, BtnID]));
end;

procedure TCollectibleActionMap.TestProcOneIAC(BtnID, Action: Integer; cla: TAlphaColor);
begin
  TestProcOne(BtnID, Action);
end;

procedure TCollectibleActionMap.TestProcAll(BtnID, Action: Integer);
begin
  TestList.Add(Format('%s %d/%d %s = %s', [
    TestName,
    TestPage,
    BtnID,
    GetFederActionLong(Action),
    GetFederActionShort(Action)
    ]));
end;

procedure TCollectibleActionMap.TestProcAllIAC(BtnID, Action: Integer; cla: TAlphaColor);
begin
  TestProcAll(BtnID, Action);
end;

end.
