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

  TActionMap = class
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

implementation

uses
  RiggVar.FB.ActionShort,
  RiggVar.FB.ActionLong;

{ TActionMap }

procedure TActionMap.InitActions(Layout: Integer);
begin
  //virtual
end;

constructor TActionMap.Create;
begin
  FPageCount := 1;
end;

procedure TActionMap.InitAction(BtnID, Action: Integer);
begin
  if Assigned(ActionProc) then
    ActionProc(BtnID, Action);
end;

procedure TActionMap.IAC(BtnID, Action: Integer; cla: TAlphaColor);
begin
  if Assigned(ActionColorProc) then
    ActionColorProc(BtnID, Action, cla);
end;

procedure TActionMap.SetActionColorProc(const Value: TInitActionColorProc);
begin
  FActionColorProc := Value;
end;

procedure TActionMap.SetActionProc(const Value: TInitActionProc);
begin
  FActionProc := Value;
end;

procedure TActionMap.SetPageCount(const Value: Integer);
begin
  FPageCount := Value;
end;

end.
