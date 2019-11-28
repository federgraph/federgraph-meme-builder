unit RiggVar.FB.TextBase;

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
  System.Classes,
  FMX.Layouts;

type
  TFederTouch0 = class(TLayout)
  protected
    FActionMap: Integer;
    FActionPage: Integer;
    FFrameVisible: Boolean;

    function GetActionMap: Integer; virtual;
    function GetActionPage: Integer; virtual;
    function GetFrameVisible: Boolean; virtual;

    procedure SetActionMap(const Value: Integer); virtual;
    procedure SetActionPage(const Value: Integer); virtual;
    procedure SetFrameVisible(const Value: Boolean); virtual;
  public
    InitOK: Boolean;

    constructor Create(AOwner: TComponent); override;
    procedure Init; virtual;

    procedure ToggleTouchFrame; virtual;
    procedure UpdateText; virtual;

    property ActionMap: Integer read GetActionMap write SetActionMap;
    property ActionPage: Integer read GetActionPage write SetActionPage;
    property FrameVisible: Boolean read GetFrameVisible write SetFrameVisible;
  end;

implementation

{ TFederTouch0 }

constructor TFederTouch0.Create(AOwner: TComponent);
begin
  inherited;
  FFrameVisible := True;
end;

function TFederTouch0.GetActionMap: Integer;
begin
  result := FActionMap;
end;

function TFederTouch0.GetActionPage: Integer;
begin
  result := FActionPage;
end;

function TFederTouch0.GetFrameVisible: Boolean;
begin
  result := FFrameVisible;
end;

procedure TFederTouch0.SetFrameVisible(const Value: Boolean);
begin
  FFrameVisible := Value;
end;

procedure TFederTouch0.SetActionMap(const Value: Integer);
begin
  FActionMap := Value;
end;

procedure TFederTouch0.SetActionPage(const Value: Integer);
begin
  FActionPage := Value;
end;

procedure TFederTouch0.ToggleTouchFrame;
begin

end;

procedure TFederTouch0.UpdateText;
begin

end;

procedure TFederTouch0.Init;
begin

end;

end.
