unit RiggVar.FederModel.TouchBase;

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
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.Generics.Collections,
  FMX.Graphics,
  FMX.Types,
  FMX.Objects,
  FMX.Controls,
  RiggVar.FB.ActionConst,
  RiggVar.FB.ActionMap,
  RiggVar.FB.Action,
  RiggVar.FB.TextBase;

type
  TTouchBtn = class(TControl)
  private
    FID: Integer;
    FMenuBtn: Boolean;
    FOptiBtn: Boolean;
    FCaption: string;
    FText: TText;
    FShape: TShape;
    FAction: TFederAction;
    procedure SetColor(const Value: TAlphaColor);
    procedure SetCaption(const Value: string);
    procedure SetAction(const Value: TFederAction);
  protected
    procedure HandleClick(Sender: TObject);
  public
    X0, Y0: Integer;
    X, Y: Integer;
    class var
      OffsetX: Integer;
      OffsetY: Integer;
      BtnWidth: Integer;
      BtnHeight: Integer;
      Circle: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitPosition; virtual;

    procedure Init;
    procedure CheckState;
    procedure CheckCircleState;

    property ID: Integer read FID write FID;
    property Caption: string read FCaption write SetCaption;
    property Action: TFederAction read FAction write SetAction;
    property Shape: TShape read FShape;
    property Text: TText read FText;
    property Color: TAlphaColor write SetColor;
    property MenuBtn: Boolean read FMenuBtn;
    property OptiBtn: Boolean read FOptiBtn;
  end;

  TCornerPos = (
    cpTL, //TopLeft corner
    cpTR,
    cpBL,
    cpBR,
    cpT, //top side
    cpR,
    cpB,
    cpL
  );

  TCornerBtn = class(TTouchBtn)
  private
    procedure UpdateUV;
  public
    U, V: Integer;
    CornerPos: TCornerPos;
    procedure InitPosition; override;
  end;

  TCornerMenu = class
  public
    function NewBtn(
      CornerPos: TCornerPos;
      X, Y: Integer;
      BtnColor: TAlphaColor;
      Action: TFederAction;
      BtnID: Integer = 0
      ): TCornerBtn;

    class var
    TStart: Integer;
    RStart: Integer;
    BStart: Integer;
    LStart: Integer;
    TCount: Integer;
    RCount: Integer;
    BCount: Integer;
    LCount: Integer;
  end;

  TFederTouchBase = class(TFederTouch0)
  private
    procedure SetOwnsMouse(const Value: Boolean);
    function GetMaxCount: Integer;
    function GetMinCount: Integer;
  protected
    FMinBtnCount: Integer;
    FMaxBtnCount: Integer;
    FMaxBtnPhone: Integer;
    FMinBtnPhone: Integer;

    MissID: TList<Integer>;
    MissBtnListB: TObjectList<TCornerBtn>;
    MissBtnListS: TObjectList<TCornerBtn>;

    MaxPageIndex: Integer;
    EscapePageIndex: Integer;

    OldX: single;
    OldY: single;
    Down: Boolean;
    FOwnsMouse: Boolean;

    ToolBtn: TCircle;
    ConnLED: TCircle;
    LAT: TText;
    LHT: single;

    function FindMissBtn(id: Integer): TCornerBtn;
    function FindMissBtnB(id: Integer): TCornerBtn;
    function FindMissBtnS(id: Integer): TCornerBtn;
    procedure AddMissingB;
    procedure AddMissingS;
    procedure RemoveMissingB;
    procedure RemoveMissingS;
    procedure InitAllBtnList;

    function LineHeight(t: TText): single;

    procedure OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure OnMouseLeave(Sender: TObject);
    procedure BorderTrack(Sender: TObject; X, Y: single);

    procedure SetActionPage(const Value: Integer); override;
    procedure SetFrameVisible(const Value: Boolean); override;

    procedure InitAction(BtnID: Integer; fa: TFederAction);
    procedure InitActionWithColor(BtnID: Integer; fa: TFederAction; ac: TAlphaColor);
  public
    AllBtnList: TObjectList<TCornerBtn>;
    CornerBtnList: TObjectList<TCornerBtn>;
    CornerMenu: TCornerMenu;

    HomeBtn: TCornerBtn;
    PageBtnP: TCornerBtn;
    PageBtnM: TCornerBtn;

    SL00: TCornerBtn;
    ST00: TCornerBtn;
    SB00: TCornerBtn;
    SR00: TCornerBtn;

    class var
      OpacityValue: single;
      OwnerComponent: TComponent;
      ParentObject: TFmxObject;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init; override;
    procedure CheckState;
    procedure CheckCircleState; virtual;
    procedure MoveText; virtual;
    procedure CheckBtnOrder; virtual;

    function FindCornerBtn(id: Integer): TCornerBtn;
    function FindAllBtn(id: Integer): TCornerBtn;

    procedure InitActions(Layout: Integer); virtual;
    procedure UpdateMissing; virtual;
    procedure UpdateToolSet(Delta: Integer);
    procedure UpdateShape;
    procedure UpdateColorScheme; virtual;
    procedure UpdatePageBtnText;

    property FrameVisible: Boolean read FFrameVisible write SetFrameVisible;
    property OwnsMouse: Boolean read FOwnsMouse write SetOwnsMouse;

    property MinCount: Integer read GetMinCount;
    property MaxCount: Integer read GetMaxCount;
  end;

const
  BtnBorderColor: TAlphaColor = claNull;
  BtnBorderWidth: Integer = 4;
  BtnBorderRadius: Integer = 8;
  DockStartX: Integer = 2;

implementation

uses
  RiggVar.App.Main;

{ TTouchBtn }

constructor TTouchBtn.Create(AOwner: TComponent);
begin
  inherited;
  Width := BtnWidth;
  Height := BtnHeight;
end;

destructor TTouchBtn.Destroy;
begin
  inherited;
end;

procedure TTouchBtn.HandleClick(Sender: TObject);
begin
  Main.ActionHandler.Execute(Action);
  Main.FederText.CheckState; // if not done in Execute
end;

procedure TTouchBtn.CheckState;
var
  r: TRectangle;
  b: Boolean;
begin
  if FShape is TRectangle then
  begin
    b := Main.ActionHandler.GetChecked(self.Action);
    r := FShape as TRectangle;
    if not b then
      r.Corners := [TCorner.TopLeft, TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight]
    else
      r.Corners := [
        TCorner.TopLeft,
  //      TCorner.TopRight,
        TCorner.BottomLeft,
        TCorner.BottomRight
        ];
  end;
end;

procedure TTouchBtn.CheckCircleState;
var
  c: TCircle;
  b: Boolean;
begin
  if FShape is TCircle then
  begin
    b := Main.ActionHandler.GetChecked(self.Action);
    c := FShape as TCircle;
    if b then
      c.Fill.Color := claCyan
    else
      c.Fill.Color := MainVar.ColorScheme.claTouchBtnFill;
  end;
end;

procedure TTouchBtn.SetAction(const Value: TFederAction);
begin
  FAction := Value;
end;

procedure TTouchBtn.SetCaption(const Value: string);
begin
  FCaption := Value;
  if Assigned(FText) then
    FText.Text := Value;
end;

procedure TTouchBtn.SetColor(const Value: TAlphaColor);
begin
  if Assigned(FShape) then
  begin
    FShape.Fill.Color := Value;
  end;
end;

procedure TTouchBtn.Init;
var
  r: TRectangle;
begin
  InitPosition;

  if Circle then
    FShape := TCircle.Create(Self)
  else
  begin
    r := TRectangle.Create(Self);
    r.CornerType := TCornerType.Round;
    r.Corners := [TCorner.TopLeft, TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight];
    r.XRadius := BtnBorderRadius;
    r.YRadius := BtnBorderRadius;
    FShape := r;
  end;

  FShape.Fill.Color := MainVar.ColorScheme.claTouchBtnFill;
  FShape.Stroke.Color := BtnBorderColor;
  FShape.Stroke.Thickness := BtnBorderWidth;
  FShape.Opacity := 1.0;
  FShape.Align := TAlignLayout.Client;

  FText := TText.Create(Self);
  FText.Align := TAlignLayout.Center;
  FText.Text := FCaption;
  FText.Opacity := 1.0;
  FText.Font.Size := MainConst.DefaultBtnFontSize;

  FText.OnClick := HandleClick;
  FShape.OnClick := HandleClick;

  FShape.AddObject(FText);
  Self.AddObject(FShape);
end;

procedure TTouchBtn.InitPosition;
begin
  Position.X := X * BtnWidth + OffsetX;
  Position.Y := Y * BtnHeight + OffsetY;
end;

{ TCornerMenu }

function TCornerMenu.NewBtn(
  CornerPos: TCornerPos;
  X, Y: Integer;
  BtnColor: TAlphaColor;
  Action: TFederAction;
  BtnID: Integer = 0
  ): TCornerBtn;
var
  b: TCornerBtn;
begin
  b := TCornerBtn.Create(TFederTouchBase.OwnerComponent);
  b.CornerPos := CornerPos;
  b.FID := BtnID;
  b.X := X;
  b.Y := Y;
  b.Action := Action;
  b.Opacity := TFederTouchBase.OpacityValue;
  b.Init;
  b.Color := BtnColor;
  TFederTouchBase.ParentObject.AddObject(b);

  b.Caption := Main.ActionHandler.GetShortCaption(Action);
  b.FText.Color := MainVar.ColorScheme.claCornerBtnText;
  b.FText.Font.Size := MainConst.DefaultBtnFontSize;
  b.FText.Opacity := 1.0;
  b.FShape.Opacity := 1.0;

  result := b;
end;

{ TCornerBtn }

procedure TCornerBtn.UpdateUV;
begin
  U := X;
  V := Y;

  case CornerPos of
    cpTL, cpTR,  cpBL, cpBR:
    begin
      if not Main.IsLandscape then
      begin
        U := Y;
        V := X;
      end;

      if U > 0 then
      begin
        case CornerPos of
          cpTL:
          begin
            Inc(TCornerMenu.TStart);
            Inc(TCornerMenu.TCount);
          end;
          cpTR:
          begin
            Inc(TCornerMenu.TCount);
          end;
          cpBL:
          begin
            Inc(TCornerMenu.BStart);
            Inc(TCornerMenu.BCount);
          end;
          cpBR:
          begin
            Inc(TCornerMenu.BCount);
          end;
        end;
      end;
      if V > 0 then
      begin
        case CornerPos of
          cpTL:
          begin
            Inc(TCornerMenu.LStart);
            Inc(TCornerMenu.LCount);
          end;
          cpTR:
          begin
            Inc(TCornerMenu.RStart);
            Inc(TCornerMenu.RCount);
          end;
          cpBL:
          begin
            Inc(TCornerMenu.LCount);
          end;
          cpBR:
          begin
            Inc(TCornerMenu.RCount);
          end;
        end;
      end;

    end;
  end;
end;

procedure TCornerBtn.InitPosition;
var
  mx, my: Boolean;
begin
  UpdateUV;
  case CornerPos of
    cpTL:
    begin
      mx := False;
      my := False;
    end;
    cpTR:
    begin
      mx := True;
      my := False;
    end;
    cpBR:
    begin
      mx := True;
      my := True;
    end;
    cpBL:
    begin
      mx := False;
      my := True;
    end;

    cpT:
    begin
      mx := False;
      my := False;
    end;
    cpR:
    begin
      mx := True;
      my := False;
    end;
    cpB:
    begin
      mx := False;
      my := True;
    end;
    cpL:
    begin
      mx := False;
      my := False;
    end;

    else
    begin
      mx := False;
      my := False;
    end;
  end;

  if mx then
    Position.X := MainVar.ClientWidth - (U + 1) * MainVar.Raster
  else
    Position.X := U * MainVar.Raster;

  if my then
    Position.Y := MainVar.ClientHeight - (V + 1) * MainVar.Raster
  else
    Position.Y := V * MainVar.Raster;
end;

{ TFederTouchBase }

procedure TFederTouchBase.MoveText;
begin
  Width := MainVar.ClientWidth;
  Height := MainVar.ClientHeight;
end;

procedure TFederTouchBase.CheckState;
var
  b: TCornerBtn;
begin
  for b in CornerBtnList do
    b.CheckState;
  CheckCircleState;
end;

procedure TFederTouchBase.CheckCircleState;
begin
  //virtual
end;

procedure TFederTouchBase.CheckBtnOrder;
begin
  //virtual
end;

constructor TFederTouchBase.Create(AOwner: TComponent);
begin
  inherited;
  FFrameVisible := True;
  FMaxBtnCount := 12;
  FMinBtnCount := 8;
  FMaxBtnPhone := 6;
  FMinBtnPhone := 4;

  FActionPage := 1;
  EscapePageIndex := 1;
  MissID := TList<Integer>.Create;
  MissBtnListB := TObjectList<TCornerBtn>.Create;
  MissBtnListS := TObjectList<TCornerBtn>.Create;
  AllBtnList := TObjectList<TCornerBtn>.Create;
  AllBtnList.OwnsObjects := False;
  CornerBtnList := TObjectList<TCornerBtn>.Create;
  CornerBtnList.OwnsObjects := False;
  CornerMenu := TCornerMenu.Create;
end;

destructor TFederTouchBase.Destroy;
begin
  MissID.Free;
  CornerMenu.Free;
  AllBtnList.Free;
  CornerBtnList.Free;
  MissBtnListB.Free;
  MissBtnListS.Free;
  inherited;
end;

function TFederTouchBase.GetMinCount: Integer;
begin
  result := Min(MainVar.ClientWidth, MainVar.ClientHeight) div MainVar.Raster;
end;

function TFederTouchBase.GetMaxCount: Integer;
begin
  result := Max(MainVar.ClientWidth, MainVar.ClientHeight) div MainVar.Raster;
end;

procedure TFederTouchBase.Init;
begin

end;

procedure TFederTouchBase.SetActionPage(const Value: Integer);
begin
  if Value = -3 then
  begin
    if FActionPage = 1 then
      FActionPage := EscapePageIndex
    else
      FActionPage := 1;
  end
  else if Value = -2 then
    FActionPage := MaxPageIndex
  else if Value = -1 then
    FActionPage := EscapePageIndex
  else if (Value = EscapePageIndex) and (FActionPage = EscapePageIndex + 1) then
    FActionPage := EscapePageIndex
  else if (Value = EscapePageIndex) and (FActionPage = EscapePageIndex - 1) then
    FActionPage := 1
  else
    FActionPage := Value;

  if FActionPage > MaxPageIndex then
    FActionPage := 1;
  if FActionPage < 1 then
      FActionPage := EscapePageIndex-1;

  InitActions(FActionPage);

  UpdateText;
  CheckState;
end;

procedure TFederTouchBase.UpdateColorScheme;
begin

end;

procedure TFederTouchBase.SetFrameVisible(const Value: Boolean);
var
  b: TCornerBtn;
begin
  FFrameVisible := Value;

  if Value then
    ToolBtn.Opacity := 0.1
  else
    ToolBtn.Opacity := 0.05;

  for b in CornerBtnList do
    b.Visible := Value;

  ST00.Visible := Value;
  SR00.Visible := Value;
  SB00.Visible := Value;
  SL00.Visible := Value;
end;

procedure TFederTouchBase.BorderTrack(Sender: TObject; X, Y: single);
begin
  if Sender = SB00.Shape then
  begin
    if (Round(Abs(X - OldX)) div 8) > 0 then
    begin
      Main.DoTouchbarBottom((OldX-X) / 20);
      OldX := X;
      OldY := Y;
    end;
  end
  else if Sender = ST00.Shape then
  begin
    if Abs(X - OldX) > 0 then
    begin
      Main.DoTouchBarTop(X - OldX);
      OldX := X;
      OldY := Y;
    end;
  end
  else if Sender = SL00.Shape then
  begin
    if (Round(Abs(Y - OldY)) div 8) > 0 then
    begin
      Main.DoTouchbarLeft(OldY - Y);
      OldX := X;
      OldY := Y;
    end;
  end
  else if Sender = SR00.Shape then
  begin
    if (Round(Abs(Y - OldY)) div 8) > 0 then
    begin
      Main.DoTouchbarRight(OldY - Y);
      OldX := X;
      OldY := Y;
    end;
  end
  else
  begin
    OldX := X;
    OldY := Y;
  end;
end;

procedure TFederTouchBase.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  Down := True;
  OldX := X;
  OldY := Y;
  FOwnsMouse := True;
end;

procedure TFederTouchBase.OnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  if Main.IsUp then
  begin
    if Down then
      BorderTrack(Sender, X, Y);
  end;
end;

procedure TFederTouchBase.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  Down := False;
  FOwnsMouse := False;
end;

procedure TFederTouchBase.SetOwnsMouse(const Value: Boolean);
begin
  FOwnsMouse := Value;
end;

procedure TFederTouchBase.OnMouseLeave(Sender: TObject);
begin
  Down := False;
end;

procedure TFederTouchBase.UpdateShape;
var
  b: TCornerBtn;
begin
  if InitOK then
  begin
    TCornerMenu.TStart := 1;
    TCornerMenu.RStart := 1;
    TCornerMenu.BStart := 1;
    TCornerMenu.LStart := 1;

    TCornerMenu.TCount := 2;
    TCornerMenu.RCount := 2;
    TCornerMenu.BCount := 2;
    TCornerMenu.LCount := 2;

    for b in CornerBtnList do
      b.InitPosition;

    ST00.Position.X := TCornerMenu.TStart * MainVar.Raster;
    ST00.Position.Y := 0;

    SR00.Position.X := MainVar.ClientWidth - MainVar.Raster;
    SR00.Position.Y := TCornerMenu.RStart * MainVar.Raster;

    SB00.Position.X := TCornerMenu.BStart * MainVar.Raster;
    SB00.Position.Y := MainVar.ClientHeight - MainVar.Raster;

    SL00.Position.X := 0;
    SL00.Position.Y := TCornerMenu.LStart * MainVar.Raster;

    ST00.Width := MainVar.ClientWidth - (TCornerMenu.TCount) * MainVar.Raster;
    SB00.Width := MainVar.ClientWidth - (TCornerMenu.BCount) * MainVar.Raster;
    SL00.Height := MainVar.ClientHeight - (TCornerMenu.LCount) * MainVar.Raster;
    SR00.Height := MainVar.ClientHeight - (TCornerMenu.RCount) * MainVar.Raster;

    ToolBtn.Position.X := MainVar.Raster;
    ToolBtn.Position.Y := MainVar.Raster;
    ToolBtn.Width := MainVar.Raster;
    if Assigned(ConnLED) then
    begin
      ConnLED.Position.X := ToolBtn.Position.X + 0.5 * MainVar.Raster - 7;
      ConnLED.Position.Y := ToolBtn.Position.Y + 0.5 * MainVar.Raster - 7;
    end;

    MoveText;
    CheckBtnOrder;
  end;
end;

procedure TFederTouchBase.UpdateToolSet(Delta: Integer);
begin
  ActionPage := FActionPage + Delta;
end;

function TFederTouchBase.FindAllBtn(id: Integer): TCornerBtn;
var
  tb: TCornerBtn;
begin
  result := nil;
  for tb in AllBtnList do
  begin
    if tb.ID = id then
    begin
      result := tb;
      break;
    end;
  end;
end;

procedure TFederTouchBase.InitAction(BtnID: Integer; fa: TFederAction);
var
  tb: TCornerBtn;
begin
  tb := FindCornerBtn(BtnID);
  if not Assigned(tb) then
    tb := FindMissBtn(BtnID);
  if Assigned(tb) then
  begin
    tb.Action := fa;
    tb.Caption := Main.ActionHandler.GetShortCaption(fa);
  end;
end;

procedure TFederTouchBase.InitActionWithColor(BtnID: Integer; fa: TFederAction; ac: TAlphaColor);
var
  tb: TCornerBtn;
begin
  tb := FindCornerBtn(BtnID);
  if not Assigned(tb) then
    tb := FindMissBtn(BtnID);
  if Assigned(tb) then
  begin
    tb.Action := fa;
    tb.Caption := Main.ActionHandler.GetShortCaption(fa);
    tb.Color := ac;
  end;
end;

procedure TFederTouchBase.InitActions(Layout: Integer);
begin

end;

function TFederTouchBase.LineHeight(t: TText): single;
begin
  result := t.Font.Size + Round(t.Font.Size / 4);
end;

function TFederTouchBase.FindCornerBtn(id: Integer): TCornerBtn;
var
  cb: TCornerBtn;
begin
  result := nil;
  for cb in CornerBtnList do
    if cb.ID = id then
    begin
      result := cb;
      break;
    end;
end;

function TFederTouchBase.FindMissBtn(id: Integer): TCornerBtn;
var
  cb: TCornerBtn;
begin
  result := nil;
  for cb in MissBtnListB do
    if cb.ID = id then
    begin
      result := cb;
      break;
    end;
  for cb in MissBtnListS do
    if cb.ID = id then
    begin
      result := cb;
      break;
    end;
end;

function TFederTouchBase.FindMissBtnB(id: Integer): TCornerBtn;
var
  cb: TCornerBtn;
begin
  result := nil;
  for cb in MissBtnListB do
    if cb.ID = id then
    begin
      result := cb;
      break;
    end;
end;

function TFederTouchBase.FindMissBtnS(id: Integer): TCornerBtn;
var
  cb: TCornerBtn;
begin
  result := nil;
  for cb in MissBtnListS do
    if cb.ID = id then
    begin
      result := cb;
      break;
    end;
end;

procedure TFederTouchBase.AddMissingB;
var
  i: Integer;
  bid: Integer;
  cb: TCornerBtn;
  mb: TCornerBtn;
begin
  for i := 0 to MissID.Count-1 do
  begin
    bid := MissID[i];
    cb := FindCornerBtn(bid);
    mb := FindMissBtnB(bid);
    if Assigned(mb) then
      mb.Visible := FFrameVisible;
    if (cb = nil) and (mb <> nil) then
      CornerBtnList.Add(mb);
  end;
end;

procedure TFederTouchBase.AddMissingS;
var
  i: Integer;
  bid: Integer;
  cb: TCornerBtn;
  mb: TCornerBtn;
begin
  for i := 0 to MissID.Count-1 do
  begin
    bid := MissID[i];
    cb := FindCornerBtn(bid);
    mb := FindMissBtnS(bid);
    if Assigned(mb) then
      mb.Visible := FFrameVisible;
    if (cb = nil) and (mb <> nil) then
      CornerBtnList.Add(mb);
  end;
end;

procedure TFederTouchBase.RemoveMissingB;
var
  i: Integer;
  bid: Integer;
  cb: TCornerBtn;
  mb: TCornerBtn;
begin
  for i := 0 to MissID.Count-1 do
  begin
    bid := MissID[i];
    cb := FindCornerBtn(bid);
    mb := FindMissBtnB(bid);
    if Assigned(mb) then
      mb.Visible := False;
    if (cb <> nil) and (mb <> nil) then
      CornerBtnList.Remove(mb);
  end;
end;

procedure TFederTouchBase.RemoveMissingS;
var
  i: Integer;
  bid: Integer;
  cb: TCornerBtn;
  mb: TCornerBtn;
begin
  for i := 0 to MissID.Count-1 do
  begin
    bid := MissID[i];
    cb := FindCornerBtn(bid);
    mb := FindMissBtnS(bid);
    if Assigned(mb) then
      mb.Visible := False;
    if (cb <> nil) and (mb <> nil) then
      CornerBtnList.Remove(mb);
  end;
end;

procedure TFederTouchBase.UpdateMissing;
begin
  if MaxCount > FMaxBtnCount then
    AddMissingB
  else
    RemoveMissingB;

  if MinCount > FMinBtnCount then
    AddMissingS
  else
    RemoveMissingS;
end;

procedure TFederTouchBase.UpdatePageBtnText;
begin
  PageBtnP.Text.Text := IntToStr(ActionPage);
  PageBtnM.Text.Text := IntToStr(ActionPage);
  ST00.Text.Text := TActionMap.CurrentPageCaption;
end;

procedure TFederTouchBase.InitAllBtnList;
var
  cb: TCornerBtn;
begin
  AllBtnList.Clear;
  for cb in CornerBtnList do
    AllBtnList.Add(cb);
  for cb in MissBtnListB do
    AllBtnList.Add(cb);
  for cb in MissBtnListS do
    AllBtnList.Add(cb);
end;

end.