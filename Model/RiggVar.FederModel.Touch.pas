unit RiggVar.FederModel.Touch;

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
  System.Math,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.Generics.Collections,
  FMX.Types,
  FMX.Objects,
  FMX.Layouts,
  RiggVar.FB.ActionConst,
  RiggVar.FB.Action,
  RiggVar.FederModel.TouchBase;

type
  TFederTouch = class(TFederTouchBase)
  private
    FCornerBtnOpacity: single;

    BListL: TList<Integer>;
    BListP: TList<Integer>;

    procedure ResetCornerMenu0;
    procedure ResetCornerMenu2;
    procedure ResetCornerMenu;

    procedure ToolBtnClick(Sender: TObject);

    procedure InitBList;
    procedure InitCornerMenu;

    function GetAllBtnID(AIndex: Integer): Integer;
    function GetBtnID(AIndex: Integer): Integer;
    function GetCornerBtnCount: Integer;
    function GetActionText(AIndex: Integer): string;
    procedure AddBL(Value: Integer);
    procedure AddBP(Value: Integer);
    function GetBottomLayoutBelowTouchMenu: Boolean;
  protected
    procedure InitShapes;

    procedure SetActionMap(const Value: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function FindAllBtn(id: Integer): TCornerBtn;

    procedure Reset;

    procedure Init; override;
    procedure InitActions(Layout: Integer); override;
    procedure UpdateColorScheme; override;
    procedure UpdateText; override;

    procedure ToggleTouchFrame; override;

    property BtnID[AIndex: Integer]: Integer read GetBtnID;
    property AllBtnID[AIndex: Integer]: Integer read GetAllBtnID;
    property ActionText[AIndex: Integer]: string read GetActionText;
    property CornerBtnCount: Integer read GetCornerBtnCount;
    property WantBottomLayoutBelowTouchMenu: Boolean read GetBottomLayoutBelowTouchMenu;
  end;

implementation

uses
  RiggVar.App.Main;

{ TFederTouch }

constructor TFederTouch.Create(AOwner: TComponent);
begin
  inherited;
  ActionMap := 1;
  OpacityValue := 0.5;

  Main.ActionMapTablet.ActionProc := InitAction;
  Main.ActionMapTablet.ActionColorProc := InitActionWithColor;

  BListL := TList<Integer>.Create;
  BListP := TList<Integer>.Create;
end;

destructor TFederTouch.Destroy;
begin
  BListL.Free;
  BListP.Free;
  inherited;
end;

procedure TFederTouch.Init;
begin
  if not InitOK then
  begin
    InitShapes;
    UpdateColorScheme;
    InitOK := True;
    UpdateShape;
  end;
end;

procedure TFederTouch.InitShapes;
begin
  if not InitOK then
  begin
    ToolBtn := TCircle.Create(OwnerComponent);
//  Position set in TFederTouchBase.UpdateShape
    ToolBtn.Width := MainVar.Raster;
    ToolBtn.Height := MainVar.Raster;
    ToolBtn.Opacity := 0.1;
    ToolBtn.Fill.Color := MainVar.ColorScheme.claToolBtnFill;
    ToolBtn.OnClick := ToolBtnClick;
    ParentObject.AddObject(ToolBtn);

    InitCornerMenu;

    SL00.Shape.OnMouseDown := OnMouseDown;
    SR00.Shape.OnMouseDown := OnMouseDown;
    ST00.Shape.OnMouseDown := OnMouseDown;
    SB00.Shape.OnMouseDown := OnMouseDown;

    SL00.Shape.OnMouseMove := OnMouseMove;
    SR00.Shape.OnMouseMove := OnMouseMove;
    ST00.Shape.OnMouseMove := OnMouseMove;
    SB00.Shape.OnMouseMove := OnMouseMove;

    SL00.Shape.OnMouseUp := OnMouseUp;
    SR00.Shape.OnMouseUp := OnMouseUp;
    ST00.Shape.OnMouseUp := OnMouseUp;
    SB00.Shape.OnMouseUp := OnMouseUp;

    SL00.Shape.OnMouseLeave := OnMouseLeave;
    SR00.Shape.OnMouseLeave := OnMouseLeave;
    ST00.Shape.OnMouseLeave := OnMouseLeave;
    SB00.Shape.OnMouseLeave := OnMouseLeave;
  end;
end;

procedure TFederTouch.UpdateColorScheme;
var
  b: TCornerBtn;
  tc1, tc2: TAlphaColor;
begin
  for b in CornerBtnList do
    b.Text.Color := MainVar.ColorScheme.claCornerBtnText;

  tc1 := MainVar.ColorScheme.claCornerScrollbar;
  tc2 := MainVar.ColorScheme.claCornerBtnText;

  ST00.Shape.Fill.Color := tc1;
  ST00.Text.Color := tc2;
  SB00.Shape.Fill.Color := tc1;
  SB00.Text.Color := tc2;
  SL00.Shape.Fill.Color := tc1;
  SL00.Text.Color := tc2;
  SR00.Shape.Fill.Color := tc1;
  SR00.Text.Color := tc2;
end;

procedure TFederTouch.ToolBtnClick(Sender: TObject);
begin
  Main.ActionHandler.Execute(faToggleTouchFrame);
end;

procedure TFederTouch.InitCornerMenu;
var
  cp: TCornerPos;
  cl: TAlphaColor;
  fa: Integer;
begin
  MissID.Clear;

  TCornerBtn.OffsetX := 0;
  TCornerBtn.OffsetY := 0;
  TCornerBtn.BtnWidth := MainVar.Raster;
  TCornerBtn.BtnHeight := MainVar.Raster;
  TCornerBtn.Circle := False;

  cl := claGray;
  fa := faNoop;

  cp := cpTL;
  PageBtnM := CornerMenu.NewBtn(cp, 0, 0, cl, fa, 1);
  CornerBtnList.Add(PageBtnM);
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 0, 1, cl, fa, 2));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 1, 0, cl, fa, 3));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 2, 0, cl, fa, 4));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 3, 0, cl, fa, 5));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 4, 0, cl, fa, 6));

  cp := cpTR;
  PageBtnP := CornerMenu.NewBtn(cp, 0, 0, cl, fa, 7);
  CornerBtnList.Add(PageBtnP);
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 0, 1, cl, fa, 8));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 0, 2, cl, fa, 9));
  MissBtnListS.Add(CornerMenu.NewBtn(cp, 0, 3, cl, fa, 10));
  MissID.Add(10);

  CornerBtnList.Add(CornerMenu.NewBtn(cp, 1, 0, cl, fa, 11));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 2, 0, cl, fa, 12));
  MissBtnListB.Add(CornerMenu.NewBtn(cp, 3, 0, cl, fa, 13));
  MissBtnListB.Add(CornerMenu.NewBtn(cp, 4, 0, cl, fa, 14));
  MissID.Add(13);
  MissID.Add(14);

  cp := cpBL;
  HomeBtn := CornerMenu.NewBtn(cp, 0, 0, cl, fa, 15);
  CornerBtnList.Add(HomeBtn);
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 1, 0, cl, fa, 16));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 2, 0, cl, fa, 17));
  MissBtnListB.Add(CornerMenu.NewBtn(cp, 3, 0, cl, fa, 18));
  MissBtnListB.Add(CornerMenu.NewBtn(cp, 4, 0, cl, fa, 19));
  MissID.Add(18);
  MissID.Add(19);

  CornerBtnList.Add(CornerMenu.NewBtn(cp, 0, 1, cl, fa, 20));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 0, 2, cl, fa, 21));
  MissBtnListS.Add(CornerMenu.NewBtn(cp, 0, 3, cl, fa, 22));
  MissID.Add(22);

  cp := cpBR;
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 0, 0, cl, fa, 23));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 0, 1, cl, fa, 24));

  CornerBtnList.Add(CornerMenu.NewBtn(cp, 1, 0, cl, fa, 25));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 2, 0, cl, fa, 26));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 3, 0, cl, fa, 27));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 4, 0, cl, fa, 28));

  InitBList;
  InitAllBtnList;

  cl := MainVar.ColorScheme.claCornerScrollbar;
  ST00 := CornerMenu.NewBtn(cpT, 0, 0, cl, faNoop);
  SR00 := CornerMenu.NewBtn(cpR, 0, 0, cl, faNoop);
  SB00 := CornerMenu.NewBtn(cpB, 0, 0, cl, faNoop);
  SL00 := CornerMenu.NewBtn(cpL, 0, 0, cl, faNoop);

  ST00.Text.Align := TAlignLayout.Client;
  ST00.Text.HitTest := False;
  ST00.Text.Font.Size := MainConst.DefaultBtnFontSize;

  SB00.Text.Align := TAlignLayout.Client;
  SB00.Text.HitTest := False;
  SB00.Text.Font.Size := MainConst.DefaultBtnFontSize;

  SL00.Text.Align := TAlignLayout.Client;
  SL00.Text.HitTest := False;
  SL00.Text.Font.Size := MainConst.DefaultBtnFontSize;

  SR00.Text.Align := TAlignLayout.Client;
  SR00.Text.HitTest := False;
  SR00.Text.Font.Size := MainConst.DefaultBtnFontSize;

  FCornerBtnOpacity := HomeBtn.Opacity;

  UpdateMissing;
  ResetCornerMenu;
end;

procedure TFederTouch.UpdateText;
begin
  if InitOK then
  begin
    UpdatePageBtnText;
  end;
end;

procedure TFederTouch.ToggleTouchFrame;
begin
  FrameVisible := not FrameVisible;
end;

procedure TFederTouch.InitActions(Layout: Integer);
begin
  Main.ActionMapTablet.InitActions(Layout);
end;

procedure TFederTouch.AddBL(Value: Integer);
begin
  if not MissID.Contains(Value) then
    BListL.Add(Value);
end;

procedure TFederTouch.AddBP(Value: Integer);
begin
  if not MissID.Contains(Value) then
    BListP.Add(Value);
end;

procedure TFederTouch.InitBList;
begin
  { Landscape }
  BListL.Clear;
  { 1} AddBL(1);
  { 2} AddBL(3);
  { 3} AddBL(4);
  { 4} AddBL(5);
  { 5} AddBL(6);
  { 6} AddBL(14);
  { 7} AddBL(13);
  { 8} AddBL(12);
  { 9} AddBL(11);
  {10} AddBL(7);
  {11} AddBL(8);
  {12} AddBL(9);
  {13} AddBL(10);
  {14} AddBL(24);
  {15} AddBL(23);
  {16} AddBL(22);
  {17} AddBL(25);
  {18} AddBL(26);
  {19} AddBL(27);
  {20} AddBL(28);
  {21} AddBL(19);
  {22} AddBL(18);
  {23} AddBL(17);
  {24} AddBL(16);
  {25} AddBL(15);
  {26} AddBL(20);
  {27} AddBL(21);
  {28} AddBL(2);

  { Portrait }
  BListP.Clear;
  { 1} AddBP(1);
  { 2} AddBP(2);
  { 3} AddBP(10);
  { 4} AddBP(9);
  { 5} AddBP(8);
  { 6} AddBP(7);
  { 7} AddBP(11);
  { 8} AddBP(12);
  { 9} AddBP(13);
  {10} AddBP(14);
  {11} AddBP(28);
  {12} AddBP(27);
  {13} AddBP(26);
  {14} AddBP(25);
  {15} AddBP(23);
  {16} AddBP(24);
  {17} AddBP(22);
  {18} AddBP(21);
  {19} AddBP(20);
  {20} AddBP(15);
  {21} AddBP(16);
  {22} AddBP(17);
  {23} AddBP(18);
  {24} AddBP(19);
  {25} AddBP(6);
  {26} AddBP(5);
  {27} AddBP(4);
  {28} AddBP(3);
end;

function TFederTouch.GetActionText(AIndex: Integer): string;
begin
  result := Main.ActionHandler.GetShortCaption(AIndex);
end;

function TFederTouch.GetAllBtnID(AIndex: Integer): Integer;
var
  cb: TCornerBtn;
begin
  result := 0;
  if AIndex > -1 then
  begin
    cb := FindAllBtn(AIndex+1);
    if cb <> nil then
      result := cb.ID;
  end;
end;

function TFederTouch.GetBottomLayoutBelowTouchMenu: Boolean;
begin
  result := Main.IsPortrait;
end;

function TFederTouch.GetBtnID(AIndex: Integer): Integer;
begin
  result := 0;
  if AIndex > -1 then
  begin
    if Main.IsLandscape and (AIndex < BListL.Count) then
      result := BListL[AIndex]
    else if Main.IsPortrait and (AIndex < BListP.Count) then
      result := BListP[AIndex];
  end;
end;

function TFederTouch.GetCornerBtnCount: Integer;
begin
  result := CornerBtnList.Count;
end;

procedure TFederTouch.ResetCornerMenu;
begin
  case ActionMap of
    1, 2: ResetCornerMenu2;
    else
      ResetCornerMenu0;
  end;
end;

procedure TFederTouch.ResetCornerMenu2;
var
  cb: TCornerBtn;
begin
  ActionPage := ActionPage;

  for cb in CornerBtnList do
    cb.Opacity := FCornerBtnOpacity;
end;

procedure TFederTouch.ResetCornerMenu0;
var
  cb: TCornerBtn;
begin
  for cb in CornerBtnList do
  begin
    case cb.ID of
      1: cb.Action := faActionPageM;
      2..6: cb.Action := faNoop;
      7: cb.Action := faActionPageP;
      8..12: cb.Action := faNoop;
      13: cb.Action := faMemeSelectTop;
      14: cb.Action := faMemeSelectBottom;
      15: cb.Action := faMemeToggleDropTarget;
      16: cb.Action := faNoop;
      17: cb.Action := faNoop;
      18: cb.Action := faNoop;
      19: cb.Action := faNoop;
      20: cb.Action := faNoop;
      21: cb.Action := faNoop;
      22: cb.Action := faNoop;
      23: cb.Action := faNoop;
      24: cb.Action := faNoop;
      25: cb.Action := faNoop;
      26: cb.Action := faNoop;
      27: cb.Action := faMemeSampleP;
      28: cb.Action := faMemeSampleM;
    end;
  end;

  for cb in CornerBtnList do
  begin
    case cb.ID of
      1: cb.Shape.Fill.Color := claGreen;
      2..6: cb.Shape.Fill.Color := claLightGray;
      7: cb.Shape.Fill.Color := claOrange;
      8..14: cb.Shape.Fill.Color := claLightGray;
      15: cb.Shape.Fill.Color := claOrange;
      16..21: cb.Shape.Fill.Color := claBeige;
      22: cb.Shape.Fill.Color := claGray;
      23, 24: cb.Shape.Fill.Color := claCrimson;
      25, 26: cb.Shape.Fill.Color := claCornflowerBlue;
      27, 28: cb.Shape.Fill.Color := claGray;
    end;
  end;

  for cb in CornerBtnList do
  begin
    cb.Caption := Main.ActionHandler.GetShortCaption(cb.Action);
    cb.Opacity := FCornerBtnOpacity;
  end;
end;

procedure TFederTouch.Reset;
begin
  ResetCornerMenu;
  ActionPage := ActionPage; //--> InitActions
end;

procedure TFederTouch.SetActionMap(const Value: Integer);
begin
  inherited;

  MaxPageIndex := Main.ActionMapTablet.PageCount;
  EscapePageIndex := Main.ActionMapTablet.EscapeIndex;

  if InitOK then
    Reset;
end;

function TFederTouch.FindAllBtn(id: Integer): TCornerBtn;
var
  cb: TCornerBtn;
begin
  result := nil;
  for cb in CornerBtnList do
    if cb.ID = id then
    begin
      result := cb;
      Exit;
    end;

  for cb in MissBtnListB do
    if cb.ID = id then
    begin
      result := cb;
      Exit;
    end;

  for cb in MissBtnListS do
    if cb.ID = id then
    begin
      result := cb;
      Exit;
    end;

end;

end.
