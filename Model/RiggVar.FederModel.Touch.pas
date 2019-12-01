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
  System.Classes,
  System.UITypes,
  System.UIConsts,
  FMX.Types,
  FMX.Objects,
  RiggVar.FB.ActionConst,
  RiggVar.FederModel.TouchBase;

type
  TFederTouch = class(TFederTouchBase)
  private
    FCornerBtnOpacity: single;

    procedure ToolBtnClick(Sender: TObject);

    procedure InitCornerMenu;
    procedure ResetCornerMenu;
  protected
    procedure InitShapes;

    procedure SetActionMap(const Value: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Init; override;
    procedure InitActions(Layout: Integer); override;
    procedure UpdateColorScheme; override;
    procedure UpdateText; override;

    procedure ToggleTouchFrame; override;
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
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 0, 3, cl, fa, 10));

  CornerBtnList.Add(CornerMenu.NewBtn(cp, 1, 0, cl, fa, 11));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 2, 0, cl, fa, 12));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 3, 0, cl, fa, 13));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 4, 0, cl, fa, 14));

  cp := cpBL;
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 0, 0, cl, fa, 15));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 1, 0, cl, fa, 16));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 2, 0, cl, fa, 17));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 3, 0, cl, fa, 18));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 4, 0, cl, fa, 19));

  CornerBtnList.Add(CornerMenu.NewBtn(cp, 0, 1, cl, fa, 20));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 0, 2, cl, fa, 21));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 0, 3, cl, fa, 22));

  cp := cpBR;
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 0, 0, cl, fa, 23));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 0, 1, cl, fa, 24));

  CornerBtnList.Add(CornerMenu.NewBtn(cp, 1, 0, cl, fa, 25));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 2, 0, cl, fa, 26));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 3, 0, cl, fa, 27));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 4, 0, cl, fa, 28));

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

  FCornerBtnOpacity := PageBtnM.Opacity;

  ResetCornerMenu;
end;

procedure TFederTouch.UpdateText;
begin
  { Text has been removed,
    there used to be Text inside here as well, which needed an update.
  }
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

procedure TFederTouch.ResetCornerMenu;
var
  cb: TCornerBtn;
begin
  { Assign actions to buttons, and Color, Caption, Opacity ... }

  ActionPage := ActionPage; // call virtual setter --> InitActions

  for cb in CornerBtnList do
    cb.Opacity := FCornerBtnOpacity;
end;

procedure TFederTouch.SetActionMap(const Value: Integer);
begin
  inherited;

  MaxPageIndex := Main.ActionMapTablet.PageCount;

  if InitOK then
    ResetCornerMenu;
end;

end.
