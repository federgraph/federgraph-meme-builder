unit RiggVar.FederModel.TouchPhone;

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
  FMX.Types,
  FMX.Objects,
  RiggVar.FB.ActionConst,
  RiggVar.FB.Action,
  RiggVar.FederModel.TouchBase;

type
  TFederTouchPhone = class(TFederTouchBase)
  private
    procedure ToolBtnClick(Sender: TObject);
    procedure InitCornerMenu;
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

{ TFederTouchPhone }

constructor TFederTouchPhone.Create(AOwner: TComponent);
begin
  inherited;
  ActionMap := 1;
  OpacityValue := 0.5;
  Main.ActionMapPhone.ActionProc := InitAction;
  Main.ActionMapPhone.ActionColorProc := InitActionWithColor;
end;

procedure TFederTouchPhone.Init;
begin
  Width := MainVar.ClientWidth;
  Height := MainVar.ClientHeight;
  if not InitOK then
  begin
    InitShapes;
    UpdateColorScheme;
    InitOK := True;
    UpdateShape;
  end;
end;

procedure TFederTouchPhone.InitShapes;
begin
  if not InitOK then
  begin
    ToolBtn := TCircle.Create(OwnerComponent);
    ToolBtn.Position.X := MainVar.Raster;
    ToolBtn.Position.Y := MainVar.Raster;
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

procedure TFederTouchPhone.UpdateColorScheme;
var
  b: TCornerBtn;
  tc1, tc2: TAlphaColor;
begin
  ToolBtn.Fill.Color := MainVar.ColorScheme.claToolBtnFill;

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

procedure TFederTouchPhone.ToolBtnClick(Sender: TObject);
begin
  FrameVisible := not FrameVisible;

  if FrameVisible then
  begin
    ToolBtn.Opacity := 0.1;
   end
  else
  begin
    ToolBtn.Opacity := 0.05;
  end;

  MoveText;
end;

procedure TFederTouchPhone.InitCornerMenu;
var
  cp: TCornerPos;
  cl: TAlphaColor;
begin
  TCornerBtn.OffsetX := 0;
  TCornerBtn.OffsetY := 0;
  TCornerBtn.BtnWidth := MainVar.Raster;
  TCornerBtn.BtnHeight := MainVar.Raster;
  TCornerBtn.Circle := false;

  cl := claYellow;
  PageBtnP := CornerMenu.NewBtn(cpTR, 0, 0, cl, faActionPageP, 9);
  PageBtnM := CornerMenu.NewBtn(cpTL, 0, 0, cl, faActionPageM, 10);
  CornerBtnList.Add(PageBtnP);
  CornerBtnList.Add(PageBtnM);
  cl := claCornflowerBlue;
  CornerBtnList.Add(CornerMenu.NewBtn(cpBL, 0, 0, cl, faNoop, 7));
  CornerBtnList.Add(CornerMenu.NewBtn(cpBR, 0, 0, cl, faNoop, 8));

  cp := cpTL;
  cl := claWhite;
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 1, 0, cl, faNoop, 1));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 2, 0, cl, faNoop, 2));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 3, 0, cl, faNoop, 3));

  cp := cpBR;
  cl := claWhite;
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 1, 0, cl, faNoop, 6));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 2, 0, cl, faNoop, 5));
  CornerBtnList.Add(CornerMenu.NewBtn(cp, 3, 0, cl, faNoop, 4));

  cl := MainVar.ColorScheme.claCornerScrollbar;
  ST00 := CornerMenu.NewBtn(cpT, 0, 0, cl, faNoop);
  SR00 := CornerMenu.NewBtn(cpR, 0, 0, cl, faNoop);
  SB00 := CornerMenu.NewBtn(cpB, 0, 0, cl, faNoop);
  SL00 := CornerMenu.NewBtn(cpL, 0, 0, cl, faNoop);

  ST00.Text.Align := TAlignLayout.Client;
  ST00.Text.HitTest := false;
  ST00.Text.Font.Size := MainConst.DefaultBtnFontSize;

  SB00.Text.Align := TAlignLayout.Client;
  SB00.Text.HitTest := false;
  SB00.Text.Font.Size := MainConst.DefaultBtnFontSize;

  SL00.Text.Align := TAlignLayout.Client;
  SL00.Text.HitTest := false;
  SL00.Text.Font.Size := MainConst.DefaultBtnFontSize;

  SR00.Text.Align := TAlignLayout.Client;
  SR00.Text.HitTest := false;
  SR00.Text.Font.Size := MainConst.DefaultBtnFontSize;

  InitActions(1);
end;

procedure TFederTouchPhone.UpdateText;
begin
  if InitOK then
  begin
    PageBtnP.Text.Text := IntToStr(ActionPage);
    PageBtnM.Text.Text := IntToStr(ActionPage);
  end;
end;

procedure TFederTouchPhone.ToggleTouchFrame;
begin
  FrameVisible := not FrameVisible;
end;

procedure TFederTouchPhone.InitActions(Layout: Integer);
begin
  Main.ActionMapPhone.InitActions(Layout);
end;

procedure TFederTouchPhone.SetActionMap(const Value: Integer);
begin
  inherited;
  MaxPageIndex := Main.ActionMapPhone.PageCount;
end;

end.
