unit FrmMain;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.UIConsts,
  FMX.Graphics,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.StdCtrls,
  FMX.Effects,
  FMX.Filter.Effects,
  FMX.Objects,
  FMX.ExtCtrls,
  FMX.Edit,
  FMX.Controls.Presentation;

type
  TFormMain = class(TForm)
    TopText: TText;
    BottomText: TText;
    BottomGlow: TGlowEffect;
    TopGlow: TGlowEffect;
    TopEdit: TEdit;
    BottomEdit: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DropTargetDropped(Sender: TObject; const Data: TDragObject;
      const Point: TPointF);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure TopEditKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure BottomEditKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    fa: Integer;
    fo: Integer;
    MaxFont: Integer;
    DropTarget: TDropTarget;
    CheckerBitmap: TBitmap;
    CheckerImage: TImage;
    FDropTargetVisible: Boolean;
    DefaultCaption: string;
    TestID: Integer;
    procedure CopyBitmap;
    procedure CreateCheckerBitmap;
    procedure InitChecker;
    procedure InitDropTarget;
    procedure UpdateChecker;
    procedure SetDropTargetVisible(const Value: Boolean);
    procedure OnDropTargetDropped(fn: string);
    procedure ClearImage;
    procedure CycleFontP;
    procedure CycleFontM;
    procedure HandleWheel(Delta: Integer);
    procedure CycleFont(Value: Integer);
    procedure UpdateFormat(w, h: Integer);
    procedure Reset;
    procedure UpdateParam(afa: Integer);
    property DropTargetVisible: Boolean read FDropTargetVisible write SetDropTargetVisible;
  protected
    function FindTarget(P: TPointF; const Data: TDragObject): IControl; override;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

//uses
//  RiggVar.Util.FMX;

const
  faTopMargin = 1;
  faBottomMargin = 2;
  faTopSize = 3;
  faBottomSize = 4;
  faTopGlow = 5;
  faBottomGlow = 6;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;

  fo := 1;
  MaxFont := 9;

  TestID := 0;
  Reset;
  Caption := DefaultCaption;

  InitChecker;

  TopText.BringToFront;
  BottomText.BringToFront;

  TopEdit.Visible := false;
  BottomEdit.Visible := false;

  DropTargetVisible := true;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  CheckerBitmap.Free;
end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if Key = vkEscape then
  begin
    TopEdit.Visible := not TopEdit.Visible;
    BottomEdit.Visible := TopEdit.Visible;
    Caption := DefaultCaption;
  end;

  if Key = vkC then
  begin
    CopyBitmap;
    Caption := 'Bitmap copied.';
  end

  else if TopEdit.Visible then
    //do nothing when editing

  else if KeyChar = 'd' then
    DropTargetVisible := not DropTargetVisible

  else if KeyChar = 'c' then
    ClearImage

  else if KeyChar = 'f' then
    CycleFontP

  else if KeyChar = 'F' then
    CycleFontM

  else if KeyChar = 'g' then
    UpdateParam(faTopGlow)

  else if KeyChar = 'h' then
    UpdateParam(faBottomGlow)

  else if KeyChar = 't' then
    UpdateParam(faTopSize)

  else if KeyChar = 'b' then
    UpdateParam(faBottomSize)

  else if KeyChar = 'n' then
    UpdateParam(faTopMargin)

  else if KeyChar = 'm' then
    UpdateParam(faBottomMargin)

  else if KeyChar = 'r' then
    Caption := DefaultCaption

  else if KeyChar = 'R' then
    Reset

  else if KeyChar = 'x' then
  begin
    Inc(TestID);
    TestID := TestID mod 2;
    Reset;
  end

  else if KeyChar = '1' then
    UpdateFormat(640, 480)

  else if KeyChar = '2' then
    UpdateFormat(800, 600)

  else if KeyChar = '3' then
    UpdateFormat(1024, 768)

  else if KeyChar = '8' then
    UpdateFormat(800, 800)

  else if KeyChar = '9' then
    UpdateFormat(900, 900)

  else if KeyChar = '0' then
    UpdateFormat(1024, 1024)
end;

procedure TFormMain.UpdateFormat(w, h: Integer);
begin
  ClientWidth := w;
  ClientHeight := h;
  Caption := Format('%d x %d', [ClientWidth, ClientHeight]);
end;

procedure TFormMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  if WheelDelta > 0 then
    HandleWheel(1)
  else
    HandleWheel(-1)
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  BottomText.AutoSize := False;
  BottomText.AutoSize := True;

  TopText.AutoSize := False;
  TopText.AutoSize := True;

  TopEdit.Position.X := 10;
  TopEdit.Width := ClientWidth - 20;

  BottomEdit.Position.X := 10;
  BottomEdit.Width := ClientWidth - 20;

  UpdateChecker;
end;

procedure TFormMain.HandleWheel(Delta: Integer);
var
  f: single;
begin
  case fa of
    faTopSize:
    begin
      f := TopText.Font.Size;
      f := f + Delta;
      if (f > 10) and (f < 150) then
        TopText.Font.Size := Round(f);
      Caption := Format('TopText.Font.Size = %d', [Round(f)]);
    end;

    faBottomSize:
    begin
      f := BottomText.Font.Size;
      f := f + Delta;
      if (f > 10) and (f < 150) then
        BottomText.Font.Size := Round(f);
      Caption := Format('BottomText.Font.Size = %d', [Round(f)]);
    end;

    faTopMargin:
    begin
      f := TopText.Margins.Top;
      f := f + Delta;
      if (f >= 0) and (f <= 30) then
        TopText.Margins.Top := Round(f);
      Caption := Format('TopText.Margins.Top = %d', [Round(f)]);
    end;

    faBottomMargin:
    begin
      f := BottomText.Margins.Bottom;
      f := f + Delta;
      if (f >= 0) and (f <= 30) then
        BottomText.Margins.Bottom := Round(f);
      Caption := Format('BottomText.Margins.Bottom = %d', [Round(f)]);
    end;

    faTopGlow:
    begin
      f := TopGlow.Softness;
      f := f + 0.05 * Delta;
      if f < 0.01 then
        f := 0.01;
      if f > 0.99 then
        f := 0.99;
      TopGlow.Softness := f;
      Caption := Format('TopGlow.Softness = %.1g', [f]);
    end;

    faBottomGlow:
    begin
      f := BottomGlow.Softness;
      f := f + 0.05 * Delta;
      if f < 0.01 then
        f := 0.01;
      if f > 0.99 then
        f := 0.99;
      BottomGlow.Softness := f;
      Caption := Format('BottomGlow.Softness = %.1g', [f]);
    end;

  end;
end;

procedure TFormMain.InitChecker;
begin
  CreateCheckerBitmap;

  CheckerImage := TImage.Create(Self);
  CheckerImage.Parent := Self;
  CheckerImage.Bitmap := CheckerBitmap;
  CheckerImage.WrapMode := TImageWrapMode.Tile;
  CheckerImage.CanFocus := False;
  CheckerImage.SendToBack;

  UpdateChecker;
end;

procedure TFormMain.BottomEditKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  BottomText.Text := BottomEdit.Text;
end;

procedure TFormMain.ClearImage;
begin
  CheckerImage.Bitmap.Clear(claPurple);
end;

procedure TFormMain.CopyBitmap;
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create(Round(ClientWidth), Round(ClientHeight));
  bmp.Clear(0);
  if bmp.Canvas.BeginScene then
  try
    PaintTo(bmp.Canvas);
  finally
    bmp.Canvas.EndScene;
  end;
//  CopyBitmapToClipboard(bmp);
  bmp.Free;
end;

procedure TFormMain.UpdateChecker;
begin
  CheckerImage.Position.X := 0;
  CheckerImage.Position.Y := 0;
  CheckerImage.Width := ClientWidth;
  CheckerImage.Height := ClientHeight;
end;

procedure TFormMain.InitDropTarget;
begin
  if Assigned(DropTarget) then
  begin
    DropTarget.Width := 180;
    DropTarget.Height := 120;
    DropTarget.Position.X := 10;
    DropTarget.Position.Y := 200;
    DropTarget.Filter := '*.jpeg;*.jpg;*.png';
    DropTarget.Text := 'drop jpg or png';
    DropTarget.OnDropped := DropTargetDropped;
  end;
end;

procedure TFormMain.DropTargetDropped(Sender: TObject; const Data: TDragObject;
  const Point: TPointF);
var
  fn: string;
begin
  if Length(Data.Files) = 1 then
  begin
    fn := Data.Files[0];
    OnDropTargetDropped(fn);
  end;
end;

procedure TFormMain.TopEditKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  TopText.Text := TopEdit.Text;
end;

procedure TFormMain.SetDropTargetVisible(const Value: Boolean);
begin
  FDropTargetVisible := Value;
  if Value then
  begin
    if not Assigned(DropTarget) then
    begin
      DropTarget := TDropTarget.Create(Self);
      DropTarget.Parent := Self;
      InitDropTarget;
    end
    else
    begin
      DropTarget.Visible := True;
      DropTarget.BringToFront;
    end;
  end
  else
  begin
    DropTarget.Visible := False;
  end;
end;

procedure TFormMain.CreateCheckerBitmap;
var
  cb: TBitmap;
  sr, dr: TRectF;
  d: Integer;
begin
  d := 30;
  sr := RectF(0, 0, d, d);

  cb := TBitmap.Create(d, d);
  cb.Canvas.BeginScene;
  cb.Canvas.Clear(claDarkGray);
  cb.Canvas.EndScene;

  CheckerBitmap := TBitmap.Create(2*d, 2*d);
  CheckerBitmap.Canvas.BeginScene;
  CheckerBitmap.Canvas.Clear(claGray);

  dr.Left := 0;
  dr.Top := 0;
  dr.Right := dr.Left + d;
  dr.Bottom := dr.Top + d;

  CheckerBitmap.Canvas.DrawBitmap(cb, sr, dr, 1.0);
  dr.Left := dr.Left + d;
  dr.Top := dr.Top + d;
  dr.Left := dr.Right + d;
  dr.Top := dr.Bottom + d;
  CheckerBitmap.Canvas.DrawBitmap(cb, sr, dr, 1.0);

  CheckerBitmap.Canvas.EndScene;

  cb.Free;
end;

procedure TFormMain.CycleFontP;
begin
  Inc(fo);
  if fo > MaxFont then
    fo := 1;
  CycleFont(fo);
end;

procedure TFormMain.CycleFontM;
begin
  Dec(fo);
  if fo < 1 then
    fo := MaxFont;
  CycleFont(fo);
end;

procedure TFormMain.CycleFont(Value: Integer);
var
  s: string;
begin
  case Value of
    1: s := 'Stencil';
    2: s := 'Showcard Gothic';
    3: s := 'Sitka Text';
    4: s := 'Playbill';
    5: s := 'Old English Text MT';
    6: s := 'Small Fonts';
    7: s := 'Vivaldi';
    8: s := 'Vladimir Script';
    9: s := 'Comic Sans MS';
  end;

  if fa = faTopSize then
    TopText.Font.Family := s;
  if fa = faBottomSize then
    BottomText.Font.Family := s;

  Caption := s;
end;

procedure TFormMain.OnDropTargetDropped(fn: string);
begin
  CheckerBitmap.LoadFromFile(fn);
  CheckerImage.Bitmap := CheckerBitmap;
  CheckerImage.WrapMode := TImageWrapMode.Fit;
end;

function TFormMain.FindTarget(P: TPointF; const Data: TDragObject): IControl;
var
  i: Integer;
  NewObj: IControl;
begin
  Result := nil;
  for i := ChildrenCount - 1 downto 0 do
    if Supports(Children[i], IControl, NewObj) and NewObj.Visible and NewObj.HitTest then
    begin
      NewObj := NewObj.FindTarget(P, Data);

      if Assigned(NewObj) then
        Exit(NewObj);
    end;
end;

procedure TFormMain.Reset;
begin
  case TestID of
    0:
    begin
      //DefaultCaption := 'Federgraph Meme Builder App, 2016 Victory Edition';
      DefaultCaption := Application.Title;

      TopText.Text := 'Made with Delphi';
      BottomText.Text := 'FMX Meme Builder !!!';

      TopText.Font.Size := 58;
      BottomText.Font.Size := 84;

      TopText.Font.Family := 'Showcard Gothic';
      BottomText.Font.Family := 'Vladimir Script';
    end;

    1:
    begin
      DefaultCaption := 'Federgraph Meme Builder App';

      // in dinghy sailing topic, right of way rules apply:
      TopText.Text := 'Hau ab - Mach Platz - Raum !!!';
      BottomText.Text := 'Here comes 420 GER 5XXXX';

      TopText.Font.Size := 70;
      BottomText.Font.Size := 70;

      TopText.Font.Family := 'Vladimir Script';
      BottomText.Font.Family := 'Stencil';
    end;

  end;

  TopEdit.Text := TopText.Text;
  BottomEdit.Text := BottomText.Text;

  TopText.Margins.Top := 10;
  BottomText.Margins.Bottom := 10;

  TopGlow.Softness := 0.4;
  BottomGlow.Softness := 0.4;

  Caption := DefaultCaption;
end;

procedure TFormMain.UpdateParam(afa: Integer);
var
  s: string;
begin
  fa := afa;
  case fa of
    faTopMargin: s := 'Margin Top';
    faBottomMargin: s := 'Margin Bottom';
    faTopSize: s := 'Font Size Top';
    faBottomSize: s := 'Font Size Bottom';
    faTopGlow: s := 'Glow Softness Top';
    faBottomGlow: s := 'Glow Softness Botton';
    else
      s := 'unknown param key';
  end;
  Caption := s;
end;

end.
