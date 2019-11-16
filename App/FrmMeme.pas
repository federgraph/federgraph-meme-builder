unit FrmMeme;

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
{$ifdef MACOS}
  MacApi.Appkit,
  Macapi.CoreFoundation,
  Macapi.Foundation,
{$endif}
{$ifdef MSWINDOWS}
  Winapi.Messages,
  Winapi.Windows,
{$endif}
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.Rtti,
  FMX.Platform,
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
  FMX.Surfaces,
  FMX.Controls.Presentation;

type
  TSelectedText = (
    stTop,
    stBottom
  );

  TFormMeme = class(TForm)
    TopText: TText;
    BottomText: TText;
    BottomGlow: TGlowEffect;
    TopGlow: TGlowEffect;
    TopEdit: TEdit;
    BottomEdit: TEdit;
    HelpText: TText;
    ReportText: TText;
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
    DropTarget: TDropTarget;
    CheckerBitmap: TBitmap;
    CheckerImage: TImage;
    FDropTargetVisible: Boolean;
    DefaultCaption: string;
    TextID: Integer;
    SelectedText: TSelectedText;
    FontFamilyList: TStringList;
    HasOfficeFonts: Boolean;
    WantOfficeFonts: Boolean;
    UseOfficeFonts: Boolean;
    SL: TStringList;
    procedure CopyBitmapToClipboard(ABitmap: TBitmap);
    procedure CopyBitmap;
    procedure CreateCheckerBitmap;
    procedure InitChecker;
    procedure InitDropTarget;
    procedure InitHelpText;
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
//    procedure UpdateCaption;
    procedure UpdateParam(afa: Integer);
    procedure SetBitmap(value: TBitmap);
    function GetParamText: string;
    procedure InitOfficeFonts;
    procedure InitNormalFonts;
    procedure CollectFonts(FontList: TStringList);
    procedure InitFontList;
    function GetSelectedText: string;
    procedure UpdateReport;
    function IsShiftKeyPressed: Boolean;
    procedure AdaptFormSize;
    procedure PasteBitmapFromClipboard;
    property DropTargetVisible: Boolean read FDropTargetVisible write SetDropTargetVisible;
  protected
    function FindTarget(P: TPointF; const Data: TDragObject): IControl; override;
  public
    property Background: TBitmap read CheckerBitmap write SetBitmap;
  end;

var
  FormMeme: TFormMeme;

implementation

{$R *.fmx}

const
  faTopMargin = 1;
  faBottomMargin = 2;
  faTopSize = 3;
  faBottomSize = 4;
  faTopGlow = 5;
  faBottomGlow = 6;

  MaxTextID = 4;
  MaxEdgeDistance = 200;
  BoolStr: array[Boolean] of string = ('False', 'True');

procedure TFormMeme.FormCreate(Sender: TObject);
begin
{$ifdef DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$endif}
  FormatSettings.DecimalSeparator := '.';

  FontFamilyList := TStringList.Create;

  WantOfficeFonts := True;
  InitFontList;

  Reset;
  Caption := DefaultCaption;

  InitChecker;

  TopText.BringToFront;
  BottomText.BringToFront;

  TopEdit.Visible := false;
  BottomEdit.Visible := false;
  ReportText.Visible := false;

  if Application.Title = 'FC96' then
    DropTargetVisible := true;

  InitHelpText;
  SL := TStringList.Create;
end;

procedure TFormMeme.FormDestroy(Sender: TObject);
begin
  CheckerBitmap.Free;
  FontFamilyList.Free;
  SL.Free;
end;

procedure TFormMeme.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if Key = vkEscape then
  begin
    HelpText.Visible := False;
    ReportText.Visible := False;
    DropTargetVisible := False;
    TopEdit.Visible := not TopEdit.Visible;
    BottomEdit.Visible := TopEdit.Visible;
    Caption := DefaultCaption;
  end;

  if TopEdit.Visible then
  begin
    { do nothing when editing, exit here }
  end

  else if Key = vkC then
  begin
    CopyBitmap;
    Caption := 'Bitmap copied.';
  end

  else if Key = vkV then
  begin
    PasteBitmapFromClipboard;
    Caption := 'Bitmap pasted.';
  end

  else if KeyChar = 'b' then
  begin
    SelectedText := TSelectedText.stBottom;
    Caption := 'Bottom Text';
  end

  else if KeyChar = 'a' then
    AdaptFormSize

  else if KeyChar = 'd' then
  begin
    DropTargetVisible := not DropTargetVisible;
    ReportText.Visible := false;
  end

  else if KeyChar = 'c' then
    ClearImage

  else if KeyChar = 'f' then
    CycleFontP

  else if KeyChar = 'F' then
    CycleFontM

  else if KeyChar = 'g' then
    UpdateParam(faTopGlow)

  else if KeyChar = 'G' then
    UpdateParam(faBottomGlow)

  else if KeyChar = 'h' then
  begin
    DropTargetVisible := False;
    HelpText.Visible := not HelpText.Visible;
    ReportText.Visible := False;
  end

  else if KeyChar = 'm' then
    UpdateParam(faTopMargin)

  else if KeyChar = 'M' then
    UpdateParam(faBottomMargin)

  else if KeyChar = 'o' then
  begin
    if HasOfficeFonts then
      InitOfficeFonts;
  end

  else if KeyChar = 'O' then
  begin
    InitNormalFonts;
  end

  else if KeyChar = 'r' then
  begin
    Caption := DefaultCaption;
    DropTargetVisible := False;
    HelpText.Visible := False;
    if not ReportText.Visible then
      ReportText.Visible := True;
  end

  else if KeyChar = 'R' then
    Reset

  else if KeyChar = 's' then
    UpdateParam(faTopSize)

  else if KeyChar = 'S' then
    UpdateParam(faBottomSize)

  else if KeyChar = 't' then
  begin
    SelectedText := TSelectedText.stTop;
    Caption := 'Top Text';
  end

  else if KeyChar = 'x' then
  begin
    Inc(TextID);
    TextID := TextID mod 2;
    Reset;
  end

  else if KeyChar = 'y' then
  begin
    Inc(TextID);
    if TextID > MaxTextID then
      TextID := 0;
    Reset;
  end

  else if KeyChar = 'Y' then
  begin
    Dec(TextID);
    if TextID < 0 then
      TextID := MaxTextID;
    Reset;
  end

  else if KeyChar = '1' then
  begin
    { new default landscape }
    UpdateFormat(1000, 750);
  end

  else if KeyChar = '2' then
    UpdateFormat(800, 600)

  else if KeyChar = '3' then
    UpdateFormat(640, 480)

  else if KeyChar = '8' then
    UpdateFormat(800, 800)

  else if KeyChar = '9' then
    UpdateFormat(900, 900)

  else if KeyChar = '0' then
  begin
    { new default portrait }
    { iPad Screenshot Portrait, scaled to fit on HD Screen }
    Top := 0;
    UpdateFormat(750, 1000)
  end;

  if ReportText.Visible then
    UpdateReport;
end;

procedure TFormMeme.InitHelpText;
var
  ML: TStringList;
begin
  ML := TStringList.Create;

  ML.Add('  Esc  - toggle text edit controls');
  ML.Add('');
  ML.Add('KeyChar Legend (active when edits not visible):');
  ML.Add('  h    - toggle help text');
  ML.Add('  d    - toggle drop target');
  ML.Add('  b, t - select bottom or top text for font change');
  ML.Add('  f, F - cycle Font for current text (forward, backwards)');
  ML.Add('  o, O - use Office fonts or not');
  ML.Add('  s, S - param Font Size for top or bottom text');
  ML.Add('  m, M - param Margin for top or bottom text');
  ML.Add('  g, G - param Glow Softness for top or bottom text');
  ML.Add('  x    - toggle between Text0 and Text1');
  ML.Add('  y, Y - cycle through Text templates');
  ML.Add('  r    - togle Report');
  ML.Add('  R    - Reset');
//  ML.Add('  a    - adapt form size');
  ML.Add('  c    - clear image command');
  ML.Add('  ^c   - copy image to clipboard command');
  ML.Add('  1, 2, 3, 8, 9, 0 - Window Format selection');

  HelpText.Text := ML.Text;
  HelpText.AutoSize := False;
  HelpText.AutoSize := True;
  HelpText.Visible := False;

  ML.Free;
end;

procedure TFormMeme.UpdateReport;
begin
  SL.Clear;
  SL.Add(Application.Title + ' see Federgraph.de');
  Sl.Add('');
  SL.Add('TopText.Font.Family = ' + TopText.Font.Family);
  SL.Add('BottomText.Font.Family = ' + BottomText.Font.Family);
  SL.Add(Format('TopText.Font.Family = %.g', [TopText.Font.Size]));
  SL.Add(Format('BottomText.Font.Family = %.g', [BottomText.Font.Size]));
  SL.Add('');
  SL.Add('UseOfficeFonts = ' + BoolStr[UseOfficeFonts]);
  SL.Add('TextID = ' + IntToStr(TextID));
  SL.Add('Current Text = ' + GetSelectedText);
  SL.Add('Current Param = ' + GetParamText);
  SL.Add(Format('Client-W-H = (%d, %d)', [ClientWidth, ClientHeight]));
  SL.Add(Format('Bitmap-W-H = (%d, %d)', [CheckerBitmap.Width, CheckerBitmap.Height]));
  SL.Add(Format('Handle.Scale = %.1f', [Handle.Scale]));

  ReportText.Text := SL.Text;
  ReportText.Visible := True;
end;

procedure TFormMeme.UpdateFormat(w, h: Integer);
begin
  ClientWidth := w;
  ClientHeight := h;
  Caption := Format('%d x %d', [ClientWidth, ClientHeight]);
end;

procedure TFormMeme.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  if WheelDelta > 0 then
    HandleWheel(1)
  else
    HandleWheel(-1)
end;

procedure TFormMeme.FormResize(Sender: TObject);
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

function TFormMeme.GetSelectedText: string;
begin
  if SelectedText = stTop then
    result := 'Top'
  else
    result := 'Bottom';
end;

function TFormMeme.GetParamText: string;
begin
  case fa of
    faTopMargin: result := 'Margin Top';
    faBottomMargin: result := 'Margin Bottom';
    faTopSize: result := 'Font Size Top';
    faBottomSize: result := 'Font Size Bottom';
    faTopGlow: result := 'Glow Softness Top';
    faBottomGlow: result := 'Glow Softness Botton';
    else
      result := 'None';
  end;
end;

procedure TFormMeme.HandleWheel(Delta: Integer);
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
      if (f >= 0) and (f <= MaxEdgeDistance) then
        TopText.Margins.Top := Round(f);
      Caption := Format('TopText.Margins.Top = %d', [Round(f)]);
    end;

    faBottomMargin:
    begin
      f := BottomText.Margins.Bottom;
      f := f + Delta;
      if (f >= 0) and (f <= MaxEdgeDistance) then
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

procedure TFormMeme.InitChecker;
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

procedure TFormMeme.BottomEditKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  BottomText.Text := BottomEdit.Text;
end;

procedure TFormMeme.ClearImage;
begin
  CheckerImage.Bitmap.Clear(claPurple);
end;

procedure TFormMeme.CopyBitmap;
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
  CopyBitmapToClipboard(bmp);
  bmp.Free;
end;

procedure TFormMeme.UpdateChecker;
begin
  CheckerImage.Position.X := 0;
  CheckerImage.Position.Y := 0;
  CheckerImage.Width := ClientWidth;
  CheckerImage.Height := ClientHeight;
end;

procedure TFormMeme.InitDropTarget;
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

procedure TFormMeme.DropTargetDropped(Sender: TObject; const Data: TDragObject;
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

procedure TFormMeme.TopEditKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  TopText.Text := TopEdit.Text;
end;

procedure TFormMeme.SetDropTargetVisible(const Value: Boolean);
begin
  FDropTargetVisible := Value;
  if Value then
  begin
    HelpText.Visible := False;
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
  else if DropTarget <> nil then
  begin
    DropTarget.Visible := False;
  end;
end;

procedure TFormMeme.CreateCheckerBitmap;
var
  cb: TBitmap;
  sr, dr: TRectF;
  d: Integer;
begin
  d := 100;
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

procedure TFormMeme.CycleFontP;
begin
  Inc(fo);
  if fo >= FontFamilyList.Count then
    fo := 0;
  CycleFont(fo);
end;

procedure TFormMeme.CycleFontM;
begin
  Dec(fo);
  if fo < 0 then
    fo := FontFamilyList.Count-1;
  CycleFont(fo);
end;

procedure TFormMeme.InitOfficeFonts;
var
  ML: TStrings;
begin
  UseOfficeFonts := True;
  ML := FontFamilyList;
  ML.Clear;
  { I thought these fonts would be useful }
  ML.Add('Stencil');
  ML.Add('Showcard Gothic');
  ML.Add('Sitka Text'); // 8.1
  ML.Add('Playbill'); // Office Font
  ML.Add('Old English Text MT');
  ML.Add('Small Fonts');
  ML.Add('Vivaldi'); // Office Font
  ML.Add('Vladimir Script'); // Office Font
  ML.Add('Comic Sans MS'); // 95
end;

procedure TFormMeme.InitNormalFonts;
var
  ML: TStrings;
begin
// wikipedia: List_of_typefaces_included_with_Microsoft_Windows

  UseOfficeFonts := False;
  ML := FontFamilyList;
  ML.Clear;
  { What fonts should I use when Office Fonts are not available ? }
  ML.Add('Arial'); // 3.1
  ML.Add('Courier New'); // 3.1
  ML.Add('Times New Roman'); // 3.1
  ML.Add('Consolas'); // Vista
  ML.Add('Verdana'); // 95
  ML.Add('Calibri'); // Vista
  ML.Add('Lucida Handwriting'); // 98
  ML.Add('Impact'); // 98
  ML.Add('Comic Sans MS'); // 95
end;

procedure TFormMeme.CycleFont(Value: Integer);
var
  s: string;
begin
  if (Value >= 0) and (Value < FontFamilyList.Count) then
    s := FontFamilyList[Value]
  else
    s := 'Arial';

  if SelectedText = TSelectedText.stTop then
    TopText.Font.Family := s
  else
    BottomText.Font.Family := s;

  Caption := IntToStr(Value) + ' ' + s;
end;

procedure TFormMeme.OnDropTargetDropped(fn: string);
begin
  CheckerBitmap.LoadFromFile(fn);
  CheckerImage.Bitmap := CheckerBitmap;
  CheckerImage.WrapMode := TImageWrapMode.Fit;

  if IsShiftKeyPressed then
    AdaptFormSize;
end;

function TFormMeme.FindTarget(P: TPointF; const Data: TDragObject): IControl;
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

procedure TFormMeme.Reset;
var
 fs: Integer;
 fn: string;
begin
  DefaultCaption := Application.Title;

  TopText.Text := 'Made with Delphi';
  BottomText.Text := 'FMX Meme Builder.';

  fs := 84;
  fn := 'Impact';

  case TextID of

    1: ;

    2:
    begin
      if UseOfficeFonts then
        fn := 'Vladimir Script';
    end;

    3:
    begin
      if UseOfficeFonts then
        fn := 'Vivaldi'
      else
        fn := 'Verdana';
    end;

    { insert new text templates here, and update MaxTextID }

    MaxTextID:
    begin
      DefaultCaption := 'Federgraph Meme Builder App';

      TopText.Text := 'Press Escape to edit text';
      BottomText.Text := '#Remain';

      fs := 60;
      TopText.Font.Size := 32;
      BottomText.Font.Size := fs;

      if UseOfficeFonts then
        fn := 'Stencil'
      else
        fn := 'Impact';
    end;

    else
    begin
      { TextID === 0 }
      DefaultCaption := 'Federgraph Meme Builder App';

      TopText.Text := 'federgraph.de/federgraph-meme-builder.html';
      BottomText.Text := 'press h to toggle help text';

      fs := 24;
      TopText.Font.Size := fs;
      BottomText.Font.Size := fs;

      fn := 'Courier New';
    end;

  end;

  if fs = 84 then
  begin
    TopText.Font.Size := fs;
    BottomText.Font.Size := fs;
  end;

  TopText.Font.Family := fn;
  BottomText.Font.Family := TopText.Font.Family;

  TopEdit.Text := TopText.Text;
  BottomEdit.Text := BottomText.Text;

  TopText.Margins.Top := 10;
  BottomText.Margins.Bottom := 10;

  TopGlow.Softness := 0.4;
  BottomGlow.Softness := 0.4;

  Caption := DefaultCaption;
end;

//procedure TFormMeme.UpdateCaption;
//var
//  sa, sb, sc: string;
//begin
//  sa := Application.Title;
//  sb := GetSelectedText;
//  sc := GetParamText;
//  Caption := Format('%s, Selected: %s, Param: %s, TextID: %d', [sa, sb, sc, TextID]);
//end;

procedure TFormMeme.UpdateParam(afa: Integer);
begin
  fa := afa;

  case fa of
    faTopMargin,
    faTopSize,
    faTopGlow: SelectedText := TSelectedText.stTop;
    else
      SelectedText := stBottom;
  end;

  Caption := GetParamText;
end;

procedure TFormMeme.CopyBitmapToClipboard(ABitmap: TBitmap);
var
  Svc: IFMXClipboardService;
begin
  if not Assigned(ABitmap) then
    Exit;
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc) then
    Svc.SetClipboard(ABitmap);
end;

procedure TFormMeme.PasteBitmapFromClipboard;
var
  Svc: IFMXClipboardService;
  Value: TValue;
  Bitmap: TBitmap;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc) then
  begin
    Value := Svc.GetClipboard;
    if not Value.IsEmpty then
    begin
      if Value.IsType<string> then
      begin
        { do nothing }
      end
      else if Value.IsType<TBitmapSurface> then
      try
        Bitmap := TBitmap.Create;
        try
          Bitmap.Assign(Value.AsType<TBitmapSurface>);
          Background := Bitmap;
        finally
          Bitmap.Free;
        end;
      finally
        Value.AsType<TBitmapSurface>.Free;
      end;
    end;
  end;
end;

procedure TFormMeme.SetBitmap(value: TBitmap);
// var
//   s: single;
begin
  CheckerImage.Bitmap.Clear(claPurple);

  if Application.Title = 'FC96' then
  begin
    { scale value.Width before assignment ? }
//    s := Handle.Scale;
//    ClientWidth := Round(value.Width / s);
//    ClientHeight := Round(value.Height / s);

    ClientWidth := value.Width;
    ClientHeight := value.Height;
  end
  else
  begin
    {
      In Federgraph App the droptarget is not shown initially,
      Bitmap will be assigned before the form is shown.
      ClientWidth and ClientHeight will be assigned there.
    }
  end;

  CheckerImage.Bitmap := value;
  CheckerImage.WrapMode := TImageWrapMode.Fit;

  AdaptFormSize;
end;

{$ifdef MSWINDOWS}
function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; Data: Pointer): Integer; stdcall;
var
  S: TStrings;
  Temp: string;
begin
  S := TStrings(Data);
  Temp := LogFont.lfFaceName;
  if (S.Count = 0) or (AnsiCompareText(S[S.Count-1], Temp) <> 0) then
    S.Add(Temp);
  Result := 1;
end;
{$endif}

procedure TFormMeme.CollectFonts(FontList: TStringList);
var
{$ifdef MACOS}
  fManager: NsFontManager;
  list:NSArray;
  lItem:NSString;
  i: Integer;
{$endif}
{$ifdef MSWINDOWS}
  DC: HDC;
  LFont: TLogFont;
{$endif}
begin
// stackoverflow: how-to-get-the-list-of-fonts-available-delphi-xe3-firemonkey-2

{ not yet tested on MACOS at all }

{$ifdef MACOS}
  fManager := TNsFontManager.Wrap(TNsFontManager.OCClass.sharedFontManager);
  list := fManager.availableFontFamilies;
  if (List <> nil) and (List.count > 0) then
  begin
    for i := 0 to List.Count-1 do
    begin
      lItem := TNSString.Wrap(List.objectAtIndex(i));
      FontList.Add(String(lItem.UTF8String))
    end;
  end;
{$endif}
{$ifdef MSWINDOWS}
  DC := GetDC(0);
  FillChar(LFont, sizeof(LFont), 0);
  LFont.lfCharset := DEFAULT_CHARSET;
  EnumFontFamiliesEx(DC, LFont, @EnumFontsProc, Winapi.Windows.LPARAM(FontList), 0);
  ReleaseDC(0, DC);
{$endif}
end;

procedure TFormMeme.InitFontList;
var
  f: string;
  SL: TStringList;
begin
  HasOfficeFonts := True;
  InitOfficeFonts;

  SL := TStringList.Create;
  try
    CollectFonts(SL);

    for f in FontFamilyList do
    begin
      if SL.IndexOf(f) = -1 then
      begin
        HasOfficeFonts := False;
        break;
      end;
    end;

  finally
    SL.Free;
  end;

  UseOfficeFonts := HasOfficeFonts and WantOfficeFonts;

  if not UseOfficeFonts then
    InitNormalFonts;
end;

function TFormMeme.IsShiftKeyPressed: Boolean;
begin
// stackoverflow: getkeystate-in-firemonkey
{$IFDEF MSWINDOWS}
  Result := GetKeyState(VK_SHIFT) < 0;
{$ELSE}
  Result := NSShiftKeyMask and TNSEvent.OCClass.modifierFlags = NSShiftKeyMask;
{$ENDIF}
end;

procedure TFormMeme.AdaptFormSize;
var
  w, h: Integer;
  wmin, hmin: Integer;
  wmax, hmax: Integer;
  dw, dh: Integer;
begin
  if CheckerImage.Bitmap = nil then
    Exit;

  wmin := 512;
  hmin := 512;

  Screen.UpdateDisplayInformation;

  dw := Width - ClientWidth;
  dh := Height - ClientHeight;
  wmax := Round(Screen.WorkAreaWidth / Handle.Scale) - dw;
  hmax := Round(Screen.WorkAreaHeight / Handle.Scale) - dh;

  w := CheckerImage.Bitmap.Width;
  h := CheckerImage.Bitmap.Height;

  if (w < wmin) or (h < hmin) then
    Exit;

  { scale down height }
  if h > hmax then
  begin
    w := Round(w * hmax / h);
    h := hmax;
  end;

  { then scale down width }
  if w > wmax then
  begin
    h := Round(h * wmax / w);
    w := wmax;
  end;

  { check again }
  if (w < wmin) or (h < hmin) then
    Exit;

  if w <> ClientWidth then
    ClientWidth := w;
  if h <> ClientHeight then
    ClientHeight := h;

  if Top + Height >= Round(Screen.WorkAreaHeight / Handle.Scale) then
    Top := 0;

  if Left + Width >= Round(Screen.WorkAreaWidth / Handle.Scale) then
    Left := 0;
end;

end.
