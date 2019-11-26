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
  FMX.Controls.Presentation,
  RiggVar.MB.Def,
  FMX.ScrollBox,
  FMX.Memo;

type
  TFormMeme = class(TForm)
    TopText: TText;
    BottomText: TText;
    BottomGlow: TGlowEffect;
    TopGlow: TGlowEffect;
    HelpText: TText;
    ReportText: TText;
    TopEdit: TMemo;
    BottomEdit: TMemo;
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
    FParam: TMemeParam;
    FSelectedText: TSelectedText;
    FActiveSampleManagerID: Integer;
    fo: Integer;
    DropTarget: TDropTarget;
    CheckerBitmap: TBitmap;
    CheckerImage: TImage;
    FDropTargetVisible: Boolean;
    DefaultCaption: string;
    FontFamilyList: TStringList;
    HasOfficeFonts: Boolean;
    WantOfficeFonts: Boolean;
    SL: TStringList;
    WantNormal: Boolean;
    FScale: single;
    ScreenshotSaver: IScreenshotSaver;
    SampleManager00: ISampleTextManager;
    SampleManager01: ISampleTextManager;
    SampleManager02: ISampleTextManager;
    SampleManager: ISampleTextManager;
    FUseOfficeFonts: Boolean;
    ColorIndexDark: Integer;
    ColorIndexLight: Integer;
    Picker: IPicker;
    DefaultMargin: single;
    Raster: Integer;
    procedure CopyBitmapToClipboard(ABitmap: TBitmap);
    procedure CopyBitmap;
    procedure CreateCheckerBitmap;
    procedure InitChecker(WantNewChecker: Boolean = true);
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
    procedure UpdateParam(fp: TMemeParam);
    procedure SetBitmap(value: TBitmap);
    function GetParamText: string;
    procedure InitOfficeFonts;
    procedure InitNormalFonts;
    procedure InitFontList;
    function GetSelectedText: string;
    procedure UpdateReport;
    procedure AdaptFormSize;
    procedure PasteBitmapFromClipboard;
    procedure ToggleTiling;
    procedure ToggleFontColor;
    procedure GotoLandscape;
    procedure GotoNormal;
    procedure GotoPortrait;
    procedure GotoSquare;
    procedure Flash(s: string);
    procedure ToggleTextColor;
    procedure SetUseOfficeFonts(const Value: Boolean);
    procedure InitMemo(Memo: TMemo);
    procedure SwapText;
    function GetCurrentTextControl: TText;
    function GetScreenshot: TBitmap;
    procedure SaveBitmap;
    procedure PickFont;
    procedure PickColor;
    procedure CycleColorDark(delta: Integer = 1);
    procedure CycleColorLight(delta: Integer = 1);
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
    function GetActionFromKey(Key: Word): Integer;
    function GetActionFromKeyChar(KeyChar: char): Integer;
    procedure ToggleEdits;
    procedure HA(fa: Integer);
    function GetSampleIndex: Integer;
    property DropTargetVisible: Boolean read FDropTargetVisible write SetDropTargetVisible;
    property UseOfficeFonts: Boolean read FUseOfficeFonts write SetUseOfficeFonts;
  protected
    function FindTarget(P: TPointF; const Data: TDragObject): IControl; override;
  public
    procedure HandleAction(fa: Integer);
    property Background: TBitmap read CheckerBitmap write SetBitmap;
    property Param: TMemeParam read FParam;
    property SelectedText: TSelectedText read FSelectedText;
    property ActiveSampleManagerID: Integer read FActiveSampleManagerID;
    property SampleIndex: Integer read GetSampleIndex;
    property IsDropTargetVisible: Boolean read FDropTargetVisible;
  end;

var
  FormMeme: TFormMeme;

implementation

{$R *.fmx}

uses
  RiggVar.MB.Picker,
  RiggVar.MB.SampleText00,
  RiggVar.MB.SampleText01,
  RiggVar.MB.SampleText02;

const
  MaxEdgeDistance = 200;
  BoolStr: array[Boolean] of string = ('False', 'True');

  HelpCaptionText = 'press h for help';
  ApplicationTitleText = 'FC96';

procedure TFormMeme.ApplicationEventsException(Sender: TObject; E: Exception);
begin
end;

procedure TFormMeme.FormCreate(Sender: TObject);
begin
{$ifdef DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$endif}
  FormatSettings.DecimalSeparator := '.';
  FScale := Handle.Scale;

  Application.OnException := ApplicationEventsException;

  Self.Position := TFormPosition.ScreenCenter;
  FontFamilyList := TStringList.Create;

  ScreenshotSaver := TScreenshotSaver.Create;

  SampleManager00 := TSampleTextManagerBase.Create;
  SampleManager01 := TSampleTextManager01.Create;
  SampleManager02 := TSampleTextManager02.Create;

  SampleManager := SampleManager00;

  Picker := TPicker.Create;

  WantOfficeFonts := True;
  InitFontList;

  Reset;

  InitChecker(True);

  { Raster := 70 if Form has Button-Frame }
  Raster := 0;
  DefaultMargin := Raster + 10;

  ReportText.Position.X := DefaultMargin;
  HelpText.Position.X := ReportText.Position.X;

  ReportText.Position.Y := Raster + 70 + 10;
  HelpText.Position.Y := ReportText.Position.Y;

  TopText.Margins.Top := DefaultMargin;
  TopText.Margins.Left := DefaultMargin;
  TopText.Margins.Right := DefaultMargin;

  BottomText.Margins.Bottom := DefaultMargin;
  BottomText.Margins.Left := DefaultMargin;
  BottomText.Margins.Right := DefaultMargin;

  TopText.BringToFront;
  BottomText.BringToFront;

  InitMemo(TopEdit);
  InitMemo(BottomEdit);

  TopEdit.BringToFront;
  BottomEdit.BringToFront;

  TopEdit.Visible := false;
  BottomEdit.Visible := false;
  ReportText.Visible := false;

  if Application.Title = 'FC96' then
  begin
    DropTargetVisible := true;
  end;

  InitHelpText;
  SL := TStringList.Create;

  Caption := HelpCaptionText;
end;

procedure TFormMeme.FormDestroy(Sender: TObject);
begin
  CheckerBitmap.Free;
  FontFamilyList.Free;
  SL.Free;
end;

procedure TFormMeme.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
var
  fa: Integer;
begin
  if Key = vkEscape then
    HA(faMemeToggleEdits)
  else if TopEdit.Visible then
    Exit
  else
  begin
    fa := GetActionFromKey(Key);
    if fa = faMemeNoop then
      fa := GetActionFromKeyChar(KeyChar);

    if fa <> faMemeNoop then
      HA(fa);

    if ReportText.Visible then
      UpdateReport;
  end
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
  ML.Add('  r    - toggle Report');
  ML.Add('  R    - Reset');
  ML.Add('  u    - toggle tile/fit');
  ML.Add('  v, V - toggle color, help text and meme text');
  ML.Add('  a    - adapt form size');
  ML.Add('  c, C   - clear image command');
  ML.Add('  ^c   - copy image to clipboard command');
  ML.Add('  1..9, 0 - Format selection');
  ML.Add('  1, p, q - Landscape, Portrait, Square');

  HelpText.Text := ML.Text;
  HelpText.AutoSize := False;
  HelpText.AutoSize := True;
  HelpText.Visible := False;

  ML.Free;
end;

procedure TFormMeme.UpdateReport;
begin
  SL.Clear;
  SL.Add(Application.Title + ', see Federgraph.de');
  Sl.Add('');
  SL.Add('TopText.Font.Family = ' + TopText.Font.Family);
  SL.Add('BottomText.Font.Family = ' + BottomText.Font.Family);
  SL.Add(Format('TopText.Font.Family = %.g', [TopText.Font.Size]));
  SL.Add(Format('BottomText.Font.Family = %.g', [BottomText.Font.Size]));
  SL.Add('');
  SL.Add('UseOfficeFonts = ' + BoolStr[UseOfficeFonts]);
  SL.Add('TextID = ' + IntToStr(SampleManager.GetCurrentIndex));
  SL.Add('Current Text = ' + GetSelectedText);
  SL.Add('Current Param = ' + GetParamText);
  SL.Add(Format('Client-W-H = (%d, %d)', [ClientWidth, ClientHeight]));
  SL.Add(Format('Bitmap-W-H = (%d, %d)', [CheckerBitmap.Width, CheckerBitmap.Height]));
  SL.Add(Format('Handle.Scale = %.1f', [Handle.Scale]));

  ReportText.Text := SL.Text;
  ReportText.Visible := True;
end;

procedure TFormMeme.ToggleEdits;
begin
  HelpText.Visible := False;
  ReportText.Visible := False;
  DropTargetVisible := False;
  TopEdit.Visible := not TopEdit.Visible;
  BottomEdit.Visible := TopEdit.Visible;
  Flash(DefaultCaption);
end;

procedure TFormMeme.UpdateFormat(w, h: Integer);
begin
  ClientWidth := w;
  ClientHeight := h;
  Flash(Format('%d x %d', [ClientWidth, ClientHeight]));
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
  case FParam of
    fpTopMargin: result := 'Margin Top';
    fpBottomMargin: result := 'Margin Bottom';
    fpTopSize: result := 'Font Size Top';
    fpBottomSize: result := 'Font Size Bottom';
    fpTopGlow: result := 'Glow Softness Top';
    fpBottomGlow: result := 'Glow Softness Botton';
    else
      result := 'None';
  end;
end;

procedure TFormMeme.HandleWheel(Delta: Integer);
var
  f: single;
begin
  case FParam of
    fpTopSize:
    begin
      f := TopText.Font.Size;
      f := f + Delta;
      if (f > 10) and (f < 150) then
        TopText.Font.Size := Round(f);
      Flash(Format('TopText.Font.Size = %d', [Round(f)]));
    end;

    fpBottomSize:
    begin
      f := BottomText.Font.Size;
      f := f + Delta;
      if (f > 10) and (f < 150) then
        BottomText.Font.Size := Round(f);
      Flash(Format('BottomText.Font.Size = %d', [Round(f)]));
    end;

    fpTopMargin:
    begin
      f := TopText.Margins.Top;
      f := f + 4 * Delta;
      if (f >= 0) and (f <= MaxEdgeDistance) then
        TopText.Margins.Top := Round(f);
      Flash(Format('TopText.Margins.Top = %d', [Round(f)]));
    end;

    fpBottomMargin:
    begin
      f := BottomText.Margins.Bottom;
      f := f + 4 * Delta;
      if (f >= 0) and (f <= MaxEdgeDistance) then
        BottomText.Margins.Bottom := Round(f);
      Flash(Format('BottomText.Margins.Bottom = %d', [Round(f)]));
    end;

    fpTopGlow:
    begin
      f := TopGlow.Softness;
      f := f + 0.05 * Delta;
      if f < 0.01 then
        f := 0.01;
      if f > 0.99 then
        f := 0.99;
      TopGlow.Softness := f;
      Flash(Format('TopGlow.Softness = %.1g', [f]));
    end;

    fpBottomGlow:
    begin
      f := BottomGlow.Softness;
      f := f + 0.05 * Delta;
      if f < 0.01 then
        f := 0.01;
      if f > 0.99 then
        f := 0.99;
      BottomGlow.Softness := f;
      Flash(Format('BottomGlow.Softness = %.1g', [f]));
    end;

  end;
end;

procedure TFormMeme.InitChecker(WantNewChecker: Boolean);
begin
  if CheckerImage = nil then
  begin
    CheckerImage := TImage.Create(Self);
    CheckerImage.Parent := Self;
  end;

  CheckerImage.Bitmap.Clear(claPurple);
  if WantNewChecker then
  begin
    CheckerBitmap.Free;
    CreateCheckerBitmap;
  end;

  CheckerImage.Bitmap := CheckerBitmap;
  CheckerImage.WrapMode := TImageWrapMode.Tile;
  CheckerImage.CanFocus := False;
  CheckerImage.SendToBack;

  UpdateChecker;
end;

procedure TFormMeme.ToggleTiling;
begin
  if CheckerImage.WrapMode = TImageWrapMode.Tile then
    CheckerImage.WrapMode := TImageWrapMode.Fit
  else
    CheckerImage.WrapMode := TImageWrapMode.Tile;
end;

procedure TFormMeme.ToggleFontColor;
begin
  if ReportText.TextSettings.FontColor = claWhite then
  begin
    ReportText.TextSettings.FontColor := claBlack;
    HelpText.TextSettings.FontColor := claBlack;
  end
  else
  begin
    ReportText.TextSettings.FontColor := claWhite;
    HelpText.TextSettings.FontColor := claWhite;
  end;
end;

procedure TFormMeme.ToggleTextColor;
begin
  if TopText.TextSettings.FontColor = claWhite then
  begin
    TopText.TextSettings.FontColor := claBlack;
    BottomText.TextSettings.FontColor := claBlack;
  end
  else
  begin
    TopText.TextSettings.FontColor := claWhite;
    BottomText.TextSettings.FontColor := claWhite;
  end;
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

function TFormMeme.GetSampleIndex: Integer;
begin
  result := 0;
  if SampleManager <> nil then
    result := SampleManager.GetCurrentIndex;
end;

function TFormMeme.GetScreenshot: TBitmap;
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
  result := bmp;
end;

procedure TFormMeme.CopyBitmap;
var
  bmp: TBitmap;
begin
  bmp := GetScreenshot;
  try
    CopyBitmapToClipboard(bmp);
    Flash('Bitmap copied.');
  finally
    bmp.Free;
  end;
end;

procedure TFormMeme.SaveBitmap;
var
  bmp: TBitmap;
  ret: Boolean;
begin
  if not ScreenshotSaver.CanSave then
  begin
    Flash('saving screenshot not possible');
    Exit;
  end;

  ret := False;
  bmp := GetScreenshot;
  try
    try
      ret := ScreenshotSaver.SaveScreenshot(bmp);
    except
    end;
  finally
    bmp.Free;
  end;

  if ret then
    Flash('Screenshot saved.')
  else
    Flash('TODO: check implementation of SaveScreenshot()');
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
    DropTarget.Position.X := DefaultMargin;
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

procedure TFormMeme.SetUseOfficeFonts(const Value: Boolean);
begin
  FUseOfficeFonts := Value;

  SampleManager00.SetUseOfficeFonts(Value);
  SampleManager01.SetUseOfficeFonts(Value);
  SampleManager02.SetUseOfficeFonts(Value);
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

  Flash(IntToStr(Value) + ' ' + s);
end;

procedure TFormMeme.OnDropTargetDropped(fn: string);
begin
  CheckerBitmap.LoadFromFile(fn);
  CheckerImage.Bitmap := CheckerBitmap;
  CheckerImage.WrapMode := TImageWrapMode.Fit;

  if Picker.IsShiftKeyPressed then
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
  i: TSampleTextItem;
begin
  i := SampleManager.GetSampleItem;

  if i.Top.Text = '' then
    i.Top.Text := ' ';
  if i.Bottom.Text = '' then
    i.Bottom.Text := ' ';
  if i.Top.FontSize < 12 then
    i.Top.FontSize := 12;
  if i.Bottom.FontSize < 12 then
    i.Bottom.FontSize := 12;

  TopText.Font.Size := i.Top.FontSize;
  BottomText.Font.Size := i.Bottom.FontSize;

  TopText.Font.Family := i.Top.FontName;
  BottomText.Font.Family := i.Bottom.FontName;

  TopText.Text := i.Top.Text;
  BottomText.Text := i.Bottom.Text;

  TopEdit.Text := i.Top.Text;
  BottomEdit.Text := i.Bottom.Text;

  TopText.Margins.Top := DefaultMargin;
  BottomText.Margins.Bottom := DefaultMargin;

  TopGlow.Softness := 0.4;
  BottomGlow.Softness := 0.4;

  DefaultCaption := i.Caption;

  Flash(DefaultCaption);
end;

procedure TFormMeme.UpdateParam(fp: TMemeParam);
begin
  FParam := fp;

  case FParam of
    fpTopMargin,
    fpTopSize,
    fpTopGlow: FSelectedText := TSelectedText.stTop;
    else
      FSelectedText := stBottom;
  end;

  Flash(GetParamText);
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
  // Delphi example: Multi-Device Samples\User Interface\CopyPaste
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
          Flash('Bitmap pasted.');
        finally
          Bitmap.Free;
        end;
      finally
        Value.AsType<TBitmapSurface>.Free;
      end;
    end;
  end;
end;

function TFormMeme.GetCurrentTextControl: TText;
begin
  if SelectedText = TSelectedText.stTop then
    result := TopText
  else
    result := BottomText;
end;

procedure TFormMeme.PickColor;
var
  tt: TText;
  cla: TAlphaColor;
begin
  tt := GetCurrentTextControl;
  cla := tt.TextSettings.FontColor;
  cla := Picker.SelectAlphaColor(cla);
  tt.TextSettings.FontColor := cla;
end;

procedure TFormMeme.PickFont;
var
  tt: TText;
  fn: string;
begin
  tt := GetCurrentTextControl;
  fn := tt.Font.Family;
  fn := Picker.SelectFontFamilyName(fn);
  tt.Font.Family := fn;
end;

procedure TFormMeme.SetBitmap(value: TBitmap);
begin
  CheckerImage.Bitmap.Clear(claPurple);

  { Application.Title is assigned/hardcoded in .dpr file }
  if Application.Title = ApplicationTitleText then
  begin
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

procedure TFormMeme.InitFontList;
var
  f: string;
  SL: TStringList;
begin
  HasOfficeFonts := True;
  InitOfficeFonts;

  SL := TStringList.Create;
  try
    Picker.CollectFontFamilyNames(SL);

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

  Flash('AdpatFormSize');
end;

procedure TFormMeme.GotoNormal;
begin
  if WindowState = TWindowState.wsMaximized then
    WindowState := TWindowState.wsNormal;

  Screen.UpdateDisplayInformation;

  if WantNormal then
  begin
    Top := 100;
    Left := 100;
    ClientWidth := 800;
    ClientHeight := 600;
  end;
end;

procedure TFormMeme.GotoLandscape;
begin
  GotoNormal;
  if Screen.Width > Screen.Height then
  begin
    Height := Round(Screen.WorkAreaHeight / FScale);
    ClientWidth := Round(ClientHeight * 4 / 3);
    Top := 0;
  end
  else
  begin
    Width := Round(Screen.WorkAreaWidth / FScale);
    ClientHeight := Round(ClientWidth * 3 / 4);
    Left := 0;
  end;
  Flash('Landscape');
end;

procedure TFormMeme.GotoPortrait;
begin
  GotoNormal;
  if Screen.Width > Screen.Height then
  begin
    Height := Round(Screen.WorkAreaHeight / FScale);
    ClientWidth := Round(ClientHeight * 3 / 4);
    Top := 0;
  end
  else
  begin
    Width := Round(Screen.WorkAreaWidth / FScale);
    ClientHeight := Round(ClientWidth * 4 / 3);
    Left := 0;
    Top := 0;
  end;
  Flash('Portrait');
end;

procedure TFormMeme.GotoSquare;
begin
  GotoNormal;
  if Screen.Width > Screen.Height then
  begin
    Height := Round(Screen.WorkAreaHeight / FScale);
    ClientWidth := Round(ClientHeight);
    Top := 0;
  end
  else
  begin
    Width := Round(Screen.WorkAreaWidth / FScale);
    ClientHeight := Round(ClientWidth);
    Left := 0
  end;
  Flash('Square');
end;

procedure TFormMeme.Flash(s: string);
begin
  Caption := s;
end;

procedure TFormMeme.InitMemo(Memo: TMemo);
begin
  Memo.Position.X := DefaultMargin;
  Memo.Width := ClientWidth - 2 * DefaultMargin;

  Memo.ControlType := TControlType.Styled;
  Memo.StyledSettings := [];
  Memo.ShowScrollBars := True;
  Memo.TextSettings.Font.Family := 'Consolas';
  Memo.TextSettings.Font.Size := 15;
  Memo.TextSettings.FontColor := claBlue;

  Memo.WordWrap := True;

  Memo.Anchors := [
    TAnchorKind.akLeft,
    TAnchorKind.akTop,
    TAnchorKind.akRight
    ];
end;

procedure TFormMeme.SwapText;
var
  s: string;
begin
  s := TopText.Text;
  TopText.Text := BottomText.Text;
  BottomText.Text := s;
end;

procedure TFormMeme.CycleColorDark(delta: Integer);
var
  cla: TColor;
  l: Integer;
begin
  l := Length(DarkColors);
  if delta > 0 then
  begin
    Inc(ColorIndexDark);
    ColorIndexDark := ColorIndexDark mod l;
  end
  else
  begin
    Dec(ColorIndexDark);
    if ColorIndexDark < 0 then
      ColorIndexDark := l-1;
  end;

  cla := DarkColors[ColorIndexDark];
  GetCurrentTextControl.TextSettings.FontColor := cla;
end;

procedure TFormMeme.CycleColorLight(delta: Integer);
var
  cla: TColor;
  l: Integer;
begin
  l := Length(LightColors);
  if delta > 0 then
  begin
    Inc(ColorIndexLight);
    ColorIndexLight := ColorIndexLight mod l;
  end
  else
  begin
    Dec(ColorIndexLight);
    if ColorIndexLight < 0 then
      ColorIndexLight := l-1;
  end;
  cla := LightColors[ColorIndexLight];
  GetCurrentTextControl.TextSettings.FontColor := cla;
end;

procedure TFormMeme.HandleAction(fa: Integer);
begin
  HA(fa);
end;

procedure TFormMeme.HA(fa: Integer);
begin
  case fa of
    faMemeToggleEdits: ToggleEdits;
    faMemeSaveBitmap: SaveBitmap;
    faMemeCopyBitmap: CopyBitmap;
    faMemePasteBitmap: PasteBitmapFromClipboard;

    faMemeSample00:
    begin
      SampleManager := SampleManager00;
      FActiveSampleManagerID := 0;
      Reset;
      Flash('using SampleManager00');
    end;

    faMemeSample01:
    begin
      SampleManager := SampleManager01;
      FActiveSampleManagerID := 1;
      Reset;
      Flash('using SampleManager01');
    end;

    faMemeSample02:
    begin
      SampleManager := SampleManager02;
      FActiveSampleManagerID := 2;
      Reset;
      Flash('using SampleManager02');
    end;

    faMemeSelectBottom:
    begin
      FSelectedText := TSelectedText.stBottom;
      Flash('Bottom Text');
    end;

    faMemeAdaptFormSize: AdaptFormSize;

    faMemeClearImage: ClearImage;
    faMemeInitChecker: InitChecker(True);

    faMemeToggleDropTarget:
    begin
      DropTargetVisible := not DropTargetVisible;
      ReportText.Visible := false;
    end;

    faMemeCycleFontP: CycleFontP;
    faMemeCycleFontM: CycleFontM;

    faMemeParamTopGlow: UpdateParam(fpTopGlow);
    faMemeParamBottomGlow: UpdateParam(fpBottomGlow);

    faMemeToggleHelp:
    begin
      DropTargetVisible := False;
      HelpText.Visible := not HelpText.Visible;
      ReportText.Visible := False;
    end;

    faMemeParamTopMargin: UpdateParam(fpTopMargin);
    faMemeParamBottomMargin: UpdateParam(fpBottomMargin);

    faMemeFontOffice:
    begin
      if HasOfficeFonts then
      begin
        if UseOfficeFonts then
          InitNormalFonts
        else
          InitOfficeFonts;
      end;
      Reset;
      if UseOfficeFonts then
        Flash('Office Fonts')
      else
        Flash('Normal Fonts');
    end;

    faMemeFontNormal:
    begin
      InitNormalFonts;
      Reset;
    end;

    faMemeCycleDarkColorP: CycleColorDark(1);
    faMemeCycleDarkColorM: CycleColorDark(-1);

    faMemeCycleLightColorP: CycleColorLight(1);
    faMemeCycleLightColorM: CycleColorLight(-1);

    faMemePickFont: PickFont;
    faMemePickColor: PickColor;

    faMemeGotoLandscape: GotoLandscape;
    faMemeGotoPortrait: GotoPortrait;
    faMemeGotoSquare: GotoSquare;

    faMemeToggleReport:
    begin
      Flash(HelpCaptionText);
      DropTargetVisible := False;
      HelpText.Visible := False;
      ReportText.Visible := not ReportText.Visible;
      if ReportText.Visible then
        UpdateReport;
    end;

    faMemeReset: Reset;

    faMemeParamTopSize: UpdateParam(fpTopSize);
    faMemeParamBottomSize: UpdateParam(fpBottomSize);

    faMemeSelectTop:
    begin
      FSelectedText := TSelectedText.stTop;
      Flash('Top Text');
    end;

    faMemeToggleTiling: ToggleTiling;
    faMemeToggleFontColor: ToggleFontColor;
    faMemeToggleTextColor: ToggleTextColor;

    faMemeSampleT:
    begin
      SampleManager.Toggle;
      Reset;
    end;

    faMemeSampleP:
    begin
      SampleManager.Next;
      Reset;
    end;

    faMemeSampleM:
    begin
      SampleManager.Previous;
      Reset;
    end;

    faMemeSwapText: SwapText;

    faMemeFormat1: UpdateFormat(1000, 750);
    faMemeFormat2: UpdateFormat(800, 600);
    faMemeFormat3: UpdateFormat(640, 480);
    faMemeFormat4: UpdateFormat(480, 480);
    faMemeFormat5: UpdateFormat(512, 512);
    faMemeFormat6: UpdateFormat(600, 600);
    faMemeFormat7: UpdateFormat(700, 700);
    faMemeFormat8: UpdateFormat(800, 800);
    faMemeFormat9: UpdateFormat(900, 900);
    faMemeFormat0:
    begin
      Top := 0;
      UpdateFormat(750, 1000)
    end;

    else
    begin
      { do nothing }
    end;

  end;
end;

function TFormMeme.GetActionFromKey(Key: Word): Integer;
begin
  result := faMemeNoop;
  case Key of
    vkEscape: result := faMemeToggleEdits;
    vkF12: result := faMemeSaveBitmap;
    vkC: result := faMemeCopyBitmap;
    vkV: result := faMemePasteBitmap;
  end
end;

function TFormMeme.GetActionFromKeyChar(KeyChar: char): Integer;
var
  fa: Integer;
begin
  case KeyChar of
    '=': fa := faMemeSample00;
    '?': fa := faMemeSample01;
    '!': fa := faMemeSample02;

    'a': fa := faMemeAdaptFormSize;
    'b': fa := faMemeSelectBottom;

    'c': fa := faMemeClearImage;
    'C': fa := faMemeInitChecker;

    'd': fa := faMemeToggleDropTarget;

    'f': fa := faMemeCycleFontP;
    'F': fa := faMemeCycleFontM;

    'g': fa := faMemeParamTopGlow;
    'G': fa := faMemeParamBottomGlow;

    'h': fa := faMemeToggleHelp;

    'l': fa := faMemeGotoLandscape;

    'm': fa := faMemeParamTopMargin;
    'M': fa := faMemeParamBottomMargin;

    'o': fa := faMemeFontOffice;
    'O': fa := faMemeFontNormal;

    'p': fa := faMemeGotoPortrait;

    'q': fa := faMemeGotoSquare;

    'r': fa := faMemeToggleReport;
    'R': fa := faMemeReset;

    's': fa := faMemeParamTopSize;
    'S': fa := faMemeParamBottomSize;

    't': fa := faMemeSelectTop;

    'u': fa := faMemeToggleTiling;

    'v': fa := faMemeToggleFontColor;
    'V': fa := faMemeToggleTextColor;

    'x': fa := faMemeSampleT;

    'y': fa := faMemeSampleP;
    'Y': fa := faMemeSampleM;

    'Z': fa := faMemeSwapText;

    '0': fa := faMemeFormat0;
    '1': fa := faMemeFormat1;
    '2': fa := faMemeFormat2;
    '3': fa := faMemeFormat3;
    '4': fa := faMemeFormat4;
    '5': fa := faMemeFormat5;
    '6': fa := faMemeFormat6;
    '7': fa := faMemeFormat7;
    '8': fa := faMemeFormat8;
    '9': fa := faMemeFormat9;

    'ä': fa := faMemePickFont;
    'Ä': fa := faMemePickColor;

    'ö': fa := faMemeCycleDarkColorP;
    'Ö': fa := faMemeCycleDarkColorM;

    'ü': fa := faMemeCycleLightColorP;
    'Ü': fa := faMemeCycleLightColorM;

    else fa := faMemeNoop;

  end;
  result := fa;
end;

end.
