unit FrmMain;

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
  TFormMain = class(TForm)
    TopText: TText;
    BottomText: TText;
    BottomGlow: TGlowEffect;
    TopGlow: TGlowEffect;
    TopEdit: TMemo;
    BottomEdit: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure TopEditKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure BottomEditKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure DropTargetDropped(Sender: TObject; const Data: TDragObject; const Point: TPointF);
  private
    FParam: TMemeParam;
    FWantButtonFrameReport: Boolean;
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
    procedure UpdateChecker;
    procedure ClearImage;
    procedure ToggleTiling;
    procedure SetBitmap(value: TBitmap);
    procedure InitDropTarget;
    procedure SetDropTargetVisible(const Value: Boolean);
    procedure OnDropTargetDropped(fn: string);
    procedure CycleFontP;
    procedure CycleFontM;
    procedure CycleFont(Value: Integer);
    procedure InitHelpText;
    procedure UpdateFormat(w, h: Integer);
    procedure Reset;
    procedure UpdateParam(fp: TMemeParam);
    function GetParamText: string;
    procedure InitOfficeFonts;
    procedure InitNormalFonts;
    procedure InitFontList;
    function GetSelectedText: string;
    procedure PasteBitmapFromClipboard;
    procedure AdaptFormSize;
    procedure GotoLandscape;
    procedure GotoNormal;
    procedure GotoPortrait;
    procedure GotoSquare;
    procedure Flash(s: string);
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
    procedure ToggleTextColor;
    procedure SetUseOfficeFonts(const Value: Boolean);
    procedure InitMemo(Memo: TMemo);
    procedure SwapText;
    function GetCurrentTextControl: TText;
    function GetScreenshot: TBitmap;
    procedure SaveBitmap;
    procedure ShowColorPicker;
    procedure ShowFontPicker;
    procedure PickFont;
    procedure PickColor;
    procedure CycleColorDark(delta: Integer = 1);
    procedure CycleColorLight(delta: Integer = 1);
    procedure ToggleEdits;
    function GetSampleIndex: Integer;
    property DropTargetVisible: Boolean read FDropTargetVisible write SetDropTargetVisible;
    property UseOfficeFonts: Boolean read FUseOfficeFonts write SetUseOfficeFonts;
    procedure InitPicker;
  protected
    function FindTarget(P: TPointF; const Data: TDragObject): IControl; override;
  public
    procedure HandleWheel(Delta: Integer);
    procedure HandleAction(fa: Integer);
    function GetChecked(fa: Integer): Boolean;
    procedure UpdateReport;
    procedure UpdateBackgroundColor(AColor: TAlphaColor);
    property Background: TBitmap read CheckerBitmap write SetBitmap;
    property Param: TMemeParam read FParam;
    property SelectedText: TSelectedText read FSelectedText;
    property ActiveSampleManagerID: Integer read FActiveSampleManagerID;
    property SampleIndex: Integer read GetSampleIndex;
    property IsDropTargetVisible: Boolean read FDropTargetVisible;
    property WantButtonFrameReport: Boolean read FWantButtonFrameReport;
  public
    function GetActionFromKey(Key: Word): Integer;
    function GetActionFromKeyChar(KeyChar: char): Integer;
  public
    HintText: TText;
    HelpText: TText;
    ReportText: TText;
    ComponentsCreated: Boolean;
    procedure CreateComponents;
    procedure SetupText(T: TText; fs: single = 16);
    procedure UpdateColorScheme;
    procedure LayoutComponents;
    procedure HandleShowHint(Sender: TObject);
  protected
    procedure DestroyForms;
    procedure MemoBtnClick(Sender: TObject);
    procedure ActiBtnClick(Sender: TObject);
    procedure CheckFormBounds(AForm: TForm);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  FrmAction,
  FrmMemo,
  RiggVar.App.Main,
  RiggVar.MB.Picker,
  RiggVar.MB.Picker.Win,
  RiggVar.MB.Picker.Mac,
  RiggVar.MB.SampleText00,
  RiggVar.MB.SampleText01,
  RiggVar.MB.SampleText02,
  RiggVar.FB.ActionConst;

const
  MaxEdgeDistance = 200;
  BoolStr: array[Boolean] of string = ('False', 'True');

  HelpCaptionText = 'press h for help';
  ApplicationTitleText = 'MB01';

{ TFormMeme }

procedure TFormMain.ApplicationEventsException(Sender: TObject; E: Exception);
begin
  if (Main <> nil) and (Main.Logger <> nil) then
    Main.Logger.Info(E.Message);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
{$ifdef Debug}
  ReportMemoryLeaksOnShutdown := True;
{$endif}
  FormatSettings.DecimalSeparator := '.';

  FScale := 1.0;
{$ifdef MSWINDOWS}
  { On MACOS Screen.WorkAreaHeight is not scaled,
    so it would be wrong to div by scale.

    On Windows Screen.WorkAreaHeight is scaled and should be divved. }
  FScale := Handle.Scale;
{$endif}

  Application.OnException := ApplicationEventsException;

  FormMain := self;

  { RSP-20787 when TFormPosition.ScreenCenter}
//  Self.Position := TFormPosition.ScreenCenter;

  Main := TMain.Create;
  Main.InitText;
  Main.IsUp := True;
  Raster := MainVar.Raster;

  FontFamilyList := TStringList.Create;

  ScreenshotSaver := TScreenshotSaver.Create;

  SampleManager00 := TSampleTextManagerBase.Create;
  SampleManager01 := TSampleTextManager01.Create;
  SampleManager02 := TSampleTextManager02.Create;

  SampleManager := SampleManager00;

  InitPicker;

  WantOfficeFonts := True;
  InitFontList;

  Reset;

  InitChecker(True);

  DefaultMargin := Raster + 10;

  CreateComponents;
  LayoutComponents;

  InitMemo(TopEdit);
  InitMemo(BottomEdit);

  TopText.BringToFront;
  BottomText.BringToFront;

  HintText.BringToFront;
  HelpText.BringToFront;
  ReportText.BringToFront;

  TopEdit.BringToFront;
  BottomEdit.BringToFront;

  TopEdit.Visible := false;
  BottomEdit.Visible := false;
  ReportText.Visible := false;
  HelpText.Visible := false;

  InitHelpText;

  DropTargetVisible := true;

  UpdateBackgroundColor(MainVar.ColorScheme.claBackground);

  SL := TStringList.Create;

  Caption := HelpCaptionText;

  Main.FederText.CheckState;

{$ifdef MACOS}
  { OnKeyUp does not work well on Mac, RSP-2766 }
  OnKeyUp := nil;
  { we will use OnKeyDown instead }
  OnKeyDown := FormKeyUp;
{$endif}

  Application.OnHint := HandleShowHint;
  Main.ColorScheme := MainVar.ColorScheme.DarkScheme;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  Main.Free;
  Main := nil;

  CheckerBitmap.Free;
  FontFamilyList.Free;
  SL.Free;
end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
var
  fa: Integer;
begin
  if Key = vkEscape then
    HandleAction(faMemeToggleEdits)
  else if TopEdit.Visible then
    Exit
  else
  begin
    fa := GetActionFromKey(Key);
    if fa = faNoop then
      fa := GetActionFromKeyChar(KeyChar);

    if fa <> faNoop then
      HandleAction(fa);

    UpdateReport;

    Main.FederText.CheckState;
  end
end;

procedure TFormMain.InitHelpText;
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
  ML.Add('  x    - toggle between Sample Items, 0 or 1');
  ML.Add('  y, Y - cycle through Text templates');
  ML.Add('  r    - toggle Report');
  ML.Add('  R    - Reset');
  ML.Add('  u    - toggle image tile mode (tiled or fit)');
  ML.Add('  v, V - toggle color of help text and meme text');
  ML.Add('  a    - adapt form size');
  ML.Add('  c, C - clear image command');
  ML.Add('  ^c   - copy image to clipboard command');
  ML.Add('  1..9, 0 - Format selection');
  ML.Add('  1, p, q - Landscape, Portrait, Square');

  HelpText.Text := ML.Text;
  HelpText.AutoSize := False;
  HelpText.AutoSize := True;
  HelpText.Visible := False;

  ML.Free;
end;

procedure TFormMain.UpdateReport;
begin
  if not ReportText.Visible then
    Exit;

  SL.Clear;

  if WantButtonFrameReport then
  begin
    Main.FederText.Report(SL);
  end
  else
  begin
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
  end;

  ReportText.Text := SL.Text;
  ReportText.Visible := True;
end;

procedure TFormMain.ToggleEdits;
begin
  HelpText.Visible := False;
  ReportText.Visible := False;
  DropTargetVisible := False;
  TopEdit.Visible := not TopEdit.Visible;
  BottomEdit.Visible := TopEdit.Visible;
  Flash(DefaultCaption);

  { When you have copied an image (TBitmap) to the clipboard,
    and you make the Memos visible,
    and the paste into them with ^v or via context menu,
    then there is a bug, reported as RSP-26546.
    You can use this application as a test case. }
end;

procedure TFormMain.UpdateFormat(w, h: Integer);
begin
  ClientWidth := w;
  ClientHeight := h;
  Flash(Format('%d x %d', [ClientWidth, ClientHeight]));
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
  { if I do not force AutoSize off/on
      then the text does not center in the middle after resizing ? }
  BottomText.AutoSize := False;
  BottomText.AutoSize := True;

  TopText.AutoSize := False;
  TopText.AutoSize := True;

  UpdateChecker;

  if (Main <> nil) and Main.IsUp then
  begin
    Main.UpdateTouch;
    Main.UpdateText;
  end;
  UpdateReport;
end;

function TFormMain.GetSelectedText: string;
begin
  if SelectedText = stTop then
    result := 'Top'
  else
    result := 'Bottom';
end;

function TFormMain.GetParamText: string;
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

procedure TFormMain.HandleWheel(Delta: Integer);
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

procedure TFormMain.InitChecker(WantNewChecker: Boolean);
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

procedure TFormMain.ToggleTiling;
begin
  if CheckerImage.WrapMode = TImageWrapMode.Tile then
    CheckerImage.WrapMode := TImageWrapMode.Fit
  else
    CheckerImage.WrapMode := TImageWrapMode.Tile;
end;

procedure TFormMain.ToggleTextColor;
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

procedure TFormMain.BottomEditKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  BottomText.Text := BottomEdit.Text;
end;

procedure TFormMain.ClearImage;
begin
  CheckerImage.Bitmap.Clear(claPurple);
end;

procedure TFormMain.UpdateBackgroundColor(AColor: TAlphaColor);
begin
  if CheckerImage <> nil then
    if CheckerImage.Bitmap <> nil then
      CheckerImage.Bitmap.Clear(AColor);
end;

function TFormMain.GetSampleIndex: Integer;
begin
  result := 0;
  if SampleManager <> nil then
    result := SampleManager.GetCurrentIndex;
end;

function TFormMain.GetScreenshot: TBitmap;
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

procedure TFormMain.CopyBitmap;
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

procedure TFormMain.SaveBitmap;
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
    DropTarget.Position.X := DefaultMargin;
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

procedure TFormMain.SetUseOfficeFonts(const Value: Boolean);
begin
  FUseOfficeFonts := Value;

  SampleManager00.SetUseOfficeFonts(Value);
  SampleManager01.SetUseOfficeFonts(Value);
  SampleManager02.SetUseOfficeFonts(Value);
end;

procedure TFormMain.ShowColorPicker;
begin
  Picker.ShowColorPicker;
end;

procedure TFormMain.ShowFontPicker;
begin
  Picker.ShowFontPicker;
end;

procedure TFormMain.CreateCheckerBitmap;
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

procedure TFormMain.CycleFontP;
begin
  Inc(fo);
  if fo >= FontFamilyList.Count then
    fo := 0;
  CycleFont(fo);
end;

procedure TFormMain.CycleFontM;
begin
  Dec(fo);
  if fo < 0 then
    fo := FontFamilyList.Count-1;
  CycleFont(fo);
end;

procedure TFormMain.InitOfficeFonts;
var
  ML: TStrings;
begin
  UseOfficeFonts := True;
  ML := FontFamilyList;
  ML.Clear;
{$ifdef MSWINDOWS}
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
{$endif}

{$ifdef MACOS}
  ML.Add('Herculanum');
  ML.Add('Kokonor');
  ML.Add('Luminari');
  ML.Add('Noteworthy');
  ML.Add('Optima');
  ML.Add('Snell Roundhand');
  ML.Add('SignPainter');
  ML.Add('Trattatello');
  ML.Add('Zapfino');
{$endif}
end;

procedure TFormMain.InitNormalFonts;
var
  ML: TStrings;
begin
  { What fonts should I use when Office Fonts are not available? }
  { wikipedia: List_of_typefaces_included_with_Microsoft_Windows }

  UseOfficeFonts := False;
  ML := FontFamilyList;
  ML.Clear;

  ML.Add('Arial');
  ML.Add('Comic Sans MS');
  ML.Add('Courier New');
  ML.Add('Impact');
  ML.Add('Times New Roman');

{$ifdef MSWINDOWS}
  ML.Add('Consolas');
  ML.Add('Calibri');
  ML.Add('Lucida Handwriting');
  ML.Add('Verdana');
{$endif}

{$ifdef MACOS}
  ML.Add('Avenir');
  ML.Add('American Typewriter');
  ML.Add('Arial Black');
  ML.Add('Baskerville');
  ML.Add('Bradley Hand');
  ML.Add('Brush Script MT');
  ML.Add('Copperplate');
{$endif}
end;

procedure TFormMain.CycleFont(Value: Integer);
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

procedure TFormMain.OnDropTargetDropped(fn: string);
begin
  CheckerBitmap.LoadFromFile(fn);
  CheckerImage.Bitmap := CheckerBitmap;
  CheckerImage.WrapMode := TImageWrapMode.Fit;

  if Picker.IsShiftKeyPressed then
    AdaptFormSize;
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
var
  i: TSampleTextItem;
begin
  i := SampleManager.GetSampleItem;

  if i.Top.Text = '' then
    i.Top.Text := ' '; // workaround, see RSP-26648
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

procedure TFormMain.UpdateParam(fp: TMemeParam);
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

procedure TFormMain.CopyBitmapToClipboard(ABitmap: TBitmap);
var
  Svc: IFMXClipboardService;
begin
  if not Assigned(ABitmap) then
    Exit;
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc) then
    Svc.SetClipboard(ABitmap);
end;

procedure TFormMain.PasteBitmapFromClipboard;
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
{$Hints off}
        Value.AsType<TBitmapSurface>.Free;
{$Hints on}
      end;
    end;
  end;
end;

function TFormMain.GetCurrentTextControl: TText;
begin
  if SelectedText = TSelectedText.stTop then
    result := TopText
  else
    result := BottomText;
end;

procedure TFormMain.PickColor;
var
  tt: TText;
  cla: TAlphaColor;
begin
  tt := GetCurrentTextControl;
  cla := tt.TextSettings.FontColor;
  cla := Picker.SelectAlphaColor(cla);
  tt.TextSettings.FontColor := cla;
end;

procedure TFormMain.PickFont;
var
  tt: TText;
  fn: string;
begin
  tt := GetCurrentTextControl;
  fn := tt.Font.Family;
  fn := Picker.SelectFontFamilyName(fn);
  tt.Font.Family := fn;
end;

procedure TFormMain.SetBitmap(value: TBitmap);
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

procedure TFormMain.InitFontList;
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
        log.d('missing font: ' + f);
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

procedure TFormMain.AdaptFormSize;
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

  wmax := Round(Screen.WorkAreaWidth / FScale) - dw;
  hmax := Round(Screen.WorkAreaHeight / FScale) - dh;

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

  if Top + Height >= Round(Screen.WorkAreaHeight / FScale) then
    Top := 0;

  if Left + Width >= Round(Screen.WorkAreaWidth / FScale) then
    Left := 0;

  Flash('AdpatFormSize');
end;

procedure TFormMain.GotoNormal;
begin
  if WindowState = TWindowState.wsMaximized then
    WindowState := TWindowState.wsNormal;

  Screen.UpdateDisplayInformation;

  { workaround, because of RSP-26601 }
  if WantNormal then
  begin
    Top := 100;
    Left := 100;
    ClientWidth := 800;
    ClientHeight := 600;
  end;
end;

procedure TFormMain.GotoLandscape;
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

procedure TFormMain.GotoPortrait;
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

procedure TFormMain.GotoSquare;
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

procedure TFormMain.Flash(s: string);
begin
  Caption := s;
end;

procedure TFormMain.SetupText(T: TText; fs: single);
begin
  T.Parent := Self;
  T.WordWrap := False;
  T.HorzTextAlign := TTextAlign.Leading;
  T.Font.Family := 'Consolas';
  T.Font.Size := fs;
  T.AutoSize := True;
  T.HitTest := False;
end;

procedure TFormMain.InitMemo(Memo: TMemo);
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

procedure TFormMain.SwapText;
var
  s: string;
begin
  s := TopText.Text;
  TopText.Text := BottomText.Text;
  BottomText.Text := s;
end;

procedure TFormMain.CycleColorDark(delta: Integer);
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

procedure TFormMain.CycleColorLight(delta: Integer);
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

procedure TFormMain.HandleAction(fa: Integer);
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

    faMemeShowFontPicker: ShowFontPicker;
    faMemeShowColorPicker: ShowColorPicker;

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
      UpdateReport;
    end;

    faMemeSampleM:
    begin
      SampleManager.Previous;
      Reset;
      UpdateReport;
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

    { Attention: You must handle any action you feed to Execute in Main }
    { otherwise there would be a loop, see TMain0.HandleAction }
    faActionPageP: Main.ActionHandler.Execute(faActionPageP);
    faActionPageM: Main.ActionHandler.Execute(faActionPageM);
    faCycleColorSchemeP: Main.ActionHandler.Execute(faCycleColorSchemeP);
    faCycleColorSchemeM: Main.ActionHandler.Execute(faCycleColorSchemeM);
    faMemeToggleFontColor: Main.ActionHandler.Execute(faMemeToggleFontColor);
    faToggleTouchFrame: Main.ActionHandler.Execute(faToggleTouchFrame);

    faButtonFrameReport:
    begin
      FWantButtonFrameReport := not WantButtonFrameReport;
      UpdateReport;
    end;

    faShowActi: ActiBtnClick(nil);
    faShowMemo: MemoBtnClick(nil);

    else
    begin
      { do nothing }
    end;

  end;
end;

function TFormMain.GetActionFromKey(Key: Word): Integer;
begin
  result := faNoop;
  case Key of
    vkEscape: result := faMemeToggleEdits;
    vkF12: result := faMemeSaveBitmap;
    vkC: result := faMemeCopyBitmap;
    vkV: result := faMemePasteBitmap;
  end
end;

function TFormMain.GetActionFromKeyChar(KeyChar: char): Integer;
var
  fa: Integer;
begin
  case KeyChar of
    '=': fa := faMemeSample00;
    '?': fa := faMemeSample01;
    '!': fa := faMemeSample02;

    'a': fa := faMemeAdaptFormSize;

    'b': fa := faMemeSelectBottom;
    'B': fa := faButtonFrameReport;

    'c': fa := faMemeClearImage;
    'C': fa := faMemeInitChecker;

    'd': fa := faMemeToggleDropTarget;

    'f': fa := faMemeCycleFontP;
    'F': fa := faMemeCycleFontM;

    'g': fa := faMemeParamTopGlow;
    'G': fa := faMemeParamBottomGlow;

    'h': fa := faMemeToggleHelp;

    'i': fa := faMemeCycleLightColorP;
    'I': fa := faMemeCycleLightColorM;

    'j': fa := faMemeCycleDarkColorP;
    'J': fa := faMemeCycleDarkColorM;

    'k': fa := faCycleColorSchemeP;
    'K': fa := faCycleColorSchemeM;

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

    'w': fa := faToggleTouchFrame;

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

    'ö': fa := faMemeShowColorPicker;
    'Ö': fa := faMemeShowFontPicker;

    '+': fa := faActionPageP;
    '*': fa := faActionPageM;

    else fa := faNoop;

  end;
  result := fa;
end;

procedure TFormMain.InitPicker;
begin
{$ifdef MSWINDOWS}
  Picker := TPickerWin.Create;
{$endif}

{$ifdef MACOS}
{$ifndef IOS}
  Picker := TPickerMac.Create;
{$endif}
{$endif}

{ else use Dummy }

  if Picker = nil then
    Picker := TPicker.Create;
end;

procedure TFormMain.CreateComponents;
begin
  HintText := TText.Create(Self);
  SetupText(HintText, 18);

  HelpText := TText.Create(Self);
  SetupText(HelpText);

  ReportText := TText.Create(Self);
  SetupText(ReportText);

  ComponentsCreated := True;
end;

procedure TFormMain.LayoutComponents;
begin
  HintText.Position.X := 5 * Raster + 20;
  HintText.Position.Y := 0 * Raster + 20;

  ReportText.Position.X := DefaultMargin;
  ReportText.Position.Y := 2 * Raster + 10;

  HelpText.Position.X := DefaultMargin;
  HelpText.Position.Y := ReportText.Position.Y;

  TopText.Margins.Top := DefaultMargin;
  TopText.Margins.Left := DefaultMargin;
  TopText.Margins.Right := DefaultMargin;

  BottomText.Margins.Bottom := DefaultMargin;
  BottomText.Margins.Left := DefaultMargin;
  BottomText.Margins.Right := DefaultMargin;
end;

procedure TFormMain.CheckFormBounds(AForm: TForm);
begin
  if Screen.Height <= 768 then
    AForm.Top := 0;
  if Screen.Width <= 768 then
    AForm.Left := 0;
  if AForm.Left + AForm.Width > Screen.Width then
    AForm.Width := Screen.Width - AForm.Left - 20;
  if AForm.Top + AForm.Height > Screen.Height then
    AForm.Height := Screen.Width - AForm.Top - 20;
end;

procedure TFormMain.MemoBtnClick(Sender: TObject);
begin
  if not Assigned(FormMemo) then
  begin
    FormMemo := TFormMemo.Create(nil);
    FormMemo.Parent := self; //needed for Alt-Tab
    FormMemo.Memo.Lines.Clear;
    //Main.WriteVersion1Diff(FormMemo.Memo.Lines);
    CheckFormBounds(FormMemo);
  end;
  FormMemo.Visible := True;
  FormMemo.Show; //needed on Mac
end;

procedure TFormMain.ActiBtnClick(Sender: TObject);
begin
  if not Assigned(FormAction) then
  begin
    FormAction := TFormAction.Create(nil);
    FormAction.Parent := self;
    CheckFormBounds(FormAction);
  end;
  FormAction.Visible := True;
  FormAction.Show;
end;

procedure TFormMain.DestroyForms;
begin
  if FormAction <> nil then
  begin
    FormAction.DisposeOf;
    FormAction := nil;
  end;
  if FormMemo <> nil then
  begin
    FormMemo.DisposeOf;
    FormMemo := nil;
  end;
end;

procedure TFormMain.UpdateColorScheme;
begin
  if not ComponentsCreated then
    Exit;

  HintText.TextSettings.FontColor := MainVar.ColorScheme.claLabelText;
  HelpText.TextSettings.FontColor := MainVar.ColorScheme.claSampleText;
  ReportText.TextSettings.FontColor := MainVar.ColorScheme.claEquationText;
end;

function TFormMain.GetChecked(fa: Integer): Boolean;
begin
  result := False;
end;

procedure TFormMain.HandleShowHint(Sender: TObject);
begin
  HintText.Text := Application.Hint;
end;

end.
