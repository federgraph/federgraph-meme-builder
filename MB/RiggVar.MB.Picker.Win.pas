unit RiggVar.MB.Picker.Win;

interface

{$ifdef MSWINDOWS}
uses
  System.SysUtils,
  System.Classes,
  System.UIConsts,
  System.UITypes,
  FMX.Forms,
  RiggVar.MB.Def;

type
  TPickerWin = class(TInterfacedObject, IPicker)
  public
    function SelectAlphaColor(AColor: TAlphaColor): TAlphaColor;
    function SelectFontFamilyName(AFontName: string): string;
    procedure CollectFontFamilyNames(ML: TStrings);
    function IsShiftKeyPressed: Boolean;
 end;

{$endif}

implementation

{$ifdef MSWINDOWS}
uses
  FMX.Platform.Win,
  Winapi.Windows,
  Winapi.CommDlg;

const
  MaxCustomColors = 16;

type
  TCustomColors = array[0..MaxCustomColors - 1] of LongInt;

{ TPicker }

function TPickerWin.SelectAlphaColor(AColor: TAlphaColor): TAlphaColor;
var
  cc : TChooseColor;
  cla: TCustomColors;
begin
  // stackoverflow: questions/19843975/firemonkey-colordialog
  result := AColor;
  FillChar(cc, SizeOf(cc), 0);
  cc.lStructSize := SizeOf(cc);
  cc.hwndOwner := FmxHandleToHWND(Application.MainForm.Handle);
  cc.lpCustColors := @cla;
  cc.rgbResult := RGBtoBGR(claWhite);
  cc.Flags := CC_FULLOPEN or CC_RGBINIT;
  if (ChooseColor(cc)) then
    result := MakeColor(
      GetRValue(cc.rgbResult),
      GetGValue(cc.rgbResult),
      GetBValue(cc.rgbResult));
end;

function TPickerWin.SelectFontFamilyName(AFontName: string): string;
var
 cc : TChooseFont;
 lf: LogFont;
begin
  result := AFontName;
  FillChar(cc, SizeOf(cc), 0);
  FillChar(lf, SizeOf(LogFont), 0);
  cc.lStructSize := SizeOf(cc);
  cc.lpLogFont := @lf;
  cc.hwndOwner := FmxHandleToHWND(Application.MainForm.Handle);
  if ChooseFont(cc) then
    result := string(cc.lpLogFont.lfFaceName);
end;

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

procedure TPickerWin.CollectFontFamilyNames(ML: TStrings);
var
  DC: HDC;
  LFont: TLogFont;
begin
// stackoverflow: how-to-get-the-list-of-fonts-available-delphi-xe3-firemonkey-2
  DC := GetDC(0);
  FillChar(LFont, sizeof(LFont), 0);
  LFont.lfCharset := DEFAULT_CHARSET;
  EnumFontFamiliesEx(DC, LFont, @EnumFontsProc, Winapi.Windows.LPARAM(ML), 0);
  ReleaseDC(0, DC);
end;

function TPickerWin.IsShiftKeyPressed: Boolean;
begin
// stackoverflow: getkeystate-in-firemonkey
  result := GetKeyState(VK_SHIFT) < 0;
end;

{$endif}

end.
