unit RiggVar.MB.Picker;

interface

uses
  System.UIConsts,
  System.UITypes,
  FMX.Forms,
  FMX.Graphics,
  RiggVar.MB.Def;

type
  TPicker = class(TInterfacedObject, IPicker)
  public
    function SelectAlphaColor(AColor: TAlphaColor): TAlphaColor;
    function SelectFontFamilyName(AFontName: string): string;
 end;

implementation

uses
  FMX.Platform.Win,
  Winapi.Windows,
  Winapi.CommDlg;

const
  MaxCustomColors = 16;

type
  TCustomColors = array[0..MaxCustomColors - 1] of LongInt;

{ TChooser }

function TPicker.SelectAlphaColor(AColor: TAlphaColor): TAlphaColor;
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

function TPicker.SelectFontFamilyName(AFontName: string): string;
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

end.
