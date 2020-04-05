unit RiggVar.FB.ActionTable;

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

{$ifdef FPC}
{$mode delphi}
{$endif}

uses
  System.SysUtils,
  System.Classes,
  System.UITypes,
  RiggVar.FB.ActionConst,
  RiggVar.FB.ActionName,
  RiggVar.FB.Action;

type
  TActionHelper = class
  private
    FAH: IFederActionHandler;
  public
    WantEnumName: Boolean;
    WantCategoryColumn: Boolean;
    WantShortCutColumn: Boolean;

    constructor Create(ah: IFederActionHandler);

    procedure WriteCSV(ML: TStrings);
    procedure WriteHTM(ML: TStrings);
    procedure ExportTable(ML: TStrings);
    function GetShortcutString(fa: TFederAction): string;
    function GetActionName(fa: TFederAction): string;
    function GetActionGroup(fa: TFederAction): Integer;
    function GetShortcutReport(ML: TStrings): string;
  end;

implementation

uses
  TypInfo,
  RiggVar.App.Main;

type
  TShortcutReportHelper = record
  public
    Key: Word;
    KeyChar: Char;
    Shift: TShiftState;
    ML: TStrings;
    Index: Integer;

    procedure EnumKeys;
    procedure EnumKey(Name: string; Value: Word);
  end;

procedure TShortcutReportHelper.EnumKeys;
var
  fa1, fa2: TFederAction;
  s1, s2: string;
begin
  Inc(Index);
  for fa1 := 0 to faMax - 1 do
  begin
    fa2 := Main.Keyboard.KeyUpAction(Key, KeyChar, Shift);
    if fa1 = fa2 then
    begin
      s1 := ML[Index];
      s2 := GetFederActionName(fa1);
      ML[Index] := s1 + ' ' + s2;
    end;
  end;
end;

procedure TShortcutReportHelper.EnumKey(Name: string; Value: Word);
var
  fa1, fa2: TFederAction;
  s1, s2: string;
begin
  ML.Add(Name);
  Inc(Index);
  for fa1 := 0 to faMax - 1 do
  begin
    fa2 := Main.Keyboard.KeyUpAction(Value, KeyChar, []);
    if fa1 = fa2 then
    begin
      s1 := ML[Index];
      s2 := GetFederActionName(fa1);
      ML[Index] := s1 + ' ' + s2;
    end;
  end;
end;

{ TActionHelper }

constructor TActionHelper.Create(ah: IFederActionHandler);
begin
  FAH := ah;
  WantEnumName := false;
  WantShortCutColumn := True;
end;

procedure TActionHelper.ExportTable(ML: TStrings);
begin
  WriteHTM(ML);
end;

function TActionHelper.GetActionName(fa: TFederAction): string;
begin
  result := GetFederActionName(fa);
end;

function TActionHelper.GetActionGroup(fa: TFederAction): Integer;
begin
  result := Main.ActionGroupList.GetGroup(fa);
end;

function TActionHelper.GetShortcutString(fa: TFederAction): string;
begin
  result := Main.Keyboard.GetShortcut(fa);
end;

procedure TActionHelper.WriteCSV(ML: TStrings);
var
  fa: TFederAction;
  an: string;
  sc: string;
  lc: string;
  ShortCutString: string;
  s: string;
  i: Integer;
begin
  i := -1;
  for fa := 1 to faMax-1 do
  begin
    Inc(i);
    ShortCutString := GetShortcutString(fa);
    an := GetActionName(fa);
    sc := FAH.GetShortCaption(fa);
    lc := FAH.GetCaption(fa);
    s := Format('%d;%s;%s;%s;%s', [i, an, lc, sc, ShortCutString]);
    ML.Add(s);
  end;
end;

procedure TActionHelper.WriteHTM(ML: TStrings);
var
  fa: TFederAction;
  an: string;
  sc: string;
  lc: string;
  ShortCutString: string;
begin
  ShortCutString := '';
  ML.Add('<html>');
  ML.Add('<head>');
  ML.Add('<title></title>');
  ML.Add('</head>');
  ML.Add('<body>');
  ML.Add('');
  ML.Add('<h2>Federgraph Action Table</h2>');
  ML.Add('');
  ML.Add('<table border=''1'' cellpadding=''1'' cellspacing=''1''>');

  ML.Add('');
  ML.Add('<tr>');
  ML.Add('<th>pos</th>');
  if WantCategoryColumn then
    ML.Add('<th>category</th>');
  ML.Add('<th>action name</th>');
  ML.Add('<th>short caption</th>');
  ML.Add('<th>long caption</th>');
  if WantShortCutColumn then
    ML.Add('<th>shortcut</th>');
  ML.Add('</tr>');
  ML.Add('');

  for fa := 1 to faMax-1 do
  begin
    ML.Add('<tr>');

    an := GetActionName(fa);
    sc := FAH.GetShortCaption(fa);
    lc := FAH.GetCaption(fa);
    if WantShortCutColumn then
      ShortCutString := GetShortcutString(fa);

    if WantCategoryColumn then
      ML.Add(Format('<td>&nbsp;</td>', [fa]));
    ML.Add(Format('<td>%d</td>', [fa]));
    ML.Add(Format('<td>%s</td>', [an]));
    ML.Add(Format('<td>%s</td>', [sc]));
    ML.Add(Format('<td>%s</td>', [lc]));
    if WantShortCutColumn then
      ML.Add(Format('<td>%s</td>', [ShortCutString]));

    ML.Add('</tr>');
    ML.Add('');
  end;
  ML.Add('</table>');
  ML.Add('</body>');
  ML.Add('</html>');
end;

function TActionHelper.GetShortcutReport(ML: TStrings): string;
var
  c: Char;
  H: TShortcutReportHelper;
{$WARN WIDECHAR_REDUCED OFF}
  cs: set of char;
{$WARN WIDECHAR_REDUCED ON}
begin
  { build a list }
  cs := [];

  cs := cs + ['A'..'Z'];
  cs := cs + ['a'..'z'];
  cs := cs + ['0'..'9'];

  Include(cs, '!');
  Include(cs, '"');
  Include(cs, '§');
  Include(cs, '$');
  Include(cs, '%');
  Include(cs, '&');

  Include(cs, '+');
  Include(cs, '#');
  Include(cs, '*');
  Include(cs, '=');
  Include(cs, '?');

  Include(cs, '/');
  Include(cs, '°');

  Include(cs, '~');
  Include(cs, 'µ');
  Include(cs, '@');
  Include(cs, '€');

  Include(cs, '²');
  Include(cs, '³');

  Include(cs, '^');
  Include(cs, '`');
  Include(cs, '´');
  Include(cs, '''');
  Include(cs, '|');

  Include(cs, 'ß');
  Include(cs, '(');
  Include(cs, ')');
  Include(cs, '[');
  Include(cs, ']');
  Include(cs, '{');
  Include(cs, '}');

  Include(cs, ';');
  Include(cs, ':');
  Include(cs, '_');

  Include(cs, ',');
  Include(cs, '.');
  Include(cs, '-');

  Include(cs, 'ä');
  Include(cs, 'Ä');
  Include(cs, 'ö');
  Include(cs, 'Ö');
  Include(cs, 'ü');
  Include(cs, 'Ü');

  Include(cs, '<');
  Include(cs, '>');

  Include(cs, ' ');

  for c in cs do
  begin
    ML.Add(Format('%.3d %s', [Ord(c), c]));
  end;

  { find and add shortcuts to lines of list }
  H.Index := -1;
  H.ML := ML;
  H.Key := 0;

  H.Shift := [];
  for c in cs do
  begin
    H.KeyChar := c;
    H.EnumKeys;
  end;

  H.EnumKey('vkC', vkC);
  H.EnumKey('vkD', vkD);
  H.EnumKey('vkL', vkL);
  H.EnumKey('vkS', vkS);
  H.EnumKey('vkO', vkO);

  H.EnumKey('vkSpace', vkSpace);
  H.EnumKey('vkDelete', vkDelete);
  H.EnumKey('vkReturn', vkReturn);

  H.EnumKey('vkF1', vkF1);
  H.EnumKey('vkF2', vkF2);
  H.EnumKey('vkF3', vkF3);
  H.EnumKey('vkF4', vkF4);
  H.EnumKey('vkF5', vkF5);

  H.EnumKey('vkLeft', vkLeft);
  H.EnumKey('vkRight', vkRight);
  H.EnumKey('vkUp', vkUp);
  H.EnumKey('vkDown', vkDown);

  H.EnumKey('vkNext', vkNext);
  H.EnumKey('vkPrior', vkPrior);
  H.EnumKey('vkHome', vkHome);
  H.EnumKey('vkEscape', vkEscape);

  H.Shift := [ssShift];

  H.EnumKey('Shift + vkLeft', vkLeft);
  H.EnumKey('Shift + vkRight', vkRight);
  H.EnumKey('Shift + vkUp', vkUp);
  H.EnumKey('Shift + vkDown', vkDown);

  H.EnumKey('Shift + vkEscape', vkEscape);
  H.EnumKey('Shift + vkSpace', vkSpace);
end;

end.

