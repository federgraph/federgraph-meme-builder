unit RiggVar.FB.ActionKeys;

interface

uses
  System.SysUtils,
  System.Classes,
  System.UITypes;

type
  TKeyTestHelper = record
    ML: TStrings;
    Key: Word;
    KeyChar: Char;
    Shift: TShiftState;
    Index: Integer;
    fat: Integer;
  end;

  TFederKeyboard = class
  private
    SL: TStringList;
    KTH: TKeyTestHelper;
    function GetKeys: Boolean;
    procedure GetKey(Name: string; Value: Word);
    procedure TestKey(Name: string; Value: Word);
    procedure TestKeys;
    procedure GetSC(fa: Integer; ML: TStrings);
  public
    TestName: string;
    KeyMapping: Integer;
    function KeyDownAction(
      var Key: Word;
      var KeyChar: Char;
      Shift: TShiftState): Integer; virtual;
    function KeyUpAction(
      var Key: Word;
      var KeyChar: Char;
      Shift: TShiftState): Integer; virtual;

    constructor Create;
    destructor Destroy; override;
    procedure GetShortcuts(fa: Integer; ML: TStrings);
    function GetShortcut(fa: Integer): string;
  end;

implementation

uses
  RiggVar.FB.ActionConst;

type
{$WARN WIDECHAR_REDUCED OFF}
  TShortcutSet = set of char;
{$WARN WIDECHAR_REDUCED ON}

var
  ShortcutSet: TShortcutSet;

{ TFederKeyboard }

function TFederKeyboard.KeyDownAction(var Key: Word; var KeyChar: Char;
  Shift: TShiftState): Integer;
begin
  result := faNoop;
end;

function TFederKeyboard.KeyUpAction(var Key: Word; var KeyChar: Char;
  Shift: TShiftState): Integer;
begin
  result := faNoop;
end;

constructor TFederKeyboard.Create;
begin
  TestName := 'KB';
  SL := TStringList.Create;
end;

destructor TFederKeyboard.Destroy;
begin
  SL.Free;
  inherited;
end;

function TFederKeyboard.GetShortcut(fa: Integer): string;
begin
  result := '';
  if fa <> faNoop then
  begin
    SL.Clear;
    GetSC(fa, SL);
    if SL.Count > 0 then
      result := SL[0];
  end;
end;

procedure TFederKeyboard.GetSC(fa: Integer; ML: TStrings);
var
  c: Char;
begin
  KTH.ML := ML;
  KTH.Key := 0;
  KTH.Shift := [];
  KTH.fat := fa;
  for c in ShortcutSet do
  begin
    KTH.KeyChar := c;
    if GetKeys then
      break;
  end;

  GetKey('vkC', vkC);
  GetKey('vkD', vkD);
  GetKey('vkL', vkL);
  GetKey('vkS', vkS);
  GetKey('vkO', vkO);

  GetKey('vkSpace', vkSpace);
  GetKey('vkDelete', vkDelete);
  GetKey('vkReturn', vkReturn);

  GetKey('vkF1', vkF1);
  GetKey('vkF2', vkF2);
  GetKey('vkF3', vkF3);
  GetKey('vkF4', vkF4);
  GetKey('vkF5', vkF5);

  GetKey('vkLeft', vkLeft);
  GetKey('vkRight', vkRight);
  GetKey('vkUp', vkUp);
  GetKey('vkDown', vkDown);

  GetKey('vkNext', vkNext);
  GetKey('vkPrior', vkPrior);
  GetKey('vkHome', vkHome);
  GetKey('vkEscape', vkEscape);

  KTH.Shift := [ssShift];

  GetKey('Shift + vkLeft', vkLeft);
  GetKey('Shift + vkRight', vkRight);
  GetKey('Shift + vkUp', vkUp);
  GetKey('Shift + vkDown', vkDown);

  GetKey('Shift + vkEscape', vkEscape);
  GetKey('Shift + vkSpace', vkSpace);
end;

procedure TFederKeyboard.GetShortcuts(fa: Integer; ML: TStrings);
var
  c: Char;
begin
  KTH.ML := ML;
  KTH.Key := 0;
  KTH.Shift := [];
  KTH.fat := fa;
  for c in ShortcutSet do
  begin
    KTH.KeyChar := c;
    TestKeys;
  end;

  TestKey('vkC', vkC);
  TestKey('vkD', vkD);
  TestKey('vkL', vkL);
  TestKey('vkS', vkS);
  TestKey('vkO', vkO);

  TestKey('vkSpace', vkSpace);
  TestKey('vkDelete', vkDelete);
  TestKey('vkReturn', vkReturn);

  TestKey('vkF1', vkF1);
  TestKey('vkF2', vkF2);
  TestKey('vkF3', vkF3);
  TestKey('vkF4', vkF4);
  TestKey('vkF5', vkF5);

  TestKey('vkLeft', vkLeft);
  TestKey('vkRight', vkRight);
  TestKey('vkUp', vkUp);
  TestKey('vkDown', vkDown);

  TestKey('vkNext', vkNext);
  TestKey('vkPrior', vkPrior);
  TestKey('vkHome', vkHome);
  TestKey('vkEscape', vkEscape);

  KTH.Shift := [ssShift];

  TestKey('Shift + vkLeft', vkLeft);
  TestKey('Shift + vkRight', vkRight);
  TestKey('Shift + vkUp', vkUp);
  TestKey('Shift + vkDown', vkDown);

  TestKey('Shift + vkEscape', vkEscape);
  TestKey('Shift + vkSpace', vkSpace);
end;

procedure TFederKeyboard.TestKeys;
var
  fa2: TFederAction;
  s: string;
begin
  fa2 := KeyUpAction(KTH.Key, KTH.KeyChar, KTH.Shift);
  if KTH.fat = fa2 then
  begin
    s := KTH.KeyChar;
    KTH.ML.Add(TestName + ' ' + s);
  end;
end;

procedure TFederKeyboard.TestKey(Name: string; Value: Word);
var
  fa2: TFederAction;
begin
  fa2 := KeyUpAction(Value, KTH.KeyChar, []);
  if KTH.fat = fa2 then
  begin
    KTH.ML.Add(TestName + ' ' + Name);
  end;
end;

function TFederKeyboard.GetKeys: Boolean;
var
  fa2: TFederAction;
begin
  result := False;
  fa2 := KeyUpAction(KTH.Key, KTH.KeyChar, KTH.Shift);
  if KTH.fat = fa2 then
  begin
    KTH.ML.Add(KTH.KeyChar);
    result := True;
  end;
end;

procedure TFederKeyboard.GetKey(Name: string; Value: Word);
var
  fa2: TFederAction;
begin
  fa2 := KeyUpAction(Value, KTH.KeyChar, []);
  if KTH.fat = fa2 then
  begin
    KTH.ML.Add(Name);
  end;
end;

function GetShortcutSet: TShortcutSet;
var
  cs: TShortcutSet;
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

  result := cs;
end;

initialization
  ShortcutSet := GetShortcutSet;

end.
