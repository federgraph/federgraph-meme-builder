unit RiggVar.FB.ActionTest;

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
  SysUtils,
  Classes;

type
  TActionTest = class
  protected
    function TestNames: Boolean;
    function TestShortCaptions: Boolean;
    function TestLongCaptions: Boolean;
    function TestUniqueShortCaptions: Boolean;
    function TestGroupUsage: Boolean;
  public
    WantNew: Boolean;
    SL: TStringList;
    constructor Create;
    destructor Destroy; override;

    procedure TestAll;

    procedure WriteActionConst(ML: TStrings);
    procedure WriteActionNames(ML: TStrings);

    procedure WriteNewActionConst(ML: TStrings);
  end;

implementation

uses
  RiggVar.FB.ActionName,
  RiggVar.FB.ActionShort,
  RiggVar.FB.ActionLong,
  RiggVar.FB.ActionGroup,
  RiggVar.FB.ActionGroups,
  RiggVar.FB.ActionConst;

type
  TActionStringList = class(TStringList)
  public
    function AddObject(const S: string; AObject: TObject): Integer; override;
  end;

{ TActionTest }

constructor TActionTest.Create;
begin
  SL := TStringList.Create;
  WantNew := False;
end;

destructor TActionTest.Destroy;
begin
  SL.Free;
  inherited;
end;

function TActionTest.TestNames: Boolean;
var
  fa: Integer;
  s, t: string;
begin
  result := True; // assume OK
  SL.Add('Test for missing Names:');
  for fa := 0 to faMax - 1 do
  begin
    if fa = faNoop then
      Continue;
    s := GetFederActionName(fa);
    if s = '??' then
    begin
      result := False;
      t := GetFederActionLong(fa);
      SL.Add(Format('%d = %s', [fa, t]));
    end;
  end;
  if result then
    SL.Add('ok');
  SL.Add('');
end;

function TActionTest.TestShortCaptions: Boolean;
var
  fa: Integer;
  s, t: string;
begin
  result := True; // assume OK
  SL.Add('Test for missing Short Captions:');
  for fa := 0 to faMax - 1 do
  begin
    if fa = faNoop then
      Continue;
    s := GetFederActionShort(fa);
    if s = '??' then
    begin
      result := false;
      t := GetFederActionName(fa);
      SL.Add(t);
    end;
  end;
  if result then
    SL.Add('ok');
  SL.Add('');
end;

function TActionTest.TestLongCaptions: Boolean;
var
  fa: Integer;
  s, t: string;
begin
  result := True; // assume OK
  SL.Add('Test for missing Long Captions:');
  for fa := 0 to faMax - 1 do
  begin
    if fa = faNoop then
      Continue;
    s := GetFederActionLong(fa);
    if s = '??' then
    begin
      result := False;
      t := GetFederActionName(fa);
      SL.Add(Format('%d = %s', [fa, t]));
    end;
  end;
  if result then
    SL.Add('ok');
  SL.Add('');
end;

function TActionTest.TestUniqueShortCaptions: Boolean;
var
  fa: Integer;
  s, t: string;
  ML: TStringList;
begin
  result := True; // assume OK
  SL.Add('Test for unique Short Captions:');
  ML := TActionStringList.Create;
  ML.CaseSensitive := True;
  ML.Duplicates := TDuplicates.dupError;
  ML.Sorted := True;
  for fa := 0 to faMax - 1 do
  begin
    s := GetFederActionShort(fa);
    try
      if ML.Add(s) = -1 then
      begin
        result := false;
        t := GetFederActionName(fa);
        SL.Add(Format('%d = %s = %s', [fa, s, t]));
      end;
    except
      result := false;
      t := GetFederActionName(fa);
      SL.Add(Format('%d = %s = %s', [fa, s, t]));
    end;
  end;
  if result then
    SL.Add('ok');
  SL.Add('');
  ML.Free;
end;

function TActionTest.TestGroupUsage: Boolean;
var
  fa: Integer;
  s, n: string;
  i: Integer;
  j: Integer;
  l: Integer;
  al: TActionGroupList;
  cr: TActionGroup;
  TL: TStringList; // TestList
  sOne: string;
begin
  SL.Add('Test for Actions not in a Group:');

  al := TActionGroupList.Create;
  TL := TStringList.Create;

  for i := 0 to faMax-1 do
    TL.Add(Format('%d=0', [i]));

  sOne := '1';
  for i := 0 to al.Count-1 do
  begin
    cr := al.Items[i];
    l := Length(cr);
    for j := 0 to l-1 do
    begin
      TL.Values[IntToStr(cr[j])] := sOne;
    end;
  end;

  for i := TL.Count-1 downto 0 do
    if (TL.Values[IntToStr(i)] = '1') then
      TL.Delete(i);

  result := TL.Count = 0;
  for i := 0 to TL.Count - 1 do
  begin
    fa := StrToIntDef(TL.Names[i], -1);
    s := GetFederActionShort(fa);
    n := GetFederActionName(fa);
    SL.Add(Format('%d = %s = %s', [fa, s, n]))
  end;

  al.Free;
  TL.Free;

  if result then
    SL.Add('ok');
  SL.Add('');
end;

procedure TActionTest.TestAll;
begin
  SL.Clear;
  TestNames;
  TestLongCaptions;
  TestShortCaptions;
  TestUniqueShortCaptions;
  TestGroupUsage;
end;

procedure TActionTest.WriteActionConst(ML: TStrings);
var
  fa: Integer;
  gn, an: string;
  i, j, k: Integer;
  l: Integer;
  al: TActionGroupList;
  cr: TActionGroup;
begin
  k := -1;
  ML.Add('// --- generated code snippet ---');
  ML.Add('// Note that some of the defined actions');
  ML.Add('//   may not be implemented in this version of the app.');
  ML.Add('');
  ML.Add('const');
  al := TActionGroupList.Create;
  for i := 0 to al.Count-1 do
  begin
    cr := al.Items[i];
    l := Length(cr);
    gn := al.GetGroupName(i);
    ML.Add('');
    ML.Add('{ ' + gn + ' }');
    for j := 0 to l-1 do
    begin
      fa := cr[j];
      Inc(k);
      an := GetFederActionName(fa);
      if WantNew then
        ML.Add(Format('%s = %d;', [an, k])) //new, count up from 0
      else
        ML.Add(Format('%s = %d;', [an, fa])); //use current
    end;
  end;
  Inc(k);
  ML.Add('');
  ML.Add(Format('%s = %d;', ['faMax', k]));
  al.Free;
end;

procedure TActionTest.WriteActionNames(ML: TStrings);
var
  fa: Integer;
  gn, an: string;
  i, j: Integer;
  l: Integer;
  al: TActionGroupList;
  cr: TActionGroup;
begin
  ML.Add('// --- generated code snippet ---');
  al := TActionGroupList.Create;
  for i := 0 to al.Count-1 do
  begin
    cr := al.Items[i];
    l := Length(cr);
    gn := al.GetGroupName(i);
    ML.Add('');
    ML.Add('{ ' + gn + ' }');
    for j := 0 to l-1 do
    begin
      fa := cr[j];
      an := GetFederActionName(fa);
      ML.Add(Format('%s: result := ''%s'';', [an, an]));
    end;
  end;
  al.Free;
end;

procedure TActionTest.WriteNewActionConst(ML: TStrings);
begin
  WantNew := True;
  WriteActionConst(ML);
  WantNew := False;
end;

{ TActionStringList }

function TActionStringList.AddObject(const S: string; AObject: TObject): Integer;
begin
  if not Sorted then
    Result := Count
  else
    if Find(S, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError:
        begin
          result := -1;
          Exit; // Error(@SDuplicateString, 0);
        end;
      end;
  InsertItem(Result, S, AObject);
end;

end.
