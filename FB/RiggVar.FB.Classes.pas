unit RiggVar.FB.Classes;

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
  System.Math,
  System.Math.Vectors,
  System.Types;

const
  BoolStr: array[Boolean] of string = ('False', 'True');
  BoolInt: array[Boolean] of Integer = (0, 1);
  BoolFloat: array[Boolean] of single = (0.0, 1.0);

type
  TTokenParser = class
  public
    sToken: string;
    sRest: string;
    procedure NextToken;
    function NextTokenX(TokenName: string): Integer;
  end;

  TLineParser = class
  private
    SL: TStringList; // helper object, Commatext used to parse line when loading
  protected
    function ParseKeyValue(Key, Value: string): Boolean; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function ParseLine(const s: string): Boolean;
  end;

  TextWriter = class
  private
    fFileName: string;
    SL: TStringList;
    function GetStrings: TStrings;
  public
    constructor Create(fn: string);
    destructor Destroy; override;
    procedure Write(s: string);
    procedure WriteLine(s: string);
    procedure Flush;
    procedure Close;
    property Strings: TStrings read GetStrings;
  end;

  TWebContentLoader = class
  private
    SL: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetContent(fn: string): string;
  end;

  TUtils = class
  public
    class function IsFalse(const Value: string): Boolean; static;
    class function IsTrue(const Value: string): Boolean; static;
    class function IsEmptyOrTrue(const Value: string): Boolean; static;
    class function StartsWith(const s, substring: string): Boolean; static;
    class function EndsWith(const s, substring: string): Boolean; static;
    class function Cut(delim: string; s: string; var token: string): string; static;
    class function IncludeTrailingSlash(const s: string): string; static;
    class function StrToBoolDef(const Value: string; DefaultValue: Boolean): Boolean; static;
    class function StripFirstWord(var s: string): string; static;
    class function CreateRotationMatrix3D(const AnAxis: TPoint3D; Angle: Single): TMatrix3D; static;
    class function RotateDegrees(ov: TPoint3D; wi: single): TPoint3D; static;
    class function RotateRadians(ov: TPoint3D; wi: single): TPoint3D; static;
    class function Round(Value: Extended; Decimals: Integer): double; static;
    class function MakeEulerAngles(M: TMatrix3D; WantDegreeNormalize: Boolean): TPoint3D; static;
    class function IsEssentiallyZero(const Value: Single): Boolean; static;
  end;

implementation

{ Utils }

class function TUtils.StartsWith(const s, substring: string): Boolean;
begin
  result := Copy(s, 1, Length(substring)) = substring;
end;

class function TUtils.EndsWith(const s, substring: string): Boolean;
begin
  result := Pos(substring, s) = Length(s) - Length(substring) + 1;
end;

class function TUtils.IsEmptyOrTrue(const Value: string): Boolean;
var
  s: string;
begin
  result := False;
  s := UpperCase(Value);
  if (s = '') or (s = 'TRUE') or (s = 'T') or (s = '1') then
    result := True
end;

class function TUtils.IsTrue(const Value: string): Boolean;
var
  s: string;
begin
  result := False;
  s := UpperCase(Value);
  if (s = 'TRUE') or (s = 'T') or (s = '1') then
    result := True
end;

class function TUtils.IsFalse(const Value: string): Boolean;
var
  s: string;
begin
  result := False;
  s := UpperCase(Value);
  if (s = 'FALSE') or (s = 'F') or (s = '0') then
    result := True
end;

class function TUtils.StrToBoolDef(const Value: string; DefaultValue: Boolean): Boolean;
begin
  if Value = '' then
    result := DefaultValue
  else
    result := IsTrue(Value);
end;

class function TUtils.Cut(delim: string; s: string; var token: string): string;
//Trennt einen String beim Trennzeichen
// delim : Trennzeichen -> erstes Auftreten = Trennposition
// parameter s : input
// var parameter token : output, vorn abgeschnitten
// Result liefert den rest
var
  posi: integer; // Trennposition
begin
  posi := pos(delim, s);
  if posi > 0 then
  begin
    token := trim(copy(s, 1, posi - 1));
    result := trim(copy(s, posi + 1, length(s)));
  end
  else
  begin
    token := s;
    result := '';
  end;
end;

class function TUtils.IncludeTrailingSlash(const s: string): string;
begin
  if s = '' then
    result := '/'
  else if not (s[Length(s)] = '/') then
    result := s + '/'
  else
    result := s;
end;

class function TUtils.StripFirstWord(var s : string) : string;
var
  i, Size: Integer;
  S1: String;
begin
  {----------------------------------------------------
  Strip the first word from a sentence S,
  return word S1 and a shortened sentence S.
  Return an empty string S1 if there is no first word.
  -----------------------------------------------------}

  i := Pos(#32, s);
  if i = 0 then
  begin
    Result := '';
    Exit; {Kein erstes Wort, Satz bleibt gleich}
  end;

  {Erstes Wort:}
  SetLength(S1, i-1); {Speicher reservieren!}
  Move(S[1], S1[1], i-1);

  {Verkürzter Satz:}
  Size := (Length(S) - i);
  Move(S[i + 1], S[1], Size);
  SetLength(S, Size);

  Result := S1;
end;

class function TUtils.CreateRotationMatrix3D(const AnAxis: TPoint3D; Angle: Single): TMatrix3D;
var
  Axis: TPoint3D;
  Cos, Sin, OneMinusCos: Extended;
const
  X = 0;
  Y = 1;
  Z = 2;
  W = 3;
begin
  SinCos(Angle, Sin, Cos);
  OneMinusCos := 1 - Cos;
  Axis := AnAxis.Normalize;

  FillChar(Result, SizeOf(Result), 0);

  Result.M[X].V[X] := (OneMinusCos * Axis.V[0] * Axis.V[0]) + Cos;
  Result.M[X].V[Y] := (OneMinusCos * Axis.V[0] * Axis.V[1]) - (Axis.V[2] * Sin);
  Result.M[X].V[Z] := (OneMinusCos * Axis.V[2] * Axis.V[0]) + (Axis.V[1] * Sin);

  Result.M[Y].V[X] := (OneMinusCos * Axis.V[0] * Axis.V[1]) + (Axis.V[2] * Sin);
  Result.M[Y].V[Y] := (OneMinusCos * Axis.V[1] * Axis.V[1]) + Cos;
  Result.M[Y].V[Z] := (OneMinusCos * Axis.V[1] * Axis.V[2]) - (Axis.V[0] * Sin);

  Result.M[Z].V[X] := (OneMinusCos * Axis.V[2] * Axis.V[0]) - (Axis.V[1] * Sin);
  Result.M[Z].V[Y] := (OneMinusCos * Axis.V[1] * Axis.V[2]) + (Axis.V[0] * Sin);
  Result.M[Z].V[Z] := (OneMinusCos * Axis.V[2] * Axis.V[2]) + Cos;

  Result.M[W].V[W] := 1;
end;

class function TUtils.RotateRadians(ov: TPoint3D; wi: single): TPoint3D;
begin
  result := TUtils.RotateDegrees(ov, RadToDeg(wi));
end;

class function TUtils.RotateDegrees(ov: TPoint3D; wi: single): TPoint3D;
var
  a: single;
  m: TMatrix3D;
begin
  a := DegToRad(DegNormalize(Abs(wi)));
  if wi >= 0 then
    m := TUtils.CreateRotationMatrix3D(TPoint3D.Create(0,0,1), a)
  else
    m := TUtils.CreateRotationMatrix3D(TPoint3D.Create(0,0,-1), a);
  result := ov * m;
end;

class function TUtils.Round(Value: Extended; Decimals: Integer): double;
var
  p: Extended;
begin
  { Mathematical Rounding }
  p := Power(10, Decimals);
  if Value < 0 then
     result := Trunc(Value * p - 0.5) / p
  else
     result := Trunc(Value * p + 0.5) / p;

 { Bankers Rounding }
 //result := Math.Round(Value * p) / p;
end;

class function TUtils.MakeEulerAngles(M: TMatrix3D; WantDegreeNormalize: Boolean): TPoint3D;
var
  tmp: single;
  u: TPoint3D;
begin
  tmp := abs(M.m31);
  if (tmp > 0.999999) then
  begin
    u.X := RadToDeg(0); //roll
    u.Y := RadToDeg(-Pi / 2 * M.m31 / tmp); //pitch
    u.Z := RadToDeg(ArcTan2(-M.m12, -M.m31 * M.m13)); //yaw
  end
  else
  begin
    u.X := RadToDeg(ArcTan2(M.m32, M.m33)); //roll
    u.Y := RadToDeg(ArcSin(-M.m31)); //pitch
    u.Z := RadToDeg(ArcTan2(M.m21, M.m11)); //yaw
  end;

  result := -u;

  if WantDegreeNormalize then
  begin
    result.X := DegNormalize(result.X);
    result.Y := DegNormalize(result.Y);
    result.Z := DegNormalize(result.Z);
  end;
end;

class function TUtils.IsEssentiallyZero(const Value: Single): Boolean;
begin
  result := ((Value < Epsilon2) and (Value > -Epsilon2));
end;

{ TTokenParser }

procedure TTokenParser.NextToken;
begin
  sRest := TUtils.Cut('.', sRest, sToken);
end;

function TTokenParser.NextTokenX(TokenName: string): Integer;
var
  l: Integer;
begin
  NextToken;
  result := -1;
  l := Length(TokenName);
  if Copy(sToken, 1, l) = TokenName then
  begin
    sToken := Copy(sToken, l+1, Length(sToken) - l);
    result := StrToIntDef(sToken, -1);
  end;
end;

{ TextWriter }

procedure TextWriter.Close;
begin
//  if fFileName <> '' then
//    SL.SaveToFile(fFileName);
end;

constructor TextWriter.Create(fn: string);
begin
  inherited Create;
  fFileName := fn;
  SL := TStringList.Create;
end;

destructor TextWriter.Destroy;
begin
  SL.Free;
  inherited;
end;

procedure TextWriter.Flush;
begin
  //
end;

function TextWriter.GetStrings: TStrings;
begin
  result := SL;
end;

procedure TextWriter.Write(s: string);
begin
  SL[SL.Count-1] := SL[SL.Count-1] + s;
end;

procedure TextWriter.WriteLine(s: string);
begin
  SL.Add(s);
end;

{ TLineParser }

constructor TLineParser.Create;
begin
  inherited Create;
  SL := TStringList.Create;
end;

destructor TLineParser.Destroy;
begin
  SL.Free;
  inherited;
end;

function TLineParser.ParseLine(const s: string): Boolean;
var
  sK: string;
  sV: string;
  temp: string;
  i: Integer;
begin
  SL.Clear;
  i := Pos('=', s);
  if i > 0 then
    temp := Trim(Copy(s, 1, i-1)) + '=' + Trim(Copy(s, i+1, Length(s)))
  else
    temp := StringReplace(Trim(s), ' ', '_', [rfReplaceAll]);

  if Pos('=', temp) = 0 then
    temp := temp + '=';
  SL.Add(temp);
  sK := SL.Names[0];
  sV := SL.Values[sK];
  //StringReplace(sV, '_', ' ', [rfReplaceAll]);
  result := ParseKeyValue(sK, sV);
end;

function TLineParser.ParseKeyValue(Key, Value: string): Boolean;
begin
  //virtual, this implementation only used in unit test.
  result := (Key = 'Key') and (Value = 'Value');
end;

{ TWebContentLoader }

constructor TWebContentLoader.Create;
begin
  inherited Create;
  SL := TStringList.Create;
end;

destructor TWebContentLoader.Destroy;
begin
  SL.Free;
  inherited Destroy;
end;

function TWebContentLoader.GetContent(fn: string): string;
begin
  try
    SL.LoadFromFile(fn);
    result := SL.Text;
    SL.Clear;
  except
    result := '';
  end;
end;

end.
