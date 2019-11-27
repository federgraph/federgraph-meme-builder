unit RiggVar.MB.Picker;

interface

uses
  System.SysUtils,
  System.Classes,
  System.UITypes,
  RiggVar.MB.Def;

type
  TPicker = class(TInterfacedObject, IPicker)
  public
    function SelectAlphaColor(AColor: TAlphaColor): TAlphaColor;
    function SelectFontFamilyName(AFontName: string): string;
    procedure CollectFontFamilyNames(ML: TStrings);
    function IsShiftKeyPressed: Boolean;
 end;

implementation

{ TPicker }

function TPicker.SelectAlphaColor(AColor: TAlphaColor): TAlphaColor;
begin
  result := AColor;
end;

function TPicker.SelectFontFamilyName(AFontName: string): string;
begin
  result := AFontName;
end;

procedure TPicker.CollectFontFamilyNames(ML: TStrings);
begin
end;

function TPicker.IsShiftKeyPressed: Boolean;
begin
  result := False;
end;

end.
