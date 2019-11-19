unit RiggVar.MB.SampleText00;

interface

uses
  RiggVar.MB.Def;

type
  TSampleTextManagerBase = class(TInterfacedObject, ISampleTextManager)
  private
    FUseOfficeFonts: Boolean;
    function GetCount: Integer;
    function GetCurrentIndex: Integer;
    procedure SetUseOfficeFonts(const Value: Boolean);
  protected
    TextID: Integer;
    MaxTextID: Integer;
    function GetSample0: TSampleTextItem;
    function GetSample1: TSampleTextItem;
    function GetSample(Index: Integer): TSampleTextItem; virtual;
  public
    constructor Create;

    function GetSampleItem: TSampleTextItem;

    procedure Next;
    procedure Previous;
    procedure Toggle;

    property Count: Integer read GetCount;
    property CurrentIndex: Integer read GetCurrentIndex;
    property UseOfficeFonts: Boolean read FUseOfficeFonts write SetUseOfficeFonts;
  end;

implementation

{ TDefaultSampleTextManager }

constructor TSampleTextManagerBase.Create;
begin
  MaxTextID := 1;
end;

function TSampleTextManagerBase.GetCount: Integer;
begin
  result := MaxTextID;
end;

function TSampleTextManagerBase.GetCurrentIndex: Integer;
begin
  result := TextID;
end;

function TSampleTextManagerBase.GetSampleItem: TSampleTExtItem;
begin
  result := GetSample(TextID);
end;

procedure TSampleTextManagerBase.Toggle;
begin
  Inc(TextID);
  TextID := TextID mod 2;
end;

procedure TSampleTextManagerBase.Next;
begin
  Inc(TextID);
  if TextID > MaxTextID then
    TextID := 0;
end;

procedure TSampleTextManagerBase.Previous;
begin
  Dec(TextID);
  if TextID < 0 then
    TextID := MaxTextID;
end;

procedure TSampleTextManagerBase.SetUseOfficeFonts(const Value: Boolean);
begin
  FUseOfficeFonts := Value;
end;

function TSampleTextManagerBase.GetSample0: TSampleTextItem;
begin
  result.Caption := 'Federgraph Meme Builder App';

  result.Top.Text := 'federgraph.de/federgraph-meme-builder.html';
  result.Bottom.Text := 'press h to toggle help text';

  result.Top.FontName := 'Courier New';
  result.Bottom.FontName := 'Courier New';

  result.Top.FontSize := 24;
  result.Bottom.FontSize := 24;
end;

function TSampleTextManagerBase.GetSample1: TSampleTextItem;
begin
  result.Caption := 'FC96A';

  result.Top.Text := 'Made with Delphi';
  result.Top.FontName := 'Impact';
  result.Top.FontSize := 84;

  result.Bottom.Text := 'FMX Meme Builder';
  result.Bottom.FontName := 'Impact';
  result.Bottom.FontSize := 84;
end;

function TSampleTextManagerBase.GetSample(Index: Integer): TSampleTextItem;
begin
  if Index = 1 then
    result := GetSample1
  else
    result := GetSample0;
end;

end.

