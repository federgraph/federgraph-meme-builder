unit RiggVar.MB.SampleText00;

interface

uses
  RiggVar.MB.Def;

type
  TSampleText = class
  public
    Text: string;
    FontName: string;
    FontSize: single;
    class var UseOfficeFonts: Boolean;

    constructor Create;

    procedure FontCalibri(AFontSize: single = 24);
    procedure FontConsolas(AFontSize: single = 24);
    procedure FontCourierNew(AFontSize: single = 24);
    procedure FontHandwriting(AFontSize: single = 24);
    procedure FontImpact(AFontSize: single = 32);
    procedure FontSitka(AFontSize: single = 24);
    procedure FontTimesRoman(AFontSize: single = 24);
    procedure FontVivaldi(AFontSize: single = 48);
  end;

  TSampleData = class
  public
    Caption: string;
    Top: TSampleText;
    Bottom: TSampleText;
    constructor Create;
    destructor Destroy; override;
    procedure Sample0;
    procedure Sample1;
  end;

  TSampleTextManagerBase = class(TInterfacedObject, ISampleTextManager)
  private
    FUseOfficeFonts: Boolean;
    function GetCount: Integer;
    function GetCurrentIndex: Integer;
    procedure SetUseOfficeFonts(const Value: Boolean);
    function GetSample(Index: Integer): TSampleTextItem;
  protected
    FCurrentIndex: Integer;
    FCount: Integer;
    SD: TSampleData;
    PageCount: Integer;
    procedure InitSD(Index: Integer); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function GetSampleItem: TSampleTextItem;

    procedure Next;
    procedure Previous;
    procedure Toggle;

    property Count: Integer read GetCount;
    property CurrentIndex: Integer read GetCurrentIndex;
    property UseOfficeFonts: Boolean read FUseOfficeFonts write SetUseOfficeFonts;
  end;

implementation

{ TSampleData }

constructor TSampleData.Create;
begin
  Top := TSampleText.Create;
  Bottom := TSampleText.Create;
end;

destructor TSampleData.Destroy;
begin
  Top.Free;
  Bottom.Free;
  inherited;
end;

procedure TSampleData.Sample0;
begin
  Caption := 'Federgraph Meme Builder App';

  Top.Text := 'federgraph.de/federgraph-meme-builder.html';
  Top.FontCourierNew;

  Bottom.Text := 'press h to toggle help text';
  Bottom.FontCourierNew;
end;

procedure TSampleData.Sample1;
begin
  Caption := 'FC96';

  Top.Text := 'Made with Delphi';
  Top.FontImpact(84);

  Bottom.Text := 'FMX Meme Builder';
  Bottom.FontImpact(84);
end;

{ TSampleText }

constructor TSampleText.Create;
begin
  Text := ' ';
  FontName := 'Consolas';
  FontSize := 16;
end;

procedure TSampleText.FontCalibri(AFontSize: single);
begin
  if UseOfficeFonts then
  begin
    FontName := 'Calibri';
    FontSize := AFontSize;
  end
  else
  begin
    FontName := 'Arial';
    FontSize := AFontSize;
  end;
end;

procedure TSampleText.FontConsolas(AFontSize: single);
begin
  FontName := 'Consolas';
  FontSize := AFontSize;
end;

procedure TSampleText.FontCourierNew(AFontSize: single);
begin
  FontName := 'Courier New';
  FontSize := AFontSize;
end;

procedure TSampleText.FontHandwriting(AFontSize: single);
begin
  if UseOfficeFonts then
  begin
    FontName := 'Lucida Handwriting';
    FontSize := AFontSize;
  end
  else
  begin
    FontName := 'Arial';
    FontSize := AFontSize;
  end;
end;

procedure TSampleText.FontImpact(AFontSize: single);
begin
  FontName := 'Impact';
  FontSize := AFontSize;
end;

procedure TSampleText.FontSitka(AFontSize: single);
begin
  if UseOfficeFonts then
  begin
    FontName := 'Sitka Text';
    FontSize := AFontSize;
  end
  else
  begin
    FontName := 'Arial';
    FontSize := AFontSize;
  end;
end;

procedure TSampleText.FontTimesRoman(AFontSize: single);
begin
  FontName := 'Times New Roman';
  FontSize := AFontSize;
end;

procedure TSampleText.FontVivaldi(AFontSize: single);
begin
  if UseOfficeFonts then
  begin
    FontName := 'Vivaldi';
    FontSize := AFontSize;
  end
  else
  begin
    FontName := 'Impact';
    FontSize := AFontSize;
  end;
end;

{ TDefaultSampleTextManager }

constructor TSampleTextManagerBase.Create;
begin
  inherited;
  SD := TSampleData.Create;

  PageCount := 0;
  FCount := 2; // 0, 1, 2
end;

destructor TSampleTextManagerBase.Destroy;
begin
  SD.Free;
  inherited;
end;

function TSampleTextManagerBase.GetCount: Integer;
begin
  result := FCount;
end;

function TSampleTextManagerBase.GetCurrentIndex: Integer;
begin
  result := FCurrentIndex;
end;

function TSampleTextManagerBase.GetSampleItem: TSampleTExtItem;
begin
  result := GetSample(FCurrentIndex);
end;

procedure TSampleTextManagerBase.Toggle;
begin
  Inc(FCurrentIndex);
  FCurrentIndex := FCurrentIndex mod 2;
end;

procedure TSampleTextManagerBase.Next;
begin
  Inc(FCurrentIndex);
  if FCurrentIndex >= FCount then
    FCurrentIndex := 0;
end;

procedure TSampleTextManagerBase.Previous;
begin
  Dec(FCurrentIndex);
  if FCurrentIndex < 0 then
    FCurrentIndex := FCount-1;
end;

procedure TSampleTextManagerBase.SetUseOfficeFonts(const Value: Boolean);
begin
  FUseOfficeFonts := Value;
  SD.Top.UseOfficeFonts := Value; // static
end;

function TSampleTextManagerBase.GetSample(Index: Integer): TSampleTextItem;
begin
  InitSD(Index);

  result.Caption := SD.Caption;

  result.Top.Text := SD.Top.Text;
  result.Top.FontName := SD.Top.FontName;
  result.Top.FontSize := SD.Top.FontSize;

  result.Bottom.Text := SD.Bottom.Text;
  result.Bottom.FontName := SD.Bottom.FontName;
  result.Bottom.FontSize := SD.Bottom.FontSize;
end;

procedure TSampleTextManagerBase.InitSD(Index: Integer);
begin
  SD.Sample0;

  case Index of
    1: SD.Sample1;

    2:
    begin
      SD.Top.Text := 'Hello from Vivaldi(64) template.';
      SD.Top.FontVivaldi(64);

      SD.Bottom.Text := 'For current source of this App visit GitHub.';
      SD.Bottom.FontConsolas(16);
    end;

    else
    begin
      // already done, already initialized SD to default, see above
    end;
  end;
end;

end.

