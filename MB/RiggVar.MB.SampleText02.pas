unit RiggVar.MB.SampleText02;

interface

uses
  System.SysUtils,
  System.Classes,
  RiggVar.MB.Def,
  RiggVar.MB.SampleText00;

type
  TSampleTextManager02 = class(TSampleTextManagerBase)
  private
    procedure InitUrlHelpString;
    procedure InitDummyContent;
  protected
    ML: TStringList;
    function GetParagraph(sep: string = ' '): string;
    procedure InitSD(Index: Integer); override;
  public
    Title: string;
    Quote1: string;
    Quote2: string;
    QuoteSource: string;
    UrlString: string;
    UrlHelpString: string;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TSampleTextManager02 }

constructor TSampleTextManager02.Create;
begin
  inherited;
  ML := TStringList.Create;

  InitUrlHelpString;

  ML.Clear;
  QuoteSource := '~ Noam Chomsky';
  ML.Add('The smart way to keep people passive and obedient is to strictly limit the spectrum of acceptable opinion,');
  ML.Add('but allow very lively debate within that spectrum — even encourage the more critical and dissident views.');
  ML.Add('That gives people the sense that there’s free thinking going on,');
  ML.Add('while all the time the presuppositions of the system are being reinforced by the limits put on the range of the debate.');
  Quote1 := GetParagraph + sLineBreak + QuoteSource;

  ML.Clear;
  Title := 'The new Lorem Ipsum';
  UrlString := 'https://www.federgraph.de/example-content.html';
  QuoteSource := '~ Caitlin Johnstone';
  ML.Add('And for that matter, having every idea and innovation be required to make money is also killing us.');
  ML.Add('We need the ability to fund things that will not make profit.');
  ML.Add('How many times have you been in a conversation and someone’s come up with an idea that will solve a major environmental,');
  ML.Add('energy or health problem and no one’s got excited because it will never get off the ground because it will never make money?');
  ML.Add('Fully disappearing a problem never made anyone any money.');
  ML.Add('Healthy people, for example, never spend a dime at the doctors.');
  ML.Add('The way out of this is detaching human innovation from money and allowing solutions to flourish without the imposition of also having to turn a profit.');
  Quote2 := GetParagraph + sLineBreak + QuoteSource;

  InitDummyContent;

  PageCount := 4;
  FCount := PageCount + ML.Count;
end;

destructor TSampleTextManager02.Destroy;
begin
  ML.Free;
  inherited;
end;

procedure TSampleTextManager02.InitUrlHelpString;
begin
  ML.Add('To copy the url for pasting into a browser,');
  ML.Add('press Escape key to show the Edit controls,');
  ML.Add('then copy the url from the Edit control to the clipboard,');
  ML.Add('using ^c or the context menu.');
  UrlHelpString := GetParagraph;
end;

procedure TSampleTextManager02.InitDummyContent;
begin
  ML.Clear;
  QuoteSource := '~ Quote Source';
  ML.Add('Begin of quote list.');
  ML.Add('1. Abcdefg Hijklmno Pqrstuvw Xyz.');
  ML.Add('2. Abcdefg Hijklmno Pqrstuvw Xyz. Abcdefg Hijklmno Pqrstuvw Xyz.');
  ML.Add('3. Abcdefg Hijklmno Pqrstuvw Xyz. Abcdefg Hijklmno Pqrstuvw Xyz. Abcdefg Hijklmno Pqrstuvw Xyz. Abcdefg Hijklmno Pqrstuvw Xyz.');
  ML.Add('4. Abcdefg Hijklmno Pqrstuvw Xyz. Abcdefg Hijklmno Pqrstuvw Xyz. Abcdefg Hijklmno Pqrstuvw Xyz. Abcdefg Hijklmno Pqrstuvw Xyz. Abcdefg Hijklmno Pqrstuvw Xyz. Abcdefg Hijklmno Pqrstuvw Xyz.');
  ML.Add('5. Abcdefg Hijklmno Pqrstuvw Xyz. Abcdefg Hijklmno Pqrstuvw Xyz. Abcdefg Hijklmno Pqrstuvw Xyz. Abcdefg Hijklmno Pqrstuvw Xyz. Abcdefg Hijklmno Pqrstuvw Xyz. Abcdefg Hijklmno Pqrstuvw Xyz. Abcdefg Hijklmno Pqrstuvw Xyz. Abcdefg Hijklmno Pqrstuvw Xyz.');
  ML.Add('End of quote list.');
end;

function TSampleTextManager02.GetParagraph(sep: string): string;
begin
  result := StringReplace(ML.Text, sLineBreak, sep, [rfReplaceAll, rfIgnoreCase]);
end;

procedure TSampleTextManager02.InitSD(Index: Integer);
var
  MemoIndex: Integer;
begin
  SD.Sample0;

  case Index of
    0: ; //SD.Sample0;
    1: SD.Sample1;

    2:
    begin
      SD.Caption := 'Url';
      SD.Top.FontCalibri(26);
      SD.Top.Text := 'https://en.wikipedia.org/wiki/Overton_window';
      SD.Bottom.FontSitka(22);
      SD.Bottom.Text := Quote1;
    end;

    3:
    begin
      SD.Caption := 'Quote';
      SD.Top.FontVivaldi;
      SD.Top.Text := Title;
      SD.Bottom.FontSitka(22);
      SD.Bottom.Text := Quote2 + slb + UrlString;
    end;

    { Pages: 0..PageCount-1 }

    else
    begin
      { Content Lines: 0..ML.Count-1 }
       MemoIndex := Index-PageCount;
      if (MemoIndex >= 0) and (MemoIndex < ML.Count) then
      begin
        SD.Top.Text := ML[MemoIndex];
        SD.Bottom.Text := QuoteSource;
      end;
    end;

  end;

end;

end.

