unit RiggVar.MB.SampleText01;

interface

uses
  System.SysUtils,
  System.Classes,
  RiggVar.MB.Def,
  RiggVar.MB.SampleText00;

type
  TSampleTextManager01= class(TSampleTextManagerBase)
  private
    ML: TStringList;
    function GetParagraph(sep: string = ' '): string;
  protected
    procedure InitSD(Index: Integer); override;
  public
    PageCount: Integer;
    FC01: string;
    FC02: string;
    RG01: string;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TSampleTextManager01 }

constructor TSampleTextManager01.Create;
begin
  inherited;

  ML := TStringList.Create; // will end up holding the 'content' lines

  ML.Clear;
  ML.Add('Federgraph is a special 3D function plotter app.');
  ML.Add('It will evaluate z = f (x, y) as part of the vertex shader invocation.');
  ML.Add('The one and only mesh in the 3D scene has lots of vertices.');
  ML.Add('It will help you expose the details.');
  ML.Add('Antialiasing will make it look pretty,');
  ML.Add('execution on the GPU will make it fast,');
  ML.Add('and encoding the problem in a standard language will make your work usable elsewhere.');
  FC01 := GetParagraph;

  ML.Clear;
  ML.Add('Yes, you can write shader code for your problem; but an interesting default model is included!');
  ML.Add('I like the way the current parameter value can be changed.');
  ML.Add('You will reuse these parameters when you choose to explore your own models.');
  ML.Add('Do live experiments with the formula.');
  FC02 := GetParagraph;

  ML.Clear;
  ML.Add('Trimm-Simulation für Segelboote, die ähnlich wie ein 420 geriggt sind.');
  ML.Add('Sie selektieren einen Parameter, zum Beispiel Vorstag, und können den aktuellen Wert des Parameters verändern.');
  ML.Add('Die Grafik zeigt sofort die veränderte Stellung des "Getriebes".');
  ML.Add('Mit anderen Worten, Sie ziehen am Vorstag und der Mast kommt nach vorne und biegt.');
  ML.Add('Es können absolute Werte und Differenzwerte abgelesen werden.');
  RG01 := GetParagraph;

  ML.Clear;
  ML.Add('Fleetrace project — build from source!');
  ML.Add('Federgraph App' + dlb + 'Productivity: Parameter value change will produce result in real time.');
  ML.Add('Federgraph App' + dlb + 'Education: Learn how to do experiments, how to use the graphics chip.');
  ML.Add('Federgraph App' + dlb + 'Entertainment: Guaranteed by choice of the built-in model.');
  ML.Add('Großmutter, why do we neeed “Fake — News”?' + dlb + 'So that you can take the quote chars from it and the dash.');

  PageCount := 5; // Index 0..4
  FCount := PageCount + ML.Count;
end;

destructor TSampleTextManager01.Destroy;
begin
  ML.Free;
  inherited;
end;

function TSampleTextManager01.GetParagraph(sep: string): string;
begin
  result := StringReplace(ML.Text, sLineBreak, sep, [rfReplaceAll, rfIgnoreCase]);
end;

procedure TSampleTextManager01.InitSD(Index: Integer);
var
  MemoIndex: Integer;
begin
  SD.Sample0;

  case Index of
    0: ; //SD.Sample0;
    1: SD.Sample1;

    2:
    begin
      SD.Caption := 'About Federgraph App';
      SD.Top.FontCalibri(26);
      SD.Top.Text := FC01;
      SD.Bottom.FontSitka(22);
      SD.Bottom.Text := FC02;
    end;

    3:
    begin
      SD.Top.FontVivaldi(64);
      SD.Top.Text := 'Trimm 420 App';
      SD.Bottom.FontSitka(22);
      SD.Bottom.Text := RG01;
    end;

    4:
    begin
      SD.Caption := 'Quotes';
      SD.Top.FontCalibri(26);
      SD.Top.Text := 'www.federgraph.de' + slb + 'github.com/federgraph';
      SD.Bottom.FontSitka(22);
      SD.Bottom.Text := '( Press Escape  —  then copy Url from Memo. )';
    end;

    else
    begin
      MemoIndex := Index-PageCount;
      if (MemoIndex >= 0) and (MemoIndex < ML.Count) then
      begin
        SD.Top.Text := ML[MemoIndex];
        SD.Bottom.FontHandwriting(24);
        SD.Bottom.Text := 'Trimm 420 App. Federgraph App. Fleetrace Project.';
      end;
    end;

  end;

end;

end.

