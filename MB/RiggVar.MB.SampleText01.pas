unit RiggVar.MB.SampleText01;

interface

uses
  RiggVar.MB.Def,
  RiggVar.MB.SampleText00;

type
  TDefaultSampleTextManager = class(TSampleTextManagerBase)
  protected
    function GetSample(Index: Integer): TSampleTextItem; override;
  public
    constructor Create;
  end;

implementation

{ TDefaultSampleTextManager }

constructor TDefaultSampleTextManager.Create;
begin
  MaxTextID := 4;
end;

function TDefaultSampleTextManager.GetSample(Index: Integer): TSampleTextItem;
var
  i: TSampleTextItem;
begin
  i := GetSample1;

  case Index of

    1: ;

    2:
    begin
      if UseOfficeFonts then
        i.Top.FontName := 'Vladimir Script';
      i.Bottom.FontName := i.Top.FontName;
    end;

    3:
    begin
      if UseOfficeFonts then
        i.Top.FontName := 'Vivaldi'
      else
        i.Top.FontName := 'Verdana';
      i.Bottom.FontName := i.Top.FontName;
    end;

    4:
    begin
      i.Caption := 'Federgraph Meme Builder App';

      i.Top.Text := 'Press Escape to edit text';
      i.Bottom.Text := '#Remain';

      i.Top.FontSize := 32;
      i.Bottom.FontSize := 60;

      if UseOfficeFonts then
        i.Top.FontName := 'Stencil'
      else
        i.Top.FontName := 'Impact';
      i.Bottom.FontName := i.Top.FontName;
    end;

    else
    begin
      i := GetSample0;
    end;

  end;

  result := i;
end;

end.
