unit RiggVar.MB.Def;

interface

type
  TSampleTextProps = record
    Text: string;
    FontName: string;
    FontSize: single;
  end;

  TSampleTextItem = record
    Caption: string;
    Top: TSampleTextProps;
    Bottom: TSampleTextProps;
  end;

  { will change without notice, I am playing ... }
  ISampleTextManager = interface
  ['{B8BEB01B-AC6F-4DB2-9F5F-E0B747705F62}']
    function GetCount: Integer;
    function GetCurrentIndex: Integer;
    function GetSampleItem: TSampleTextItem;
    procedure SetUseOfficeFonts(const Value: Boolean);

    procedure Next;
    procedure Previous;
    procedure Toggle;
  end;

implementation

end.
