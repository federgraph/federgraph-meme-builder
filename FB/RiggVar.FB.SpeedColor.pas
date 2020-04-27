unit RiggVar.FB.SpeedColor;

interface

uses
  System.UITypes,
  System.UIConsts;

type
  TSpeedColorValue = (
    clvScheme,

    clvBack,
    clvHot,
    clvLog,
    clvReport,
    clvOption,
    clvProp,
    clvData,
    clvWheel,
    clvTrimm,

    clvHintText,
    clvTrimmText,
    clvReportText,
    clvHelpText,
    clvParamList,
    clvReportList,

    clvGraph,
    clvSegment,
    clvBogen,
    clvImage,
    clvMemory,
    clvRigg,
    clvView,
    clvZoom
  );

  TSpeedColorScheme = record
    claScheme: TAlphaColor;

    claBack: TAlphaColor;
    claHot: TAlphaColor;
    claLog: TAlphaColor;
    claReport: TAlphaColor;
    claOption: TAlphaColor;
    claProp: TAlphaColor;
    claData: TAlphaColor;
    claWheel: TAlphaColor;
    claTrimm: TAlphaColor;

    claGraph: TAlphaColor;
    claSegment: TAlphaColor;
    claBogen: TAlphaColor;
    claImage: TAlphaColor;
    claMemory: TAlphaColor;
    claRigg: TAlphaColor;
    claView: TAlphaColor;
    claZoom: TAlphaColor;

    claHintText: TAlphaColor;
    claTrimmText: TAlphaColor;
    claReportText: TAlphaColor;
    claHelpText: TAlphaColor;
    claParamList: TAlphaColor;
    claReportList: TAlphaColor;

    claList1: TAlphaColor;
    claList2: TAlphaColor;
    claList3: TAlphaColor;

    claMemo: TAlphaColor;

    IsDark: Boolean;
    procedure Init(WantDark: Boolean);
    procedure InitDark;
    procedure InitLight;
    function GetColor(Value: TSpeedColorValue): TAlphaColor;
  end;

implementation

{ TSpeedColorScheme }

function TSpeedColorScheme.GetColor(Value: TSpeedColorValue): TAlphaColor;
begin
  case Value of
    clvBack: result := claBack;
    clvHot: result := claHot;

    clvScheme: result := claScheme;

    clvLog: result := claLog;
    clvReport: result := claReport;
    clvOption: result := claOption;
    clvProp: result := claProp;
    clvData: result := claData;
    clvWheel: result := claWheel;
    clvTrimm: result := claTrimm;

    clvGraph: result := claGraph;
    clvSegment: result := claSegment;
    clvBogen: result := claBogen;
    clvImage: result := claImage;
    clvMemory: result := claMemory;
    clvRigg: result := claRigg;
    clvView: result := claView;
    clvZoom: result := claZoom;

    clvHintText: result := claHintText;
    clvTrimmText: result := claTrimmText;
    clvReportText: result := claReportText;
    clvHelpText: result := claHelpText;
    clvParamList: result := claParamList;
    clvReportList: result := claReportList;

    else
      result := claRed;
  end;
end;

procedure TSpeedColorScheme.Init(WantDark: Boolean);
begin
  if WantDark then
    InitDark
  else
    InitLight;
end;

procedure TSpeedColorScheme.InitDark;
begin
  IsDark := True;

  claBack := claSlateGray;
  claHot := claBeige;

  claScheme := claOrange;

  claLog := claOrange;
  claReport := claOrange;
  claOption := claOrangeRed;
  claProp := claGoldenrod;
  claData := claLime;
  claWheel := claAqua;
  claTrimm := claYellow;

  claGraph := claCoral;
  claSegment := claCrimson;
  claBogen := claDodgerblue;
  claImage := claGoldenrod;
  claMemory := claBeige;
  claRigg := claAquamarine;
  claView := claBeige;
  claZoom := claTeal;

  claHintText := claYellow;
  claTrimmText := claPlum;
  claReportText := claAntiqueWhite;
  claHelpText := claWhite;

  claParamList := claAqua;
  claReportList := claAquamarine;

  claList1 := claGold;
  claList2 := claCoral;
  claList3 := claAntiquewhite;

  claMemo := claDodgerblue;
end;

procedure TSpeedColorScheme.InitLight;
begin
  IsDark := False;

  claScheme := claOrange;

  claBack := StringToAlphaColor('#FFF0F0F0');
  claHot := claBlack;
  claLog := claOrange;
  claReport := claBurlywood;
  claOption := claOrangeRed;
  claProp := claGoldenrod;
  claData := claGreen;
  claWheel := claCoral;
  claTrimm := claDodgerblue;

  claGraph := claCoral;
  claSegment := claCrimson;
  claBogen := claDodgerblue;
  claImage := claGoldenrod;
  claMemory := claSlategray;
  claRigg := claSlateblue;
  claView := claBurlywood;
  claZoom := claPurple;

  claHintText := claDarkorange;
  claTrimmText := claDodgerblue;
  claReportText := claBlack;
  claHelpText := claBlue;

  claParamList := claOrange;
  claReportList := claSlateblue;

  claList1 := claSlateblue;
  claList2 := claSlategray;
  claList3 := claOrangered;

  claMemo := claDodgerblue;
end;

end.
