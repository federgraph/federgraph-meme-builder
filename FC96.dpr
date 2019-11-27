program FC96;

uses
  FMX.Forms,
  FrmMeme in 'App\FrmMeme.pas' {FormMeme},
  RiggVar.MB.Def in 'MB\RiggVar.MB.Def.pas',
  RiggVar.MB.SampleText00 in 'MB\RiggVar.MB.SampleText00.pas',
  RiggVar.MB.SampleText01 in 'MB\RiggVar.MB.SampleText01.pas',
  RiggVar.MB.SampleText02 in 'MB\RiggVar.MB.SampleText02.pas',
  RiggVar.MB.Picker in 'MB\RiggVar.MB.Picker.pas',
  RiggVar.FB.ActionConst in 'FB\RiggVar.FB.ActionConst.pas',
  RiggVar.MB.Picker.Mac in 'MB\RiggVar.MB.Picker.Mac.pas',
  RiggVar.MB.Picker.Win in 'MB\RiggVar.MB.Picker.Win.pas',
  RiggVar.FB.Action in 'FB\RiggVar.FB.Action.pas',
  RiggVar.FB.ActionLong in 'FB\RiggVar.FB.ActionLong.pas',
  RiggVar.FB.ActionMap in 'FB\RiggVar.FB.ActionMap.pas',
  RiggVar.FB.ActionShort in 'FB\RiggVar.FB.ActionShort.pas',
  RiggVar.FB.Scheme in 'FB\RiggVar.FB.Scheme.pas',
  RiggVar.FB.TextBase in 'FB\RiggVar.FB.TextBase.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'FC96';
  Application.CreateForm(TFormMeme, FormMeme);
  Application.Run;
end.
