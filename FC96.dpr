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
  RiggVar.MB.Picker.Win in 'MB\RiggVar.MB.Picker.Win.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'FC96';
  Application.CreateForm(TFormMeme, FormMeme);
  Application.Run;
end.
