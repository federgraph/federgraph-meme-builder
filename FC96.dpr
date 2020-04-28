﻿program FC96;

uses
  System.StartUpCopy,
  FMX.Forms,
  FrmMeme in 'App\FrmMeme.pas' {FormMeme},
  RiggVar.FB.Action in 'FB\RiggVar.FB.Action.pas',
  RiggVar.FB.ActionConst in 'FB\RiggVar.FB.ActionConst.pas',
  RiggVar.FB.ActionLong in 'FB\RiggVar.FB.ActionLong.pas',
  RiggVar.FB.ActionShort in 'FB\RiggVar.FB.ActionShort.pas',
  RiggVar.FB.SpeedColor in 'FB\RiggVar.FB.SpeedColor.pas',
  RiggVar.MB.Def in 'MB\RiggVar.MB.Def.pas',
  RiggVar.MB.Picker.Mac in 'MB\RiggVar.MB.Picker.Mac.pas',
  RiggVar.MB.Picker in 'MB\RiggVar.MB.Picker.pas',
  RiggVar.MB.Picker.Win in 'MB\RiggVar.MB.Picker.Win.pas',
  RiggVar.MB.SampleText00 in 'MB\RiggVar.MB.SampleText00.pas',
  RiggVar.MB.SampleText01 in 'MB\RiggVar.MB.SampleText01.pas',
  RiggVar.MB.SampleText02 in 'MB\RiggVar.MB.SampleText02.pas',
  RiggVar.Util.Logger in 'Util\RiggVar.Util.Logger.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'FC96';
  Application.CreateForm(TFormMeme, FormMeme);
  Application.Run;
end.
