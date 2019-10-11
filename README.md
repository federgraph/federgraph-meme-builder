# Federgraph Meme Builder App

*Federgraph Meme Builder App, initial look.*<br>
![Meme Builder](images/Meme-Builder-03.png)

Website [link](https://federgraph.de/federgraph-meme-builder-source.html), has more pictures.

Could be used as a companion app to the Federgraph application, just in case you wanted to build a Memo with the Federgraph "Emoji" picture.

## Design

This is a small one Form only Delphi FMX application.

( There is no configuration, would be overkill. )

- There is a drop target where you can drop the image, initially shown.
- You can hide the drop target.
- A checker bitmap is shown when no actual image is loaded.
- There is a top text and a bottom text component.
- Both text components have a glow effect.
- Default text is used, initially.
- There are some actions and options.
- You can edit, arrange and style the text.

## Actions

There are 6 actions defined, by means of action constants.

```pascal
const
  faTopMargin = 1;
  faBottomMargin = 2;
  faTopSize = 3;
  faBottomSize = 4;
  faTopGlow = 5;
  faBottomGlow = 6;
```

Use the keyboard to trigger the action.
These actions will select the current parameter.

You are supposed to select a parameter and then use the scroll wheel to change the value of that parameter.

## Options

- The Font Name is optional, choose the font from a limited set of hardcoded font names.
- The Form Size is optional, you can switch between some standard sizes.
- The Text Content is optional, you can play with some hardcoded combinations of top and bottom text.

## Keyboard usage

This is a desktop application. You need to use the keyboard.
If it was a tablet app, I would have included touch buttons, it would have been more complicated.

```pascal
procedure TFormMain.FormKeyUp(
  Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkEscape then
  begin
    TopEdit.Visible := not TopEdit.Visible;
    BottomEdit.Visible := TopEdit.Visible;
    Caption := DefaultCaption;
  end;

  if Key = vkC then
  begin
    CopyBitmap;
    Caption := 'Bitmap copied.';
  end

  else if TopEdit.Visible then
    //do nothing when editing

  else if KeyChar = 'd' then
    DropTargetVisible := not DropTargetVisible

  else if KeyChar = 'c' then
    ClearImage

  else if KeyChar = 'f' then
    CycleFontP

  else if KeyChar = 'F' then
    CycleFontM

  else if KeyChar = 'g' then
    UpdateParam(faTopGlow)

  else if KeyChar = 'h' then
    UpdateParam(faBottomGlow)

  else if KeyChar = 't' then
    UpdateParam(faTopSize)

  else if KeyChar = 'b' then
    UpdateParam(faBottomSize)

  else if KeyChar = 'n' then
    UpdateParam(faTopMargin)

  else if KeyChar = 'm' then
    UpdateParam(faBottomMargin)

  else if KeyChar = 'r' then
    Caption := DefaultCaption

  else if KeyChar = 'R' then
    Reset

  else if KeyChar = 'x' then
  begin
    Inc(TestID);
    TestID := TestID mod 2;
    Reset;
  end

  else if KeyChar = '1' then
    UpdateFormat(640, 480)

  else if KeyChar = '2' then
    UpdateFormat(800, 600)

  else if KeyChar = '3' then
    UpdateFormat(1024, 768)

  else if KeyChar = '8' then
    UpdateFormat(800, 800)

  else if KeyChar = '9' then
    UpdateFormat(900, 900)

  else if KeyChar = '0' then
    UpdateFormat(1024, 1024)
end;
```

Other then selecting the current parameter you can

- change the visibility of the text edits
- change the size of the image (ClientWidth, ClientHeight)
- reset the text to hardcoded default
- cycle between alternative standard text definitions, if any
- cycle the font using a range of predefined font names

Note that when you cycle through the fonts - it will apply the next font to the top or bottom text,
depending on what the current parameter is.

> You need to press Escape key to show the edit controls for the text (not visible in picture).

( Press Escape key again to hide the edits (toggle them On/Off). )

## The one an only Main Form

*Main Form at design time.*<br>
![Meme Builder](images/Meme-Builder-02.png)

( My global CopyBitmapToClipboard procedure is in folder Util.
  Bitmap here means image, and you need to provide the routine as part of your homework.
  If you don't you will be missing a feature, remove the application frame manually in Paint. )