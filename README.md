# Federgraph Meme Builder App

*Federgraph Meme Builder App, no image loaded yet.*<br>
![Meme Builder](images/Meme-Builder-03.png)

Website [link](https://federgraph.de/federgraph-meme-builder-source.html) has more pictures.

It could be used as a companion app to the Federgraph application,
just in case you wanted to build a Meme with one of the Federgraph *Emoji* pictures.

Drop your image onto the drop target.

Then press Escape key to show the edit controls for the text (not visible in picture above),
and press Escape key again to hide the edits.

## Design

This is a small one-form-only Delphi FMX application.

( There is no configuration, because it would be overkill. )

- It has a drop target where you can drop the image, initially shown.
- The drop target can be hidden.
- ( A checker bitmap is shown when no actual image is loaded. )
- It has an image component, of course.
- It has top and bottom text components, always shown.
- Both text components have a glow effect.
- It has top and bottom edit components to change the text, initially hidden.

You can edit, arrange and style the text to some extent,
and you should be able to copy the finished image to the clipboard. 

## Params

There are 6 actions defined, by means of Integer action constants.

```pascal
const
  faTopMargin = 1;
  faBottomMargin = 2;
  faTopSize = 3;
  faBottomSize = 4;
  faTopGlow = 5;
  faBottomGlow = 6;
```

- First use the keyboard to select a param.
- Then use the scroll wheel of the mouse to change the value of the current parameter.

## Keyboard usage

It is a desktop application. You need a keyboard.

Using the keyboard you can:

- toggle the visibility of help text
- toggle the visibility of the drop target for the background image
- change the size of the image (ClientWidth, ClientHeight)
- change the visibility of the text edit controls
- edit the text !
- cycle the predefined fonts
- select the current parameter
- and change current param value with mouse wheel.

## The one and only Main Form

<a href="images/Meme-Builder-02.png">*Main Form at design time.*<br>
![Meme Builder](images/Meme-Builder-02.png)</a>