# Federgraph Meme Builder App

*Federgraph Meme Builder App, no image loaded yet.*<br>
![Meme Builder](images/Meme-Builder-03.png)

Website [link](https://federgraph.de/federgraph-meme-builder-examples.html) has more pictures.

There are two projects now in this repository, FR96 and MB01.

The Main Form of FR96 is very similar to the Form that is included in the Federgraph application.
It was intended to help with adding text to one of the Federgraph pictures.

MB01 adds a buttons located on a Layout which can be hidden before you make a screenshot.
MB01 can be used to create social card images for GitHub repositories.

No matter which project you use, the first action is usually to drop a background image onto the drop target,
then you press Escape key to show the text edit controls, before you fiddle with the properties of the text.
Finally you will export the image to the clipboard. So far, saving an image is done via another application like notepad.

Key h will toggle keyboard shortcut help text. It should tell you that you can
- show and hide the drop target control with key d,
- toggle the button frame - if any - with key w,
- and copy the final image to the clipboard with ctrl c.

## Design

This is a small Delphi FMX application.

( There is no configuration, because it would be overkill. )

- It has an image component of course, initially empty.
- It has a drop target where you can drop the image, initially shown.
- The drop target can be hidden.
- A checker bitmap is shown when no actual image is loaded.
- It has top and bottom text components, always shown.
- Font family name can be chosen from a hardcoded set.
- Font size is a parameter.
- Distance of text from border (Margin) is a parameter.
- Both text components have a glow effect.
- Glow softness is a parameter.
- It has top and bottom text Edit components, initially hidden.

You can edit, arrange and style the text to some extent,
and you should be able to copy the finished image to the clipboard. 

The *invisible* application state (no visual feedback) includes the selected text (top or bottom) and the selected parameter.

- If you cycle through fonts, it effects the **selected text**.
- If you scroll the mouse wheel, it effects the **selected param**, for the selected text.

## Params

There are 6 params:

```pascal
  TMemeParam = (
    fpTopMargin,
    fpBottomMargin,
    fpTopSize,
    fpBottomSize,
    fpTopGlow,
    fpBottomGlow
  );
```

> The **mouse wheel** can be used to change the value of the current parameter.

Parameters are at the heart of the App.

- First use the keyboard to select a param,
- then use the scroll wheel of the mouse to change the value of the current parameter.

This is the very basic principle of using the App, as in the Federgraph App.

## Keyboard usage

You need a keyboard.

Using the keyboard you can:

- toggle the visibility of help text
- toggle the visibility of the drop target for the background image
- change the visibility of the text edit controls
- edit the text !
- cycle through the list of predefined fonts
- change the size of the image (ClientWidth, ClientHeight)
- select the current parameter

The OnKeyUp event of the Form looks at the key strokes.
But note that it does so only if the text edit controls are hidden.
When the text edit controls are visible the keyboard is used to edit the text of course.
