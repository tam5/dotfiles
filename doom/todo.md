# TODO

## Command Palette

- [ ] theme, colors, fonts, sizes, etc. should all match properly with the rest of the currently selected color theme, rather than being hardcoded to the sublime screenshots
- [ ] there is currently a bug that causes a horizontal bar to show up at the bottom of the frame when the palette is open, it should not be there. see bug.png
- [ ] Remove the search icon, the text input line should be left aligned with the content, as in palette-1-annotated.png - annotation 1
- [ ] there should be a small input line, as in palette-1-annotated.png - annotation 2
- [ ] when there is additional secondary helper text to be displayed, it should be displayed on the following line, for example in palette-2.png and in palette-4.png
- [ ] when there is a keybind to be displayed it should be displayed showing the icons and right aligned with the item as in palette-3.png
- [ ] the behavior of the palette, when opened for project find file, should be the same as it is in sublime - meaning the buffer should instantly switch to whatever file is currently being hovered. i've recorded demonstration of that in sublime in sublime-ff.mov

## Frames / Geometry

- [ ] the roundedness of both the actual emacs frame as well as the command palette frame should be much less dramatic and more squared off, more similar to the screenshots from sublime (noted in geo.png annotations 1 and 2)
- [ ] this light frame/window border should go away or be much less dramatic - see geo.png annotation 3
- [ ] the color of the main emacs titlebar should follow the color of the treemacs buffer / darker of the solarized faces - see geo.png annotation 4

## Additional polish tweaks

- [ ] this small bit of space between the edge and the indicator should be removed - tweaks.png annotation 1
- [ ] the highlight line color should extend the full width all the way to the sidebar and not have a gap here - the gap is because sometimes there is a git fringe, but when there is none, then the hl-line needs to extend the whole way - tweaks.png annotation 2
- [ ] there should be a little extra vertical padding/spacing between the top of the titlebar here and the first item in the treemacs - tweaks.png annotation 3
- [ ] there should be additional padding/spacing between the line numbers and the content of the buffer - tweaks.png annotation 4
