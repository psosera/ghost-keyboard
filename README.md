Ghost Keyboard
==============
Quick and dirty program that displays keystrokes via low-level hooks to a
WinForms GUI.  Useful for accessibility or screencasts where you want to
show keystrokes in real-time.

To use, simply run the program and the ghost keyboard.  Resize and position
as needed.

The GUI renders an image of a keyboard and highlights regions of the screen
corresponding to pressed keys.  To do this, the program is powered by a
data file describing where keys are located in the image.  Each line
corresponds to one button that should be reflected in the GUI and has the
following format.

[Keys-enum-value|mouse1|mouse2] x y width height

The first field is either an enumeration value from
System.Windows.Forms.Keys that corresponds to a keyboard keystroke or mouse1
or mouse2 which correspond to mouse button presses.  Custom keyboard layouts
can be supported by providing graphics files and datasets for each of the
keys.

(The provided keyboard image is taken and modified from
http://en.wikipedia.org/wiki/File:KB_United_States-NoAltGr.svg.)

Version History:
v1 (4/26/2012) - initial release
