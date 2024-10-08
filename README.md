<a>
   <img src="doc/logo.gif" align="right" alt="Easy_Graphics Logo, generated by Easy_Graphics!">
</a>

# Easy_Graphics Package

Easy_Graphics is a simple library for generating graphical output from your Ada program.

Typical use cases might be educational projects, prototyping, programming challenges, or
simply when you want to visualise something quickly in a larger system.

The logo on the right is created with just a few lines of code, see [this program](<tests/src/create_logo.adb>)

The package is free to use, with no warranty, under the MIT license.

## Main Features

* Image output as portable PPM, PAM, and GIF files (other formats to follow)
* Images of any size using whatever (integer) coordinate ranges you like
* Simple set of graphics primitives provided
* Basic turtle graphics are supported
* Images are 24-bit colour + 8-bit alpha
* Basic (HTML/CSS) colours predefined as constants, as is TRANSPARENT

### Utility Subprograms
* `New_Image` create a new image ("canvas"), prefilled with a colour or transparent
* `Write_PPM` create "raw" or "plain" NetPBM PPM file from image, Alpha is ignored
* `Write_GIF` create GIF file - only partially implemented at the moment
* `Write_PAM` create NetPBM PAM file from image, includes transparency.  N.B. Convert to PNG with `convert img.pam img.png` or view directly with `pqiv`
* `HSV_To_RGB` convert colour values


### Drawing Primitives
* `Plot` a point on the image
* `Set_Alpha` changes the transparency of a point
* `Fill` an entire image with a colour
* `Line` draw a line between two `Point`s
* `Rect` (filled or outline)
* `Triangle` (filled or outline)
* `Circle` (filled or outline)
* `Char` draw a character using a simple built-in font
* `Text` draw a string

### Turtle Graphics
* `New_Turtle` creates a virtual turtle for an image
* `Home` centres the turtle
* `Go_To` a specific coordinate
* `Pen_Up`, `Pen_Down` and `Pen_Color`
* `Forward`, `Back`
* `Left` and `Right` turn relative to the current heading
* `Turn_To` turns to an absolute heading

### Example Code
There are several example programs using Easy_Graphics in the `tests/src` directory.
The [tests.adb file](<tests/src/tests.adb>) is intended to demonstrate all features
that are currently implemented.