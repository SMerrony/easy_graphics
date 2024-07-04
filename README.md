# Easy_Graphics Package

Easy_Graphics is a simple library for generating graphical output from your Ada program.

Typical use cases might be educational projects, prototyping, programming challenges, or
simply when you want to visualise something quickly in a larger system.

It is free to use, with no warranty, under the MIT license.

## Main Features

* Image output as portable PPM files (other formats to follow)
* Images of any size using whatever (integer) coordinate ranges you like
* Simple set of graphics primitives provided
* 24-bit colour, some basic colours predefined as constants

### Utility Subprograms
* `New_Image` create a new image (canvas), prefilled with a colour
* `HSV_To_RGB` convert colour values
* `Write_PPM` create raw or plain PPM file from image

### Drawing Primitives
* `Plot` a point on the image
* `Fill` an entire image with a colour
* `Line` draw a line between two `Point`s
* `Rect` (filled or outline)
* `Triangle` (filled or outline)
* `Circle` (filled or outline)
* `Char` draw a character using a simple built-in font
* `Text` draw a string

### Turtle Graphics
Basic turtle graphics are supported.
* `New_Turtle` creates a turtle for an image
* `Home` centres the turtle
* `Pen_Up`, `Pen_Down` and `Pen_Color`
* `Forward`
* `Left` and `Right` turn relative to the current heading
* `Turn_To` turns to an absolute heading
