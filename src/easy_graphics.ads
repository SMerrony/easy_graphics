--  SPDX-License-Identifier: MIT
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

pragma Ada_2022;

package Easy_Graphics is

   SEM_VER : constant String := "0.1.0";  --  TODO Update on each release

   type Level_8  is mod 2 ** 8;

   type RGB_8 is record
       R, G, B : Level_8;
   end record;

   type RGBA_8 is record
      R, G, B, A : Level_8;
   end record;

   --  The Basic Standard HTML/CSS colours
   BLACK   : constant RGB_8 := (0, 0, 0);
   BLUE    : constant RGB_8 := (0, 0, 255);
   CYAN    : constant RGB_8 := (0, 255, 255);
   GREEN   : constant RGB_8 := (0, 128, 0);
   GREY    : constant RGB_8 := (128, 128, 128);
      GRAY    : constant RGB_8 := GREY;
   LIME    : constant RGB_8 := (0, 255, 0);
   MAGENTA : constant RGB_8 := (255, 0, 255);
      FUCHSIA : constant RGB_8 := MAGENTA;
   MAROON  : constant RGB_8 := (128, 0, 0);
   NAVY    : constant RGB_8 := (0, 0, 128);
   OLIVE   : constant RGB_8 := (128, 0, 128);
   PURPLE  : constant RGB_8 := (128, 0, 128);
   RED     : constant RGB_8 := (255, 0, 0);
   SILVER  : constant RGB_8 := (192, 192, 192);
   TEAL    : constant RGB_8 := (0, 128, 128);
   WHITE   : constant RGB_8 := (255, 255, 255);
   YELLOW  : constant RGB_8 := (255, 255, 0);

   function HSV_To_RGB (H, S, V : Float) return RGB_8;
   --  Convert given HSV colour to RGB_8

   type Point is record
      X, Y : Integer;
   end record;

   type Image_Types is (Alpha_8, Non_Alpha_8);

   type Image_8  (<>) is private;
   type Image_8A (<>) is private;

   function New_Image (X_Min, Y_Min, X_Max, Y_Max : Integer; Colour : RGB_8) return Image_8;
   --  Returns a new Image_8 with the specified bounds and colour.

   function X_First (Img : Image_8) return Integer;
   function Y_First (Img : Image_8) return Integer;
   function X_Last  (Img : Image_8) return Integer;
   function Y_Last  (Img : Image_8) return Integer;

   type Ordinates_Arr is array (Integer range <>) of Integer;
   function Xs (Img : Image_8) return Ordinates_Arr;
   --  Returns an array containing all X ordinates, suitable for iterating via 'of'.
   function Ys (Img : Image_8) return Ordinates_Arr;
   --  Returns an array containing all Y ordinates, suitable for iterating via 'of'.

   type PPM_Type is (Plain, Raw);
   type Filled_Or_Outline is (Filled, Outline);

   procedure Plot (Img : in out Image_8; Pt : Point; Colour : RGB_8);
   --  Sets a single pixel to the given colour.
   --  N.B. Does nothing if given coordinates are outside the image boundary.

   procedure Fill (Img : in out Image_8; Colour : RGB_8);
   --  Fill given image with colour.

   procedure Line (Img : in out Image_8;
                   Start, Stop : Point;
                   Colour : RGB_8);
   --  Draw line from Start to Stop using Bresenham's algorithm.
   --  Shamelessly adapted from Rosetta Code task.

   procedure Rect (Img : in out Image_8;
                   Bottom_Left, Top_Right : Point;
                   Colour : RGB_8;
                   Fill   : Filled_Or_Outline);
   --  Draw a horizontally-aligned rectangle on the image.
   --  Optionally filled.

   procedure Triangle (Img        : in out Image_8;
                       P1, P2, P3 : Point;
                       Colour     : RGB_8;
                       Fill       : Filled_Or_Outline);
   --  Draw an optionally filled triangle on the given image.

   procedure Circle (Img    : in out Image_8;
                     Centre : Point;
                     Radius : Positive;
                     Colour : RGB_8;
                     Fill   : Filled_Or_Outline);
   --  Draw an optionally filled circle using the given centre-point,
   --  radius, & colour using Bresenham's algorithm.

   type Weight is (Light, Normal, Heavy);

   procedure Char (Img : in out Image_8;
                   Chr : Character;
                   Bottom_Left : Point;
                   Height, Width : Positive;
                   Colour : RGB_8;
                   Thickness : Weight := Normal);
   --  Draw a character on the image using the built-in 16-segment style font.
   --  The font is intended only for simple annotations and consists of (latin)
   --  letter, numeric, and a few symbol characters.  Characters not in the 
   --  font will be displayed as spaces.
   --  It is suggested that Height := 2 * Width.

   procedure Text (Img : in out Image_8;
                   S   : String;
                   Bottom_Left : Point;
                   Height, Width, Spacing : Positive;
                   Colour : RGB_8;
                   Thickness : Weight := Normal);
   --  Draw a string on the image using the 16-segment style built-in font.
   --  The font is intended only for simple annotations and consists of (latin) 
   --  letter, numeric, and a few symbol characters.  Characters not in the 
   --  font will be displayed as spaces.
   --  The _character_ height and width must be specified and it is suggested
   --  that Height := 2 * Width.
   --  Spacing is the number of points that will separate each character.

   procedure Write_PPM (Img : Image_8;  Filename : String; Plain_Raw : PPM_Type := Raw);
   --  Write an image as a PPM file to disk using plain (ASCII) or raw (mostly binary) format.
   --  Raw PPMs are significantly smaller than plain, but there is still no compression whatsoever.

private
   type Image_8  is array (Integer range <>, Integer range <>) of RGB_8;
   type Image_8A is array (Integer range <>, Integer range <>) of RGBA_8;

   type Seg_16_T is mod 2 ** 16;
   type Seg_16_Font_T is array (Character range Character'Val (32) .. Character'Val (255)) of Seg_16_T;

   Seg_16_Font : constant Seg_16_Font_T := [
      '_' => 2#0000000000000011#,
      '-' => 2#0000_0001_1000_0000#,
      ',' => 2#0000_0001_0010_0000#,
      '.' => 2#0000_0001_0101_0010#,
      ''' => 2#0000100000000000#,
      '"' => 2#0010100000000000#,
      '(' | '<' => 2#0000010000001000#,
      ')' | '>' => 2#0001000000100000#,
      '[' => 2#0100100000010001#,
      ']' => 2#1000100000010010#,
      '{' => 2#0100100100010001#,
      '}' => 2#1000100010010010#,
      '@' => 2#1110101011000011#,
      '*' => 2#0001_1101_1011_1000#,
      '/' => 2#0000_0100_0010_0000#,
      '&' => 2#0011010001101000#,
      '%' => 2#1010110110110101#,
      '+' => 2#0000_1001_1001_0000#,
      '=' => 2#0000000110000011#,
      '$' => 2#1110100110010111#,
      '£' => 2#0100100110010011#,
      '0' => 2#1110_0110_0110_0111#,
      '1' => 2#0000_1000_0001_0000#,
      '2' => 2#1100_0011_1100_0011#,
      '3' => 2#1100_0011_1000_0111#,
      '4' => 2#0010_0011_1000_0100#,
      '5' => 2#1110_0001_1000_0111#,
      '6' => 2#1110_0001_1100_0111#,
      '7' => 2#1100_0100_0001_0000#,
      '8' => 2#1110_0011_1100_0111#,
      '9' => 2#1110_0011_1000_0111#,
      'A' => 2#1110001111000100#,
      'B' => 2#1010100111000111#,
      'C' => 2#1110000001000011#,
      'D' => 2#1100101000010111#,
      'E' => 2#1110000101000011#,
      'F' => 2#1110000101000000#,
      'G' => 2#1110000011000111#,
      'H' => 2#0010001111000100#,
      'I' => 2#1100100000010011#,
      'J' => 2#0100001000000111#,
      'K' => 2#0000110000011000#,
      'L' => 2#0010000001000011#,
      'M' => 2#0011011001000100#,
      'N' => 2#0011001001001100#,
      'O' => 2#1110001001000111#,
      'P' => 2#1110001111000000#,
      'Q' => 2#1110001001001111#,
      'R' => 2#1110001111001000#,
      'S' => 2#1110_0001_1000_0111#,  --  Same a s 5 for now
      'T' => 2#1100100000010000#,
      'U' => 2#0010001001000111#,
      'V' => 2#0010010001100000#,
      'W' => 2#0010001001101100#,
      'X' => 2#0001010000101000#,
      'Y' => 2#0001010000010000#,
      'Z' => 2#1100010000100011#,
      'a' => 2#1100001111000111#,
      'b' => 2#0010000111000111#,
      'c' => 2#0000000111000011#,
      'd' => 2#0000001111000111#,
      'e' => 2#1110001111000011#,
      'f' => 2#0100100110010000#,
      'g' => 2#1110_0011_1000_0111#, --  Same as 9 for now
      'h' => 2#0010000111000100#,
      'i' => 2#1000000000010000#,
      'j' => 2#0000100000010010#,
      'k' => 2#0000100010011000#,
      'l' => 2#1000100000010001#,
      'm' => 2#0000000111010100#,
      'n' => 2#0000000111000100#,
      'o' => 2#0000000111000111#,
      'p' => 2#1010100101000000#,
      'q' => 2#1010100100010000#,
      'r' => 2#0000000101000000#,
      's' => 2#1010000100010010#,
      't' => 2#0000100110010001#,
      'u' => 2#0000000001000111#,
      'v' => 2#0000000001100000#,
      'w' => 2#0000000001010111#,
      'x' => 2#0000000110010011#,
      'y' => 2#0010001110000111#,
      'z' => 2#0000000100100010#,
      others => 0
   ];

   SEG_1 :  constant Seg_16_T := 2#1000_0000_0000_0000#;
   SEG_2 :  constant Seg_16_T := 2#0100_0000_0000_0000#;
   SEG_3 :  constant Seg_16_T := 2#0010_0000_0000_0000#;
   SEG_4 :  constant Seg_16_T := 2#0001_0000_0000_0000#;
   SEG_5 :  constant Seg_16_T := 2#0000_1000_0000_0000#;
   SEG_6 :  constant Seg_16_T := 2#0000_0100_0000_0000#;
   SEG_7 :  constant Seg_16_T := 2#0000_0010_0000_0000#;
   SEG_8 :  constant Seg_16_T := 2#0000_0001_0000_0000#;
   SEG_9 :  constant Seg_16_T := 2#0000_0000_1000_0000#;
   SEG_10 : constant Seg_16_T := 2#0000_0000_0100_0000#;
   SEG_11 : constant Seg_16_T := 2#0000_0000_0010_0000#;
   SEG_12 : constant Seg_16_T := 2#0000_0000_0001_0000#;
   SEG_13 : constant Seg_16_T := 2#0000_0000_0000_1000#;
   SEG_14 : constant Seg_16_T := 2#0000_0000_0000_0100#;
   SEG_15 : constant Seg_16_T := 2#0000_0000_0000_0010#;
   SEG_16 : constant Seg_16_T := 2#0000_0000_0000_0001#;

   procedure Fill_Bottom_Flat_Triangle (Img : in out Image_8;
                                        P1, P2, P3 : Point;
                                        Colour : RGB_8);
   procedure Fill_Top_Flat_Triangle (Img : in out Image_8;
                                     P1, P2, P3 : Point;
                                     Colour : RGB_8);
end Easy_Graphics;
