--  SPDX-License-Identifier: MIT
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

pragma Ada_2022;

with Ada.Containers.Vectors;
with Ada.Numerics;                      use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Streams.Stream_IO;             use  Ada.Streams.Stream_IO;
with Ada.Text_IO;

with Interfaces;

package body Easy_Graphics is

   package ASIO renames Ada.Streams.Stream_IO;
   package ATIO renames Ada.Text_IO;

   function HSV_To_RGB (H, S, V : Float) return RGBA_8 is
      RR, GG, BB : Float;
      RGB        : RGBA_8;
      Hue_Int    : constant Float := Float'Floor (H);
      Hue_Frac   : constant Float := H - Hue_Int;
      P          : constant Float := V - S;
      Q          : constant Float := V - S * Hue_Frac;
      T          : constant Float := V - S * (V - Hue_Frac);
   begin
      case Integer (Hue_Int) is
         when 0 => RR := V; GG := T; BB := P;
         when 1 => RR := Q; GG := V; BB := P;
         when 2 => RR := P; GG := V; BB := T;
         when 3 => RR := P; GG := Q; BB := V;
         when 4 => RR := T; GG := P; BB := V;
         when 5 => RR := V; GG := P; BB := Q;
         when others => null;
      end case;
      RGB.R := Level_8 (Integer (Float'Floor (RR * 255.0)));
      RGB.G := Level_8 (Integer (Float'Floor (GG * 255.0)));
      RGB.B := Level_8 (Integer (Float'Floor (BB * 255.0)));
      RGB.A := 255;
      return RGB;
   end HSV_To_RGB;

   function New_Image (Bottom_Left, Top_Right: Point; Colour : RGBA_8) return Image_8 is
      Img : Image_8 (Bottom_Left.X .. Top_Right.X, Bottom_Left.Y .. Top_Right.Y);
   begin
      Fill (Img, Colour);
      return Img;
   end New_Image;

   function New_Turtle (Img_Acc : access Image_8) return Turtle_Rec is
      Turtle : Turtle_Rec;
   begin
      Turtle.Image     := Img_Acc;
      Turtle.Position  := (Img_Acc'First (1) + (Img_Acc'Last (1) - Img_Acc'First (1)) / 2,
                           Img_Acc'First (2) + (Img_Acc'Last (2) - Img_Acc'First (2)) / 2);
      Turtle.Direction := 0; --  "North"
      Turtle.Pen_Down  := False;
      Turtle.Colour    := BLACK;
      return Turtle;
   end New_Turtle;

   procedure Home (Turtle : in out Turtle_Rec) is
      From_Pos : constant Point := Turtle.Position;
      New_Pos  : constant Point := (Turtle.Image'First (1) + (Turtle.Image'Last (1) - Turtle.Image'First (1)) / 2,
                           Turtle.Image'First (2) + (Turtle.Image'Last (2) - Turtle.Image'First (2)) / 2);
   begin
      Turtle.Position  := New_Pos;
      if Turtle.Pen_Down then
         Line (Turtle.Image.all, From_Pos, Turtle.Position, Turtle.Colour);
      end if;
   end Home;

   procedure Pen_Up (Turtle : in out Turtle_Rec) is
   begin
      Turtle.Pen_Down := False;
   end Pen_Up;

   procedure Pen_Down (Turtle : in out Turtle_Rec) is
   begin
      Turtle.Pen_Down := True;
   end Pen_Down;

   procedure Pen_Color (Turtle : in out Turtle_Rec; Colour : RGBA_8) is
   begin
      Turtle.Colour := Colour;
   end Pen_Color;

   procedure Forward (Turtle : in out Turtle_Rec; Steps : Natural) is
      From_Pos     : constant Point := Turtle.Position;
      New_X, New_Y : Integer;
   begin
      New_X := From_Pos.X + Integer (Float (Steps) * Sin (Float (Turtle.Direction), 360.0));
      New_Y := From_Pos.Y + Integer (Float (Steps) * Cos (Float (Turtle.Direction), 360.0));
      Turtle.Position := (New_X, New_Y);
      --  Ada.Text_IO.Put_Line ("DEBUG: Old Posn" & From_Pos'Image & " New Posn" & Turtle.Position'Image);
      if Turtle.Pen_Down then
         Line (Turtle.Image.all, From_Pos, Turtle.Position, Turtle.Colour);
      end if;
   end Forward;

   procedure Left (Turtle : in out Turtle_Rec; Degrees : Natural) is
   begin
      Turtle.Direction := @ - Degrees;
   end Left;

   procedure Right (Turtle : in out Turtle_Rec; Degrees : Natural) is
   begin
      Turtle.Direction := @ + Degrees;
   end Right;

   procedure Turn_To (Turtle : in out Turtle_Rec; Degrees : Natural) is
   begin
      Turtle.Direction := Degrees;
   end Turn_To;

   function X_First (Img : Image_8) return Integer is (Img'First (1));
   function Y_First (Img : Image_8) return Integer is (Img'First (2));
   function X_Last  (Img : Image_8) return Integer is (Img'Last  (1));
   function Y_Last  (Img : Image_8) return Integer is (Img'Last  (2));

   function Xs (Img : Image_8) return Ordinates_Arr is
      Ords : Ordinates_Arr (Img'First (1) .. Img'Last (1));
   begin
      for I in Ords'Range loop
         Ords (I) := I;
      end loop;
      return Ords;
   end Xs;

   function Ys (Img : Image_8) return Ordinates_Arr is
      Ords : Ordinates_Arr (Img'First (2) .. Img'Last (2));
   begin
      for I in Ords'Range loop
         Ords (I) := I;
      end loop;
      return Ords;
   end Ys;

   procedure Plot (Img : in out Image_8; Pt : Point; Colour : RGBA_8) is
   begin
      if Pt.X in Img'Range (1) and then Pt.Y in Img'Range (2) then
         Img (Pt.X, Pt.Y) := Colour;
      end if;
   end Plot;

   procedure Set_Alpha (Img : in out Image_8; Pt : Point; Alpha : Level_8) is
   begin
      if Pt.X in Img'Range (1) and then Pt.Y in Img'Range (2) then
         Img (Pt.X, Pt.Y).A := Alpha;
      end if;
   end Set_Alpha;

   procedure Fill (Img : in out Image_8; Colour : RGBA_8) is
   begin
      Img := [others => [others => Colour]];
   end Fill;

   procedure Line (Img : in out Image_8; Start, Stop : Point; Colour : RGBA_8) is
      DX  : constant Float := abs Float (Stop.X - Start.X);
      DY  : constant Float := abs Float (Stop.Y - Start.Y);
      Err : Float;
      X   : Integer := Start.X;
      Y   : Integer := Start.Y;
      Step_X : Integer := 1;
      Step_Y : Integer := 1;
   begin
      if Start.X > Stop.X then
         Step_X := -1;
      end if;
      if Start.Y > Stop.Y then
         Step_Y := -1;
      end if;
      if DX > DY then
         Err := DX / 2.0;
         while X /= Stop.X loop
            Plot (Img, (X, Y), Colour);
            Err := Err - DY;
            if Err < 0.0 then
               Y := Y + Step_Y;
               Err := Err + DX;
            end if;
            X := X + Step_X;
         end loop;
      else
         Err := DY / 2.0;
         while Y /= Stop.Y loop
            Plot (Img, (X, Y), Colour);
            Err := Err - DX;
            if Err < 0.0 then
               X := X + Step_X;
               Err := Err + DY;
            end if;
            Y := Y + Step_Y;
         end loop;
      end if;
      Plot (Img, (X, Y), Colour);
   end Line;

   procedure Rect (Img : in out Image_8;
                   Bottom_Left, Top_Right : Point;
                   Colour : RGBA_8;
                   Fill   : Filled_Or_Outline)
   is
   begin
      if Fill = Filled then
         for Y in Bottom_Left.Y .. Top_Right.Y loop
            for X in Bottom_Left.X .. Top_Right.X loop
               Plot (Img, (X, Y), Colour);
            end loop;
         end loop;
      else
         for Y in Bottom_Left.Y .. Top_Right.Y loop
            if Y = Bottom_Left.Y or else Y = Top_Right.Y then
               for X in  Bottom_Left.X .. Top_Right.X loop
                  Plot (Img, (X, Y), Colour);
               end loop;
            else
               Plot (Img, (Bottom_Left.X, Y), Colour);
               Plot (Img, (Top_Right.X, Y), Colour);
            end if;
         end loop;
      end if;
   end Rect;

   procedure Fill_Bottom_Flat_Triangle (Img : in out Image_8;
                                        P1, P2, P3 : Point;
                                        Colour : RGBA_8)
   is
   begin
      --  Ada.Text_IO.Put_Line ("Fill Bottom Flat called with " & P1'Image & ", " & P2'Image & ", " & P3'Image);
      for X in P2.X .. P3.X loop
         Line (Img, P1, (X, P2.Y), Colour);
      end loop;
   end Fill_Bottom_Flat_Triangle;

   procedure Fill_Top_Flat_Triangle (Img : in out Image_8;
                                     P1, P2, P3 : Point;
                                     Colour : RGBA_8)
   is
   begin
      --  Ada.Text_IO.Put_Line ("Fill Top Flat called with " & P1'Image & ", " & P2'Image & ", " & P3'Image);
      for X in P1.X .. P2.X loop
         Line (Img, P3, (X, P1.Y), Colour);
      end loop;
   end Fill_Top_Flat_Triangle;

   procedure Swap_Points (P1, P2 : in out Point) is
      Tmp : constant Point := P1;
   begin
      P1 := P2;
      P2 := Tmp;
   end Swap_Points;

   procedure Triangle (Img        : in out Image_8;
                       P1, P2, P3 : Point;
                       Colour     : RGBA_8;
                       Fill       : Filled_Or_Outline) is
   begin
      if Fill = Outline then
         Line (Img, P1, P2, Colour);
         Line (Img, P2, P3, Colour);
         Line (Img, P3, P1, Colour);
      else
         declare
            V1 : Point := P1;
            V2 : Point := P2;
            V3 : Point := P3;
            V4 : Point;
            A, B, C : Integer;
         begin
            --  Sort into heighest first
            if V1.Y < V2.Y then Swap_Points (V1, V2); end if;
            if V2.Y < V3.Y then Swap_Points (V2, V3); end if;
            if V1.Y < V2.Y then Swap_Points (V1, V2); end if;
            A  := V1.Y - V3.Y;
            B  := V3.X - V1.X;
            C  := ((V1.X - V3.X) * V1.Y) + ((V3.Y - V1.Y) * V1.X);
            if V2.Y = V3.Y then
               Fill_Bottom_Flat_Triangle (Img, V1, V2, V3, Colour);
            elsif V1.Y = V2.Y then
               Fill_Top_Flat_Triangle (Img, V1, V2, V3, Colour);
            else
               V4 := ((-C - (B * V2.Y)) / A, V2.Y);
               Fill_Bottom_Flat_Triangle (Img, V1, V2, V4, Colour);
               Fill_Top_Flat_Triangle    (Img, V2, V4, V3, Colour);
            end if;
         end;
      end if;
   end Triangle;

   procedure Circle (Img    : in out Image_8;
                     Centre : Point;
                     Radius : Positive;
                     Colour : RGBA_8;
                     Fill   : Filled_Or_Outline)
   is
      X : Integer := -Radius;
      Y : Integer := 0;
      Err : Integer := 2 - (2 * Radius);
      Tmp_R : Integer;
   begin
      loop
         if Fill = Filled then
            Line (Img, (Centre.X - X, Centre.Y + Y), (Centre.X + X, Centre.Y + Y), Colour);
            Line (Img, (Centre.X - X, Centre.Y - Y), (Centre.X + X, Centre.Y - Y), Colour);
         else
            Plot (Img, (Centre.X - X, Centre.Y + Y), Colour);
            Plot (Img, (Centre.X - Y, Centre.Y - X), Colour);
            Plot (Img, (Centre.X + X, Centre.Y - Y), Colour);
            Plot (Img, (Centre.X + Y, Centre.Y + X), Colour);
         end if;
         Tmp_R := Err;
         if Tmp_R <= Y then
            Y := Y + 1;
            Err := Err + (Y * 2) + 1;
         end if;
         if Tmp_R > X or else Err > Y then
            X := X + 1;
            Err := Err + (X * 2) + 1;
         end if;
         exit when X >= 0;
      end loop;

   end Circle;

   procedure Char (Img : in out Image_8;
                   Chr : Character;
                   Baseline : Point;
                   Height, Width : Positive;
                   Colour : RGBA_8;
                   Thickness : Weight := Normal)
   is
      Seg_16_Bits : constant Seg_16_T := Seg_16_Font (Chr);
      Anchor    : Point := Baseline;
      Adj_Ht    : Positive := Height;
      Top_Left, Top_Mid, Top_Right, Mid_Left, Mid_Mid, Mid_Right, Bot_Mid, Bot_Right : Point;
      subtype Descenders is Character with 
         Static_Predicate => Descenders in 'g' | 'j' | 'p' | 'q' | 'y';
      subtype Squashers is Character with 
         Static_Predicate => Squashers in 'a' | 'e' | 's' | 'x';   
   begin
      --  special case for full-stop
      if Chr = '.' then
         Circle (Img, (Baseline.X + (Width / 2), Baseline.Y + (Width / 4)), Width / 4, Colour, Filled);
      else
         --  adjust lower-case descenders and squashers
         if Chr in Descenders then
            Anchor.Y := @ - (Height / 2);
         end if;
         if Chr in Squashers then
            Adj_Ht := @ / 2;
         end if;
         Top_Left  := (Anchor.X,               Anchor.Y + Adj_Ht);
         Top_Mid   := (Anchor.X + (Width / 2), Anchor.Y + Adj_Ht);
         Top_Right := (Anchor.X + Width,       Anchor.Y + Adj_Ht);
         Mid_Left  := (Anchor.X,               Anchor.Y + (Adj_Ht / 2));
         Mid_Mid   := (Anchor.X + (Width / 2), Anchor.Y + (Adj_Ht / 2));
         Mid_Right := (Anchor.X + Width,       Anchor.Y + (Adj_Ht / 2));
         Bot_Mid   := (Anchor.X + (Width / 2), Anchor.Y);
         Bot_Right := (Anchor.X + Width,       Anchor.Y);
         if (Seg_16_Bits and SEG_1)  /= 0 then Line (Img, Top_Left, Top_Mid, Colour); end if;
         if (Seg_16_Bits and SEG_2)  /= 0 then Line (Img, Top_Right, Top_Mid, Colour); end if;
         if (Seg_16_Bits and SEG_3)  /= 0 then Line (Img, Top_Left, Mid_Left, Colour); end if;
         if (Seg_16_Bits and SEG_4)  /= 0 then Line (Img, Top_Left, Mid_Mid, Colour); end if;
         if (Seg_16_Bits and SEG_5)  /= 0 then Line (Img, Top_Mid, Mid_Mid, Colour); end if;
         if (Seg_16_Bits and SEG_6)  /= 0 then Line (Img, Top_Right, Mid_Mid, Colour); end if;
         if (Seg_16_Bits and SEG_7)  /= 0 then Line (Img, Top_Right, Mid_Right, Colour); end if;
         if (Seg_16_Bits and SEG_8)  /= 0 then Line (Img, Mid_Left, Mid_Mid, Colour); end if;
         if (Seg_16_Bits and SEG_9)  /= 0 then Line (Img, Mid_Mid, Mid_Right, Colour); end if;
         if (Seg_16_Bits and SEG_10) /= 0 then Line (Img, Mid_Left, Anchor, Colour); end if;
         if (Seg_16_Bits and SEG_11) /= 0 then Line (Img, Mid_Mid, Anchor, Colour); end if;
         if (Seg_16_Bits and SEG_12) /= 0 then Line (Img, Mid_Mid, Bot_Mid, Colour); end if;
         if (Seg_16_Bits and SEG_13) /= 0 then Line (Img, Mid_Mid, Bot_Right, Colour); end if;
         if (Seg_16_Bits and SEG_14) /= 0 then Line (Img, Mid_Right, Bot_Right, Colour); end if;
         if (Seg_16_Bits and SEG_15) /= 0 then Line (Img, Anchor, Bot_Mid, Colour); end if;
         if (Seg_16_Bits and SEG_16) /= 0 then Line (Img, Bot_Mid, Bot_Right, Colour); end if;
      end if;
      case Thickness is
         when Heavy =>
            Char (Img, Chr, (Baseline.X + 1, Baseline.Y + 1), Height, Width, Colour, Light);
            Char (Img, Chr, (Baseline.X + 1, Baseline.Y), Height, Width, Colour, Light);
            Char (Img, Chr, (Baseline.X,     Baseline.Y + 1), Height, Width, Colour, Light);
         when Normal =>
            Char (Img, Chr, (Baseline.X + 1, Baseline.Y + 1), Height, Width, Colour, Light);
         when Light =>
            null;
      end case;
   end Char;

   procedure Text (Img : in out Image_8;
                  S   : String;
                  Bottom_Left : Point;
                  Height, Width, Spacing : Positive;
                  Colour : RGBA_8;
                  Thickness : Weight := Normal)
   is
      X : Integer := Bottom_Left.X;
   begin
      for C of S loop
         Char (Img, C, (X, Bottom_Left.Y), Height, Width, Colour, Thickness);
         X := X + Width + Spacing;
      end loop;
   end Text;

   procedure Write_PPM (Img       : Image_8;
                        Filename  : String;
                        Plain_Raw : PPM_Type := Raw) is
      Plain_PPM_File : ATIO.File_Type;
      Raw_PPM_File   : ASIO.File_Type;
      Raw_Stream     : Stream_Access;
      NL             : constant Character := Character'Val (10);
   begin
      case Plain_Raw is
         when Plain =>
            ATIO.Create (Plain_PPM_File, ATIO.Out_File, Filename);
            ATIO.Put_Line (Plain_PPM_File, "P3");
            ATIO.Put_Line (Plain_PPM_File, "# Created with Easy_Graphics " & SEM_VER);
            ATIO.Put_Line (Plain_PPM_File, Img'Length (1)'Image & Img'Length (2)'Image);
            ATIO.Put_Line (Plain_PPM_File, "255");
         when Raw =>
            ASIO.Create (Raw_PPM_File, ASIO.Out_File, Filename);
            Raw_Stream := Stream (Raw_PPM_File);
            String'Write (Raw_Stream, "P6" & NL);
            String'Write (Raw_Stream, "# Created with Easy_Graphics " & SEM_VER & NL);
            String'Write (Raw_Stream, Img'Length (1)'Image & Img'Length (2)'Image & NL);
            String'Write (Raw_Stream, "255" & NL);
      end case;
      for Y in reverse Img'Range (2) loop
         for X in Img'Range (1) loop
            case Plain_Raw is
               when Plain =>
                  ATIO.Put_Line (Plain_PPM_File, Img (X, Y).R'Image & Img (X, Y).G'Image & Img (X, Y).B'Image);
               when Raw =>
                  Level_8'Write (Raw_Stream, Img (X, Y).R);
                  Level_8'Write (Raw_Stream, Img (X, Y).G);
                  Level_8'Write (Raw_Stream, Img (X, Y).B);
            end case;
         end loop;
      end loop;
      case Plain_Raw is
         when Plain => ATIO.Close (Plain_PPM_File);
         when Raw   => ASIO.Close (Raw_PPM_File);
      end case;
   end Write_PPM;

   procedure Write_PAM (Img : Image_8;  Filename : String) is
      PAM_File   : ASIO.File_Type;
      PAM_Stream : Stream_Access;
      NL         : constant Character := Character'Val (10);
   begin
      ASIO.Create (PAM_File, ASIO.Out_File, Filename);
      PAM_Stream := Stream (PAM_File);
      String'Write (PAM_Stream, "P7" & NL);
      String'Write (PAM_Stream, "# Created with Easy_Graphics " & SEM_VER & NL);
      String'Write (PAM_Stream, "WIDTH" & Img'Length (1)'Image & NL);
      String'Write (PAM_Stream, "HEIGHT" & Img'Length (2)'Image & NL);
      String'Write (PAM_Stream, "DEPTH 4" & NL);
      String'Write (PAM_Stream, "MAXVAL 255" & NL);
      String'Write (PAM_Stream, "TUPLTYPE RGB_ALPHA" & NL);
      String'Write (PAM_Stream, "ENDHDR" & NL);
      for Y in reverse Img'Range (2) loop
         for X in Img'Range (1) loop
            Level_8'Write (PAM_Stream, Img (X, Y).R);
            Level_8'Write (PAM_Stream, Img (X, Y).G);
            Level_8'Write (PAM_Stream, Img (X, Y).B);
            Level_8'Write (PAM_Stream, Img (X, Y).A);
         end loop;
      end loop;
      ASIO.Close (PAM_File);
   end Write_PAM;

   procedure Write_GIF (Img : Image_8;  Filename : String) is
      use Ada.Containers;
      use Interfaces;
      GIF_File   : ASIO.File_Type;
      GIF_Stream : Stream_Access;

      --  function Lower_Byte (U16 : Unsigned_16) return Unsigned_8 is
      --     (Unsigned_8 (U16 and 16#00ff#));
      --  function Upper_Byte (U16 : Unsigned_16) return Unsigned_8 is
      --     (Unsigned_8 (Shift_Right (U16 and 16#ff00#, 8)));
      --  function Swap_Bytes (U16 : Unsigned_16) return Unsigned_16 is
      --     (Shift_Right (U16 and 16#ff00#, 8) or Shift_Left (U16 and 16#00ff#, 8));

      function RGBA_8_To_RGB_24 (RGBA8 : RGBA_8) return Unsigned_24 is
         ((Unsigned_24 (RGBA8.R) * (2 ** 16)) +
          (Unsigned_24 (RGBA8.G) * (2 ** 8)) +
          Unsigned_24 (RGBA8.B));

      package Colour_Vectors is new Ada.Containers.Vectors (Positive, Unsigned_24);
      Used_Colors : Colour_Vectors.Vector;
      Colour_24 : Unsigned_24;
      MAX_UNCOMPRESSED_COLOURS : constant Integer := 128;
      BLK_SIZE : constant Integer := 120;
      Image_Block : array (1 .. BLK_SIZE) of Unsigned_8;
      Blk_Ix : Positive;
   begin
      ASIO.Create (GIF_File, ASIO.Out_File, Filename);
      GIF_Stream := Stream (GIF_File);
      String'Write (GIF_Stream, "GIF89a");
      Unsigned_16'Write (GIF_Stream, Unsigned_16 (Img'Length (1)));
      Unsigned_16'Write (GIF_Stream, Unsigned_16 (Img'Length (2)));
      Unsigned_8'Write (GIF_Stream, 16#f6#);  --  7-bit colour palette, with GCT
      Unsigned_8'Write (GIF_Stream, 0);       --  bg colour
      Unsigned_8'Write (GIF_Stream, 0);       --  pixel aspect ratio
      for Y in reverse Img'Range (2) loop
         for X in Img'Range (1) loop
            Colour_24 := RGBA_8_To_RGB_24 (Img (X, Y));
            if not Used_Colors.Contains (Colour_24) then
               --  ATIO.Put_Line ("DEBUG: Adding colour: " & Colour_24'Image);
               Used_Colors.Append (Colour_24);
            end if;
         end loop;
      end loop;
      --  For now, we give up if there are more than 128 colours in the Image
      if Integer (Used_Colors.Length) > MAX_UNCOMPRESSED_COLOURS then
         ASIO.Close (GIF_File);
         raise Too_Many_Colours with
            "GIF export currently limted to 128 colours, image has" &
            Used_Colors.Length'Image;
      end if;
      --  ATIO.Put_Line ("DEBUG: Colours found: " & Used_Colors.Length'Image);
      --  Write pallette
      for C in 1 .. Used_Colors.Length loop
         --  Unsigned_24'Write (GIF_Stream, Used_Colors (Positive (C)));
         Unsigned_8'Write (GIF_Stream, Unsigned_8 (Used_Colors (Positive (C)) / (2 ** 16)));
         Unsigned_8'Write (GIF_Stream, Unsigned_8 ((Used_Colors (Positive (C)) and 16#00ff00#) / (2 ** 8)));
         Unsigned_8'Write (GIF_Stream, Unsigned_8 ((Used_Colors (Positive (C)) and 16#0000ff#)));
      end loop;
      for C in Integer (Used_Colors.Length) + 1 .. MAX_UNCOMPRESSED_COLOURS loop
         Unsigned_8'Write (GIF_Stream, 0);
         Unsigned_8'Write (GIF_Stream, 0);
         Unsigned_8'Write (GIF_Stream, 0);
      end loop;
      --  No GCE for now
      --  Image descriptor
      Unsigned_8'Write  (GIF_Stream, 16#2c#);  --  ASCII comma
      Unsigned_32'Write (GIF_Stream, 0);       --  Top-left coordinates
      Unsigned_16'Write (GIF_Stream, Unsigned_16 (Img'Length (1)));
      Unsigned_16'Write (GIF_Stream, Unsigned_16 (Img'Length (2)));
      Unsigned_8'Write  (GIF_Stream, 0);       --  No local colour table
      --  Write an UNCOMPRESSED image
      Unsigned_8'Write  (GIF_Stream, 7);       --  7-bits-per-colour => 8-bit pixels
      Image_Block (1) := Unsigned_8 (BLK_SIZE) - 1;
      Image_Block (2) := 16#80#;  --  CLEAR
      --  Image_Block (BLK_SIZE) := 16#81#;
      Blk_Ix := 3;
      for Y in reverse Img'Range (2) loop
         for X in Img'Range (1) loop
            Image_Block (Blk_Ix) := Unsigned_8 (Used_Colors.Find_Index (RGBA_8_To_RGB_24 (Img (X, Y)))) - 1;
            Blk_Ix := Blk_Ix + 1;
            if Blk_Ix > BLK_SIZE then
               for B of Image_Block loop  --  write out complete block
                  Unsigned_8'Write (GIF_Stream, B);
               end loop;
               Blk_Ix := 3;
            end if;
         end loop;
      end loop;
      --  ATIO.Put_Line ("DEBUG: After main loop Blk_Ix: " & Blk_Ix'Image);
      if Blk_Ix /= 3 then
         Image_Block (1) := Unsigned_8 (Blk_Ix) - 1;
         for B in 1 .. Blk_Ix loop  --  write out partial block
            Unsigned_8'Write (GIF_Stream, Image_Block (B));
         end loop;
      end if;
      Unsigned_8'Write  (GIF_Stream, 16#81#);
      Unsigned_8'Write  (GIF_Stream, 0);
      Unsigned_8'Write  (GIF_Stream, 16#3b#);
      ASIO.Close (GIF_File);
   end Write_GIF;

end Easy_Graphics;