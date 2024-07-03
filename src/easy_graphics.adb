--  SPDX-License-Identifier: MIT
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

pragma Ada_2022;

with Ada.Streams.Stream_IO;             use  Ada.Streams.Stream_IO;
with Ada.Text_IO;

package body Easy_Graphics is

   package ASIO renames Ada.Streams.Stream_IO;
   package ATIO renames Ada.Text_IO;

   function HSV_To_RGB (H, S, V : Float) return RGB_8 is
      RR, GG, BB : Float;
      RGB        : RGB_8;
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
      return RGB;
   end HSV_To_RGB;

   function New_Image (X_Min, Y_Min, X_Max, Y_Max : Integer; Colour : RGB_8) return Image_8 is
      Img : Image_8 (X_Min .. X_Max, Y_Min .. Y_Max);
   begin
      Fill (Img, Colour);
      return Img;
   end New_Image;

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

   procedure Plot (Img : in out Image_8; Pt : Point; Colour : RGB_8) is
   begin
      if Pt.X in Img'Range (1) and then Pt.Y in Img'Range (2) then
         Img (Pt.X, Pt.Y) := Colour;
      end if;
   end Plot;

   procedure Fill (Img : in out Image_8; Colour : RGB_8) is
   begin
      Img := [others => [others => Colour]];
   end Fill;

   procedure Line (Img : in out Image_8; Start, Stop : Point; Colour : RGB_8) is
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

   procedure Fill_Bottom_Flat_Triangle (Img : in out Image_8;
                                        P1, P2, P3 : Point;
                                        Colour : RGB_8)
   is
   begin
      --  Ada.Text_IO.Put_Line ("Fill Bottom Flat called with " & P1'Image & ", " & P2'Image & ", " & P3'Image);
      for X in P2.X .. P3.X loop
         Line (Img, P1, (X, P2.Y), Colour);
      end loop;
   end Fill_Bottom_Flat_Triangle;

   procedure Fill_Top_Flat_Triangle (Img : in out Image_8;
                                     P1, P2, P3 : Point;
                                     Colour : RGB_8)
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
                       Colour     : RGB_8;
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
                     Colour : RGB_8;
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
                   Bottom_Left : Point;
                   Height, Width : Positive;
                   Colour : RGB_8;
                   Thickness : Weight := Normal)
   is
      Seg_16_Bits : constant Seg_16_T := Seg_16_Font (Chr);
      Top_Left  : constant Point := (Bottom_Left.X,               Bottom_Left.Y + Height);
      Top_Mid   : constant Point := (Bottom_Left.X + (Width / 2), Bottom_Left.Y + Height);
      Top_Right : constant Point := (Bottom_Left.X + Width,       Bottom_Left.Y + Height);
      Mid_Left  : constant Point := (Bottom_Left.X,               Bottom_Left.Y + (Height / 2));
      Mid_Mid   : constant Point := (Bottom_Left.X + (Width / 2), Bottom_Left.Y + (Height / 2));
      Mid_Right : constant Point := (Bottom_Left.X + Width,       Bottom_Left.Y + (Height / 2));
      Bot_Mid   : constant Point := (Bottom_Left.X + (Width / 2), Bottom_Left.Y);
      Bot_Right : constant Point := (Bottom_Left.X + Width,       Bottom_Left.Y);
   begin
      if (Seg_16_Bits and SEG_1)  /= 0 then Line (Img, Top_Left, Top_Mid, Colour); end if;
      if (Seg_16_Bits and SEG_2)  /= 0 then Line (Img, Top_Right, Top_Mid, Colour); end if;
      if (Seg_16_Bits and SEG_3)  /= 0 then Line (Img, Top_Left, Mid_Left, Colour); end if;
      if (Seg_16_Bits and SEG_4)  /= 0 then Line (Img, Top_Left, Mid_Mid, Colour); end if;
      if (Seg_16_Bits and SEG_5)  /= 0 then Line (Img, Top_Mid, Mid_Mid, Colour); end if;
      if (Seg_16_Bits and SEG_6)  /= 0 then Line (Img, Top_Right, Mid_Mid, Colour); end if;
      if (Seg_16_Bits and SEG_7)  /= 0 then Line (Img, Top_Right, Mid_Right, Colour); end if;
      if (Seg_16_Bits and SEG_8)  /= 0 then Line (Img, Mid_Left, Mid_Mid, Colour); end if;
      if (Seg_16_Bits and SEG_9)  /= 0 then Line (Img, Mid_Mid, Mid_Right, Colour); end if;
      if (Seg_16_Bits and SEG_10) /= 0 then Line (Img, Mid_Left, Bottom_Left, Colour); end if;
      if (Seg_16_Bits and SEG_11) /= 0 then Line (Img, Mid_Mid, Bottom_Left, Colour); end if;
      if (Seg_16_Bits and SEG_12) /= 0 then Line (Img, Mid_Mid, Bot_Mid, Colour); end if;
      if (Seg_16_Bits and SEG_13) /= 0 then Line (Img, Mid_Mid, Bot_Right, Colour); end if;
      if (Seg_16_Bits and SEG_14) /= 0 then Line (Img, Mid_Right, Bot_Right, Colour); end if;
      if (Seg_16_Bits and SEG_15) /= 0 then Line (Img, Bottom_Left, Bot_Mid, Colour); end if;
      if (Seg_16_Bits and SEG_16) /= 0 then Line (Img, Bot_Mid, Bot_Right, Colour); end if;
      case Thickness is
         when Heavy =>
            Char (Img, Chr, (Bottom_Left.X + 1, Bottom_Left.Y + 1), Height, Width, Colour, Light);
            Char (Img, Chr, (Bottom_Left.X + 1, Bottom_Left.Y), Height, Width, Colour, Light);
            Char (Img, Chr, (Bottom_Left.X,     Bottom_Left.Y + 1), Height, Width, Colour, Light);
         when Normal =>
            Char (Img, Chr, (Bottom_Left.X + 1, Bottom_Left.Y + 1), Height, Width, Colour, Light);
         when Light =>
            null;
      end case;
   end Char;

   procedure Text (Img : in out Image_8;
                  S   : String;
                  Bottom_Left : Point;
                  Height, Width, Spacing : Positive;
                  Colour : RGB_8;
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
            String'Write (Raw_Stream, "P6");
            Character'Write (Raw_Stream, Character'Val (10));
            String'Write (Raw_Stream, "# Created with Easy_Graphics " & SEM_VER);
            Character'Write (Raw_Stream, Character'Val (10));
            String'Write (Raw_Stream, Img'Length (1)'Image & Img'Length (2)'Image);
            Character'Write (Raw_Stream, Character'Val (10));
            String'Write (Raw_Stream, "255");
            Character'Write (Raw_Stream, Character'Val (10));
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

end Easy_Graphics;