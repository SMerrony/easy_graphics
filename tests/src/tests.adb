--  SPDX-License-Identifier: MIT
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

pragma Ada_2022;

with Ada.Numerics;                      use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

with Easy_Graphics;    use Easy_Graphics;

procedure Tests is

   procedure Test_Basics is
      Small_8bit_Img  : Image_8 := New_Image (0, 0, 12, 16, BLACK);
   begin
      --  Test small 8-bit image with 'standard coords'
      for X of Xs (Small_8bit_Img) loop
         for Y of Ys (Small_8bit_Img) loop
            Plot (Small_8bit_Img, (X, Y), (if X = Y then RED else WHITE));
         end loop;
      end loop;
      Write_PPM (Small_8bit_Img, "small_8bit_test_plain.ppm", Plain);
      Write_PPM (Small_8bit_Img, "small_8bit_test_raw.ppm", Raw);
   end Test_Basics;

   procedure Test_Rects is
      Rects_Img : Image_8 := New_Image (-100, -50, 400, 300, BLACK);
   begin
      Rect (Rects_Img, (-20, -25), (100, 100), SILVER, Filled);
      Rect (Rects_Img, (50, 50), (200, 150), MAGENTA, Outline);
      Write_PPM (Rects_Img, "rectangles_raw.ppm");
   end Test_Rects;

   procedure Test_Triangles is
      Triangles_Img   : Image_8 := New_Image (-200, -200, 200, 300, SILVER);
   begin
      Triangle (Triangles_Img, (0, 0), (100, 0), (50, 100), YELLOW, Outline);
      Triangle (Triangles_Img, (-150, 150), (0, 0), (100, 0), BLUE, Filled);
      Triangle (Triangles_Img, (50, 0), (150, 100), (190, -100), RED, Filled);
      Write_PPM (Triangles_Img, "triangles_8bit_test_raw.ppm");
   end Test_Triangles;

   procedure Test_Circles is
      Circles_Img     : Image_8 := New_Image (-200, -200, 200, 200, WHITE);
   begin
      Circle (Circles_Img, (0, 0), 50, BLACK, Outline);
      Circle (Circles_Img, (50, 50), 50, CYAN, Filled);
      --  Test drawing beyond the border...
      Circle (Circles_Img, (180, -180), 30, RED, Filled);
      Write_PPM (Circles_Img, "circles_test_raw.ppm", Raw);
   end Test_Circles;

   procedure Test_Graphs is
      Graph_8bit_Img  : Image_8 := New_Image (-200, -200, 200, 200, WHITE);
   begin
      --  Test bigger 8-bit image with coords crossing zero point
      for X of Xs (Graph_8bit_Img) loop
         for Y of Ys (Graph_8bit_Img) loop
            if X = Y then
               Plot (Graph_8bit_Img, (X, Y), GREEN);
            elsif X = 0 or else Y = 0 then
               Plot (Graph_8bit_Img, (X, Y), BLACK);
            end if;
         end loop;
      end loop;
      Line (Graph_8bit_Img, (-50, 120), (178, -75), SILVER);
      Line (Graph_8bit_Img, (123, 128), (-27, -20), RED);
      for X in -180 .. 180 loop
         Plot (Graph_8bit_Img, (X, Integer (Sin (Float (X), 360.0) * 100.0)), BLUE);
      end loop;
      Write_PPM (Graph_8bit_Img, "graph_8bit_test_plain.ppm", Plain);
      Write_PPM (Graph_8bit_Img, "graph_8bit_test_raw.ppm", Raw);
   end Test_Graphs;

   procedure Test_HSV_Colour_Wheel is
      Diameter  : constant Integer := 480;
      Radius    : constant Integer := Diameter / 2;
      Radius_Fl : constant Float   := Float (Radius);
      Wheel_Img : Image_8 := New_Image (-Radius, -Radius, Radius, Radius, BLACK);
      V         : constant Float   := 1.0;
      function Atan2 (Y, X : Float) return Float is
         Res : Float;
      begin
         if X > 0.0 then Res := Arctan (Y / X);
         elsif X < 0.0 and then Y >= 0.0 then Res := Arctan (Y / X) + Pi;
         elsif X < 0.0 and then Y < 0.0  then Res := Arctan (Y / X) - Pi;
         elsif X = 0.0 and then Y > 0.0  then Res := Pi / 2.0;
         elsif X = 0.0 and then Y > 0.0  then Res := -Pi / 2.0;
         else Res := -Pi / 2.0;  --  Technically: Undefined
         end if;
         return Res;
      end Atan2;
   begin
      for Y in -Radius .. Radius loop
         for X in -Radius .. Radius loop
            declare
               XX   : constant Float := Float (X);
               YY   : constant Float := Float (Y);
               Dist : constant Float := Sqrt (XX ** 2 + YY ** 2);
            begin
               if Dist <= Radius_Fl then
                  declare
                     Sat  : constant Float := Dist / Radius_Fl;
                     Hue  : Float := Atan2 (YY, XX);
                  begin
                     if Hue < 0.0 then Hue := Hue + 2.0 * Pi; end if;
                     Hue := (Hue * 180.0 / Pi) / 60.0;
                     Plot (Wheel_Img, (X, Y), HSV_To_RGB (Hue, Sat, V));
                  end;
               end if;
            end;
         end loop;
      end loop;
      Write_PPM (Wheel_Img, "hsv_colour_wheel.ppm");
   end Test_HSV_Colour_Wheel;

   procedure Test_Text is
      Text_Img : Image_8 := New_Image (0, 0, 320, 200, WHITE);
   begin
      Char (Text_Img, '2', (10, 30), 40, 20, BLUE);
      Char (Text_Img, '3', (77, 160), 40, 20, BLUE, Light);
      Text (Text_Img, "1234567890", (0, 110), 32, 16, 5, BLACK);
      Text (Text_Img, "+-*/%'£$=[({})]&@", (45, 12), 20, 10, 5, BLACK);
      Text (Text_Img, "ABCDEFGHIJKLMNOPQRSTUVWXYZ", (0, 80), 16, 8, 4, BLACK);
      Text (Text_Img, "abcdefghijklmnopqrstuvwxyz", (45, 50), 12, 6, 2, BLACK, Light);
      Text (Text_Img, "HEAVY", (150, 150), 16, 8, 4, RED, Heavy);
      Text (Text_Img, "(C)2024 STEPHEN MERRONY", (180, 1), 8, 4, 2, BLACK, Light);
      --  Text_Img.Text ("(C)2024 STEPHEN MERRONY", (160, 2), 8, 4, 2, BLACK, Light);
      Write_PPM (Text_Img, "text_raw.ppm");
   end Test_Text;

begin

   Test_Basics;
   Test_Rects;
   Test_Triangles;
   Test_Circles;
   Test_Graphs;
   Test_HSV_Colour_Wheel;
   Test_Text;

end Tests;