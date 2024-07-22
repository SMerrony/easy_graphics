--  SPDX-License-Identifier: MIT
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

pragma Ada_2022;

with Ada.Numerics;                      use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

with Easy_Graphics;    use Easy_Graphics;

procedure Tests is

   Dir : constant String := "results/";

   procedure Write_Images (Img : Easy_Image; Prefix : String) is
   begin
      Write_PPM (Img, Dir & Prefix & "_plain.ppm", Plain);
      Write_PPM (Img, Dir & Prefix & "_raw.ppm", Raw);
      Write_PAM (Img, Dir & Prefix & ".pam");
      Write_GIF (Img, Dir & Prefix & ".gif");
   end Write_Images;

   procedure Test_Basics is
      Small_8bit_Img  : Easy_Image := New_Image ((0, 0), (12, 16), BLACK);
   begin
      --  Test small 8-bit image with 'standard coords'
      for X of Xs (Small_8bit_Img) loop
         for Y of Ys (Small_8bit_Img) loop
            Plot (Small_8bit_Img, (X, Y), (if X = Y then RED else WHITE));
         end loop;
      end loop;
      Write_Images (Small_8bit_Img, "small_img");
   end Test_Basics;

   procedure Test_Rects is
      Rects_Img : Easy_Image := New_Image ((-100, -50), (400, 300), BLACK);
   begin
      Rect (Rects_Img, (-20, -25), (100, 100), SILVER, Filled);
      Rect (Rects_Img, (50, 50), (200, 150), MAGENTA, Outline);
      Write_Images (Rects_Img, "rectangles");
   end Test_Rects;

   procedure Test_Triangles is
      Triangles_Img   : Easy_Image := New_Image ((-200, -200), (200, 300), SILVER);
   begin
      Triangle (Triangles_Img, (0, 0), (100, 0), (50, 100), YELLOW, Outline);
      Triangle (Triangles_Img, (-150, 150), (0, 0), (100, 0), BLUE, Filled);
      Triangle (Triangles_Img, (50, 0), (150, 100), (190, -100), RED, Filled);
      Write_Images (Triangles_Img, "triangles");
   end Test_Triangles;

   procedure Test_Circles is
      Circles_Img     : Easy_Image := New_Image ((-200, -200), (200, 200), WHITE);
   begin
      Circle (Circles_Img, (0, 0), 50, BLACK, Outline);
      Circle (Circles_Img, (50, 50), 50, CYAN, Filled);
      --  Test drawing beyond the border...
      Circle (Circles_Img, (180, -180), 30, RED, Filled);
      Write_Images (Circles_Img, "circles");
   end Test_Circles;

   procedure Test_Graphs is
      Graph_8bit_Img  : Easy_Image := New_Image ((-200, -200), (200, 200), WHITE);
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
      Write_Images (Graph_8bit_Img, "graph");
   end Test_Graphs;

   procedure Test_HSV_Colour_Wheel is
      Diameter  : constant Integer := 480;
      Radius    : constant Integer := Diameter / 2;
      Radius_Fl : constant Float   := Float (Radius);
      Wheel_Img : Easy_Image := New_Image ((-Radius, -Radius), (Radius, Radius), BLACK);
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
      Write_PPM (Wheel_Img, Dir & "hsv_colour_wheel.ppm");
   end Test_HSV_Colour_Wheel;

   procedure Test_Text is
      Text_Img : Easy_Image := New_Image ((0, 0), (320, 200), WHITE);
   begin
      Char (Text_Img, '2', (10, 30), 40, 20, BLUE);
      Char (Text_Img, '3', (77, 160), 40, 20, BLUE, Light);
      Text (Text_Img, "1234567,890.00", (0, 110), 32, 16, 5, BLACK);
      Text (Text_Img, "+-*/%'£$=[({})]&@", (45, 12), 20, 10, 5, BLACK);
      Text (Text_Img, "ABCDEFGHIJKLMNOPQRSTUVWXYZ", (0, 80), 16, 8, 4, BLACK);
      Text (Text_Img, "abcdefghijklmnopqrstuvwxyz", (45, 50), 12, 6, 2, BLACK, Light);
      Text (Text_Img, "HEAVY", (150, 150), 16, 8, 4, RED, Heavy);
      Text (Text_Img, "(C)2024 STEPHEN MERRONY", (180, 1), 8, 4, 2, BLACK, Light);
      Write_Images (Text_Img, "text");
   end Test_Text;

   procedure Test_Turtle is
      Turtle_Img : Easy_Image := New_Image ((-300, -300), (300, 300), WHITE);
      Turtle     : Turtle_Rec := New_Turtle (Turtle_Img'Unrestricted_Access);
      Angle      : Degrees := 0;
   begin
      Turtle.Pen_Down;
      while Angle < 360 loop
         Turtle.Turn_To (Angle);
         Turtle.Forward (70);
         Turtle.Left (45);
         Turtle.Forward (100);
         Turtle.Right (90);
         Turtle.Forward (150);
         Turtle.Home;
         Angle := Angle + 15;
      end loop;
      Turtle.Pen_Color (RED);     Turtle.Turn_To (0); Turtle.Forward (275); Turtle.Home;
      Turtle.Pen_Color (GREEN);   Turtle.Right (90);  Turtle.Forward (275); Turtle.Home;
      Turtle.Pen_Color (BLUE);    Turtle.Right (90);  Turtle.Forward (275); Turtle.Home;
      Turtle.Pen_Color (MAGENTA); Turtle.Right (90);  Turtle.Forward (275); Turtle.Home;
      Write_Images (Turtle_Img, "turtle");
   end Test_Turtle;

   procedure Test_Transparent_PAM is
      PAM_Img : Easy_Image := New_Image ((-100, -100), (100, 100), TRANSPARENT);
   begin
      Circle (PAM_Img, (0, 0), 75, CYAN, Filled);
      Write_PAM (PAM_Img, Dir & "transparent_circle.pam");
   end Test_Transparent_PAM;

   procedure Test_Transparent_Logo is
      Logo_Img : Easy_Image := New_Image ((1, 1), (400, 400), TRANSPARENT);
   begin
      Circle (Logo_Img, (200, 200), 199, MAGENTA, Filled);
      Rect   (Logo_Img, (75, 225),  (400, 325), TRANSPARENT, Filled);
      Text   (Logo_Img, "Easy",     (105, 245), 60, 20, 8, CYAN, Heavy);
      Rect   (Logo_Img, (325, 175), (400, 225), TRANSPARENT, Filled);
      Rect   (Logo_Img, (75, 75),   (400, 175), TRANSPARENT, Filled);
      Text   (Logo_Img, "Graphics", (105, 95),  60, 20, 8, CYAN, Heavy);
      Write_Images (Logo_Img, "logo_on_trans");
   end Test_Transparent_Logo;

   procedure Test_Logo is
      Logo_Img : Easy_Image := New_Image ((1, 1), (400, 400), BLACK);
   begin
      Circle (Logo_Img, (200, 200), 199, MAGENTA, Filled);
      Rect   (Logo_Img, (75, 225),  (400, 325), BLACK, Filled);
      Text   (Logo_Img, "Easy",     (105, 245), 60, 20, 8, CYAN, Heavy);
      Rect   (Logo_Img, (325, 175), (400, 225), BLACK, Filled);
      Rect   (Logo_Img, (75, 75),   (400, 175), BLACK, Filled);
      Text   (Logo_Img, "Graphics", (105, 95),  60, 20, 8, CYAN, Heavy);
      Write_Images (Logo_Img, "logo_on_black");
   end Test_Logo;

begin

   Test_Basics;
   Test_Rects;
   Test_Triangles;
   Test_Circles;
   Test_Graphs;
   Test_HSV_Colour_Wheel;
   Test_Text;
   Test_Turtle;
   Test_Transparent_PAM;
   Test_Transparent_Logo;
   Test_Logo;

end Tests;