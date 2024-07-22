--  SPDX-License-Identifier: MIT
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

pragma Ada_2022;
with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Text_IO;       use Ada.Text_IO;

with Easy_Graphics;     use Easy_Graphics;

procedure sierpinski_arrowhead is

   Length : constant Integer := 450;
   Order  : Natural;
   Img    : Easy_Image    := New_Image ((1, 1), (800, 600), BLACK);
   Turtle : Turtle_Rec := New_Turtle (Img'Unrestricted_Access);

   procedure Curve (Order : Natural; Length : Integer; Angle : Degrees) is
   begin
      if Order = 0 then
         Turtle.Forward (Length);
      else
         Curve (Order - 1, Length / 2, -Angle);
         Turtle.Right (Angle);
         Curve (Order - 1, Length / 2,  Angle);
         Turtle.Right (Angle);
         Curve (Order - 1, Length / 2, -Angle);
      end if;
   end Curve;

begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: sierpinski_arrowhead <order>");
      Put_Line ("Where: <order>  is 0 .. 8");
      Put_Line ("N.B. Open sierpinski_arrowhead_curve.gif to view ouput");
      return;
   end if;
   Order := Natural'Value (Argument (1));

   Turtle.Pen_Color (MAGENTA);
   Turtle.Go_To ((400, 1));
   Turtle.Pen_Down;

   if Order mod 2 = 0 then
      Curve (Order, Length, 60);
   else
      Turtle.Turn_To (60);
      Curve (Order, Length, -60);
   end if;

   Write_GIF (Img, "sierpinski_arrowhead_curve.gif");
end sierpinski_arrowhead;
