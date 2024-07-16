--  SPDX-License-Identifier: MIT
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO;

with Easy_Graphics;

procedure sierpinski_arrowhead is

   use Ada.Command_Line, Ada.Text_IO, Easy_Graphics;

   type    Order_Type is range 0 .. 8;

   Length : constant Integer    := 450;

   Order     : Order_Type;
   Img       : Image_8 := New_Image ((1, 1), (800, 800), BLACK);
   Turtle    : Turtle_Rec := New_Turtle (Img'Unrestricted_Access);

   procedure Curve (Order : Order_Type; Length : Integer; Angle : Degrees) is
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
      Put_Line ("sierpinski_arrowhead <order>");
      Put_Line ("  <order>   0 .. 7");
      Put_Line ("open sierpinski_arrowhead_curve.gif to view ouput");
      return;
   end if;
   Order := Order_Type'Value (Argument (1));

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
