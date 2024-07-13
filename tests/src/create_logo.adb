--  SPDX-License-Identifier: MIT
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

pragma Ada_2022;
with Easy_Graphics;    use Easy_Graphics;

procedure Create_Logo is
   Logo_Img : Image_8 := New_Image ((1, 1), (400, 400), TRANSPARENT);
begin
   Circle (Logo_Img, (200, 200), 199, MAGENTA, Filled);
   Rect   (Logo_Img, (75, 225),  (400, 325), TRANSPARENT, Filled);
   Text   (Logo_Img, "Easy",     (105, 245), 60, 20, 8, CYAN, Heavy);
   Rect   (Logo_Img, (325, 175), (400, 225), TRANSPARENT, Filled);
   Rect   (Logo_Img, (75, 75),   (400, 175), TRANSPARENT, Filled);
   Text   (Logo_Img, "Graphics", (105, 95),  60, 20, 8, CYAN, Heavy);
   Write_GIF (Logo_Img, "logo.gif");
end Create_Logo;
