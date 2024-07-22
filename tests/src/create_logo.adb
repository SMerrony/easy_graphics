--  SPDX-License-Identifier: MIT
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

pragma Ada_2022;
with Easy_Graphics;    use Easy_Graphics;

procedure Create_Logo is
   Logo_Img : Easy_Image := New_Image ((1, 1), (200, 200), TRANSPARENT);
begin
   Circle (Logo_Img, (100, 100), 99, MAGENTA, Filled);
   Rect   (Logo_Img, (33, 113),  (200, 163), TRANSPARENT, Filled);
   Text   (Logo_Img, "Easy",     (44, 123), 30, 10, 5, CYAN, Heavy);
   Rect   (Logo_Img, (155, 88),  (200, 113), TRANSPARENT, Filled);
   Rect   (Logo_Img, (33, 36),   (200, 88), TRANSPARENT, Filled);
   Text   (Logo_Img, "Graphics", (44, 46),  30, 10, 5, CYAN, Heavy);
   Write_GIF (Logo_Img, "results/logo.gif");
end Create_Logo;
