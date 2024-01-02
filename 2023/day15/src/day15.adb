with Ada.Text_IO; use Ada.Text_IO;
with Inp;
with Hashmap;

procedure Day15 is

   Test_String : constant String := "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7";
   Test_Ans1   : constant Integer := Hashmap.Solve1 (Test_String);
   Ans1        : constant Integer := Hashmap.Solve1 (Inp.Real_Input);

   --Test_Ans2   : constant Integer := Hashmap.Solve2 (Test_String);
   Ans2   : constant Integer := Hashmap.Solve2 (Inp.Real_Input);
begin
   Put_Line ("Sum: " & Test_Ans1'Image);
   Put_Line ("Sum: " & Ans1'Image);
   --Put_Line ("Sum: " & Test_Ans2'Image);
   Put_Line ("Sum : " & Ans2'Image);
end Day15;
