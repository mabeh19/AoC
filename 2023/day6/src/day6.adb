with Ada.Text_IO; use Ada.Text_IO;
with Race;

procedure Day6 is
   
   package TR is new Race (
      Num_Races => 3
   );

   package TR_Real is new Race (
      Num_Races => 4
   );

   Test_File_Name : constant String := "test.txt";
   Inp_File_Name  : constant String := "input.txt";

   Test_File : File_Type;
   Inp_File  : File_Type;

   Test_Races  : TR.Races;
   Test_Sol1   : TR.Solution1;
   Test_Sol2   : TR.Win_Range;

   Inp_Races   : TR_Real.Races;
   Sol1        : TR_Real.Solution1;
   Sol2        : TR_Real.Win_Range;

begin
   Open (Test_File, In_File, Test_File_Name);
   Test_Races := TR.Parse_Input (Test_File);
   Close (Test_File);
   Open (Inp_File, In_File, Inp_File_Name);
   Inp_Races := TR_Real.Parse_Input (Inp_File);
   Close (Inp_File);
   Test_Sol1 := TR.Get_Ways_To_Beat_Records (Test_Races);
   Sol1 := TR_Real.Get_Ways_To_Beat_Records (Inp_Races);
   Test_Sol2 := TR.Sim_Race (Test_Races (1));
   Sol2 := TR_Real.Sim_Race (Inp_Races (1));


   declare
      Prod : Long_Long_Integer := 1; 
   begin
      for Ways of Sol1 loop
         Prod := Prod * (1 + Ways.Last - Ways.First);
      end loop;
      Put_Line ("Ans1: " & Prod'Image);
   end;

   declare
      Ways : constant Long_Long_Integer := 1 + Test_Sol2.Last - Test_Sol2.First;
   begin
      Put_Line ("TestAns2: " & Ways'Image);
   end;

   declare
      Ways : constant Long_Long_Integer := Sol2.Last - Sol2.First;
   begin
      Put_Line ("Ans2: " & Ways'Image);
   end;

end Day6;
