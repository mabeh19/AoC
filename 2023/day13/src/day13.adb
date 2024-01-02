with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with AoC;

procedure Day13 is

   type Int is new Long_Long_Integer;
   type Reflection_Type is (None, Vertical, Horizontal);

   function Summarize (Reflection : Reflection_Type; Idx : Int) return Int is (
      case Reflection is
               when Vertical     => Idx,
               when Horizontal   => Int (100) * Idx,
               when None         => 0
      );

   procedure Get_Reflection (Orig_Pattern : String; RT : out Reflection_Type; Idx : out Int) is

      Pattern     : constant AoC.Lines := AoC.Get_Lines (Orig_Pattern);
      Line_Length : constant Natural   := Length (Pattern (1));
      Max_Dim     : constant Integer   := Integer'Max (Line_Length, Pattern'Length);

      function Is_Match (Lines : AoC.Lines; Idx1, Idx2 : Int; RT : Reflection_Type) return Boolean is
         Allowed_Retries : constant Integer := 1;
      begin
         if RT = Horizontal then
            declare
               Idx2_Dist_To_Edge : constant Int := Int (Lines'Length) - Idx2;
               Closest_To_Edge   : constant Int := Int'Min (Idx1, Idx2_Dist_To_Edge);
               Total_Mismatches  : Integer      := 0;
            begin
               for I in 0 .. Closest_To_Edge loop
                  exit when Idx1 - I = 0 or else Idx2 + I > Int (Lines'Length);
                  declare
                     S1          : constant String    := To_String (Lines (Integer (Idx1 - I)));
                     S2          : constant String    := To_String (Lines (Integer (Idx2 + I)));
                     Mismatches  : constant Integer   := AoC.Count_Mismatches (S1, S2);
                  begin
                     Total_Mismatches := Total_Mismatches + Mismatches;
                  end;
               end loop;

               return Total_Mismatches = Allowed_Retries;
            end;
         elsif RT = Vertical then
            declare
               Idx2_Dist_To_Edge : constant Int := Int (Line_Length) - Idx2;
               Closest_To_Edge   : constant Int := Int'Min (Idx, Idx2_Dist_To_Edge);
               Total_Mismatches  : Integer      := 0;
            begin
               for I in 0 .. Closest_To_Edge loop
                  exit when Idx1 - I = 0 or else Idx2 + I > Int (Line_Length);
                  declare
                     S1          : constant String    := [for L of Lines => Element (L, Integer (Idx1 - I))];
                     S2          : constant String    := [for L of Lines => Element (L, Integer (Idx2 + I))];
                     Mismatches  : constant Integer   := AoC.Count_Mismatches (S1, S2);
                  begin
                     Total_Mismatches := Total_Mismatches + Mismatches;
                  end;
               end loop;

               return Total_Mismatches = Allowed_Retries;
            end;
         end if;
         return False;
      end Is_Match;

   begin
      for I  in 1 .. Int (Pattern'Length) loop
         if Is_Match (Pattern, I, I + 1, Horizontal) then
            RT := Horizontal;
            Idx := I;
            return;
         end if;
      end loop;
      for I in 1 .. Int (Line_Length) loop
         if Is_Match (Pattern, I, I + 1, Vertical) then
            RT := Vertical;
            Idx := I;
            return;
         end if;
      end loop;
      RT := None;
      Idx := -1;
   end Get_Reflection;

   Test_Text   : constant String := AoC.Read_File ("test.txt");
   Test1       : constant AoC.Split_Arr := AoC.Split (Test_Text, LF & LF);
   Test_Ans1_RT   : Reflection_Type;
   Test_Ans1_Idx  : Int;
   Test_Sum       : Int := 0;

   Input_Text  : constant String := AoC.Read_File ("input.txt");
   Patterns    : constant AoC.Split_Arr := AoC.Split (Input_Text, LF & LF);
   Ans1_RT     : Reflection_Type;
   Ans1_Idx    : Int;
   Ans1_Sum    : Int := 0;
begin

   for Pattern of Test1 loop
      Get_Reflection (To_String (Pattern), Test_Ans1_RT, Test_Ans1_Idx);
      Test_Sum := Test_Sum + Summarize (Test_Ans1_RT, Test_Ans1_Idx);
   end loop;

   Put_Line ("Ans1: " & Test_Sum'Image);

   for Pattern of Patterns loop
      Get_Reflection (To_String (Pattern), Ans1_RT, Ans1_Idx);
      Ans1_Sum := Ans1_Sum + Summarize (Ans1_RT, Ans1_Idx);
   end loop;

   Put_Line ("Ans1: " & Ans1_Sum'Image);

end Day13;
