with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Strings.Search;
use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Numerics.Elementary_Functions;

package body Race is

   function Parse_Input(Input : File_Type) return Races is
      R : Races := (others => (others => 0));
      L1 : constant String := Get_Line (Input);
      L2 : constant String := Get_Line (Input);
      Times       : String := Trim(L1(6..L1'Last), Ada.Strings.Left);
      Distances   : String := Trim(L2(10..L2'Last), Ada.Strings.Left);
      T_Offset    : Integer := 1;
      T_Idx       : Integer := 1;
      D_Offset    : Integer := 1;
      D_Idx       : Integer := 1;
      Nr          : Integer := 1;
   begin
      loop
         T_Idx := Ada.Strings.Search.Index (Times, " ");
         D_Idx := Ada.Strings.Search.Index (Distances, " ");
         exit when T_Idx <= 1 or D_Idx <= 1;
         R(Nr) := (
            Time => Long_Long_Integer'Value(Times(1..T_Idx - 1)),
            Record_Distance => Long_Long_Integer'Value(Distances(1..D_Idx - 1))
         );

         Nr := Nr + 1;
         T_Offset := T_Idx;
         D_Offset := D_Idx;
         Delete (Times, 1, T_Idx);
         Delete (Distances, 1, D_Idx);
         Trim (Times, Ada.Strings.Left);
         Trim (Distances, Ada.Strings.Left);
      end loop;
      return R;
   end Parse_Input;
 
   function Get_Ways_To_Beat_Records (Rs : Races) return Solution1 is
      Sol1 : Solution1;
      Idx : Integer := 1;
   begin
      for R of Rs loop 
         Sol1 (Idx) := Sim_Race (R);
         Idx := Idx + 1;
      end loop;
      return Sol1;
   end Get_Ways_To_Beat_Records;
   
   function Sim_Race (R : Race) return Win_Range is
      --
      --    All solutions can be found via 2nd degree polynomial
      --    where
      --    ax^2 + bx + c = 0 => 1 * ht^2 - tt * ht + dist = 0
      --
      --    By solving for the record distance, we get the range wherein 
      --    we beat the distance
      a : constant Float := 1.0;
      b : constant Float := Float (-R.Time);
      c : constant Float := Float (R.Record_Distance);
      d : constant Float := Ada.Numerics.Elementary_Functions.Sqrt (b ** 2 - 4.0 * a * c);
      Low   : constant Float := (-b - d) / 2.0 * a;
      High  : constant Float := (-b + d) / 2.0 * a;
      Low_Rounded : constant Float := Float'Ceiling(Low);
      High_Rounded : constant Float := Float'Floor(High);
      Ways : constant Win_Range := (
         First => Long_Long_Integer ((if Low_Rounded  > Low  then Low_Rounded else Low_Rounded + 1.0)),
         Last  => Long_Long_Integer ((if High_Rounded < High then High_Rounded  else High_Rounded - 1.0))
      );
   begin
      return Ways;
   end Sim_Race;
end Race;
