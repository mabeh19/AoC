with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

package AoC is

   function Read_File (Path : String) return String;

   type Split_Arr is array (Positive range <>) of Unbounded_String;
   subtype Lines is Split_Arr;
   function Get_Lines (S : String) return Lines;
  
   function Split (S : String; Pred : String) return Split_Arr;

   function Count_Mismatches (S1, S2 : String) return Integer;

end AoC;
