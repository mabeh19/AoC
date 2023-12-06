with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;

generic
   Num_Races : Integer;
package Race is

   type Winning_Race is record
      Time_Held         : Long_Long_Integer;
      Time_Travelling   : Long_Long_Integer;
      Distance          : Long_Long_Integer;
   end record;

   type Win_Range is record
      First : Long_Long_Integer;
      Last  : Long_Long_Integer;
   end record;

   type Race is record
      Time              : Long_Long_Integer;
      Record_Distance   : Long_Long_Integer;
   end record;

   type Solution1 is array (1..Num_Races) of Win_Range;
   type Races is array (1..Num_Races) of Race;

   function Parse_Input (Input : File_Type) return Races;
   function Get_Ways_To_Beat_Records (Rs : Races) return Solution1;
   function Sim_Race (R : Race) return Win_Range;

end Race;
