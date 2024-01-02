with Ada.Containers.Vectors;
with Ada.Strings.Bounded;

package Hashmap is
    
   function Hash (S : String) return Integer;
   function Solve1 (S : String) return Integer;
   function Solve2 (S : String) return Integer;

private
   package B_Label is new Ada.Strings.Bounded.Generic_Bounded_Length (max => 20);
   subtype Lens_Label is B_Label.Bounded_String;
   type Lens_Num is new Integer range 1 .. 9;
   type Marked_Lens is record
      Label : Lens_Label;
      Lens  : Lens_Num;
   end record;
   package Box_Vector is new Ada.Containers.Vectors
        (Index_Type   => Positive,
         Element_Type => Marked_Lens);
   subtype Box_Type is Box_Vector.Vector;
   
   procedure Perform_Op (S : String);
   procedure Remove (Label : Lens_Label);
   procedure Insert (L : Marked_Lens);
   function Focusing_Power (Box_Num : Integer) return Integer;


   Boxes : array (0 .. 255) of Box_Type;

end Hashmap;
