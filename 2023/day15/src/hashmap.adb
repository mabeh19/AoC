with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with AoC;

package body Hashmap is
   function Hash (S : String) return Integer is
      Sum : Integer := 0;
   begin
      for C of S loop
         Sum := Sum + Character'Pos (C);
         Sum := Sum * 17;
         Sum := Sum mod 256;
      end loop;
      return Sum;
   end Hash;

   function Solve1 (S : String) return Integer is
      Steps : constant AoC.Split_Arr := AoC.Split (S, ",");
      Sum : Integer := 0;
   begin
      for Input of Steps loop
         Sum := Sum + Hash (Ada.Strings.Unbounded.To_String (Input));
      end loop;

      return Sum;
   end Solve1;

   function Solve2 (S : String) return Integer is
      Steps : constant AoC.Split_Arr := AoC.Split (S, ",");
      Sum : Integer := 0;
   begin
      for Step of Steps loop
         Perform_Op (Ada.Strings.Unbounded.To_String (Step));
      end loop;
      
      for Num in Boxes'Range loop
         Sum := Sum + Focusing_Power (Num);
      end loop;

      Put_Line (Boxes'Image);
      return Sum;
   end Solve2;

   procedure Perform_Op (S : String) is
      Label_Idx : Integer := 0;
      Is_Remove : Boolean := False;
   begin
      Label_Idx := Ada.Strings.Fixed.Index (S, "=");
      if Label_Idx = 0 then
         Label_Idx := Ada.Strings.Fixed.Index (S, "-");
         Is_Remove := True;
      end if;
      declare
         Label : constant Lens_Label := B_Label.To_Bounded_String (S (1 .. Label_Idx - 1));
         L_Num : constant String := S (Label_Idx + 1 .. S'Length);
      begin
         if Is_Remove then
            Remove (Label);
         else
            declare
               Lens : Marked_Lens := (Label, Lens_Num (Integer'Value (L_Num)));
            begin
               Insert (Lens);
            end;
         end if;
      end;
   end Perform_Op;

   function Find_Label (Box : Box_Type; Label : Lens_Label) return Integer is
      Label_Idx : Integer := 1;
   begin
      for Ele of Box loop
         if B_Label."=" (Ele.Label, Label) then
            return Label_Idx;
         end if;
         Label_Idx := Label_Idx + 1;
      end loop;
      return -1;
   end Find_Label;

   procedure Remove (Label : Lens_Label) is
      Idx : constant Integer := Hash (B_Label.To_String (Label));
      Box : Box_Type renames Boxes (Idx);
      Label_Idx : Integer := Find_Label (Box, Label);
   begin    
      if Label_Idx /= -1 then
         Box.Delete (Label_Idx);
      end if;
   end Remove;

   procedure Insert (L : Marked_Lens) is
      Idx : constant Integer := Hash (B_Label.To_String (L.Label));
      Box : Box_Type renames Boxes (Idx);
      Label_Idx : Integer := Find_Label (Box, L.Label);
   begin
      if Label_Idx = -1 then
         Box.Append (L);
      else
         Box (Label_Idx) := L;
      end if;
   end Insert;

   function Focusing_Power (Box_Num : Integer) return Integer is
      Sum : Integer := 0;
      Box : Box_Type renames Boxes (Box_Num);
   begin
      for Slot in 1 .. Integer (Box.Length) loop
         Sum := Sum + (1 + Box_Num) * Slot * Integer (Box (Slot).Lens);
      end loop;
      return Sum;
   end Focusing_Power;
end Hashmap;
