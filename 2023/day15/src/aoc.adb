with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;
with Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
with Ada.Directories;

package body AoC is

   function Read_File (Path : String) return String is
      File : File_Type;
      Input : Stream_Access;
   begin
      Open (File, In_File, Path);
      Input := Stream (File);
      declare
         Len : Integer := Integer (Ada.Directories.Size (Path));
         S  : String (1 .. Len);
         C : Character;
         Idx : Integer := 1;
      begin
         while not End_Of_File (File) loop
            Character'Read (Input, C);
            S (Idx) := C;
            Idx := Idx + 1;
         end loop;
         Close (File);
         return S;
      end;
   end Read_File;

   function Get_Lines (S : String) return Lines is
   begin
      return Split (S, "" & Character'Val (10));
   end Get_Lines;

   function Split (S : String; Pred : String) return Split_Arr is
      Num_Lines : constant Integer := Ada.Strings.Fixed.Count (S, Pred) + 1;
      S_Copy   : String := S;

      Arr_Idx  : Integer := 1;
      S_Idx    : Integer := 1;
   begin
      declare
         Arr : Lines (1 .. Num_Lines);
      begin
         loop
            S_Idx := Ada.Strings.Fixed.Index (S_Copy, Pred);
            exit when S_Idx = 0;
            Arr (Arr_Idx) := To_Unbounded_String (S_Copy (1 .. S_Idx - 1));
            Ada.Strings.Fixed.Delete (S_Copy, 1, S_Idx + Pred'Length - 1);
            Arr_Idx := Arr_Idx + 1;
         end loop;
         declare
            S_UB : Unbounded_String := To_Unbounded_String (S_Copy);
         begin
            Trim (S_UB, Right);
            Arr (Arr_Idx) := S_UB;
         end;
         return Arr;
      end;
   end Split;

   function Count_Mismatches (S1, S2 : String) return Integer is
      Count : Integer := 0;
      Shortest : constant Integer := Integer'Min (S1'Length, S2'Length);
   begin
      for Idx in 1 .. Shortest loop
         if S1 (Idx) /= S2 (Idx) then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Count_Mismatches;

end AoC;
