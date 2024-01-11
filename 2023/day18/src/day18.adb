with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Containers.Vectors;
with Ada.Containers.Generic_Array_Sort;
with AoC;

procedure Day18 is

   subtype LL is Long_Long_Integer;

   type Direction is (Up, Down, Left, Right);
   subtype Horizontal is Direction range Left .. Right;
   subtype Vertical is Direction range Up .. Down;

   type Corner_Type is (
       Dead_End   -- F7 || LJ
      ,Side       -- FJ || L7
      );

   type Trench is record
      Dir : Direction;
      Len : Integer;
      Color : Integer;
   end record;

   type Point is record
      X : Integer;
      Y : Integer;
   end record;

   type Func_Line is record
      First    : Point;
      Last     : Point;
      Length   : Integer;
      Corner   : Corner_Type;
      Line_Direction : Direction;
   end record;

   function "<" (Left, Right: Func_Line) return Boolean is
   begin
      return Integer'Min (Left.First.X, Left.Last.X) < Integer'Min (Right.First.X, RIght.Last.X);
   end "<";

   type Dig_Plan is array (Positive range <>) of Trench;
   type Func_Lines is array (Positive range <>) of Func_Line;

   function Opposite_Direction (Dir : Direction) return Direction is
   begin
      return (case Dir is
               when Up => Down,
               when Down => Up,
               when Left => Right,
               when Right => Left);
   end Opposite_Direction;

   function Get_Corner_Type (C_Dir, L_Dir, N_Dir : Direction) return Corner_Type is
   begin
      if Opposite_Direction (N_Dir) = L_Dir then
         return Dead_End;
      end if;

      return Side;
   end Get_Corner_Type;

   function Get_Dist_To_Next_Func (P : Point; Funcs : Func_Lines) return Integer is
      Closest_X : Integer := Integer'Last;
      Func : Func_Line;
   begin
      Func := Funcs (Funcs'First);
      Closest_X := Integer'Min (Func.Last.X, Integer'Min (Closest_X, Func.First.X));

      return Closest_X - P.X;
   end Get_Dist_To_Next_Func;

   function Get_Overlapping_Line (P : Point; Funcs : Func_Lines) return access Func_Line is
      Overlapping : access Func_Line := null;
      Func : Func_Line;
   begin
      Func := Funcs (Funcs'First);
      --  We're not interested in corners of vertical lines
      if Func.Line_Direction in Vertical then
         if P.X = Func.First.X then
            if (P.Y > Func.First.Y and then P.Y < Func.Last.Y) or else
               (P.Y > Func.Last.Y  and then P.Y < Func.First.Y) then
               Overlapping := new Func_Line'(Func);
            end if;
         end if;
      else 
         if P.Y = Func.First.Y then
            if (P.X >= Func.First.X and then P.X <= Func.Last.X) or else
               (P.X >= Func.Last.X  and then P.X <= Func.First.X) then
               Overlapping := new Func_Line'(Func);
            end if;
         end if;
      end if;

      return Overlapping;
   end Get_Overlapping_Line;

   function Get_Potential_Overlapping_Lines (P : Point; Funcs : Func_Lines) return Func_Lines is
      package Sub_Func_Lines is new Ada.Containers.Vectors (
         Index_Type => Positive, 
         Element_Type => Func_Line
      );

      use Sub_Func_Lines;

      Overlapping_V : Vector;
   begin
      for Func of Funcs loop
         if Func.Line_Direction in Vertical then
            if (Func.First.Y < P.Y and then Func.Last.Y > P.Y) or else
               (Func.Last.Y < P.Y and then Func.First.Y > P.Y) then
               if Func.First.X > P.X then
                  Overlapping_V.Append (Func);
               end if;
            end if;
         else
            if Func.First.Y = P.Y then
               if Func.First.X > P.X then
                  Overlapping_V.Append (Func);
               end if;
            end if;
         end if;
      end loop;

      if Integer (Overlapping_V.Length) = 0 then
         declare
            Empty_Array : Func_Lines (1 .. 1) := (others => Func_Line'(
               Length   => 0,
               First    => Point'(-100000, -100000),
               Last     => Point'(-100000, -100000),
               Corner   => Dead_End,
               Line_Direction => Right
            ));
         begin
            return Empty_Array;
         end;
      end if;

      declare
         Funcs : Func_Lines (1 .. Positive (Overlapping_V.Length));
      begin
         for Idx in Funcs'Range loop
            Funcs (Idx) := Overlapping_V (Idx);
         end loop;

         return Funcs;
      end;
   end Get_Potential_Overlapping_Lines;

   procedure Sort_Funcs is new Ada.Containers.Generic_Array_Sort (
      Index_Type     => Positive, 
      Element_Type   => Func_Line,
      Array_Type     => Func_Lines);

   function Solve1 (Plan : Dig_Plan) return LL is
      Funcs : Func_Lines (Plan'Range);
      Pos : Point := (others => 0);
      Total : LL := 0;
      Pln_Idx : Integer := 1;
      Last_Dir : Direction := Plan (Plan'Last).Dir;
      Next_Dir : Direction;
      Upper_Left  : Point :=  (others => 0);
      Lower_Right : Point :=  (others => 0);
      Skip_Empty : LL := 0;
      Grouped : LL := 0;
   begin
      for T of Plan loop
         Next_Dir := Plan (Pln_Idx mod Plan'Length + 1).Dir;
         Funcs (Pln_Idx) := (
            First => Pos,
            Last  => (case T.Dir is
                        when Up     => (X => Pos.X,         Y => Pos.Y - T.Len),
                        when Down   => (X => Pos.X,         Y => Pos.Y + T.Len),
                        when Right  => (X => Pos.X + T.Len, Y => Pos.Y),
                        when Left   => (X => Pos.X - T.Len, Y => Pos.Y)),
            Corner => Get_Corner_Type (T.Dir, Last_Dir, Next_Dir),
            Line_Direction => T.Dir,
            Length => T.Len
         );
         declare
            Pl : Func_Line := Funcs (Pln_Idx);
         begin
            Upper_Left.X := Integer'Min (Upper_Left.X, Pl.First.X);
            Upper_Left.Y := Integer'Min (Upper_Left.Y, Pl.First.Y);
            Upper_Left.X := Integer'Min (Upper_Left.X, Pl.Last.X);
            Upper_Left.Y := Integer'Min (Upper_Left.Y, Pl.Last.Y);
            Lower_Right.X := Integer'Max (Lower_Right.X, Pl.First.X);
            Lower_Right.Y := Integer'Max (Lower_Right.Y, Pl.First.Y);
            Lower_Right.X := Integer'Max (Lower_Right.X, Pl.Last.X);
            Lower_Right.Y := Integer'Max (Lower_Right.Y, Pl.Last.Y);
         end;
         Pln_Idx := Pln_Idx + 1;
         Last_Dir := T.Dir;
         for I in 1 .. T.Len loop
            case T.Dir is
               when Up     => Pos.Y := Pos.Y - 1;
               when Down   => Pos.Y := Pos.Y + 1;
               when Left   => Pos.X := Pos.X - 1;
               when Right  => Pos.X := Pos.X + 1;
            end case;
         end loop;
      end loop;

      Sort_Funcs (Funcs);

      for I in Upper_Left.Y .. Lower_Right.Y loop
         declare
            Pot_Overlap : Func_Lines := Get_Potential_Overlapping_Lines (Point'(X => Upper_Left.X - 1, Y => I), Funcs);
            Line_Hit_Count : Integer := 0;
            Odd : Boolean := False;
            C_Group  : Integer := 0;
            Skip_N : Integer := 0;
            J : Integer := Upper_Left.X - 1;
         begin
            loop
               exit when J > Lower_Right.X + 1 or else Line_Hit_Count = Pot_Overlap'Last;
               declare
                  P : constant Point := (X => J, Y => I);
                  Dist_To_Next_Func : constant Integer := Get_Dist_To_Next_Func (P, Pot_Overlap (Line_Hit_Count + 1 .. Pot_Overlap'Last));
                  Overlapping_Func : constant access Func_Line := Get_Overlapping_Line (P, Pot_Overlap (Line_Hit_Count + 1 .. Pot_Overlap'Last));
               begin
                  exit when Dist_To_Next_Func = -1;
                  if Overlapping_Func /= null then
                     Line_Hit_Count := Line_Hit_Count + 1;
                     if Odd then
                        Total := Total + LL (C_Group);
                     end if;
                     C_Group := 0;
                     if Overlapping_Func.Line_Direction in Vertical then
                        Total := Total + 1;
                        Odd := not Odd;
                     else
                        Total := Total + LL (Overlapping_Func.Length) + 1;
                        J := J + Overlapping_Func.Length;
                        if Overlapping_Func.Corner = Dead_End then
                           null;
                        else
                           Odd := not Odd;
                        end if;
                     end if;
                  else
                     C_Group := Dist_To_Next_Func;
                     J := J + Dist_To_Next_Func - 1;
                  end if;
               end;
               J := J + 1;
            end loop;
         end;
      end loop;

      return Total;
   end Solve1;

   function Solve2 (Plan : Dig_Plan) return LL is
      type Num_Dir is mod 4;
      function Num_To_Dir (N : Num_Dir) return Direction is
      begin
         return (case N is
                  when 0 => Right,
                  when 1 => Down,
                  when 2 => Left,
                  when 3 => Up);
      end Num_To_Dir;
      New_Plan : Dig_Plan := Plan;
   begin
      for Idx in Plan'Range loop
         New_Plan (Idx) := Trench'(
            Dir => Num_To_Dir (Num_Dir (Plan (Idx).Color mod 4)),
            Len => Plan (Idx).Color / 2 ** 4,
            Color => Plan (Idx).Color
         );
      end loop;
      return Solve1 (New_Plan);
   end Solve2;

   function Parse (S : String) return Dig_Plan is
      Lines : constant AoC.Lines := AoC.Get_Lines (S);
      Plan : Dig_Plan (Lines'Range);
   begin
      for Idx in Lines'Range loop
         declare
            Line : constant String := To_String (Lines (Idx));
            S_L : constant AoC.Split_Arr := AoC.Split (Line, " ");
            S1 : constant String := To_String (S_L (1));
            S3 : constant String := To_String (S_L (3));
            T : Trench := (
               Dir => (if S1 = "U" then Up elsif
                           S1 = "D" then Down elsif
                           S1 = "L" then Left elsif
                           S1 = "R" then Right else Up),
               Len => Integer'Value (To_String (S_L (2))),
               Color => Integer'Value ("16#" & S3 (3 .. Ada.Strings.Fixed.Index (S3, ")") - 1) & "#")
            );
         begin
            Plan (Idx) := T;
         end;
      end loop;
      return Plan;
   end Parse;

   Test_Inp : constant String := AoC.Read_File ("test.txt");
   Test_Pln : constant Dig_Plan := Parse (Test_Inp);
   Test_Ans1 : constant LL := Solve1 (Test_Pln);
   Test_Ans2 : LL;

   Real_Inp : constant String := AoC.Read_File ("input.txt");
   Real_Pln : constant Dig_Plan := Parse (Real_Inp);
   Real_Ans1 : constant LL := Solve1 (Real_Pln);
   Real_Ans2 : constant LL := Solve2 (Real_Pln);
begin
   Put_Line ("Test Ans1 = " & Test_Ans1'Image);
   Put_Line ("Real Ans1 = " & Real_Ans1'Image);
   Test_Ans2 := Solve2 (Test_Pln);
   Put_Line ("Test Ans2 = " & Test_Ans2'Image);
   Put_Line ("Real Ans2 = " & Real_Ans2'Image);
end Day18;
