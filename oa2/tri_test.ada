with Text_IO; use Text_IO;
procedure Tritest is
  Passed : Boolean := True;
  type Triangle is (Equilateral, Isosceles, Scalene, Not_A_Triangle);
  function Tritype(Len1, Len2, Len3 : in Integer) return Triangle
                                                          is separate;
  procedure Compare(A, B, C: in Integer; Right_Answer : in Triangle)
                                                          is separate;
begin
  Compare( 3,  4,  5, Scalene);
  Compare( 6,  3,  4, Scalene);
  Compare( 4,  3,  6, Scalene);
  Compare( 3,  3,  3, Equilateral);
  Compare( 3,  3,  4, Isosceles);
  Compare( 3,  4,  3, Isosceles);
  Compare( 4,  3,  3, Isosceles);
  Compare( 7,  7,  4, Isosceles);
  Compare( 7,  4,  7, Isosceles);
  Compare( 4,  7,  7, Isosceles);
  Compare( 1,  1,  1, Equilateral);
  Compare( 0,  4,  4, Not_A_Triangle);
  Compare( 4,  0,  4, Not_A_Triangle);
  Compare( 4,  4,  0, Not_A_Triangle);
  Compare( 0,  4,  3, Not_A_Triangle);
  Compare( 3,  0,  4, Not_A_Triangle);
  Compare( 4,  3,  0, Not_A_Triangle);
  Compare(-1,  4,  4, Not_A_Triangle);
  Compare( 4, -1,  4, Not_A_Triangle);
  Compare( 4,  4, -1, Not_A_Triangle);
  Compare(-1,  4,  3, Not_A_Triangle);
  Compare( 3, -1,  4, Not_A_Triangle);
  Compare( 4,  3, -1, Not_A_Triangle);
  Compare( 2,  4,  6, Not_A_Triangle);
  Compare( 1,  3,  2, Not_A_Triangle);
  Compare( 3,  1,  2, Not_A_Triangle);
  Compare( 1,  2,  4, Not_A_Triangle);
  Compare( 1,  4,  2, Not_A_Triangle);
  Compare( 4,  1,  2, Not_A_Triangle);
  Compare( 0,  0,  0, Not_A_Triangle);
  Compare( 0,  0,  4, Not_A_Triangle);
  Compare( 0,  4,  0, Not_A_Triangle);
  Compare( 4,  0,  0, Not_A_Triangle);
  Compare( 3,  3,  7, Not_A_Triangle);
  Compare( 3,  7,  3, Not_A_Triangle);
  Compare( 6,  3,  3, Not_A_Triangle);
  Compare(-3, -4, -5, Not_A_Triangle);
  if Passed then
    Put_Line("Congratulations, you completed the assignment!");
  end if;
end Tritest;

separate (Tritest)
procedure Compare(A, B, C: in Integer; Right_Answer : in Triangle) is
  package Int_IO is new Integer_IO(Integer); use Int_IO;
  package Tri_IO is new Enumeration_IO(Triangle); use Tri_IO;
  My_Answer : Triangle := Tritype(A, B, C);
begin
  if My_Answer /= Right_Answer then
    Put("Sides:");
    Put(A, Width => 3);
    Put(B, Width => 3);
    Put(C, Width => 3);
    Put("   My answer: ");
    Put(My_Answer, Width => 14);
    Put("   Right answer: ");
    Put(Right_Answer);
    New_Line;
    Passed := False;
  end if;
end Compare;
