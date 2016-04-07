with Text_IO; use Text_IO;
procedure Fibtest is
  Passed : Boolean := True;
  function Fib(N : in Positive) return Positive is separate;
  procedure Compare (N : in Positive; Right_Answer : in Positive) is
    package Int_IO is new Integer_IO(Integer); use Int_IO;
    My_Answer : Positive := Fib(N);
  begin
    if My_Answer /= Right_Answer then
      Put("N:");  Put(N);
      Put("          My answer:");  Put(My_Answer);
      Put("          Right answer:");  Put(Right_Answer);
      New_Line;
      Passed := False;
    end if;
  end Compare;
begin
  Compare(1, 1);
  Compare(2, 1);
  Compare(3, 2);
  Compare(4, 3);
  Compare(5, 5);
  Compare(6, 8);
  Compare(7, 13);
  Compare(10, 55);
  Compare(15, 610);
  Compare(20, 6765);
  if Passed then
    Put_Line("Congratulations, you completed the assignment!");
  end if;
end Fibtest;
