with Text_IO; use Text_IO;
procedure Nextdate is
  type Month_Type is
         (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec);
  subtype Day_Subtype is Integer range 1 .. 31;
  type Date is
    record
      Day   : Day_Subtype;
      Month : Month_Type;
      Year  : Positive;
    end record;
  Passed : Boolean := True;
  function Tomorrow(Today : in Date) return Date is separate;

  procedure Display (S : in String; D : in Date) is
    package Int_IO is new Integer_IO(Integer); use Int_IO;
  begin
    Put(S);
    Put(D.Day, Width => 3);
    Put(" " & Month_Type'Image(D.Month));
    Put(D.Year, Width => 5);
    New_Line;
  end Display;

  procedure Compare(Today, Right_Answer : in Date) is
    My_Answer : Date := Tomorrow(Today);
  begin
    if My_Answer /= Right_Answer then
      Display("Today:       ", Today);
      Display("My answer:   ", My_Answer);
      Display("Right answer:", Right_Answer);
      New_Line;
      Passed := False;
    end if;
  end Compare;
begin
  Compare((12,Dec,1815), (13,Dec,1815)); -- ordinary date
  Compare(( 3,Feb,1986), ( 4,Feb,1986)); -- ordinary date in Feb.
  Compare((30,Jun,1981), ( 1,Jul,1981)); -- last day of 30-day month
  Compare((30,Sep,3999), ( 1,Oct,3999)); -- last day of 30-day month
  Compare((31,Mar,1876), ( 1,Apr,1876)); -- last day of 31-day month
  Compare((31,Aug,1984), ( 1,Sep,1984)); -- last day of 31-day month
  Compare((31,Dec,1966), ( 1,Jan,1967)); -- last day of year
  Compare((28,Feb,1980), (29,Feb,1980)); -- leap year
  Compare((28,Feb,1600), (29,Feb,1600)); -- century leap year
  Compare((28,Feb,2200), ( 1,Mar,2200)); -- century non-leap year
  Compare((28,Feb,1982), ( 1,Mar,1982)); -- non-leap year
  Compare((29,Feb,1980), ( 1,Mar,1980)); -- leap day in leap year
  if Passed then
    Put_Line("Congratulations, you completed the assignment!");
  end if;
end Nextdate;
