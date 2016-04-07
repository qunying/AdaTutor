with Text_IO; use Text_IO;
procedure Add is
  package My_Int_IO is new Integer_IO(Integer); use My_Int_IO;
begin
  Put(2 + 2);
  New_Line;
end Add;
