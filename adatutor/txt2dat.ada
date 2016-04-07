-- TXT2DAT.ADA   Ver. 4.01  2001-SEP-10   Copyright 1988-2001n J. Herro
--
-- SOFTWARE INNOVATIONS TECHNOLOGY          www.adatutor.com
-- 1083 MANDARIN DR NE
-- PALM BAY FL 32905-4706                   john@adatutor.com
--
-- (321) 951-0233
--
-- After running DAT2TXT on a PC and transferring the resulting TUTOR.TXT
-- file to another computer, compile and run this program on the other
-- computer to create ADATUTOR.DAT on that machine.
--
-- This code is written in Ada 83 and will compile on Ada 83 and Ada 95
-- compilers.
--

with Direct_IO, Text_IO;
procedure TXT2DAT is
  subtype Block_Subtype is String(1 .. 64);
  package Random_IO is new Direct_IO(Block_Subtype);
  Text_File  : Text_IO.File_Type;                            -- The input file.
  Data_File  : Random_IO.File_Type;                         -- The output file.
  OK         : Boolean := True;      -- True when both files open successfully.
  Input      : String(1 .. 65);            -- Line of text read from TUTOR.TXT.
  Len        : Integer;                  -- Length of line read from TUTOR.TXT.
  Legal_Note : constant String := " Copyright 1988-98 John J. Herro ";
                    -- Legal_Note isn't used by the program, but it causes most
                    -- compilers to place this string in the executable file.
begin
  begin
    Text_IO.Open(Text_File, Mode => Text_IO.In_File, Name => "TUTOR.TXT");
  exception
    when Text_IO.Name_Error =>
      Text_IO.Put_Line(
           "I'm sorry.  The file TUTOR.TXT seems to be missing.");
      OK := False;
  end;
  begin
    Random_IO.Create(Data_File, Random_IO.Out_File, Name => "ADATUTOR.DAT");
  exception
    when others =>
      Text_IO.Put_Line("I'm sorry.  I can't seem to create ADATUTOR.DAT.");
      Text_IO.Put_Line("Perhaps that file already exists?");
      OK := False;
  end;
  if OK then
    while not Text_IO.End_Of_File(Text_File) loop
      Text_IO.Get_Line(File => Text_File, Item => Input, Last => Len);
      if Len > 3 then     -- In case extra CRs/LFs were added to the text file.
        Input(Len + 1 .. 64) := (others => ' ');
              -- In case trailing blanks were lost when transferring TUTOR.TXT.
        Random_IO.Write(Data_File, Item => Input(1 .. 64));
      end if;
    end loop;
    Text_IO.Close(Text_File);
    Random_IO.Close(Data_File);
    Text_IO.Put_Line("ADATUTOR.DAT created.");
  end if;
end TXT2DAT;
