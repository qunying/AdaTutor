-- DAT2TXT.ADA   Ver. 4.01   2001-SEP-10   Copyright 1988-2001 John J. Herro
--
-- SOFTWARE INNOVATIONS TECHNOLOGY          www.adatutor.com
-- 1083 MANDARIN DR NE
-- PALM BAY FL 32905-4706                   john@adatutor.com
--
-- (321) 951-0233
--
-- Run this program on a PC before installing AdaTutor on another computer.
-- It translates ADATUTOR.DAT to TUTOR.TXT, a text file that can be easily
-- transferred to other computers.  Then compile and run TXT2DAT.ADA on the
-- other machine to create ADATUTOR.DAT from TUTOR.TXT.
--
-- This code is written in Ada 83 and will compile on Ada 83 and Ada 95
-- compilers.
--
with Direct_IO, Text_IO;
procedure DAT2TXT is
  subtype Block_Subtype is String(1 .. 64);
  package Random_IO is new Direct_IO(Block_Subtype);
  Data_File  : Random_IO.File_Type;                          -- The input file.
  Text_File  : Text_IO.File_Type;                           -- The output file.
  Block      : Block_Subtype;              -- A block of 64 bytes being copied.
  OK         : Boolean := True;      -- True when both files open successfully.
  Legal_Note : constant String := " Copyright 1988-2001 John J. Herro ";
                    -- Legal_Note isn't used by the program, but it causes most
                    -- compilers to place this string in the executable file.
begin
  begin
    Random_IO.Open(Data_File, Random_IO.In_File, Name => "ADATUTOR.DAT");
  exception
    when Random_IO.Name_Error =>
      Text_IO.Put_Line(
           "I'm sorry.  The file ADATUTOR.DAT seems to be missing.");
      OK := False;
  end;
  begin
    Text_IO.Create(Text_File, Name => "TUTOR.TXT");
  exception
    when others =>
      Text_IO.Put_Line("I'm sorry.  I can't seem to create TUTOR.TXT.");
      Text_IO.Put_Line("Perhaps that file already exists?");
      OK := False;
  end;
  if OK then
    while not Random_IO.End_Of_File(Data_File) loop
      Random_IO.Read(Data_File, Item => Block);
      Text_IO.Put_Line(File => Text_File, Item => Block);
    end loop;
    Random_IO.Close(Data_File);
    Text_IO.Close(Text_File);
    Text_IO.Put_Line("TUTOR.TXT created.");
  end if;
end DAT2TXT;
