-- ADATU401.ADA   Ver. 4.01   2001-SEP-10   Copyright 1988-2001 John J. Herro
--
-- SOFTWARE INNOVATIONS TECHNOLOGY          www.adatutor.com
-- 1083 MANDARIN DR NE
-- PALM BAY FL 32905-4706                   john@adatutor.com
--
-- (321) 951-0233
--
--
-- This code is written in Ada 83 and will compile on Ada 83 and Ada 95
-- compilers.
--
-- Before compiling this file, you must compile ONE of the following:
--
--    ADA95.ADA     Recommended when using any Ada 95 compiler.
--    JANUS16.PKG   Recommended when using a PC with 16-bit Janus/Ada.
--    JANUS32.PKG   Recommended when using a PC with 32-bit Janus/Ada.
--    OPEN.ADA      Recommended when using a PC with an Open Ada compiler.
--    UNIX.ADA      Recommended for UNIX based systems, if you can also
--                     compile ONECHAR.C or ALTCHAR.C with a C compiler and
--                     link with Ada.
--    VAX.ADA       Recommended when using VAX Ada.
--    VANILLA.ADA   "Plain vanilla" version for all other systems.  Should work
--                     with ANY standard Ada 83 or Ada 95 compiler.  On some
--                     systems, VANILLA.ADA may require you to strike Enter
--                     after each response.
--
-- See the PRINTME.TXT file for more information on installing AdaTutor on
-- other computers.
--
--
-- NOTE: If you compile this file (ADATU401.ADA) for a PC, the PC will have to
-- run ANSI.SYS (which comes with DOS and Windows) for AdaTutor to run
-- correctly.  The file ADATUTOR.EXE, supplied with this program, runs on a PC
-- and does NOT require ANSI.SYS.  It was produced by compiling PCSOURCE.ADA
-- rather than ADATU401.ADA.
--

-- Introduction:
--
-- AdaTutor provides interactive instruction in the Ada programming language,
-- allowing you to learn at your own pace.  Under DOS (or in a DOS partition
-- under any version of Windows), access to an Ada compiler is helpful, but not
-- required.  You can exit this program at any time by striking X, and later
-- resume the session exactly where you left off.  If you have a color screen,
-- you can set the foreground, background, and border colors at any time by
-- typing S.
--
-- AdaTutor presents a screenful of information at a time.  Screens are read
-- in 64-byte blocks from the random access file ADATUTOR.DAT, using Direct_IO.
-- For most screens, AdaTutor waits for you to strike one character to
-- determine which screen to show next.  Screens are numbered starting with
-- 101; each screen has a three-digit number.  Screens 101 through 108 have
-- special uses, as follows:
--
-- 101 - This screen is presented when you complete the Ada course.  It
--       contains a congratulatory message.  After this screen is shown,
--       control returns directly to the operating system; the program doesn't
--       wait for you to strike a character.
-- 102 - This screen is presented when you exit AdaTutor before completing the
--       course.  After this screen is shown, control returns directly to the
--       operating system; the program doesn't wait for you to strike a
--       character.
-- 103 - This screen is shown whenever you strike X.  It displays the number of
--       the last screen shown and the approximate percentage through the
--       course.  It then asks if you want to exit the program.  If you strike
--       Y, screen 102 is shown and control returns to the operating system.
--       If you type N, screen 108 is shown to provide a menu of further
--       choices.  From screen 103, you can also strike M to see the main menu
--       (screen 106).
-- 104 - This is the opening screen.  It asks if you've used AdaTutor before.
--       If you strike N, a welcome screen is presented and the course begins.
--       If you strike Y, screen 107 is shown.
-- 105 - This screen allows you to type the number of the next screen you want
--       to see.  For this screen, instead of striking one character, you type
--       a three-digit number and presses Enter.  Any number from 104 through
--       the largest screen number is accepted.
-- 106 - This screen contains the main menu of topics covered in AdaTutor.
--       When you select a main topic, an appropriate sub-menu is shown.
-- 107 - This screen is shown when you say that you've used AdaTutor before.
--       It says "Welcome back!" and provides a menu that lets you resume where
--       you left off, go back to the last question or Outside Assignment, go
--       to the main menu (screen 106), or go to any specified screen number
--       (via screen 105).
-- 108 - This screen is shown when you answer N to screen 103.  It provides a
--       menu similar to screen 107, except that the first choice takes you
--       back to the screen shown before you saw 103.  For example, if you
--       strike X while viewing screen 300, you'll see screen 103.  If you then
--       answer N, you'll see screen 108.  From 108 the first menu selection
--       takes you back to 300.
--

-- Format of the Data File:
--
-- ADATUTOR.DAT is a random access file of 64-byte blocks.  The format of this
-- file changed considerably starting with version 2.00 of AdaTutor.  It's now
-- much more compact, and, although it's still a data file, it now contains
-- only the 95 printable ASCII characters.
--
-- The first block of ADATUTOR.DAT is referred to as block 1, and the first 35
-- blocks together are called the index.  Bytes 2 through 4 of block 1 contain,
-- in ASCII, the number of the welcome screen that's shown when you say that
-- you haven't used AdaTutor before.  Bytes 6 through 8 of block 1 contain the
-- number of the highest screen in the course.  (Bytes 1 and 5 of block 1
-- contain spaces.)
--
-- Bytes 9 of block 1 through the end of block 35 contain four bytes of
-- information for each of the possible screens 101 through 658.  For example,
-- information for screen 101 is stored in bytes 9 through 12 of block 1, the
-- next four bytes are for screen 102, etc.  For screens that don't exist, all
-- four bytes contain spaces.
--
-- The first of the four bytes is A if the corresponding screen introduces an
-- Outside Assignment, Q if the screen asks a question, or a space otherwise.
-- The next two bytes give the number of the block where data for the screen
-- begins, in base 95!  A space represents 0, ! represents 1, " represents 2,
-- # represents 3, $ represents 4, etc., through all the printable characters
-- of the ASCII set.  A tilde (~) represents 94.
--
-- The last of the four bytes gives the position, 1 through 64, within the
-- block where the data for this screen starts.  Again, ! represents 1,
-- " represents 2, # represents 3, etc.
--
-- Data for the screens are stored starting in position 1 of block 36.  In the
-- screen data, the following characters have special meaning:
--
--   `  displays the number of spaces indicated by the next character (# = 3,
--        $ = 4, etc.)
--   ~  toggles reverse video.
--   ^  toggles high intensity.
--   {  causes CR-LF.
--   }  moves cursor to row 24, column 1, for a prompt.
--
-- These characters have special meaning in screen 103 only:
--
--    #  shows approximate percentage through the course.
--    $  shows the number of the screen seen before 103.
--
-- Immediately after }, b represents "Please type a space to go on, or B to go
-- back." and q represents "Please type a space to go on, or B to go back to
-- the question."
--

--
-- The data for each screen are followed by the "control information" for that
-- screen, in square brackets.  The control information is a list of characters
-- that you might strike after seeing this screen.  Each character is followed
-- by the three-digit number of the next screen to be shown when that character
-- is struck.  For example, Y107N122 is the control information for screen 104.
-- This means that if you strike Y, screen 107 will be shown next, and if you
-- strikes N, screen 122 will be shown.  Striking any other character will
-- simply cause a beep (except that X can always be typed to exit the program,
-- S can always be typed to set colors, and CR will be ignored).  If the
-- control information is simply #, you are prompted to type the next screen
-- number.  This feature is used in screen 105.
--
-- A "screen number" of 098 following a character means "go back to the last
-- Outside Assignment," and 099 means "go back to the last question."  These
-- special numbers are used in screens 107 and 108.  Number 100 means "go back
-- to the previous screen seen."
--
-- AdaTutor opens the Data File in In_File mode for read-only access.
--
--
--
-- Format of the User File:
--
-- The User File USERDATA.TXT is a text file containing four lines: the number
-- of the last screen seen the last time you ran AdaTutor, a number
-- representing the foreground color, a number representing the background
-- color, and a number representing the border color.  The numbers 0 through 7
-- represent black, red, green, yellow, blue, magenta, cyan, and white, in that
-- order.  Note that not all color PCs have a separate border color.
--

with Custom_IO, Direct_IO, Text_IO; use Custom_IO;
procedure AdaTutor is
  subtype Block_Subtype is String(1 .. 64);
  package Random_IO is new Direct_IO(Block_Subtype); use Random_IO;
  Ix_Size        : constant := 35;            -- Number of blocks in the index.
  Data_File      : File_Type;          -- The file from which screens are read.
  User_File      : Text_IO.File_Type;    -- Holds last screen seen, and colors.
  Block          : Block_Subtype;              -- Buffer for random-access I/O.
  Vpos           : Integer;                     -- Number of the current block.
  Hpos           : Integer;           -- Current position within current block.
  SN, Old_SN     : Integer := 104;      -- Screen num. and previous screen num.
  Quitting_SN    : Integer := 104;         -- Screen number where you left off.
  Highest_SN     : Integer;             -- Highest screen number in the course.
  Welcome_SN     : Integer;         -- Number of the screen shown to new users.
  Indx           : String(1 .. 64*Ix_Size);        -- Index from the Data File.
  Files_OK       : Boolean := False;      -- True when files open successfully.
  High_Int       : Boolean := False;    -- True when displaying high intensity.
  Rev_Vid        : Boolean := False;     -- True when displaying reverse video.
  Ctrl_C         : constant Character := Character'Val(3);        -- Control C.
  Beep           : constant Character := Character'Val(7);             -- Bell.
  LF             : constant Character := Character'Val(10);       -- Line Feed.
  CR             : constant Character := Character'Val(13);    -- Carr. Return.
  User_File_Name : constant String := "USERDATA.TXT";     -- Name of User File.
  Legal_Note     : constant String := " Copyright 1988-2001 John J. Herro ";
                    -- Legal_Note isn't used by the program, but it causes most
                    -- compilers to place this string in the executable file.
  procedure Open_Data_File is separate;
  procedure Open_User_File is separate;
  procedure Show_Current_Screen is separate;
  procedure Get_Next_Screen_Number is separate;
  procedure Update_User_File is separate;
begin
  Open_Data_File;
  Open_User_File;
  if Files_OK then
    Set_Border_Color(To => Border_Color);                -- Set default colors.
    Put(Normal_Colors);
    while SN > 0 loop            -- "Screen number" of 0 means end the program.
      Put(Clear_Scrn);                                     -- Clear the screen.
      Show_Current_Screen;
      Get_Next_Screen_Number;
    end loop;
    Close(Data_File);
    Update_User_File;
  end if;
end AdaTutor;

separate (AdaTutor)
procedure Open_Data_File is
  Data_File_Name : constant String := "ADATUTOR.DAT";
begin
  Open(Data_File, Mode => In_File, Name => Data_File_Name);
  for I in 1 .. Ix_Size loop             -- Read index from start of Data File.
    Read(Data_File, Item => Block, From => Count(I));
    Indx(64*I - 63 .. 64*I) := Block;
  end loop;
  Welcome_SN := Integer'Value(Indx(2 .. 4));
  Highest_SN := Integer'Value(Indx(6 .. 8));
  Files_OK := True;
exception
  when Name_Error =>
    Put("I'm sorry.  The file " & Data_File_Name);
    Put_Line(" seems to be missing.");
  when others =>
    Put("I'm sorry.  The file " & Data_File_Name);
    Put_Line(" seems to have the wrong form.");
end Open_Data_File;

separate (AdaTutor)
procedure Open_User_File is
  Input : String(1 .. 6);
  Len   : Integer;
begin
  Text_IO.Open(User_File, Mode => Text_IO.In_File, Name => User_File_Name);
  Text_IO.Get_Line(User_File, Input, Len);
  Quitting_SN      := Integer'Value(Input(1 .. Len));
  Old_SN           := Quitting_SN;
  Text_IO.Get_Line(User_File, Input, Len);
  Foregrnd_Color   := Color'Val(Integer'Value(Input(1 .. Len)));
  Fore_Color_Digit := Input(2);
  Normal_Colors(6) := Fore_Color_Digit;
  Text_IO.Get_Line(User_File, Input, Len);
  Backgrnd_Color   := Color'Val(Integer'Value(Input(1 .. Len)));
  Back_Color_Digit := Input(2);
  Normal_Colors(9) := Back_Color_Digit;
  Text_IO.Get_Line(User_File, Input, Len);
  Border_Color     := Color'Val(Integer'Value(Input(1 .. Len)));
  Text_IO.Close(User_File);
exception
  when Text_IO.Name_Error =>
    Put("I'm sorry.  The file " & User_File_Name);
    Put_Line(" seems to be missing.");
    Files_OK := False;
  when others =>
    Put("I'm sorry.  The file " & User_File_Name);
    Put_Line(" seems to have the wrong form.");
    Files_OK := False;
end Open_User_File;

separate (AdaTutor)
procedure Show_Current_Screen is
  Half_Diff : Integer := (Highest_SN - Welcome_SN) / 2;
  Percent   : Integer := (50 * (Old_SN - Welcome_SN)) / Half_Diff;
                          -- Percentage of the course completed.  Using 50 and
                          -- Half_Diff guarantees that the numerator < 2 ** 15.
  Expanding : Boolean := False;         -- True when expanding multiple spaces.
  Literal   : Boolean := False;        -- True to display next character as is.
  Prompting : Boolean := False;        -- True for first character in a prompt.
  Space     : constant String(1 .. 80) := (others => ' ');
  procedure Process_Char is separate;
begin
  Vpos := 95*(Character'Pos(Indx(SN*4 - 394)) - 32) +         -- Point to start
              Character'Pos(Indx(SN*4 - 393)) - 32;           -- of current
  Hpos := Character'Pos(Indx(SN*4 - 392)) - 32;               -- screen.
  Read(Data_File, Item => Block, From => Count(Vpos));
  if Percent < 0 then                       -- Make sure Percent is reasonable.
    Percent := 0;
  elsif Percent > 99 then
    Percent := 99;
  end if;
  while Block(Hpos) /= '[' or Expanding or Literal loop  -- [ starts ctrl info.
    if Expanding then
      if Block(Hpos) = '!' then
        Literal := True;
      else
        Put(Space(1 .. Character'Pos(Block(Hpos)) - 32));
      end if;
      Expanding := False;
    elsif Literal then
      Put(Block(Hpos));
      Literal := False;
    elsif Prompting then
      case Block(Hpos) is
        when 'b' => Put("Please type a space to go on, or B to go back.");
        when 'q' => Put("Please type a space to go on, or B to go back ");
                    Put("to the question.");
        when others => Process_Char;
      end case;
      Prompting := False;
    else
      Process_Char;
    end if;
    Hpos := Hpos + 1;
    if Hpos > Block'Length then
      Vpos := Vpos + 1;
      Hpos := 1;
      Read(Data_file, Item => Block, From => Count(Vpos));
    end if;
  end loop;
end Show_Current_Screen;

separate (AdaTutor.Show_Current_Screen)
procedure Process_Char is
begin
  case Block(Hpos) is
    when '{'    => New_Line;                            -- { = CR-LF.
    when '`'    => Expanding := True;                   -- ` = several spaces.
    when '^'    => High_Int := not High_Int;            -- ^ = toggle bright.
                   if High_Int then
                     Put(Esc & "[1m");
                   else
                     Put(Normal_Colors);
                   end if;
    when '}'    => Put(Esc & "[24;1H(Screen");          -- } = go to line 24.
                   Put(Integer'Image(SN) & ")  ");      --     and show SN.
                   Prompting := True;
    when '~'    => Rev_Vid := not Rev_Vid;              -- ~ = toggle rev. vid.
                   if Rev_Vid then
                     Put(Esc & "[7m");
                   else
                     Put(Normal_Colors);
                   end if;
    when '$'    => if SN = 103 then                     -- $ = screen #.
                     Put(Integer'Image(Old_SN));
                   else
                     Put('$');
                   end if;
    when '#'    => if SN = 103 then                     -- # = % completed.
                     Put(Integer'Image(Percent));
                   else
                     Put('#');
                   end if;
    when others => Put(Block(Hpos));
  end case;
end Process_Char;

separate (AdaTutor)
procedure Get_Next_Screen_Number is
  Ctrl_Info : Block_Subtype;           -- Control info. for the current screen.
  Place     : Integer := 1;               -- Current position within Ctrl_Info.
  Input     : String(1 .. 4);                   -- Screen number that you type.
  Len       : Integer;                             -- Length of typed response.
  Valid     : Boolean;                    -- True when typed response is valid.
  procedure Set_Colors is separate;
  procedure Input_One_Keystroke is separate;
begin
  while Block(Hpos) /= ']' loop     -- Read control information from Data File.
    Hpos := Hpos + 1;
    if Hpos > Block'Length then
      Vpos := Vpos + 1;
      Hpos := 1;
      Read(Data_File, Item => Block, From => Count(Vpos));
    end if;
    Ctrl_Info(Place) := Block(Hpos);
    Place := Place + 1;
  end loop;
  if SN = 103 then                     -- Screen 103 means you typed X to exit.
    Quitting_SN := Old_SN;
  elsif SN >= Welcome_SN then               -- Save SN so you can return to it.
    Old_SN := SN;
  end if;
  if SN < 103 then                           -- Set SN to # of the next screen.
    SN := 0;        -- Set signal to end the program after screens 101 and 102.
  elsif Ctrl_Info(1) = '#' then             -- You type the next screen number.
    Valid := False;
    while not Valid loop                -- Keep trying until response is valid.
      Put("# ");                                   -- Prompt for screen number.
      Input := "    ";  Get_Line(Input, Len);           -- Input screen number.
      if Input(1) = 'x' or Input(1) = 'X' or Input(1) = Ctrl_C then
        SN := 103;                            -- Show screen 103 if you type X.
        Valid := True;                                -- X is a valid response.
      elsif Input(1) = 's' or Input(1) = 'S' then
        Set_Colors;                                -- Set colors if you type S.
        Valid := True;                                -- S is a valid response.
      else
        begin                                         -- Convert ASCII input to
          SN := Integer'Value(Input);                 -- integer.  If in range,
          Valid := SN in 104 .. Highest_SN;           -- set Valid to True.  If
        exception                                     -- it can't be converted
          when others => null;                        -- (e.g., illegal char.),
        end;                                          -- or it's out of range,
      end if;                                         -- leave Valid = False so
      if not Valid and Len > 0 then                   -- you can try again.
        Put_Line("Incorrect number.  Please try again.");
      end if;
    end loop;
  else
    Input_One_Keystroke;
  end if;
end Get_Next_Screen_Number;

separate (AdaTutor.Get_Next_Screen_Number)
procedure Set_Colors is
  Bright    : constant String := Esc & "[1m";         -- Causes high intensity.
  Keystroke : Character := 'f';              -- Single character that you type.
  Space     : constant String(1 .. 23) := (others => ' ');
begin
  while Keystroke = 'f' or Keystroke = 'b' or Keystroke = 'r' or
        Keystroke = 'F' or Keystroke = 'B' or Keystroke = 'R' or
        Keystroke = CR  or Keystroke = LF loop
    Put(Clear_Scrn);                                       -- Clear the screen.
    New_Line;
    Put(Space & "The " & Bright & "foreground" & Normal_Colors);
    Put_Line(" color is now " & Color'Image(Foregrnd_Color) & '.');
    Put(Space & "The " & Bright & "background" & Normal_Colors);
    Put_Line(" color is now " & Color'Image(Backgrnd_Color) & '.');
    Put(Space & "The " & Bright & "  border  " & Normal_Colors);
    Put_Line(" color is now " & Color'Image(Border_Color) & '.');
    New_Line;
    Put_Line(Space & " Note:  Some color PCs don't have");
    Put_Line(Space & "     separate border colors.");
    New_Line;
    Put_Line(Space & "             Strike:");
    Put_Line(Space & "F to change the foreground color,");
    Put_Line(Space & "B to change the background color,");
    Put_Line(Space & "R to change the   border   color.");
    New_Line;
    Put_Line(Space & "Strike the space bar to continue.");
    Get(Keystroke);                         -- Get one character from keyboard.
    if Keystroke = 'f' or Keystroke = 'F' then
      Foregrnd_Color := Color'Val((Color'Pos(Foregrnd_Color) + 1) mod 8);
      if Foregrnd_Color = Backgrnd_Color then
        Foregrnd_Color := Color'Val((Color'Pos(Foregrnd_Color) + 1) mod 8);
      end if;
    elsif Keystroke = 'b' or Keystroke = 'B' then
      Backgrnd_Color := Color'Val((Color'Pos(Backgrnd_Color) + 1) mod 8);
      if Foregrnd_Color = Backgrnd_Color then
        Backgrnd_Color := Color'Val((Color'Pos(Backgrnd_Color) + 1) mod 8);
      end if;
    elsif Keystroke = 'r' or Keystroke = 'R' then
      Border_Color := Color'Val((Color'Pos(Border_Color) + 1) mod 8);
    end if;
    Fore_Color_Digit := Character'Val(48 + Color'Pos(Foregrnd_Color));
    Back_Color_Digit := Character'Val(48 + Color'Pos(Backgrnd_Color));
    Normal_Colors(6) := Fore_Color_Digit;
    Normal_Colors(9) := Back_Color_Digit;
    Put(Normal_Colors);
    Set_Border_Color(To => Border_Color);
  end loop;
end Set_Colors;

separate (AdaTutor.Get_Next_Screen_Number)
procedure Input_One_Keystroke is
  Keystroke : Character;                     -- Single character that you type.
  Valid     : Boolean := False;           -- True when typed response is valid.
  Search    : Character;     -- 'A' = last Outside Assignment; 'Q' = last Ques.
begin
  Put("  >");                                      -- Prompt for one character.
  while not Valid loop                  -- Keep trying until response is valid.
    Get(Keystroke);                         -- Get one character from keyboard.
    if Keystroke in 'a' .. 'z' then            -- Force upper case to simplify.
      Keystroke := Character'Val(Character'Pos(Keystroke) - 32);
    end if;
    if Keystroke = 'X' or Keystroke = Ctrl_C then
      SN := 103;                              -- Show screen 103 if you type X.
      Valid := True;                                  -- X is a valid response.
    elsif Keystroke = 'S' then
      Set_Colors;                                  -- Set colors if you type S.
      Valid := True;                                  -- S is a valid response.
    end if;
    Place := 1;             -- Search list of valid characters for this screen.
    Valid := Valid;               -- This statement works around a minor bug in
                                  -- ver. 1.0 of the Meridian IFORM optimizer.
    while not Valid and Ctrl_Info(Place) /= ']' loop        -- ] ends the list.
      if Keystroke = Ctrl_Info(Place) then
                  -- Typed char. found in list; get screen # from control info.
        SN := Integer'Value(Ctrl_Info(Place + 1 .. Place + 3));
        Valid := True;       -- Characters in the list are all valid responses.
      end if;
      Place := Place + 4;       -- A 3-digit number follows each char. in list.
    end loop;
    if not Valid and Keystroke /= CR and Keystroke /= LF then
      Put(Beep);                              -- Beep if response is not valid,
    end if;                                  -- but ignore CRs and LFs quietly.
  end loop;
  if SN = 98 then                        -- Go back to last Outside Assignment.
    Search := 'A';
  elsif SN = 99 then                               -- Go back to last question.
    Search := 'Q';
  elsif SN = 100 then                       -- Go back to the last screen seen.
    SN := Quitting_SN;
  end if;
  if SN = 98 or SN = 99 then
    SN := Old_SN;
    while SN > Welcome_SN and Indx(SN*4 - 395) /= Search loop
      SN := SN - 1;
    end loop;
  end if;
end Input_One_Keystroke;

separate (AdaTutor)
procedure Update_User_File is
begin
  Text_IO.Create(User_File, Mode => Text_IO.Out_File, Name => User_File_Name);
  Text_IO.Put_Line(User_File, Integer'Image(Quitting_SN));
  Text_IO.Put_Line(User_File, Integer'Image(Color'Pos(Foregrnd_Color)));
  Text_IO.Put_Line(User_File, Integer'Image(Color'Pos(Backgrnd_Color)));
  Text_IO.Put_Line(User_File, Integer'Image(Color'Pos(Border_Color)));
  Text_IO.Close(User_File);
end Update_User_File;
