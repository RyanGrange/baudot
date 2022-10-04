namespace Baudot;

/*
    Baudot Translator
    Copyright (C) 1995 Ryan T Grange

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301
    USA
*/

/*
This is very much a work in progress.
And I don't really have access to a teletype anymore to verify it would work.
The original BASIC version handled connection to a serial port.
I only plan on porting the tranlation to and from 5 bit to and from 7 bit values.
*/

// Baudot is different that just about any other character set you've worked with. Imagine if pressing and releasing the shift key sent two different hidden characters.

public class Translator
{
    private Dictionary<int, char> translations = new();

    // WIP: Need to figure out some placeholders for the fig, ltr, undef, and bell characters.
    private string USBaudot = "\0T\rO HNM\nLRGIPCVEZDBSYFXAWJ{fig}UQK{ltr}\05\r9 {undef},.\n)4&80:;3\"$?{bell}6!/-2'{fig}71({ltr}";

    private bool Figures = false;
    private Stream? inputData;
    private Stream? outputData;

    public Translator(Stream input, Stream output)
    {
        inputData = input;
        outputData = output;
    }

    public void SetLetters()
    {
        Figures = false;
    }

    public void SetFigures()
    {
        Figures = true;
    }

}

/*
' Baudot <-> ASCII translation table
'    00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31
DATA 00,69,13,65,32,83,73,85,-3,68,82,74,78,70,67,75,84,90,76,87,72,89,80,81,79,66,71,-1,77,88,86,-2
DATA 00,51,13,45,32,07,56,55,-3,36,52,39,44,33,58,40,53,34,41,50,35,54,48,49,57,63,38,-1,46,47,59,-2

\0, E, \n, A, spc, S, I, U, -3(cr), D, R, J, N, F, C, K, T, Z, L, W, H, Y, P, Q, O, B, G, -1(fig), M, X, V, -2(ltr)
\0, 3, \n, -, spc, (bell), 8, 7, -3(cr), $, 4, ', (comma), !, :, (, 5, ", ), 2, #, 6, 0, 1, 9, ?, &, -1(fig), ., /, ;, -2(ltr)


*/

/*
DECLARE SUB SlowType (a$)
DECLARE SUB OpeningScreen ()
CONST True = -1
CONST False = 0
CONST Escape = 27
DIM Char(63) AS INTEGER
DIM Option$(1 TO 4)

    CLS
    SCREEN 0
    OpeningScreen
    CLS
    COLOR 15
    PRINT "Initializing variables..."
    GOSUB GetCommand
    IF Option$(2) = "" OR Option$(2) = "*" THEN GOSUB GetMessageDir
    IF RIGHT$(Option$(2), 1) <> "\" THEN Option$(2) = Option$(2) + "\"
    SELECT CASE Option$(4)
        CASE "75"
            Speed$ = "75"
        CASE "300"
            Speed$ = "300"
        CASE ELSE
            Speed$ = "1200"
    END SELECT
    ScreenLength = 50
    SCREEN 0
    ON ERROR GOTO ScreenError
FindLength:
    WIDTH 80, ScreenLength
    LOCATE ScreenLength, 1

    ON ERROR GOTO ErrorHandler

    RESTORE TranslationTable
    FOR i = 0 TO 63
        READ Char(i)
    NEXT i

TranslationTable:
' Baudot <-> ASCII translation table
'    00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31
DATA 00,69,13,65,32,83,73,85,-3,68,82,74,78,70,67,75,84,90,76,87,72,89,80,81,79,66,71,-1,77,88,86,-2
DATA 00,51,13,45,32,07,56,55,-3,36,52,39,44,33,58,40,53,34,41,50,35,54,48,49,57,63,38,-1,46,47,59,-2


    Nul$ = CHR$(0)
    Echo = False
    Port$ = Option$(1)
    CommPort = VAL(Port$)
    GOSUB ClearScreen
    COLOR 15
    PRINT "Screen Length set to"; STR$(ScreenLength); "."
    PRINT
    PRINT "Comms port set to port #"; Port$; " at "; Speed$; " baud."
    PRINT
    PRINT "Message directory set to "; Option$(2); "."
    PRINT

    IF LEFT$(Option$(3), 1) >= "A" AND LEFT$(Option$(3), 1) <= "Z" THEN
        IF RIGHT$(Option$(3), 1) <> "\" THEN Option$(3) = Option$(3) + "\"
        COLOR 15
        PRINT "Downlink directory designated as "; Option$(3); "."
        PRINT "Auto-Downlink function available."
        PRINT
        RamDrvAvailable = True
        RamDrive$ = Option$(3)
    ELSE
        COLOR 14
        PRINT "No Downlink drive designated.  Auto-Downlink function disabled."
        PRINT
        RamDrvAvailable = False
    END IF

    GOSUB PrintStatus
    COLOR 3
    PRINT "Press F1 to see the function key help list."
    COLOR 15
    OPEN "COM" + Port$ + ": " + Speed$ + ",N,5,2,RB21767,CD0,DS0,OP0" FOR RANDOM AS #1

MainProgram:
    DO: LOOP UNTIL INKEY$ = ""
    COLOR 11: PRINT : PRINT "Comms mode active...": COLOR 7
    COM(CommPort) ON
    ON COM(CommPort) GOSUB ReadTTY

SendLoop:
    DO
       e$ = INKEY$
    LOOP UNTIL e$ <> ""
    IF e$ = CHR$(Escape) THEN GOTO EndIt
    SELECT CASE e$
        CASE Nul$ + ";"
            GOSUB Help
        CASE Nul$ + "<"
            GOSUB Rtrvfile
        CASE Nul$ + "="
            GOSUB AutoDownLink
            GOSUB PrintStatus
        CASE Nul$ + ">"
            GOSUB Savfile
            GOSUB PrintStatus
        CASE Nul$ + "?"
            Echo = NOT (Echo)
            GOSUB PrintStatus
        CASE Nul$ + "@"
            GOSUB Prntr
            GOSUB PrintStatus
        CASE Nul$ + "A"
            GOSUB ChangeSpeed
        CASE Nul$ + "B"
            GOSUB ProcessDownlink
        CASE Nul$ + "C"
            GOSUB DosShell
        CASE Nul$ + "D"
            GOSUB Relink
        'case Nul$ + "E"
        'case Nul$ + "F"
        CASE Nul$ + "G"
            GOSUB ClearScreen
        CASE "`"
            PRINT #1, CHR$(31);
            LastSent = 31
            LastShift = 31
        CASE ELSE
            GOSUB Sendit
    END SELECT
GOTO SendLoop:

EndIt:
    COLOR 3
    PRINT
    PRINT "Comms mode terminated by user."
    COLOR 7
SYSTEM

ScreenError:
    SELECT CASE ScreenLength
        CASE 50
            ScreenLength = 43
        CASE 43
            ScreenLength = 25
    END SELECT
RESUME FindLength

ErrorHandler:
    Stat = 0
    COLOR 4
    SELECT CASE ERR
        CASE 5    'Illegal function call
            Stat = 2
        CASE 25   'Device fault
            PRINT "Device error."
            Stat = 0
        CASE 53   'File not found
            PRINT : PRINT "File not found."
            IF Rtrv = True THEN Rtrv = False: CLOSE #3
            Stat = 2
        CASE 57   'Device I/O error
            Stat = 1
        CASE 61   'Disk full
            PRINT
            PRINT "Disk is full.  Save terminated."
            IF Sav = True THEN Sav = False: CLOSE #2
            Stat = 2
        CASE 62   'Input past end of file
            IF Rtrv = True THEN Rtrv = False: CLOSE #3
            Stat = 2
        CASE 64   'Bad file name
            PRINT
            PRINT "Improper file name."
            IF Sav = True THEN Sav = False: CLOSE #2
            IF Rtrv = True THEN Rtrv = False: CLOSE #3
            Stat = 2
        CASE 69   'Communication-buffer overflow
            PRINT
            PRINT "Overflowed buffer to disk drive.  Save is terminated."
            IF Sav = True THEN Sav = False: CLOSE #2
            Stat = 2
        CASE 70   'Permission denied
            PRINT
            PRINT "Disk access denied."
            IF Rtrv = True THEN Rtrv = False: CLOSE #3
            Stat = 2
        CASE 71   'Disk not ready
            PRINT "Error:  Disk not ready."
            IF Sav = True THEN Sav = False: CLOSE #2
            Stat = 2
        CASE 72   'Disk-media error
            PRINT "Disk media error."
            IF Sav = True THEN Sav = False: CLOSE #2
            IF Rtrv = True THEN Rtrv = False: CLOSE #3
            Stat = 2
        CASE 75   'Path/File access error
            PRINT "Path / File access error."
            IF Sav = True THEN Sav = False: CLOSE #2
            IF Rtrv = True THEN Rtrv = False: CLOSE #3
            Stat = 2
        CASE 76   'Path not found
            PRINT "File not found."
            IF Rtrv = True THEN Rtrv = False: CLOSE #3
            Stat = 2
        CASE ELSE
            PRINT "Error #"; ERR; "occured."
            Stat = 0
    END SELECT
    COLOR 7
    IF Stat = 1 THEN CLOSE #1: OPEN "COM" + Port$ + ": " + Speed$ + ",N,5,2,RB21767,CD0,DS0,OP0" FOR RANDOM AS #1: COM(CommPort) ON: RESUME NEXT
    IF Stat = 2 THEN CLOSE #1: OPEN "COM" + Port$ + ": " + Speed$ + ",N,5,2,RB21767,CD0,DS0,OP0" FOR RANDOM AS #1: COM(CommPort) ON: RESUME SendLoop
    COLOR 14
    PRINT
    PRINT "Comms mode terminated by error."
    COLOR 7
    PRINT "Press any key to return to system."
    DO
    LOOP UNTIL INKEY$ <> ""
    RESET
SYSTEM

ReadTTY:
    WHILE LOC(1) > 0
        a$ = INPUT$(1, #1)
        a = ASC(a$) AND 31
        SELECT CASE Char(a)
            CASE -3
                PRINT "�";
            CASE -2
                Shift = 0
            CASE -1
                Shift = 32
            CASE ELSE
                b$ = CHR$(Char(a + Shift))
                IF b$ = CHR$(13) THEN PRINT "�";
                PRINT b$;
                IF Sav = True THEN PRINT #2, b$; : IF b$ = CHR$(13) THEN PRINT #2, CHR$(10);
                IF Prn = True THEN
                    LPRINT b$;
                    IF b$ = CHR$(13) THEN PrnLine = PrnLine + 1
                    IF PrnLine = 56 THEN
                        PrnLine = 0
                        LPRINT CHR$(12);
                    END IF
                END IF
                IF b$ = "N" THEN nc = nc + 1
                IF b$ <> "N" THEN nc = 0
                IF Auto = True THEN
                    PRINT #4, b$;
                    IF b$ = CHR$(13) THEN PRINT #4, CHR$(10);
                    IF nc = 4 THEN
                        nc = 0
                        CLOSE #4
                        COM(CommPort) STOP
                        nn = nn + 1
                        nn$ = RIGHT$("00" + MID$(STR$(nn), 2), 3)
                        OPEN ff$ + nn$ FOR OUTPUT AS #4
                        PRINT
                        COLOR 14
                        PRINT "File:"; ff$; nn$
                        COLOR 7
                        COM(CommPort) ON
                    END IF
                END IF
                IF Prn = True AND nc = 4 THEN
                    LPRINT CHR$(12);
                    PrnLine = 0
                END IF
        END SELECT
    WEND
RETURN

Sendit:
    e$ = UCASE$(e$)
    e = ASC(e$)
    sc = -1
    FOR j = 0 TO 63
        IF Char(j) = e THEN sc = j: j = 63
    NEXT j
    IF sc = -1 THEN RETURN 'Reject any non-teletype codes.
    ss = 31
    IF sc > 31 THEN
        sc = sc - 32
        ss = 27
    END IF
    IF sc = 2 OR sc = 4 THEN ss = LastShift
    IF sc = 2 AND LastSent <> 2 THEN
        PRINT #1, CHR$(31); CHR$(8); CHR$(8);
        ss = 31
        LastShift = ss
    END IF
    IF Rtrv = True AND LastSent = 2 AND e = 32 THEN PRINT #1, CHR$(0);
    IF Echo = True THEN
        COLOR 2
        IF e$ = CHR$(13) THEN
            IF LastSent <> 2 THEN PRINT "��";
            PRINT "�";
        END IF
        IF e$ = CHR$(7) THEN
            BEEP
        ELSE
            PRINT e$;
        END IF
        COLOR 7
    END IF
    IF ss <> LastShift THEN PRINT #1, CHR$(ss);
    PRINT #1, CHR$(sc);
    LastSent = sc: LastShift = ss
RETURN

Rtrvfile:
    COM(CommPort) STOP
    d$ = ""
    IF POS(0) > 1 THEN PRINT
    PRINT
GetRtrvDir:
    COLOR 3
    PRINT "Default drive & directory is ";
    COLOR 12
    PRINT Option$(2)
    COLOR 3
    INPUT "Drive:\{dir\}{sub-dir\} for file to transmit:", d$
    IF d$ = "" THEN d$ = Option$(2)
    IF RIGHT$(d$, 1) <> "\" THEN d$ = d$ + "\"
    IF MID$(d$, 2, 2) <> ":\" THEN COLOR 14: PRINT "Expected format is Drive:\{dir\}{sub-dir\}.  Items in {} are optional.": COLOR 3: GOTO GetRtrvDir
    INPUT "Name of file to retreive:", f$
    IF f$ = "" THEN COLOR 4: PRINT "Retrieval aborted.": COLOR 7: RETURN
    n$ = d$ + f$
    PRINT "File:  "; n$
    PRINT
    OPEN n$ FOR INPUT AS #3
    PRINT "Sending..."
    COLOR 7
    COM(CommPort) ON
    Rtrv = True
    DO
        e$ = INPUT$(1, #3)
        GOSUB Sendit
    LOOP WHILE NOT EOF(3)
    CLOSE #3
    FOR i = 1 TO 10
        PRINT #1, Nul$;
    NEXT i
    Rtrv = False
RETURN

Savfile:
    IF Sav = True THEN Sav = False: CLOSE #2: COLOR 3: PRINT : PRINT "Save terminated.  File closed.": COLOR 7: RETURN
    COM(CommPort) STOP
    d$ = ""
    COLOR 3
    IF POS(0) > 1 THEN PRINT
    PRINT
GetSavDir:
    PRINT "Default is ";
    COLOR 12
    PRINT Option$(2)
    COLOR 3
    INPUT "Drive\{dir\}{sub-dir\}:", d$
    IF d$ = "" THEN d$ = Option$(2)
    IF RIGHT$(d$, 1) <> "\" THEN d$ = d$ + "\"
    IF MID$(d$, 2, 2) <> ":\" THEN COLOR 14: PRINT "Expected format is Drive:\{dir\}{sub-dir\}.  Items in {} are optional.": COLOR 3: GOTO GetSavDir
    INPUT "Enter name of file save:", f$
    IF f$ = "" THEN COLOR 4: PRINT "Save to file aborted.": COLOR 7: RETURN
    n$ = d$ + f$
    PRINT "File:  "; n$
    PRINT
    OPEN n$ FOR OUTPUT AS #2 LEN = 32767
    PRINT "Saving..."
    COLOR 7
    COM(CommPort) ON
    Sav = True
RETURN

Prntr:
    IF Prn = False THEN Prn = True: PrnLine = 0: RETURN
    Prn = False
    IF PrnLine > 0 THEN LPRINT CHR$(12);
    PrnLine = 0
RETURN

AutoDownLink:
    IF RamDrvAvailable = False THEN RETURN
    IF Auto = True THEN CLOSE #4: Auto = False: nc = 0: COLOR 3: PRINT : PRINT "Auto Downlinker off.": COLOR 7: RETURN
    ff$ = RamDrive$ + "DL" + LEFT$(TIME$, 2) + MID$(TIME$, 4, 2) + MID$(TIME$, 7, 2) + "."
    nn = 1
    nn$ = RIGHT$("00" + MID$(STR$(nn), 2), 3)
    COLOR 3
    IF POS(0) > 1 THEN PRINT
    PRINT "File prefix:  "; ff$
    PRINT "Auto Downlinker active..."
    COLOR 14
    PRINT "File:"; ff$; nn$
    COLOR 7
    OPEN ff$ + nn$ FOR OUTPUT AS #4
    Auto = True
RETURN

ChangeSpeed:
    SELECT CASE Speed$
        CASE "75"
            Speed$ = "300"
        CASE "300"
            Speed$ = "1200"
        CASE "1200"
            Speed$ = "75"
    END SELECT
    GOSUB PrintStatus
    GOSUB Relink
RETURN

ProcessDownlink:
    IF RamDrvAvailable = False THEN RETURN
    IF Sav = True THEN COLOR 14: PRINT : PRINT "You must turn off the Save to File feature first.": COLOR 7: RETURN
    IF Auto = True THEN COLOR 14: PRINT : PRINT "Auto-downlink must be turned off first.": COLOR 7: RETURN
    COM(CommPort) STOP
    IF ENVIRON$("PROCESS") = "" THEN
        PRINT "Default destinatin is C:\MDU\MSGIN\"
        d$ = ""
        INPUT "Destination directory:", d$
        IF d$ = "" THEN d$ = "C:\MDU\MSGIN\"
        SHELL "PROCESS.EXE " + RamDrive$ + " " + d$
    ELSE
        SHELL "PROCESS.EXE"
    END IF
    GOSUB ClearScreen
    GOSUB Relink
RETURN

DosShell:
    IF Sav = True THEN COLOR 14: PRINT : PRINT "You must turn off the Save to File feature first.": COLOR 7: RETURN
    IF Auto = True THEN COLOR 14: PRINT : PRINT "You must turn off the Auto-downlink feature first.": COLOR 7: RETURN
    WIDTH 80, 25
    CLS
    COLOR 15
    PRINT "Type 'EXIT' to return to TTY Talk."
    SHELL
    WIDTH 80, ScreenLength
    SHELL LEFT$(TtyDir$, 2)
    SHELL "CD" + MID$(TtyDir$, 3)
    GOSUB ClearScreen
    GOSUB Relink
RETURN

Relink:
    COM(CommPort) OFF
    CLOSE #1
    OPEN "COM" + Port$ + ":" + Speed$ + ",N,5,2,RB21767,CD0,DS0,OP0" FOR RANDOM AS #1
    COLOR 3
    IF POS(0) > 1 THEN PRINT
    PRINT "Comms port reset at "; Speed$; " baud."
    COLOR 7
    COM(CommPort) ON
    ON COM(CommPort) GOSUB ReadTTY
RETURN

GetCommand:
    Cmd$ = COMMAND$
    IF Cmd$ = "" THEN Cmd$ = UCASE$(ENVIRON$("TTYTALK"))
    j = 1
    FOR i = 1 TO LEN(Cmd$)
        a$ = MID$(Cmd$, i, 1)
        IF a$ <> " " THEN Option$(j) = Option$(j) + a$
        IF a$ = " " THEN j = j + 1: IF j > 4 THEN i = LEN(Cmd$)
    NEXT i
    IF j > 4 OR VAL(Option$(1)) < 1 OR VAL(Option$(1)) > 2 THEN
        CLS
        COLOR 15
        PRINT "Invalid entry."
        IF j > 4 THEN PRINT "Too many options on the command line."
        PRINT "Proper format is:"
        PRINT "TTYTALK {port} {Msg Dir} {Ramdrive} {Baud}"
        PRINT
        COLOR 7
        PRINT "The first paramater is the comm port connected to the TTY."
        PRINT
        PRINT "The second paramater is your message directory.  If you do not include this"
        PRINT "paramater or enter a '*' then the setting for {PWPLIB}+\MSG\ will be used."
        PRINT
        PRINT "The third paramater is optional, and is used to enable the use of the"
        PRINT "auto-downlink option which seperates messages at the 'NNNN'."
        PRINT "You can omit this option, or enter a '-' (dash) as a place holder."
        PRINT
        PRINT "The fourth paramater is optional.  It is the start-up baud rate for TTYTALK."
        PRINT "Omitting this option will default to 1200 baud."
        PRINT
        PRINT "Options can also be set by the DOS variable TTYTALK."
        PRINT "Example:  SET TTYTALK=2 C:\MTF\MSG\ E:\ 1200"
        PRINT "Adding this line to the AUTOEXEC.BAT file will save you having to send the"
        PRINT "paramaters every time you start TTYTALK."
        PRINT "Note: Command line options override the TTYTALK variable."
        PRINT
        PRINT "Press any key to exit.";
        DO: LOOP UNTIL INKEY$ <> ""
        CLS
        SYSTEM
    END IF
    SHELL "CD > DIR.TT"
    OPEN "DIR.TT" FOR INPUT AS #2
        LINE INPUT #2, TtyDir$
    CLOSE #2
    SHELL "DEL DIR.TT"
RETURN

GetMessageDir:
    a$ = ENVIRON$("PWPLIB")
    IF RIGHT$(a$, 1) <> "\" THEN a$ = a$ + "\"
    IF RIGHT$(a$, 4) <> "MSG\" THEN a$ = a$ + "MSG\"
    Option$(2) = a$
RETURN

Help:
    COM(CommPort) STOP
    COLOR 15
    IF POS(0) > 1 THEN PRINT
    PRINT "������������������������������������������������������������������������������͸"
    PRINT "�Press the Escape key to terminate communications program.                     �"
    PRINT "�Press F1 to display this help screen.                                         �"
    PRINT "�Press F2 to transfer a file from disk to teletype.                            �"
    PRINT "�";
    IF RamDrvAvailable = False THEN COLOR 8
    PRINT "Press F3 to toggle on/off the auto downlink file system.";
    COLOR 15
    PRINT TAB(80); "�"
    PRINT "�Press F4 to save a file from teletype to disk.  Press again to stop saving.   �"
    PRINT "�Press F5 to toggle Local Echo on/off.                                         �"
    PRINT "�Press F6 to toggle Echo to Printer on/off.                                    �"
    PRINT "�Press F7 to change speed to 75/300/1200 baud. (Default is 1200.)              �"
    PRINT "�";
    IF RamDrvAvailable = False THEN COLOR 8
    PRINT "Press F8 to process the auto downlink files.  (Auto-downlink must be OFF.)";
    COLOR 15
    PRINT TAB(80); "�"
    PRINT "�Press F9 to go to the DOS shell. (Type 'EXIT' to return to TTY Talk.)         �"
    PRINT "�Press F10 to reset the comm port if you stop receiving data.                  �"
    PRINT "�                                                                              �"
    PRINT "�Press HOME to clear the screen.                                               �"
    PRINT "�Press the ` (reverse apostrophe) to transmit a LTRS function.  (Use for LDR.) �"
    PRINT "�                                                                              �"
    PRINT "�All other keys will be transmitted to the teletype as applicable.             �"
    PRINT "������������������������������������������������������������������������������;"
    COLOR 7
    COM(CommPort) ON
RETURN

PrintStatus:
    COM(CommPort) STOP
    RcvX = POS(0)
    RcvY = CSRLIN
    VIEW PRINT
    LOCATE 3, 1
    COLOR 9
    IF RamDrvAvailable = True THEN
        PRINT "�(F3) Auto Downlink is ";
        IF Auto = True THEN
            COLOR 2
            PRINT "on. ";
        ELSE
            COLOR 4
                PRINT "off.";
        END IF
    ELSE
        PRINT "�(F3) Not available.       ";
    END IF
    COLOR 9
    PRINT " �(F4) File Saving is ";
    IF Sav = True THEN
        COLOR 2
        PRINT "on. ";
    ELSE
        COLOR 4
        PRINT "off.";
    END IF
    COLOR 9
    PRINT "  �(F5) Local echo is ";
    IF Echo = True THEN
        COLOR 2
        PRINT "on. ";
    ELSE
        COLOR 4
        PRINT "off.";
    END IF
    COLOR 9
    PRINT "�"
    PRINT "�(F6) Printer echo is ";
    IF Prn = True THEN
        COLOR 2
        PRINT "on. ";
    ELSE
        COLOR 4
        PRINT "off.";
    END IF
    COLOR 9
    PRINT "  �(F7) Baud rate:  ";
    COLOR 14
    PRINT RIGHT$("   " + Speed$, 4);
    COLOR 9
    IF RamDrvAvailable = True THEN
        PRINT "     �(F8) Process Downlink  �"
    ELSE
        PRINT "     �(F8) Not available.    �"
    END IF
    VIEW PRINT 6 TO ScreenLength
    COLOR 7
    LOCATE RcvY, RcvX
    COM(CommPort) ON
RETURN

ClearScreen:
    COM(CommPort) STOP
    VIEW PRINT
    CLS
    COLOR 11
    PRINT "TTYTALK v2.71   Press F1 for full key listing.   (C) 1995 -- RM2(SS) Ryan Grange"
    COLOR 9
    PRINT "������������������������������������������������������������������������������ķ"
    PRINT
    PRINT
    PRINT "������������������������������������������������������������������������������Ľ"
    COLOR 7
    VIEW PRINT 6 TO ScreenLength
    COM(CommPort) ON
    GOSUB PrintStatus
RETURN

SUB OpeningScreen
    CLS
    LOCATE 12, 30
    COLOR 15
    SlowType ("Teletype Emulator v2.71")
    LOCATE 24, 28
    COLOR 15
    PRINT "Press any key to continue.";
    DO: LOOP UNTIL INKEY$ <> ""
END SUB

SUB SlowType (a$)
    IF a$ <> "" THEN
        FOR i = 1 TO LEN(a$)
            PRINT MID$(a$, i, 1);
            t = TIMER
            DO: LOOP UNTIL t <> TIMER
        NEXT i
    END IF
END SUB
*/
