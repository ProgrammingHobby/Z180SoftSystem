unit System_Terminal;

{$mode objfpc}{$H+}

//***********************************************************************
//** unit System_Terminal                                              **
//**                                                                   **
//** Mein Dank gilt an dieser Stelle Ronald Daleske.                   **
//** http://www.projekte.daleske.de/                                   **
//** Projekt: Z80EMU                                                   **
//**                                                                   **
//** Die von Ronald Daleske geschriebene Terminalemulation diente      **
//** mir als Vorlage und Code-Basis für die Vorliegende Terminal Unit. **
//** Vielen Dank für die freundliche Unterstützung.                    **
//** Uwe Merker Juni 2020                                              **
//***********************************************************************

interface

uses SysUtils, Graphics, Classes;

const
    terminalColumns = 80;
    terminalRows = 24;
        {$ifdef Windows}
    terminalCharHeight = 22;
    terminalCharWidth = 10;
        {$else}
    terminalCharHeight = 22;
    terminalCharWidth = 11;
        {$endif}
    terminalStartLeft = -6;
    terminalStartTop = -18;

var
    terminalCharData: array[1..terminalRows, 1..terminalColumns] of char;
    terminalCharStyle: array[1..terminalRows, 1..terminalColumns] of TFontStyles;
    terminalCharColor: array[1..terminalRows, 1..terminalColumns] of TColor;
    terminalBackColor: array[1..terminalRows, 1..terminalColumns] of TColor;
    terminalDefaultCharColor, terminalDefaultBackColor: TColor;
    terminalFontColor, terminalBackgroundColor: TColor;
    terminalCursor: record
        column: integer;
        row: integer;
        cursorChar: char;
        Visible: boolean;
    end;

procedure terminalReset;
procedure setTerminalCrLF(enable: boolean);
procedure setTerminalLocalEcho(enable: boolean);
procedure setTerminalLogging(enable: boolean);
procedure writeTerminalCharacter(character: byte);
function readTerminalCharacter: byte;
function terminalReadable: boolean;
procedure getKeyBoardInput(key: word; shift: TShiftState);

implementation

type
    TTermMode = (STANDARD, VT52_ESC, ANSI_ESC, ANSI_ESC_PAR, DCA_ROW, DCA_COLUMN);

    TTerminalEmulation = class(TThread)
    private

    protected
        procedure Execute; override;

    public
        constructor Create(CreateSuspended: boolean);

    end;

var
    termMode: TTermMode;
    TerminalEmulation: TTerminalEmulation;
    enableCrLf: boolean;
    enableLocalEcho: boolean;
    keyboardBuffer: array[0..1023] of byte;
    keyboardReadIndex, keyboardWriteIndex: integer;
    characterBuffer: array[0..1023] of byte;
    characterReadIndex, characterWriteIndex: integer;
    fontStyle: TFontStyles;
    csiPar: array[1..8] of word;
    parCount: word;
    enableTerminalLogging: boolean;
    loggingFile: file of char;
    dcaRow: word;
    tmpFontColor: TColor;

// --------------------------------------------------------------------------------
procedure resetEscParameter;
var
    parIndex: integer;
begin
    for parIndex := 1 to 8 do begin
        csiPar[parIndex] := 0;
    end;
    parCount := 0;
end;

// --------------------------------------------------------------------------------
procedure terminalReset;
var
    row, column: integer;
begin
    for row := 1 to terminalRows do begin
        for column := 1 to terminalColumns do begin
            terminalCharData[row, column] := ' ';
            terminalCharColor[row, column] := terminalDefaultCharColor;
            terminalBackColor[row, column] := terminalDefaultBackColor;
            terminalCharStyle[row, column] := [];
        end;
    end;
    terminalCursor.column := 1;
    terminalCursor.row := 1;
    terminalCursor.cursorChar := '_';
    terminalCursor.Visible := True;
    fontStyle := [];
    terminalFontColor := terminalDefaultCharColor;
    terminalBackgroundColor := terminalDefaultBackColor;
    termMode := STANDARD;
    resetEscParameter;
end;

// --------------------------------------------------------------------------------
procedure setTerminalCrLF(enable: boolean);
begin
    enableCrLf := enable;
end;

// --------------------------------------------------------------------------------
procedure setTerminalLocalEcho(enable: boolean);
begin
    enableLocalEcho := enable;
end;

// --------------------------------------------------------------------------------
procedure setTerminalLogging(enable: boolean);
begin
    enableTerminalLogging := enable;
    if (enableTerminalLogging) then begin
        try
            Assign(loggingFile, 'Terminal.log');
            Rewrite(loggingFile);
        except
            enableTerminalLogging := False;
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure writeTerminalCharacter(character: byte);
begin
    if (character > $00) then begin
        characterBuffer[characterWriteIndex] := character;
        Inc(characterWriteIndex);
        if (characterWriteIndex > 1023) then
            characterWriteIndex := 0;
    end;
end;

// --------------------------------------------------------------------------------
function readTerminalCharacter: byte;
begin
    Result := keyboardBuffer[keyboardReadIndex];
    Inc(keyboardReadIndex);
    if (keyboardReadIndex > 1023) then
        keyboardReadIndex := 0;
end;

// --------------------------------------------------------------------------------
function terminalReadable: boolean;
begin
    if (keyboardReadIndex = keyboardWriteIndex) then begin
        Result := False;
    end
    else begin
        Result := True;
    end;
end;

// --------------------------------------------------------------------------------
procedure getKeyBoardInput(key: word; shift: TShiftState);
var
    character: byte;
begin
    character := $00;
    if (Shift = []) then begin
        case key of
            08: character := $7F;
            09: character := key; // TAB
            13: character := key; // ENTER
            27: character := $1B; // ESC
            32: character := $20; // SPACE
            33: character := $12; // Ctrl R
            34: character := $03; // Ctrl C
            37: character := $13; // links
            38: character := $05; // oben
            39: character := $04; // rechts
            40: character := $18; // unten
            45: character := $16; // Einfg = Ctrl V
            46: character := $07; // Entf = Ctrl G
            48..57: character := key; // 0..9
            65..90: character := key + 32; // a..z
            187: character := $2B; // +
            188: character := $2C; // ,
            189: character := $2D; // -
            190: character := $2E; // .
            111: character := $3A; // NUM :
            106: character := $2A; // NUM *
            109: character := $2D; // NUM -
            107: character := $2B; // NUM +
            96..105: character := key - 48; // NUM 0..9
            {$ifdef Windows}
            110: character := $2E; // NUM .
            191: character := $23; // #
            219: character := $73; // s
            220: character := $5E; // ^
            221: character := $27; // `
            226: character := $3C; // <
            {$else}
            108: character := $2E; // NUM .
            222: character := $23; // #
            220: character := $73; // s
            150: character := $5E; // ^
            146: character := $27; // `
            225: character := $3C; // <
            {$endif}
            else character := $00;
        end;
    end;

    if ((Shift = [ssShift]) and (key <> 16)) then begin
        case key of
            00: character := key;
            48: character := $3D; // =
            49: character := $21; // !
            50: character := $22; // "
            51: character := $23; // §
            52: character := $24; // $
            53: character := $25; // %
            54: character := $26; // &
            55: character := $2F; // /
            56: character := $28; // (
            57: character := $29; // )
            65..90: character := key; // A..Z
            187: character := $2A; // *
            188: character := $3B; // ;
            189: character := $5F; // _
            190: character := $3A; // :
            {$ifdef Windows}
            191: character := $27; // '
            219: character := $3F; // ?
            220: character := $7E; // ° -> ~
            221: character := $60; // `
            226: character := $3E; // >
            {$else}
            222: character := $27; // '
            220: character := $3F; // ?
            150: character := $7E; // ° -> ~
            146: character := $60; // `
            225: character := $3E; // >
            {$endif}
            else character := $00;
        end;
    end;

    if ((Shift = [ssCtrl]) and (key <> 17)) then begin
        if (key > 64) and (key < 91) then begin
            character := key - 64;
        end;
    end;

    if ((Shift = [ssAlt..ssCtrl]) and (key <> 18)) then begin
        case key of
            48: character := $7D; // }
            55: character := $7B; // {
            56: character := $5B; // [
            57: character := $5D; // ]
            81: character := $40; // @
            187: character := $7E; // ~
            {$ifdef Windows}
            219: character := $5C; // \
            226: character := $7C; // |
            {$else}
            220: character := $5C; // \
            225: character := $7C; // |
            {$endif}
            else character := $00;
        end;
    end;

    if character > $00 then begin
        keyboardBuffer[keyboardWriteIndex] := character;
        Inc(keyboardWriteIndex);
        if (keyboardWriteIndex > 1023) then
            keyboardWriteIndex := 0;
        if (enableLocalEcho) then begin
            writeTerminalCharacter(character);
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure scrollTerminalContentUp;
var
    column, row: integer;
begin
    for row := 1 to terminalRows - 1 do begin
        terminalCharData[row] := terminalCharData[row + 1];
        terminalCharColor[row] := terminalCharColor[row + 1];
        terminalCharStyle[row] := terminalCharStyle[row + 1];
    end;
    for column := 1 to terminalColumns do begin
        terminalCharData[terminalRows, column] := ' ';
        //terminalCharColor[terminalRows, column] := terminalDefaultCharColor;
        //terminalBackColor[terminalRows, column] := terminalDefaultBackColor;
        terminalCharStyle[terminalRows, column] := [];
    end;
    terminalCursor.column := 1;
    terminalCursor.row := terminalRows;
end;

// --------------------------------------------------------------------------------
procedure writeCharOnScreen(character: char);
begin
    terminalCharData[terminalCursor.row, terminalCursor.column] := character;
    terminalCharColor[terminalCursor.row, terminalCursor.column] := terminalFontColor;
    terminalBackColor[terminalCursor.row, terminalCursor.column] := terminalBackgroundColor;
    terminalCharStyle[terminalCursor.row, terminalCursor.column] := fontStyle;
    Inc(terminalCursor.column);
    if (terminalCursor.column > terminalColumns) then begin
        terminalCursor.column := 1;
        Inc(terminalCursor.row);
        if (terminalCursor.row > terminalRows) then begin
            scrollTerminalContentUp;
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure cursorHome;
begin
    terminalCursor.column := 1;
    terminalCursor.row := 1;
end;

// --------------------------------------------------------------------------------
procedure cursorLeft;
begin
    if (terminalCursor.column > 1) then begin
        Dec(terminalCursor.column);
    end;
end;

// --------------------------------------------------------------------------------
procedure cursorRight;
begin
    if (terminalCursor.column < terminalColumns) then begin
        Inc(terminalCursor.column);
    end;
end;

// --------------------------------------------------------------------------------
procedure cursorUp;
begin
    if (terminalCursor.row > 1) then begin
        Dec(terminalCursor.row);
    end;
end;

// --------------------------------------------------------------------------------
procedure cursorDown;
begin
    if (terminalCursor.row < terminalRows) then begin
        Inc(terminalCursor.row);
    end;
end;

// --------------------------------------------------------------------------------
procedure backspace;
begin
    if (terminalCursor.column > 1) then begin
        Dec(terminalCursor.column);
        terminalCharData[terminalCursor.row, terminalCursor.column] := ' ';
        //terminalCharColor[terminalCursor.row, terminalCursor.column] := terminalDefaultCharColor;
        //terminalBackColor[terminalCursor.row, terminalCursor.column] := terminalDefaultBackColor;
        terminalCharStyle[terminalCursor.row, terminalCursor.column] := [];
    end;
end;

// --------------------------------------------------------------------------------
procedure setTabulator;
begin
    terminalCursor.column := (8 * ((terminalCursor.column div 8) + 1));
    if (terminalCursor.column > terminalColumns) then begin
        terminalCursor.column := 1;
    end;
end;

// --------------------------------------------------------------------------------
procedure lineFeed;
begin
    Inc(terminalCursor.row);
    if (terminalCursor.row > terminalRows) then begin
        scrollTerminalContentUp;
    end;
end;

// --------------------------------------------------------------------------------
procedure clearScreen;
var
    row, column: integer;
begin
    for row := 1 to terminalRows do begin
        for column := 1 to terminalColumns do begin
            terminalCharData[row, column] := ' ';
            //terminalCharColor[row, column] := terminalDefaultCharColor;
            //terminalBackColor[row, column] := terminalDefaultBackColor;
            terminalCharStyle[row, column] := [];
        end;
    end;
    terminalCursor.column := 1;
    terminalCursor.row := 1;
    dcaRow := 0;
end;

// --------------------------------------------------------------------------------
procedure carriageReturn;
begin
    terminalCursor.column := 1;
end;

// --------------------------------------------------------------------------------
procedure deleteEndOfLine;
var
    column: integer;
begin
    for column := terminalCursor.column to terminalColumns do begin
        terminalCharData[terminalCursor.row, column] := ' ';
        //terminalCharColor[terminalCursor.row, column] := terminalDefaultCharColor;
        //terminalBackColor[terminalCursor.row, column] := terminalDefaultBackColor;
        terminalCharStyle[terminalCursor.row, column] := [];
    end;
end;

// --------------------------------------------------------------------------------
procedure insertLineAndScroll;
var
    column, row: integer;
begin
    for row := terminalRows downto terminalCursor.row + 1 do begin
        terminalCharData[row] := terminalCharData[row - 1];
        terminalCharColor[row] := terminalCharColor[row - 1];
        terminalCharStyle[row] := terminalCharStyle[row - 1];
    end;
    for column := 1 to terminalColumns do begin
        terminalCharData[terminalCursor.row, column] := ' ';
        //terminalCharColor[terminalCursor.row, column] := terminalDefaultCharColor;
        //terminalBackColor[terminalCursor.row, column] := terminalDefaultBackColor;
        terminalCharStyle[terminalCursor.row, column] := [];
    end;
end;

// --------------------------------------------------------------------------------
procedure reverseLineFeed;
begin
    if (terminalCursor.row > 1) then begin
        Dec(terminalCursor.row);
    end
    else begin
        insertLineAndScroll;
    end;
end;

// --------------------------------------------------------------------------------
procedure deleteEndOfScreen;
var
    row, column: integer;
begin
    deleteEndOfLine;
    if terminalCursor.row < terminalRows then begin
        for row := terminalCursor.row + 1 to terminalRows do begin
            for column := 1 to terminalColumns do begin
                terminalCharData[row, column] := ' ';
                //terminalCharColor[row, column] := terminalDefaultCharColor;
                //terminalBackColor[row, column] := terminalDefaultBackColor;
                terminalCharStyle[row, column] := [];
            end;
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure deleteLineAndScroll;
var
    column, row: integer;
begin
    for row := terminalCursor.row to terminalRows - 1 do begin
        terminalCharData[row] := terminalCharData[row + 1];
        terminalCharColor[row] := terminalCharColor[row + 1];
        terminalCharStyle[row] := terminalCharStyle[row + 1];
    end;
    for column := 1 to terminalColumns do begin
        terminalCharData[terminalRows, column] := ' ';
        //terminalCharColor[terminalRows, column] := terminalDefaultCharColor;
        //terminalBackColor[terminalRows, column] := terminalDefaultBackColor;
        terminalCharStyle[terminalRows, column] := [];
    end;
    terminalCursor.column := 1;
end;

// --------------------------------------------------------------------------------
procedure setCursorPosition(row, column: integer);
begin
    terminalCursor.row := row;
    terminalCursor.column := column;
    if (terminalCursor.row > terminalRows) then begin
        terminalCursor.row := terminalRows;
    end;
    if (terminalCursor.row < 1) then begin
        terminalCursor.row := 1;
    end;
    if (terminalCursor.column > terminalColumns) then begin
        terminalCursor.column := terminalColumns;
    end;
    if (terminalCursor.column < 1) then begin
        terminalCursor.column := 1;
    end;
end;

// --------------------------------------------------------------------------------
procedure deleteBeginningOfLine;
var
    column: integer;
begin
    for column := 1 to terminalCursor.column - 1 do begin
        terminalCharData[terminalCursor.row, column] := ' ';
        //terminalCharColor[terminalCursor.row, column] := terminalDefaultCharColor;
        //terminalBackColor[terminalCursor.row, column] := terminalDefaultBackColor;
        terminalCharStyle[terminalCursor.row, column] := [];
    end;
end;

// --------------------------------------------------------------------------------
procedure deleteLine;
var
    column: integer;
begin
    for column := 1 to terminalColumns do begin
        terminalCharData[terminalCursor.row, column] := ' ';
        //terminalCharColor[terminalCursor.row, column] := terminalDefaultCharColor;
        //terminalBackColor[terminalCursor.row, column] := terminalDefaultBackColor;
        terminalCharStyle[terminalCursor.row, column] := [];
    end;
    terminalCursor.column := 1;
end;

// --------------------------------------------------------------------------------
procedure deleteBeginningOfScreen;
var
    row, column: integer;
begin
    deleteBeginningOfLine;
    if terminalCursor.row > 1 then begin
        for row := 1 to terminalCursor.row - 1 do begin
            for column := 1 to terminalColumns do begin
                terminalCharData[row, column] := ' ';
                //terminalCharColor[row, column] := terminalDefaultCharColor;
                //terminalBackColor[row, column] := terminalDefaultBackColor;
                terminalCharStyle[row, column] := [];
            end;
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure eraseScreen;
var
    row, column: integer;
begin
    for row := 1 to terminalRows do begin
        for column := 1 to terminalColumns do begin
            terminalCharData[row, column] := ' ';
            //terminalCharColor[row, column] := terminalDefaultCharColor;
            //terminalBackColor[row, column] := terminalDefaultBackColor;
            terminalCharStyle[row, column] := [];
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure TTerminalEmulation.Execute;
var
    character: byte;

    // ----------------------------------------
    procedure normalTerminalMode;
    begin
        case (character) of
            $01: begin
                cursorHome;
            end;
            $04: begin
                cursorRight;
            end;
            $05: begin
                cursorUp;
            end;
            $07: begin
                // bell;
            end;
            $08: begin
                backspace;
            end;
            $09: begin
                setTabulator;
            end;
            $0A: begin
                lineFeed;
            end;
            $0C: begin
                clearScreen;
            end;
            $0D: begin
                carriageReturn;
                if (enableCrLf) then begin
                    lineFeed;
                end;
            end;
            $13: begin
                cursorLeft;
            end;
            $16: begin
                deleteEndOfLine;
            end;
            $18: begin
                cursorDown;
            end;
            $1B: begin
                termMode := VT52_ESC;
            end;
            $20..$7E: begin
                writeCharOnScreen(chr(character));
            end;
            $7F: begin
                backspace;
            end;
        end;
    end;

    // ----------------------------------------
    procedure vt52EscapeMode;
    begin
        case (character) of
            $3C: begin  // ESC < (Enter ANSI Mode)
                // ANSI-Mode ist immer aktiv.
            end;
            $41: begin  // ESC A (Cursor up)
                cursorUp;
                termMode := STANDARD;
            end;
            $42: begin  // ESC B (Cursor down)
                cursorDown;
                termMode := STANDARD;
            end;
            $43: begin  // ESC C (Cursor right)
                cursorRight;
                termMode := STANDARD;
            end;
            $44: begin  // ESC D (Cursor left)
                cursorLeft;
                termMode := STANDARD;
            end;
            $48: begin  // ESC H (Cursor home)
                cursorHome;
                termMode := STANDARD;
            end;
            $49: begin  // ESC I (Reverse line feed)
                reverseLineFeed;
                termMode := STANDARD;
            end;
            $4A: begin  // ESC J (Erase to end of Screen)
                deleteEndOfScreen;
                termMode := STANDARD;
            end;
            $4B: begin  // ESC K (Erase to end of Line)
                deleteEndOfLine;
                termMode := STANDARD;
            end;
            $4C: begin  // ESC L (Insert line)
                insertLineAndScroll;
                termMode := STANDARD;
            end;
            $4D: begin  // ESC M (Remove Line)
                deleteLineAndScroll;
                termMode := STANDARD;
            end;
            $59: begin  // ESC Y (Direct Cursor address)
                termMode := DCA_ROW;
            end;
            $5A: begin  // ESC Z (Identify VT52 Mode)
                keyboardBuffer[keyboardWriteIndex] := $1B;
                Inc(keyboardWriteIndex);
                if (keyboardWriteIndex > 1023) then
                    keyboardWriteIndex := 0;
                keyboardBuffer[keyboardWriteIndex] := $2F;
                Inc(keyboardWriteIndex);
                if (keyboardWriteIndex > 1023) then
                    keyboardWriteIndex := 0;
                keyboardBuffer[keyboardWriteIndex] := $5A;
                Inc(keyboardWriteIndex);
                if (keyboardWriteIndex > 1023) then
                    keyboardWriteIndex := 0;
                termMode := STANDARD;
            end;
            $5B: begin  // ESC [ (ESC Control Sequenz)
                termMode := ANSI_ESC;
            end;
            else termMode := STANDARD;
        end;
    end;

    // ----------------------------------------
    procedure ansiEscapeModeParameter;
    var
        csiCounter: integer;
    begin
        case (character) of
            $30..$39: begin
                if (parCount < 9) then begin // maximal 8 numerische Parameter möglich
                    csiPar[parCount] := (csiPar[parCount] * 10) + (character - $30);
                end;
            end;
            $3B: begin // ESC [ Pn1 ; (weiteren Parameter abfragen)
                if (parCount < 9) then begin // maximal 8 numerische Parameter möglich
                    Inc(parCount);
                end;
            end;
            $41: begin // ESC [ Pn A (Cursor up Pn lines)
                if (parCount = 1) then begin
                    if terminalCursor.row > csiPar[1] then begin
                        terminalCursor.row := terminalCursor.row - csiPar[1];
                    end
                    else begin
                        terminalCursor.row := 1;
                    end;
                end;
                resetEscParameter;
                termMode := STANDARD;
            end;
            $42: begin // ESC [ Pn B (Cursor down Pn lines)
                if (parCount = 1) then begin
                    if (terminalCursor.row + csiPar[1]) < terminalRows then begin
                        terminalCursor.row := terminalCursor.row + csiPar[1];
                    end
                    else begin
                        terminalCursor.row := terminalRows;
                    end;
                end;
                resetEscParameter;
                termMode := STANDARD;
            end;
            $43: begin // ESC [ Pn C (Cursor right Pn columns)
                if (parCount = 1) then begin
                    if (terminalCursor.column + csiPar[1]) < terminalColumns then begin
                        terminalCursor.column := terminalCursor.column + csiPar[1];
                    end
                    else begin
                        terminalCursor.column := terminalColumns;
                    end;
                end;
                resetEscParameter;
                termMode := STANDARD;
            end;
            $44: begin // ESC [ Pn D (Cursor left Pn columns)
                if (parCount = 1) then begin
                    if terminalCursor.column > csiPar[1] then begin
                        terminalCursor.column := terminalCursor.column - csiPar[1];
                    end
                    else begin
                        terminalCursor.column := 1;
                    end;
                end;
                resetEscParameter;
                termMode := STANDARD;
            end;
            $48, $66: begin // ESC [ Pn1 ; Pn2 H , ESC [ Pn1 ; Pn2 f (Move cursor to line Pn1 and column Pn2)
                if (parCount = 2) then begin
                    setCursorPosition(csiPar[1], csiPar[2]);
                    resetEscParameter;
                    termMode := STANDARD;
                end;
            end;
            $4B: begin // ESC [ Pn K
                if (parCount = 1) then begin
                    case csiPar[1] of
                        0: deleteEndOfLine;  // Erase line from cursor to end
                        1: deleteBeginningOfLine;  // Erase from beginning of line to cursor
                        2: deleteLine;  // Erase entire line but do not move cursor
                    end;
                end;
                resetEscParameter;
                termMode := STANDARD;
            end;
            $4A: begin // ESC [ Pn J
                if (parCount = 1) then begin
                    case csiPar[1] of
                        0: deleteEndOfScreen;  // Erase screen from cursor to end
                        1: deleteBeginningOfScreen;  // Erase beginning of screen to cursor
                        2: eraseScreen;  // Erase entire screenbut do not move cursor
                    end;
                end;
                resetEscParameter;
                termMode := STANDARD;
            end;
            $4C: begin // ESC [ Pn L (Insert Pn lines from cursor position)
                if (parCount = 1) then begin
                    for csiCounter := 0 to csiPar[1] - 1 do begin
                        insertLineAndScroll;
                    end;
                end;
                resetEscParameter;
                termMode := STANDARD;
            end;
            $4D: begin // ESC [ Pn M (Delete Pn lines from cursor position)
                if (parCount = 1) then begin
                    for csiCounter := 0 to csiPar[1] - 1 do begin
                        deleteLineAndScroll;
                    end;
                end;
                resetEscParameter;
                termMode := STANDARD;
            end;
            $6D: begin // ESC [ Pn (; Pn ...) m
                for csiCounter := 1 to parCount do begin
                    case csiPar[csiCounter] of
                        0: begin
                            fontStyle := [];
                            terminalFontColor := terminalDefaultCharColor;
                            terminalBackgroundColor := terminalDefaultBackColor;
                        end;
                        1: begin
                            fontStyle := fontStyle + [fsBold];
                        end;
                        4: begin
                            fontStyle := fontStyle + [fsUnderline];
                        end;
                        5: begin
                            fontStyle := fontStyle + [fsItalic];
                        end;
                        7: begin
                            fontStyle := fontStyle + [fsBold];
                            tmpFontColor := terminalFontColor;
                            terminalFontColor := clGray;
                        end;
                        22: begin
                            fontStyle := fontStyle - [fsBold];
                        end;
                        24: begin
                            fontStyle := fontStyle - [fsUnderline];
                        end;
                        25: begin
                            fontStyle := fontStyle - [fsItalic];
                        end;
                        27: begin
                            fontStyle := fontStyle - [fsBold];
                            terminalFontColor := tmpFontColor;
                        end;
                        30: begin
                            terminalFontColor := clBlack;
                        end;
                        31: begin
                            terminalFontColor := clRed;
                        end;
                        32: begin
                            terminalFontColor := clLime;
                        end;
                        33: begin
                            terminalFontColor := clYellow;
                        end;
                        34: begin
                            terminalFontColor := clBlue;
                        end;
                        35: begin
                            terminalFontColor := $FF00FF;
                        end;
                        36: begin
                            terminalFontColor := $00FFFF;
                        end;
                        37: begin
                            terminalFontColor := clWhite;
                        end;
                        40: begin
                            terminalBackgroundColor := clBlack;
                        end;
                        41: begin
                            terminalBackgroundColor := clRed;
                        end;
                        42: begin
                            terminalBackgroundColor := clLime;
                        end;
                        43: begin
                            terminalBackgroundColor := clYellow;
                        end;
                        44: begin
                            terminalBackgroundColor := clBlue;
                        end;
                        45: begin
                            terminalBackgroundColor := $FF00FF;
                        end;
                        46: begin
                            terminalBackgroundColor := $00FFFF;
                        end;
                        47: begin
                            terminalBackgroundColor := clWhite;
                        end;
                    end;
                end;
                resetEscParameter;
                termMode := STANDARD;
            end;
        end;
    end;

    // ----------------------------------------
    procedure ansiEscapeMode;
    begin
        case (character) of
            $30..$39: begin
                Inc(parCount);
                ansiEscapeModeParameter;
                termMode := ANSI_ESC_PAR;
            end;
            $41: begin  // ESC [ A (Cursor up one line)
                cursorUp;
                termMode := STANDARD;
            end;
            $42: begin  // ESC [ B (Cursor down one line)
                cursorDown;
                termMode := STANDARD;
            end;
            $43: begin  // ESC [ C (Cursor right one column)
                cursorRight;
                termMode := STANDARD;
            end;
            $44: begin  // ESC [ D (Cursor left one column)
                cursorLeft;
                termMode := STANDARD;
            end;
            $48: begin  // ESC [ H (Cursor home)
                cursorHome;
                termMode := STANDARD;
            end;
            $4A: begin  // ESC [ J (Erase Screen from cursor to end)
                deleteEndOfScreen;
                termMode := STANDARD;
            end;
            $4B: begin  // ESC [ K (Erase line from cursor to end)
                deleteEndOfLine;
                termMode := STANDARD;
            end;
            $4C: begin  // ESC [ L (Insert one line from cursor position)
                insertLineAndScroll;
                termMode := STANDARD;
            end;
            $4D: begin  // ESC [ M (Delete one line from cursor position)
                deleteLineAndScroll;
                termMode := STANDARD;
            end;
            $6D: begin  // ESC [ m (Clear all character attributes)
                fontStyle := [];
                terminalFontColor := terminalDefaultCharColor;
                terminalBackgroundColor := terminalDefaultBackColor;
                termMode := STANDARD;
            end;
            $66: begin  // ESC [ f (Cursor home)
                cursorHome;
                termMode := STANDARD;
            end;
            else begin
                termMode := STANDARD;
            end;
        end;
    end;

    // ----------------------------------------
begin
    repeat

        if (characterReadIndex = characterWriteIndex) then begin
            Sleep(50);
        end
        else begin
            character := characterBuffer[characterReadIndex];
            Inc(characterReadIndex);
            if (characterReadIndex > 1023) then
                characterReadIndex := 0;

            case termMode of
                STANDARD: normalTerminalMode;
                VT52_ESC: vt52EscapeMode;
                ANSI_ESC: ansiEscapeMode;
                ANSI_ESC_PAR: ansiEscapeModeParameter;
                DCA_ROW: begin
                    if (character >= $20) then begin
                        dcaRow := character - $20;
                        termMode := DCA_COLUMN;
                    end
                    else begin
                        termMode := STANDARD;
                    end;
                end;
                DCA_COLUMN: begin
                    if (character >= $20) then begin
                        setCursorPosition(dcaRow, character - $20);
                    end;
                    termMode := STANDARD;
                end;
            end;

            if (enableTerminalLogging) then begin
                Write(loggingFile, chr(character));
            end;

        end;

    until (Terminated);

end;

// --------------------------------------------------------------------------------
constructor TTerminalEmulation.Create(CreateSuspended: boolean);
begin
    FreeOnTerminate := False;
    inherited Create(CreateSuspended);
end;

// --------------------------------------------------------------------------------
initialization
    begin
        enableCrLf := False;
        enableLocalEcho := False;
        setTerminalLogging(False);
        terminalReset;
        characterReadIndex := 0;
        characterWriteIndex := 0;
        keyboardReadIndex := 0;
        keyboardWriteIndex := 0;
        TerminalEmulation := TTerminalEmulation.Create(False);
    end;

    // --------------------------------------------------------------------------------
finalization
    begin
        TerminalEmulation.Terminate;
        TerminalEmulation.Free;
    end;

    // --------------------------------------------------------------------------------
end.


