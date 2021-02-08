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

uses
    Classes, SysUtils, Controls, ExtCtrls, Graphics;

type

    { TSystemTerminal }

    TSystemTerminal = class(TThread)

    private   // Attribute

        type
        TTermMode = (STANDARD, VT52_ESC, ANSI_ESC, ANSI_ESC_PAR, DCA_ROW, DCA_COLUMN);

    const
        terminalColumns = 80;
        terminalRows = 30;
        {$ifdef Windows}
        charHeight = 20;
        charWidth = 9;
        startLeft = -7;  // -10 + 3
        startTop = -17;   // -20 + 3
        {$else}
        charHeight = 19;
        charWidth = 10;
        startLeft = -7;  // -10 + 3
        startTop = -16;   // -19 + 3
        {$endif}
        maxKeyboardBuffer = 16;
        maxCharacterBuffer = 1024;

    var
        imagePage: TImage;
        timerTerminalPageRefresh: TTimer;
        timerFlash: TTimer;
        charData: array[1..terminalRows, 1..terminalColumns] of char;
        charStyle: array[1..terminalRows, 1..terminalColumns] of TFontStyles;
        charColor: array[1..terminalRows, 1..terminalColumns] of TColor;
        backColor: array[1..terminalRows, 1..terminalColumns] of TColor;
        terminalCursor: record
            column: integer;
            row: integer;
            cursorChar: char;
        end;
        curPosSave: record
            column: integer;
            row: integer;
            style: TFontStyles;
            charCol: TColor;
            backCol: TColor;
        end;
        keyboardBuffer: array[1..maxKeyboardBuffer] of byte;
        keyboardReadIndex, keyboardWriteIndex: integer;
        characterBuffer: array[1..maxCharacterBuffer] of byte;
        characterReadIndex, characterWriteIndex: integer;
        enableCrLf: boolean;
        enableLocalEcho: boolean;
        enableTerminalLogging: boolean;
        loggingFile: file of char;
        fontColor, backgroundColor: TColor;
        fontStyle: TFontStyles;
        termMode: TTermMode;
        dcaRow: word;
        csiPar: array[1..8] of word;
        parCount: word;
        defaultCharColor, defaultBackColor: TColor;
        posVisible: boolean;
        monochromTerminal: boolean;

    protected // Attribute
        procedure timerFlashTimer(Sender: TObject);
        procedure timerTerminalPageRefreshTimer(Sender: TObject);

    public    // Attribute

    public  // Konstruktor/Destruktor
        constructor Create(terminalPanel: TPanel; CreateSuspended: boolean); overload;
        destructor Destroy; override;

    private   // Methoden
        procedure writeCharOnScreen(character: char);
        procedure writeKeyboardBuffer(character: byte);
        procedure scrollTerminalContentUp;
        procedure cursorHome;
        procedure cursorLeft;
        procedure cursorRight;
        procedure cursorUp;
        procedure cursorDown;
        procedure backspace;
        procedure setTabulator;
        procedure lineFeed;
        procedure clearScreen;
        procedure eraseScreen;
        procedure carriageReturn;
        procedure deleteEndOfLine;
        procedure deleteEndOfScreen;
        procedure deleteBeginningOfLine;
        procedure deleteBeginningOfScreen;
        procedure deleteLine;
        procedure deleteLineAndScroll;
        procedure insertLineAndScroll;
        procedure reverseLineFeed;
        procedure setCursorPosition(row, column: integer);
        procedure sendCursorPosition;
        procedure sendTerminalOk;
        procedure sendWhatAreYou;
        procedure resetEscParameter;

    protected // Methoden
        procedure Execute; override;

    public    // Methoden
        procedure terminalReset;
        procedure setCrLF(enable: boolean);
        procedure setLocalEcho(enable: boolean);
        procedure setLogging(enable: boolean);
        procedure setColorType(colorType: integer);
        procedure writeCharacter(character: byte);
        function readCharacter: byte;
        function terminalReadable: boolean;
        procedure getKeyBoardInput(key: word; shift: TShiftState);

    end;

var
    SystemTerminal: TSystemTerminal;

implementation

{ TSystemTerminal }

// --------------------------------------------------------------------------------
procedure TSystemTerminal.timerFlashTimer(Sender: TObject);
begin
    posVisible := (not posVisible);
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.timerTerminalPageRefreshTimer(Sender: TObject);
var
    row, column, posX, posY: integer;
    viewChar: char;
    viewStyle: TFontStyles;
    charCol, backCol, tmpCol: TColor;
begin
    for row := 1 to terminalRows do begin
        posY := startTop + (charHeight * row);
        for column := 1 to terminalColumns do begin
            viewStyle := charStyle[row, column];
            charCol := charColor[row, column];
            backCol := backColor[row, column];
            if (posVisible and (row = terminalCursor.row) and (column = terminalCursor.column)) then begin
                viewchar := terminalCursor.cursorChar;
                viewStyle := [fsBold];
            end
            else begin
                viewchar := charData[row, column];
            end;
            if (fsItalic in viewStyle) then begin
                viewStyle := viewStyle - [fsItalic];
                if (not posVisible) then begin
                    viewChar := ' ';
                end;
            end;
            if (monochromTerminal) then begin
                charCol := defaultCharColor;
                backCol := defaultBackColor;
            end;
            if (fsStrikeOut in viewStyle) then begin
                viewStyle := viewStyle - [fsStrikeOut];
                tmpCol := charCol;
                charCol := backCol;
                backCol := tmpCol;
            end;
            {$ifdef Windows}
            posX := startLeft + ((charWidth + 2) * column);
            {$else}
            posX := startLeft + (charWidth * column);
            {$endif}
            imagePage.Canvas.Brush.Color := backCol;
            imagePage.Canvas.Font.Color := charCol;
            imagePage.Canvas.Font.Style := viewStyle;
            imagePage.Canvas.TextOut(posX, posY, viewChar);
        end;
    end;
end;

// --------------------------------------------------------------------------------
constructor TSystemTerminal.Create(terminalPanel: TPanel; CreateSuspended: boolean);
var
    pageWidth, pageHeight: integer;
begin
    {$ifdef Windows}
    terminalPanel.Font.Name := 'Consolas';
    terminalPanel.Font.Size := 13;
    pageWidth := ((charWidth + 2) * terminalColumns) + charWidth;
    {$else}
    terminalPanel.Font.Name := 'Monospace';
    terminalPanel.Font.Size := 12;
    pageWidth := (charWidth * terminalColumns) + charWidth;
    {$endif}
    pageHeight := (charHeight * terminalRows) + charHeight;

    imagePage := TImage.Create(terminalPanel);
    with (imagePage) do begin
        SetBounds(0, 0, pageWidth, pageHeight);
        Canvas.Font := terminalPanel.Font;
        Parent := terminalPanel;
        Enabled := True;
        Visible := True;
    end;

    timerFlash := TTimer.Create(terminalPanel);
    timerFlash.Interval := 500;
    timerFlash.OnTimer := @timerFlashTimer;
    timerFlash.Enabled := False;

    timerTerminalPageRefresh := TTimer.Create(terminalPanel);
    {$ifdef Windows}
    timerTerminalPageRefresh.Interval := 20;
    {$else}
    timerTerminalPageRefresh.Interval := 50;
    {$endif}
    timerTerminalPageRefresh.OnTimer := @timerTerminalPageRefreshTimer;
    timerTerminalPageRefresh.Enabled := False;

    enableCrLf := False;
    enableLocalEcho := False;
    monochromTerminal := False;
    setLogging(False);
    terminalReset;

    FreeOnTerminate := False;
    inherited Create(CreateSuspended);
end;
// --------------------------------------------------------------------------------
destructor TSystemTerminal.Destroy;
begin
    timerFlash.Enabled := False;
    timerFlash.OnTimer := nil;
    timerTerminalPageRefresh.Enabled := False;
    timerTerminalPageRefresh.OnTimer := nil;
    if (enableTerminalLogging) then begin
        CloseFile(loggingFile);
    end;
    Terminate;
    inherited Destroy;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.terminalReset;
var
    row, column: integer;
begin
    for row := 1 to terminalRows do begin
        for column := 1 to terminalColumns do begin
            charData[row, column] := ' ';
            charColor[row, column] := defaultCharColor;
            backColor[row, column] := defaultBackColor;
            charStyle[row, column] := [];
        end;
    end;
    terminalCursor.column := 1;
    terminalCursor.row := 1;
    terminalCursor.cursorChar := '_';
    timerFlash.Enabled := True;
    timerTerminalPageRefresh.Enabled := True;
    fontStyle := [];
    fontColor := defaultCharColor;
    backgroundColor := defaultBackColor;
    termMode := STANDARD;
    characterReadIndex := 1;
    characterWriteIndex := 1;
    keyboardReadIndex := 1;
    keyboardWriteIndex := 1;
    posVisible := True;
    resetEscParameter;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.writeCharOnScreen(character: char);
begin
    if (terminalCursor.column > terminalColumns) then begin
        terminalCursor.column := 1;
        Inc(terminalCursor.row);
        if (terminalCursor.row > terminalRows) then begin
            scrollTerminalContentUp;
        end;
    end;
    charData[terminalCursor.row, terminalCursor.column] := character;
    charColor[terminalCursor.row, terminalCursor.column] := fontColor;
    backColor[terminalCursor.row, terminalCursor.column] := backgroundColor;
    charStyle[terminalCursor.row, terminalCursor.column] := fontStyle;
    Inc(terminalCursor.column);
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.writeKeyboardBuffer(character: byte);
begin
    keyboardBuffer[keyboardWriteIndex] := character;
    Inc(keyboardWriteIndex);
    if (keyboardWriteIndex > maxKeyboardBuffer) then begin
        keyboardWriteIndex := 1;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.Execute;
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
            $37: begin  // ESC 7 (Save cursor and attributes)
                curPosSave.column := terminalCursor.column;
                curPosSave.row := terminalCursor.row;
                curPosSave.style := fontStyle;
                curPosSave.charCol := fontColor;
                curPosSave.backCol := backgroundColor;
                termMode := STANDARD;
            end;
            $38: begin  // ESC 8 (Restore cursor and attributes)
                terminalCursor.column := curPosSave.column;
                terminalCursor.row := curPosSave.row;
                fontStyle := curPosSave.style;
                fontColor := curPosSave.charCol;
                backgroundColor := curPosSave.backCol;
                termMode := STANDARD;
            end;
            $3C: begin  // ESC < (Enter ANSI Mode)
                // ANSI-Mode ist immer aktiv.
                termMode := STANDARD;
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
                writeKeyboardBuffer($1B);
                writeKeyboardBuffer($2F);
                writeKeyboardBuffer($5A);
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
            $63: begin // ESC [ Pn c (What are you?)
                if (parCount = 1) then begin
                    case (csiPar[1]) of
                        0: begin
                            sendWhatAreYou;
                        end;
                    end;
                    resetEscParameter;
                    termMode := STANDARD;
                end;
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
                end;
                resetEscParameter;
                termMode := STANDARD;
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
            $68: begin // ESC [ ? Pn h (Derzeit nicht unterstützt)
                resetEscParameter;
                termMode := STANDARD;
            end;
            $6D: begin // ESC [ Pn (; Pn ...) m
                for csiCounter := 1 to parCount do begin
                    case csiPar[csiCounter] of
                        0: begin
                            fontStyle := [];
                            fontColor := defaultCharColor;
                            backgroundColor := defaultBackColor;
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
                            fontStyle := fontStyle + [fsStrikeOut];
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
                            fontStyle := fontStyle - [fsStrikeOut];
                        end;
                        30: begin
                            fontColor := clBlack;
                        end;
                        31: begin
                            fontColor := clRed;
                        end;
                        32: begin
                            fontColor := clLime;
                        end;
                        33: begin
                            fontColor := clYellow;
                        end;
                        34: begin
                            fontColor := clBlue;
                        end;
                        35: begin
                            fontColor := $FF00FF;
                        end;
                        36: begin
                            fontColor := $FFFF00;
                        end;
                        37: begin
                            fontColor := clWhite;
                        end;
                        40: begin
                            backgroundColor := clBlack;
                        end;
                        41: begin
                            backgroundColor := clRed;
                        end;
                        42: begin
                            backgroundColor := clLime;
                        end;
                        43: begin
                            backgroundColor := clYellow;
                        end;
                        44: begin
                            backgroundColor := clBlue;
                        end;
                        45: begin
                            backgroundColor := $FF00FF;
                        end;
                        46: begin
                            backgroundColor := $FFFF00;
                        end;
                        47: begin
                            backgroundColor := clWhite;
                        end;
                    end;
                end;
                resetEscParameter;
                termMode := STANDARD;
            end;
            $6E: begin  // ESC [ Pn n
                if (parCount = 1) then begin
                    case csiPar[1] of
                        5: begin
                            sendTerminalOk;
                        end;
                        6: begin
                            sendCursorPosition;
                        end;
                    end;
                end;
                resetEscParameter;
                termMode := STANDARD;
            end;
            else begin
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
            $3F: begin  // ESC [ ? 
                Inc(parCount);
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
                fontColor := defaultCharColor;
                backgroundColor := defaultBackColor;
                termMode := STANDARD;
            end;
            $66: begin  // ESC [ f (Cursor home)
                cursorHome;
                termMode := STANDARD;
            end;
            $63: begin // ESC [ c  (What are you?)
                sendWhatAreYou;
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
            Sleep(1);
        end
        else begin
            character := characterBuffer[characterReadIndex];
            Inc(characterReadIndex);
            if (characterReadIndex > maxCharacterBuffer) then begin
                characterReadIndex := 1;
            end;

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
procedure TSystemTerminal.scrollTerminalContentUp;
var
    column, row: integer;
begin
    for row := 1 to terminalRows - 1 do begin
        charData[row] := charData[row + 1];
        charColor[row] := charColor[row + 1];
        backColor[row] := backColor[row + 1];
        charStyle[row] := charStyle[row + 1];
    end;
    for column := 1 to terminalColumns do begin
        charData[terminalRows, column] := ' ';
        charColor[terminalRows, column] := defaultCharColor;
        backColor[terminalRows, column] := defaultBackColor;
        charStyle[terminalRows, column] := [];
    end;
    terminalCursor.column := 1;
    terminalCursor.row := terminalRows;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.cursorHome;
begin
    terminalCursor.column := 1;
    terminalCursor.row := 1;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.cursorLeft;
begin
    if (terminalCursor.column > 1) then begin
        Dec(terminalCursor.column);
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.cursorRight;
begin
    if (terminalCursor.column < terminalColumns) then begin
        Inc(terminalCursor.column);
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.cursorUp;
begin
    if (terminalCursor.row > 1) then begin
        Dec(terminalCursor.row);
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.cursorDown;
begin
    if (terminalCursor.row < terminalRows) then begin
        Inc(terminalCursor.row);
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.backspace;
begin
    if (terminalCursor.column > 1) then begin
        Dec(terminalCursor.column);
        charData[terminalCursor.row, terminalCursor.column] := ' ';
        charColor[terminalCursor.row, terminalCursor.column] := defaultCharColor;
        backColor[terminalCursor.row, terminalCursor.column] := defaultBackColor;
        charStyle[terminalCursor.row, terminalCursor.column] := [];
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.setTabulator;
begin
    terminalCursor.column := (8 * ((terminalCursor.column div 8) + 1));
    if (terminalCursor.column > terminalColumns) then begin
        terminalCursor.column := 1;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.lineFeed;
begin
    Inc(terminalCursor.row);
    if (terminalCursor.row > terminalRows) then begin
        scrollTerminalContentUp;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.clearScreen;
var
    row, column: integer;
begin
    for row := 1 to terminalRows do begin
        for column := 1 to terminalColumns do begin
            charData[row, column] := ' ';
            charColor[row, column] := defaultCharColor;
            backColor[row, column] := defaultBackColor;
            charStyle[row, column] := [];
        end;
    end;
    terminalCursor.column := 1;
    terminalCursor.row := 1;
    dcaRow := 0;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.eraseScreen;
var
    row, column: integer;
begin
    for row := 1 to terminalRows do begin
        for column := 1 to terminalColumns do begin
            charData[row, column] := ' ';
            charColor[row, column] := defaultCharColor;
            backColor[row, column] := defaultBackColor;
            charStyle[row, column] := [];
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.carriageReturn;
begin
    terminalCursor.column := 1;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.deleteEndOfLine;
var
    column: integer;
begin
    for column := terminalCursor.column to terminalColumns do begin
        charData[terminalCursor.row, column] := ' ';
        charColor[terminalCursor.row, column] := defaultCharColor;
        backColor[terminalCursor.row, column] := defaultBackColor;
        charStyle[terminalCursor.row, column] := [];
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.deleteEndOfScreen;
var
    row, column: integer;
begin
    deleteEndOfLine;
    if terminalCursor.row < terminalRows then begin
        for row := terminalCursor.row + 1 to terminalRows do begin
            for column := 1 to terminalColumns do begin
                charData[row, column] := ' ';
                charColor[row, column] := defaultCharColor;
                backColor[row, column] := defaultBackColor;
                charStyle[row, column] := [];
            end;
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.deleteBeginningOfLine;
var
    column: integer;
begin
    for column := 1 to terminalCursor.column - 1 do begin
        charData[terminalCursor.row, column] := ' ';
        charColor[terminalCursor.row, column] := defaultCharColor;
        backColor[terminalCursor.row, column] := defaultBackColor;
        charStyle[terminalCursor.row, column] := [];
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.deleteBeginningOfScreen;
var
    row, column: integer;
begin
    deleteBeginningOfLine;
    if terminalCursor.row > 1 then begin
        for row := 1 to terminalCursor.row - 1 do begin
            for column := 1 to terminalColumns do begin
                charData[row, column] := ' ';
                charColor[row, column] := defaultCharColor;
                backColor[row, column] := defaultBackColor;
                charStyle[row, column] := [];
            end;
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.deleteLine;
var
    column: integer;
begin
    for column := 1 to terminalColumns do begin
        charData[terminalCursor.row, column] := ' ';
        charColor[terminalCursor.row, column] := defaultCharColor;
        backColor[terminalCursor.row, column] := defaultBackColor;
        charStyle[terminalCursor.row, column] := [];
    end;
    terminalCursor.column := 1;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.deleteLineAndScroll;
var
    column, row: integer;
begin
    for row := terminalCursor.row to terminalRows - 1 do begin
        charData[row] := charData[row + 1];
        charColor[row] := charColor[row + 1];
        backColor[row] := backColor[row + 1];
        charStyle[row] := charStyle[row + 1];
    end;
    for column := 1 to terminalColumns do begin
        charData[terminalRows, column] := ' ';
        charColor[terminalRows, column] := defaultCharColor;
        backColor[terminalRows, column] := defaultBackColor;
        charStyle[terminalRows, column] := [];
    end;
    terminalCursor.column := 1;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.insertLineAndScroll;
var
    column, row: integer;
begin
    for row := terminalRows downto terminalCursor.row + 1 do begin
        charData[row] := charData[row - 1];
        charColor[row] := charColor[row - 1];
        backColor[row] := backColor[row - 1];
        charStyle[row] := charStyle[row - 1];
    end;
    for column := 1 to terminalColumns do begin
        charData[terminalCursor.row, column] := ' ';
        charColor[terminalCursor.row, column] := defaultCharColor;
        backColor[terminalCursor.row, column] := defaultBackColor;
        charStyle[terminalCursor.row, column] := [];
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.reverseLineFeed;
begin
    if (terminalCursor.row > 1) then begin
        Dec(terminalCursor.row);
    end
    else begin
        insertLineAndScroll;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.setCursorPosition(row, column: integer);
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
procedure TSystemTerminal.sendCursorPosition;
var
    responseString: string;
    index: integer;
begin
    responseString := chr($1B);
    responseString := responseString + '[';
    responseString := responseString + IntToStr(terminalCursor.row);
    responseString := responseString + ';';
    responseString := responseString + IntToStr(terminalCursor.column);
    responseString := responseString + 'R';
    for index := 1 to responseString.Length do begin
        writeKeyboardBuffer(byte(responseString[index]));
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.sendTerminalOk;
var
    responseString: string;
    index: integer;
begin
    responseString := chr($1B);
    responseString := responseString + '[0n';
    for index := 1 to responseString.Length do begin
        writeKeyboardBuffer(byte(responseString[index]));
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.sendWhatAreYou;
var
    responseString: string;
    index: integer;
begin
    responseString := chr($1B);
    responseString := responseString + '[?1;2c';
    for index := 1 to responseString.Length do begin
        writeKeyboardBuffer(byte(responseString[index]));
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.resetEscParameter;
var
    parIndex: integer;
begin
    for parIndex := 1 to 8 do begin
        csiPar[parIndex] := 0;
    end;
    parCount := 0;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.setCrLF(enable: boolean);
begin
    enableCrLf := enable;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.setLocalEcho(enable: boolean);
begin
    enableLocalEcho := enable;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.setLogging(enable: boolean);
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
procedure TSystemTerminal.setColorType(colorType: integer);
var
    row, column: integer;
    oldCharColor, oldBackColor: TColor;
begin
    oldCharColor := defaultCharColor;
    oldBackColor := defaultBackColor;
    case (colorType) of
        0: begin
            defaultCharColor := clBlack;
            defaultBackColor := clWhite;
            monochromTerminal := False;
        end;
        1: begin
            defaultCharColor := clWhite;
            defaultBackColor := clBlack;
            monochromTerminal := False;
        end;
        2: begin
            defaultCharColor := $00FF00;
            defaultBackColor := clBlack;
            monochromTerminal := True;
        end;
        3: begin
            defaultCharColor := $0C84E6;  //00BFFF;  //2282D0;
            defaultBackColor := clBlack;
            monochromTerminal := True;
        end;
        else begin
            defaultCharColor := clBlack;
            defaultBackColor := clWhite;
            monochromTerminal := False;
        end;
    end;
    with (imagePage) do begin
        Canvas.Font.Color := defaultCharColor;
        Canvas.Brush.Color := defaultBackColor;
        Canvas.Pen.Color := defaultBackColor;
        Canvas.Rectangle(0, 0, imagePage.Width, imagePage.Height);
    end;
    for row := 1 to terminalRows do begin
        for column := 1 to terminalColumns do begin
            if (charColor[row, column] = oldCharColor) then begin
                charColor[row, column] := defaultCharColor;
            end;
            if (backColor[row, column] = oldBackColor) then begin
                backColor[row, column] := defaultBackColor;
            end;
        end;
    end;
    fontColor := defaultCharColor;
    backgroundColor := defaultBackColor;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.writeCharacter(character: byte);
begin
    if (character > $00) then begin
        characterBuffer[characterWriteIndex] := character;
        Inc(characterWriteIndex);
        if (characterWriteIndex > maxCharacterBuffer) then begin
            characterWriteIndex := 1;
        end;
    end;
end;

// --------------------------------------------------------------------------------
function TSystemTerminal.readCharacter: byte;
begin
    Result := keyboardBuffer[keyboardReadIndex];
    Inc(keyboardReadIndex);
    if (keyboardReadIndex > maxKeyboardBuffer) then begin
        keyboardReadIndex := 1;
    end;
end;

// --------------------------------------------------------------------------------
function TSystemTerminal.terminalReadable: boolean;
begin
    if (keyboardReadIndex = keyboardWriteIndex) then begin
        Result := False;
    end
    else begin
        Result := True;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemTerminal.getKeyBoardInput(key: word; shift: TShiftState);
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
        writeKeyboardBuffer(character);
        if (enableLocalEcho) then begin
            writeCharacter(character);
        end;
    end;
end;

// --------------------------------------------------------------------------------
end.










