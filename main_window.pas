unit Main_Window;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
    StdCtrls, ComCtrls, ActnList;

type

    { TformMainWindow }

    { TMainWindow }

    TMainWindow = class(TForm)
        actionTerminalSettings: TAction;
        actionHardwareInfo: TAction;
        actionAbout: TAction;
        actionRun: TAction;
        actionSlowRun: TAction;
        actionSingleStep: TAction;
        actionReset: TAction;
        actionStop: TAction;
        actionHddDrive: TAction;
        actionFloppyDrive: TAction;
        actionMemorySettings: TAction;
        actionCpuIoRegister: TAction;
        actionCpuCoreRegister: TAction;
        actionMemoryEditor: TAction;
        actionClose: TAction;
        actionLoadFileToRam: TAction;
        actionlistMainWindow: TActionList;
        imagePage1: TImage;
        imagePage2: TImage;
        imagelistMainWindow: TImageList;
        menuCpuCoreRegister: TMenuItem;
        menuCpuIoRegister: TMenuItem;
        menuFloppyImages: TMenuItem;
        menuHddImage: TMenuItem;
        menuHardwareInfo: TMenuItem;
        menuCopyCpmFiles: TMenuItem;
        menuCreateCpmDiscImages: TMenuItem;
        panelHdd: TPanel;
        popup4Mhz: TMenuItem;
        popup8Mhz: TMenuItem;
        popup12Mhz: TMenuItem;
        popup16Mhz: TMenuItem;
        panelFdd0: TPanel;
        panelFdd1: TPanel;
        popup1Ops: TMenuItem;
        popup2Ops: TMenuItem;
        popup5Ops: TMenuItem;
        popup10Ops: TMenuItem;
        menuTerminalSettings: TMenuItem;
        menuTools: TMenuItem;
        menuReset: TMenuItem;
        menuRun: TMenuItem;
        menuSlowRun: TMenuItem;
        menuSingleStep: TMenuItem;
        N1: TMenuItem;
        menuStop: TMenuItem;
        menuMemorySettings: TMenuItem;
        menuSystem: TMenuItem;
        menuControl: TMenuItem;
        menuMemoryEditor: TMenuItem;
        menuView: TMenuItem;
        menuMainWindow: TMainMenu;
        menuFile: TMenuItem;
        menuLoadFileToRam: TMenuItem;
        menuSeparator1: TMenuItem;
        menuClose: TMenuItem;
        menuHelp: TMenuItem;
        menuAbout: TMenuItem;
        FileOpenDialog: TOpenDialog;
        cpuRun: TTimer;
        panelSystemTerminal: TPanel;
        popupmenuRunSpeed: TPopupMenu;
        popupmenuSlowRunSpeed: TPopupMenu;
        statusbarMainWindow: TPanel;
        cursorFlash: TTimer;
        terminalPageRefresh: TTimer;
        toolbarMainWindow: TToolBar;
        toolbuttonTerminal: TToolButton;
        toolbuttonSeparator5: TToolButton;
        toolbuttonSeparator1: TToolButton;
        toolbuttonMemoryEditor: TToolButton;
        toolbuttonReset: TToolButton;
        toolbuttonSingleStep: TToolButton;
        toolbuttonSlowRun: TToolButton;
        toolbuttonRun: TToolButton;
        toolbuttonCpuCoreRegister: TToolButton;
        toolbuttonCpuIoRegister: TToolButton;
        toolbuttonSeparator2: TToolButton;
        toolbuttonMemorySettings: TToolButton;
        toolbuttonFloppyImages: TToolButton;
        toolbuttonHddImage: TToolButton;
        toolbuttonSeparator3: TToolButton;
        toolbuttonStop: TToolButton;
        procedure actionAboutExecute(Sender: TObject);
        procedure actionCloseExecute(Sender: TObject);
        procedure actionCpuCoreRegisterExecute(Sender: TObject);
        procedure actionCpuIoRegisterExecute(Sender: TObject);
        procedure actionFloppyDriveExecute(Sender: TObject);
        procedure actionHardwareInfoExecute(Sender: TObject);
        procedure actionHddDriveExecute(Sender: TObject);
        procedure actionLoadFileToRamExecute(Sender: TObject);
        procedure actionMemoryEditorExecute(Sender: TObject);
        procedure actionMemorySettingsExecute(Sender: TObject);
        procedure actionResetExecute(Sender: TObject);
        procedure actionRunExecute(Sender: TObject);
        procedure actionSingleStepExecute(Sender: TObject);
        procedure actionSlowRunExecute(Sender: TObject);
        procedure actionStopExecute(Sender: TObject);
        procedure actionTerminalSettingsExecute(Sender: TObject);
        procedure cpuRunTimer(Sender: TObject);
        procedure cpuSlowRunTimer(Sender: TObject);
        procedure cursorFlashTimer(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormCreate(Sender: TObject);
        procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
        procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
        procedure FormShow(Sender: TObject);
        procedure panelFdd0Paint(Sender: TObject);
        procedure panelFdd1Paint(Sender: TObject);
        procedure panelHddPaint(Sender: TObject);
        procedure popupRunSpeedClick(Sender: TObject);
        procedure popupSlowRunSpeedClick(Sender: TObject);
        procedure terminalPageRefreshTimer(Sender: TObject);

    private
        bootRomEnabled: boolean;
        runSpeedValue: integer;
        {$ifndef Windows}
        isKeyAltGr: boolean;
        {$endif}
        procedure setInverseScreen(enable: boolean);
        procedure setSlowRunSpeed;
        procedure setRunSpeed;
    public

    end;

var
    MainWindow: TMainWindow;

implementation

{$R *.lfm}

uses UscaleDPI, System_Settings, Cpu_Register, Cpu_Io_Register, Memory_Editor, Memory_Settings,
    System_Memory, System_InOut, Z180_CPU, System_Terminal, System_Fdc, Fdd_Settings, Terminal_Settings,
    About_Window, System_Hdc, Hdd_Settings, Hardware_Info, System_Rtc;

{ TformMainWindow }

// --------------------------------------------------------------------------------
procedure TMainWindow.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    if (cpuRun.Enabled = True) then begin
        cpuRun.Enabled := False;
        cpuRun.OnTimer := nil;
    end;

    FreeAndNil(CpuRegister);
    FreeAndNil(MemoryEditor);
    FreeAndNil(CpuIoRegister);
    FreeAndNil(HardwareInfo);
    FreeAndNil(Z180Cpu);
    FreeAndNil(SystemInOut);
    FreeAndNil(SystemFdc);
    FreeAndNil(SystemHdc);
    FreeAndNil(SystemRtc);
    FreeAndNil(SystemMemory);
    SystemSettings.saveFormState(TForm(self));
    CloseAction := caFree;
end;

procedure TMainWindow.FormCreate(Sender: TObject);
var
    pageWidth, pageHeight: integer;
begin
  {$ifdef Windows}
    panelSystemTerminal.Font.Name := 'Consolas';
    panelSystemTerminal.Font.Size := 12;
    pageWidth := ((terminalCharWidth + 2) * terminalColumns) + terminalCharWidth;
    {$else}
    panelSystemTerminal.Font.Name := 'Courier New';
    panelSystemTerminal.Font.Size := 12;
    pageWidth := (terminalCharWidth * terminalColumns) + terminalCharWidth;
    {$endif}
    pageHeight := (terminalCharHeight * terminalRows) + terminalCharHeight;

    imagePage1.Parent := panelSystemTerminal;
    with (imagePage1) do begin
        Top := 0;
        Left := 0;
        Width := pageWidth;
        Height := pageHeight;
        Canvas.Font := panelSystemTerminal.Font;
    end;

    imagePage2.Parent := panelSystemTerminal;
    with (imagePage2) do begin
        Top := 0;
        Left := 0;
        Width := pageWidth;
        Height := pageHeight;
        Canvas.Font := panelSystemTerminal.Font;
    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
{$ifndef Windows}
var
    termShift: TShiftState;
{$endif}
begin
    {$ifndef Windows}
    if ((Key = 235) and (Shift = [SSALT])) then begin
        isKeyAltGr := True;
    end;
    if (isKeyAltGr) then begin
        termShift := [ssAlt..ssCtrl];
    end
    else begin
        termShift := Shift;
    end;
    getKeyBoardInput(Key, termShift);
    {$else}
    getKeyBoardInput(Key, Shift);
    {$endif}
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
    {$ifndef Windows}
    if ((Key = 235) and (Shift = [])) then begin
        isKeyAltGr := False;
    end;
    {$endif}
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.FormShow(Sender: TObject);
var
    ImageFile: string;
begin
    SystemSettings.restoreFormState(TForm(self));
    self.SetAutoSize(True);
    ScaleDPI(self, 96);
    {$ifdef Linux}
    Constraints.MinWidth := 890;
    Constraints.MinHeight := 623;
    {$else}
    Constraints.MinWidth := 970;
    Constraints.MinHeight := 616;
    {$endif}
    Constraints.MaxWidth := Constraints.MinWidth;
    Constraints.MaxHeight := Constraints.MinHeight;

    SystemMemory := TSystemMemory.Create;
    SystemFdc := TSystemFdc.Create;
    SystemHdc := TSystemHdc.Create;
    SystemRtc := TSystemRtc.Create;
    SystemInOut := TSystemInOut.Create;
    Z180Cpu := TZ180Cpu.Create;

    SystemMemory.setBootRomSize(SystemSettings.ReadString('Memory', 'RomSize', '8KB'));
    SystemMemory.setSystemRamSize(SystemSettings.ReadString('Memory', 'RamSize', '64KB'));
    SystemMemory.EnableReloadImageOnEnable(SystemSettings.ReadBoolean('Memory', 'ReloadOnEnable', False));
    ImageFile := SystemSettings.ReadString('Memory', 'RomImageFile', '');
    if ((ImageFile <> '') and (not FileExists(ImageFile))) then begin
        SystemSettings.WriteString('Memory', 'RomImageFile', '');
        ImageFile := '';
    end;
    SystemMemory.SetRomImageFile(ImageFile);
    bootRomEnabled := SystemMemory.isRomFileValid;

    setTerminalCrLF(SystemSettings.ReadBoolean('Terminal', 'UseCRLF', False));
    setTerminalLocalEcho(SystemSettings.ReadBoolean('Terminal', 'LocalEcho', False));
    setTerminalLogging(SystemSettings.ReadBoolean('Terminal', 'Loggin', False));
    setInverseScreen(SystemSettings.ReadBoolean('Terminal', 'InverseScreen', False));

    SystemFdc.setFdd0StatusPanel(panelFdd0);
    SystemFdc.setFdd0Sides(SystemSettings.ReadInteger('Fdd0', 'Sides', 2));
    SystemFdc.setFdd0Tracks(SystemSettings.ReadInteger('Fdd0', 'Tracks', 80));
    SystemFdc.setFdd0Sectors(SystemSettings.ReadInteger('Fdd0', 'Sectors', 9));
    ImageFile := SystemSettings.ReadString('Fdd0', 'ImageFile', '');
    if ((ImageFile <> '') and (not FileExists(ImageFile))) then begin
        SystemSettings.WriteString('Fdd0', 'ImageFile', '');
        ImageFile := '';
    end;
    SystemFdc.setFdd0Image(ImageFile);

    SystemFdc.setFdd1StatusPanel(panelFdd1);
    SystemFdc.setFdd1Sides(SystemSettings.ReadInteger('Fdd1', 'Sides', 2));
    SystemFdc.setFdd1Tracks(SystemSettings.ReadInteger('Fdd1', 'Tracks', 80));
    SystemFdc.setFdd1Sectors(SystemSettings.ReadInteger('Fdd1', 'Sectors', 9));
    ImageFile := SystemSettings.ReadString('Fdd1', 'ImageFile', '');
    if ((ImageFile <> '') and (not FileExists(ImageFile))) then begin
        SystemSettings.WriteString('Fdd1', 'ImageFile', '');
        ImageFile := '';
    end;
    SystemFdc.setFdd1Image(ImageFile);

    SystemHdc.setHddStatusPanel(panelHdd);
    SystemHdc.setHddHeads(SystemSettings.ReadInteger('Hdd', 'Heads', 16));
    SystemHdc.setHddTracks(SystemSettings.ReadInteger('Hdd', 'Tracks', 246));
    SystemHdc.setHddSectors(SystemSettings.ReadInteger('Hdd', 'Sectors', 63));
    ImageFile := SystemSettings.ReadString('Hdd', 'ImageFile', '');
    if ((ImageFile <> '') and (not FileExists(ImageFile))) then begin
        SystemSettings.WriteString('Hdd', 'ImageFile', '');
        ImageFile := '';
    end;
    SystemHdc.setHddImage(ImageFile);

    {$ifndef Windows}
    isKeyAltGr := False;
    {$endif}

    setSlowRunSpeed;
    setRunSpeed;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.panelFdd0Paint(Sender: TObject);
begin
    if (panelFdd0.Enabled) then begin
        imagelistMainWindow.Draw(panelFdd0.Canvas, 0, 1, 20);
    end
    else begin
        imagelistMainWindow.Draw(panelFdd0.Canvas, 0, 1, 22);
    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.panelFdd1Paint(Sender: TObject);
begin
    if (panelFdd1.Enabled) then begin
        imagelistMainWindow.Draw(panelFdd1.Canvas, 0, 1, 21);
    end
    else begin
        imagelistMainWindow.Draw(panelFdd1.Canvas, 0, 1, 23);
    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.panelHddPaint(Sender: TObject);
begin
    if (panelHdd.Enabled) then begin
        imagelistMainWindow.Draw(panelHdd.Canvas, 0, 1, 5);
    end
    else begin
        imagelistMainWindow.Draw(panelHdd.Canvas, 0, 1, 24);
    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.popupRunSpeedClick(Sender: TObject);
begin
    if (Sender = popup4Mhz) then begin
        runSpeedValue := 2000;
        SystemSettings.WriteInteger('Emulation', 'RunSpeed', 0);
    end
    else if (Sender = popup8Mhz) then begin
        runSpeedValue := 4000;
        SystemSettings.WriteInteger('Emulation', 'RunSpeed', 1);
    end
    else if (Sender = popup12Mhz) then begin
        runSpeedValue := 6000;
        SystemSettings.WriteInteger('Emulation', 'RunSpeed', 2);
    end
    else if (Sender = popup16Mhz) then begin
        runSpeedValue := 8000;
        SystemSettings.WriteInteger('Emulation', 'RunSpeed', 3);
    end
    else begin
        runSpeedValue := 2000;
        SystemSettings.WriteInteger('Emulation', 'RunSpeed', 0);
    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.popupSlowRunSpeedClick(Sender: TObject);
begin
    if (Sender = popup1Ops) then begin
        cpuRun.Interval := 1000;
        SystemSettings.WriteInteger('Emulation', 'SlowRunSpeed', 0);
    end
    else if (Sender = popup2Ops) then begin
        cpuRun.Interval := 500;
        SystemSettings.WriteInteger('Emulation', 'SlowRunSpeed', 1);
    end
    else if (Sender = popup5Ops) then begin
        cpuRun.Interval := 200;
        SystemSettings.WriteInteger('Emulation', 'SlowRunSpeed', 2);
    end
    else if (Sender = popup10Ops) then begin
        cpuRun.Interval := 100;
        SystemSettings.WriteInteger('Emulation', 'SlowRunSpeed', 3);
    end
    else begin
        cpuRun.Interval := 1000;
        SystemSettings.WriteInteger('Emulation', 'SlowRunSpeed', 0);
    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.terminalPageRefreshTimer(Sender: TObject);
var
    row, column, posX, posY: integer;
    viewChar: char;
begin
    terminalPageRefresh.Enabled := False;
    for row := 1 to terminalRows do begin
        posY := terminalStartTop + (terminalCharHeight * row);
        for column := 1 to terminalColumns do begin
            if (terminalCursor.Visible and (row = terminalCursor.row) and (column = terminalCursor.column)) then begin
                viewchar := terminalCursor.cursorChar;
            end
            else begin
                viewchar := terminalCharData[row, column];
            end;
            {$ifdef Windows}
            posX := terminalStartLeft + ((terminalCharWidth + 2) * column);
            {$else}
            posX := terminalStartLeft + (terminalCharWidth * column);
            {$endif}
            if (imagePage1.Visible) then begin
                imagePage2.Canvas.Brush.Color := terminalBackColor[row, column];
                imagePage2.Canvas.Font.Color := terminalCharColor[row, column];
                imagePage2.Canvas.Font.Style := terminalCharStyle[row, column];
                {$ifdef Windows}
                imagePage2.Canvas.Rectangle(posX - 2, posY, posX + terminalCharWidth + 1, posY + terminalCharHeight);
                {$else}
                imagePage2.Canvas.Rectangle(posX - 1, posY, posX + terminalCharWidth + 1, posY + terminalCharHeight);
                {$endif}
                imagePage2.Canvas.TextOut(posX, posY, viewChar);
            end
            else begin
                imagePage1.Canvas.Brush.Color := terminalBackColor[row, column];
                imagePage1.Canvas.Font.Color := terminalCharColor[row, column];
                imagePage1.Canvas.Font.Style := terminalCharStyle[row, column];
                 {$ifdef Windows}
                imagePage1.Canvas.Rectangle(posX - 2, posY, posX + terminalCharWidth + 1, posY + terminalCharHeight);
                {$else}
                imagePage1.Canvas.Rectangle(posX - 1, posY, posX + terminalCharWidth + 1, posY + terminalCharHeight);
                {$endif}
                imagePage1.Canvas.TextOut(posX, posY, viewChar);
            end;
        end;
    end;
    imagePage1.Visible := imagePage2.Visible;
    imagePage2.Visible := not imagePage1.Visible;
    terminalPageRefresh.Enabled := True;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.setInverseScreen(enable: boolean);
var
    row, column: integer;
begin
    if (enable) then begin
        terminalDefaultCharColor := clWhite;
        terminalDefaultBackColor := clBlack;
    end
    else begin
        terminalDefaultCharColor := clBlack;
        terminalDefaultBackColor := clWhite;
    end;
    with (imagePage1) do begin
        Canvas.Font.Color := terminalDefaultCharColor;
        Canvas.Brush.Color := terminalDefaultBackColor;
        Canvas.Pen.Color := terminalDefaultBackColor;
        Canvas.Rectangle(0, 0, imagePage1.Width, imagePage1.Height);
    end;
    with (imagePage2) do begin
        Canvas.Font.Color := terminalDefaultCharColor;
        Canvas.Brush.Color := terminalDefaultBackColor;
        Canvas.Pen.Color := terminalDefaultBackColor;
        Canvas.Rectangle(0, 0, imagePage2.Width, imagePage2.Height);
    end;
    for row := 1 to terminalRows do begin
        for column := 1 to terminalColumns do begin
            terminalCharColor[row, column] := terminalDefaultCharColor;
            terminalBackColor[row, column] := terminalDefaultBackColor;
        end;
    end;
    terminalFontColor := terminalDefaultCharColor;
    terminalBackgroundColor := terminalDefaultBackColor;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.setSlowRunSpeed;
begin
    case (SystemSettings.ReadInteger('Emulation', 'SlowRunSpeed', 2)) of
        0: begin
            popup1Ops.Checked := True;
            cpuRun.Interval := 1000;
        end;
        1: begin
            popup2Ops.Checked := True;
            cpuRun.Interval := 500;
        end;
        2: begin
            popup5Ops.Checked := True;
            cpuRun.Interval := 200;
        end;
        3: begin
            popup10Ops.Checked := True;
            cpuRun.Interval := 100;
        end;
        else begin
            popup5Ops.Checked := True;
            cpuRun.Interval := 200;
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.setRunSpeed;
begin
    case (SystemSettings.ReadInteger('Emulation', 'RunSpeed', 0)) of
        0: begin
            popup4Mhz.Checked := True;
            runSpeedValue := 2000;
        end;
        1: begin
            popup8Mhz.Checked := True;
            runSpeedValue := 4000;
        end;
        2: begin
            popup12Mhz.Checked := True;
            runSpeedValue := 6000;
        end;
        3: begin
            popup16Mhz.Checked := True;
            runSpeedValue := 8000;
        end;
        else begin
            popup4Mhz.Checked := True;
            runSpeedValue := 2000;
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.cpuRunTimer(Sender: TObject);
begin
    //cpuRun.Enabled := False;
    Z180Cpu.exec(runSpeedValue);
    //cpuRun.Enabled := True;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.cpuSlowRunTimer(Sender: TObject);
begin
    Z180Cpu.exec(1);
    if Assigned(MemoryEditor) then begin
        MemoryEditor.showMemoryData;
    end;
    if Assigned(CpuRegister) then begin
        CpuRegister.showRegisterData;
    end;
    if Assigned(CpuIoRegister) then begin
        CpuIoRegister.showRegisterData;
    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.cursorFlashTimer(Sender: TObject);
begin
  cursorFlash.Enabled := False;
    if (terminalCursor.Visible) then begin
        terminalCursor.Visible := False;
    end
    else begin
        terminalCursor.Visible := True;
    end;
    cursorFlash.Enabled := True;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionLoadFileToRamExecute(Sender: TObject);
begin
    FileOpenDialog.Title := 'Lade Binär-Datei ins RAM';
    FileOpenDialog.Filter := 'Binär Dateien (*.bin)|*.bin;*.BIN|Alle Dateien (*.*)|*.*|';
    FileOpenDialog.InitialDir := GetUserDir;
    if (FileOpenDialog.Execute) then begin
        SystemMemory.LoadRamFile(FileOpenDialog.FileName);
        bootRomEnabled := False;
        if Assigned(MemoryEditor) then begin
            MemoryEditor.showMemoryData;
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionMemoryEditorExecute(Sender: TObject);
begin
    if not Assigned(MemoryEditor) then begin
        Application.CreateForm(TMemoryEditor, MemoryEditor);
    end;

    if ((MemoryEditor.IsVisible) and (MemoryEditor.WindowState <> wsMinimized)) then begin
        MemoryEditor.Close;
    end
    else begin
        MemoryEditor.Show;
    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionMemorySettingsExecute(Sender: TObject);
var
    dialog: TMemorySettings;
begin
    Application.CreateForm(TMemorySettings, dialog);
    dialog.ShowModal;
    bootRomEnabled := SystemMemory.isRomFileValid;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionResetExecute(Sender: TObject);
begin
    Z180Cpu.reset;
    terminalReset;
    SystemMemory.EnableBootRom(bootRomEnabled);
    if Assigned(MemoryEditor) then begin
        MemoryEditor.showMemoryData;
    end;
    if Assigned(CpuRegister) then begin
        CpuRegister.showRegisterData;
    end;
    if Assigned(CpuIoRegister) then begin
        CpuIoRegister.showRegisterData;
    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionRunExecute(Sender: TObject);
begin
    if (cpuRun.Enabled = True) then begin
        cpuRun.Enabled := False;
        cpuRun.OnTimer := nil;
    end;
    cpuRun.OnTimer := @cpuRunTimer;
    setRunSpeed;
    cpuRun.Interval := 2;
    cpuRun.Enabled := True;
    actionMemorySettings.Enabled := False;
    actionFloppyDrive.Enabled := False;
    actionHddDrive.Enabled := False;
    actionLoadFileToRam.Enabled := False;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionSingleStepExecute(Sender: TObject);
begin
    if (cpuRun.Enabled = True) then begin
        cpuRun.Enabled := False;
        cpuRun.OnTimer := nil;
    end;
    Z180Cpu.exec(1);
    if Assigned(MemoryEditor) then begin
        MemoryEditor.showMemoryData;
    end;
    if Assigned(CpuRegister) then begin
        CpuRegister.showRegisterData;
    end;
    if Assigned(CpuIoRegister) then begin
        CpuIoRegister.showRegisterData;
    end;
    actionMemorySettings.Enabled := True;
    actionFloppyDrive.Enabled := True;
    actionHddDrive.Enabled := True;
    actionLoadFileToRam.Enabled := True;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionSlowRunExecute(Sender: TObject);
begin
    if (cpuRun.Enabled = True) then begin
        cpuRun.Enabled := False;
        cpuRun.OnTimer := nil;
    end;
    cpuRun.OnTimer := @cpuSlowRunTimer;
    setSlowRunSpeed;
    cpuRun.Enabled := True;
    actionMemorySettings.Enabled := False;
    actionFloppyDrive.Enabled := False;
    actionHddDrive.Enabled := False;
    actionLoadFileToRam.Enabled := False;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionStopExecute(Sender: TObject);
begin
    if (cpuRun.Enabled = True) then begin
        cpuRun.Enabled := False;
        cpuRun.OnTimer := nil;
    end;
    if Assigned(MemoryEditor) then begin
        MemoryEditor.showMemoryData;
    end;
    if Assigned(CpuRegister) then begin
        CpuRegister.showRegisterData;
    end;
    if Assigned(CpuIoRegister) then begin
        CpuIoRegister.showRegisterData;
    end;
    actionMemorySettings.Enabled := True;
    actionFloppyDrive.Enabled := True;
    actionHddDrive.Enabled := True;
    actionLoadFileToRam.Enabled := True;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionTerminalSettingsExecute(Sender: TObject);
var
    dialogResult: integer;
    dialog: TTerminalSettings;
begin
    Application.CreateForm(TTerminalSettings, dialog);
    dialog.ShowModal;
    dialogResult := dialog.getResult;
    if ((dialogResult and $0001) <> 0) then begin
        setTerminalCrLF(SystemSettings.ReadBoolean('Terminal', 'UseCRLF', False));
    end;
    if ((dialogResult and $0002) <> 0) then begin
        setTerminalLocalEcho(SystemSettings.ReadBoolean('Terminal', 'LocalEcho', False));
    end;
    if ((dialogResult and $0004) <> 0) then begin
        setTerminalLogging(SystemSettings.ReadBoolean('Terminal', 'Loggin', False));
    end;
    if ((dialogResult and $0008) <> 0) then begin
        setInverseScreen(SystemSettings.ReadBoolean('Terminal', 'InverseScreen', False));
    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionCloseExecute(Sender: TObject);
begin
    Close;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionAboutExecute(Sender: TObject);
var
    dialog: TAboutWindow;
begin
    Application.CreateForm(TAboutWindow, dialog);
    dialog.ShowModal;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionCpuCoreRegisterExecute(Sender: TObject);
begin
    if not Assigned(CpuRegister) then begin
        Application.CreateForm(TCpuRegister, CpuRegister);
    end;

    if ((CpuRegister.IsVisible) and (CpuRegister.WindowState <> wsMinimized)) then begin
        CpuRegister.Close;
    end
    else begin
        CpuRegister.Show;
    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionCpuIoRegisterExecute(Sender: TObject);
begin
    if not Assigned(CpuIoRegister) then begin
        Application.CreateForm(TCpuIoRegister, CpuIoRegister);
    end;

    if ((CpuIoRegister.IsVisible) and (CpuIoRegister.WindowState <> wsMinimized)) then begin
        CpuIoRegister.Close;
    end
    else begin
        CpuIoRegister.Show;
    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionFloppyDriveExecute(Sender: TObject);
var
    dialog: TFddSettings;
begin
    Application.CreateForm(TFddSettings, dialog);
    dialog.ShowModal;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionHardwareInfoExecute(Sender: TObject);
begin
    if not Assigned(HardwareInfo) then begin
        Application.CreateForm(THardwareInfo, HardwareInfo);
    end;

    if ((HardwareInfo.IsVisible) and (HardwareInfo.WindowState <> wsMinimized)) then begin
        HardwareInfo.Close;
    end
    else begin
        HardwareInfo.Show;
    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionHddDriveExecute(Sender: TObject);
var
    dialog: THddSettings;
begin
    Application.CreateForm(THddSettings, dialog);
    dialog.ShowModal;
end;

// --------------------------------------------------------------------------------
end.
