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
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
        procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
        procedure FormShow(Sender: TObject);
        procedure panelFdd0Paint(Sender: TObject);
        procedure panelFdd1Paint(Sender: TObject);
        procedure panelHddPaint(Sender: TObject);
        procedure popupRunSpeedClick(Sender: TObject);
        procedure popupSlowRunSpeedClick(Sender: TObject);

    private
        bootRomEnabled: boolean;
        runSpeedValue: integer;
        {$ifndef Windows}
        isKeyAltGr: boolean;
        {$endif}
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

const
  {$ifndef Windows}
    RUNSPEED_4MHZ = 2500;
    RUNSPEED_8MHZ = 4500;
    RUNSPEED_12MHZ = 6500;
    RUNSPEED_16MHZ = 8500;
  {$else}
    RUNSPEED_4MHZ = 6250;
    RUNSPEED_8MHZ = 11250;
    RUNSPEED_12MHZ = 16250;
    RUNSPEED_16MHZ = 21250;
  {$endif}
    SLOWSPEED_1OPS = 1000;
    SLOWSPEED_2OPS = 500;
    SLOWSPEED_5OPS = 200;
    SLOWSPEED_10OPS = 100;

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
    FreeAndNil(SystemTerminal);
    SystemSettings.saveFormState(TForm(self));
    CloseAction := caFree;
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
    SystemTerminal.getKeyBoardInput(Key, termShift);
    {$else}
    SystemTerminal.getKeyBoardInput(Key, Shift);
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

    SystemFdc := TSystemFdc.Create;
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

    SystemHdc := TSystemHdc.Create;
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

    SystemTerminal := TSystemTerminal.Create(panelSystemTerminal, False);
    SystemTerminal.setCrLF(SystemSettings.ReadBoolean('Terminal', 'UseCRLF', False));
    SystemTerminal.setLocalEcho(SystemSettings.ReadBoolean('Terminal', 'LocalEcho', False));
    SystemTerminal.setTerminalLogging(SystemSettings.ReadBoolean('Terminal', 'Loggin', False));
    SystemTerminal.setInverseScreen(SystemSettings.ReadBoolean('Terminal', 'InverseScreen', False));

    SystemRtc := TSystemRtc.Create;
    SystemInOut := TSystemInOut.Create;
    Z180Cpu := TZ180Cpu.Create;

    {$ifndef Windows}
    isKeyAltGr := False;
    {$endif}

    setSlowRunSpeed;
    setRunSpeed;
    actionResetExecute(nil);
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
        runSpeedValue := RUNSPEED_4MHZ;
        SystemSettings.WriteInteger('Emulation', 'RunSpeed', 0);
    end
    else if (Sender = popup8Mhz) then begin
        runSpeedValue := RUNSPEED_8MHZ;
        SystemSettings.WriteInteger('Emulation', 'RunSpeed', 1);
    end
    else if (Sender = popup12Mhz) then begin
        runSpeedValue := RUNSPEED_12MHZ;
        SystemSettings.WriteInteger('Emulation', 'RunSpeed', 2);
    end
    else if (Sender = popup16Mhz) then begin
        runSpeedValue := RUNSPEED_16MHZ;
        SystemSettings.WriteInteger('Emulation', 'RunSpeed', 3);
    end
    else begin
        runSpeedValue := RUNSPEED_4MHZ;
        SystemSettings.WriteInteger('Emulation', 'RunSpeed', 0);
    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.popupSlowRunSpeedClick(Sender: TObject);
begin
    if (Sender = popup1Ops) then begin
        cpuRun.Interval := SLOWSPEED_1OPS;
        SystemSettings.WriteInteger('Emulation', 'SlowRunSpeed', 0);
    end
    else if (Sender = popup2Ops) then begin
        cpuRun.Interval := SLOWSPEED_2OPS;
        SystemSettings.WriteInteger('Emulation', 'SlowRunSpeed', 1);
    end
    else if (Sender = popup5Ops) then begin
        cpuRun.Interval := SLOWSPEED_5OPS;
        SystemSettings.WriteInteger('Emulation', 'SlowRunSpeed', 2);
    end
    else if (Sender = popup10Ops) then begin
        cpuRun.Interval := SLOWSPEED_10OPS;
        SystemSettings.WriteInteger('Emulation', 'SlowRunSpeed', 3);
    end
    else begin
        cpuRun.Interval := SLOWSPEED_5OPS;
        SystemSettings.WriteInteger('Emulation', 'SlowRunSpeed', 2);
    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.setSlowRunSpeed;
begin
    case (SystemSettings.ReadInteger('Emulation', 'SlowRunSpeed', 2)) of
        0: begin
            popup1Ops.Checked := True;
            cpuRun.Interval := SLOWSPEED_1OPS;
        end;
        1: begin
            popup2Ops.Checked := True;
            cpuRun.Interval := SLOWSPEED_2OPS;
        end;
        2: begin
            popup5Ops.Checked := True;
            cpuRun.Interval := SLOWSPEED_5OPS;
        end;
        3: begin
            popup10Ops.Checked := True;
            cpuRun.Interval := SLOWSPEED_10OPS;
        end;
        else begin
            popup5Ops.Checked := True;
            cpuRun.Interval := SLOWSPEED_5OPS;
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.setRunSpeed;
begin
    case (SystemSettings.ReadInteger('Emulation', 'RunSpeed', 0)) of
        0: begin
            popup4Mhz.Checked := True;
            runSpeedValue := RUNSPEED_4MHZ;
        end;
        1: begin
            popup8Mhz.Checked := True;
            runSpeedValue := RUNSPEED_8MHZ;
        end;
        2: begin
            popup12Mhz.Checked := True;
            runSpeedValue := RUNSPEED_12MHZ;
        end;
        3: begin
            popup16Mhz.Checked := True;
            runSpeedValue := RUNSPEED_16MHZ;
        end;
        else begin
            popup4Mhz.Checked := True;
            runSpeedValue := RUNSPEED_4MHZ;
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
    SystemTerminal.terminalReset;
    SystemMemory.EnableBootRom(bootRomEnabled);
    SystemHdc.doReset;
    if (not cpuRun.Enabled) then begin
        actionHddDrive.Enabled := True;
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
    Z180Cpu.clrSlpHalt;
    actionMemorySettings.Enabled := True;
    actionFloppyDrive.Enabled := True;
    actionLoadFileToRam.Enabled := True;
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.actionTerminalSettingsExecute(Sender: TObject);
var
    dialog: TTerminalSettings;
begin
    Application.CreateForm(TTerminalSettings, dialog);
    dialog.ShowModal;
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
