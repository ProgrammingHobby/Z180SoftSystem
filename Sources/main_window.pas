unit Main_Window;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
    StdCtrls, ComCtrls, ActnList, Buttons;

type

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
        comboboxRun: TComboBox;
        comboboxSlowRun: TComboBox;
        imagelistMainWindow: TImageList;
        menuCpuCoreRegister: TMenuItem;
        menuCpuIoRegister: TMenuItem;
        menuFloppyImages: TMenuItem;
        menuHddImage: TMenuItem;
        menuHardwareInfo: TMenuItem;
        menuCopyCpmFiles: TMenuItem;
        menuCreateCpmDiscImages: TMenuItem;
        panelHdd: TPanel;
        panelFdd0: TPanel;
        panelFdd1: TPanel;
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
        statusbarMainWindow: TPanel;
        toolbarMainWindow: TToolBar;
        toolbuttonSeparator4: TToolButton;
        toolbuttonSlowRun: TToolButton;
        toolbuttonTerminal: TToolButton;
        toolbuttonSeparator5: TToolButton;
        toolbuttonSeparator1: TToolButton;
        toolbuttonMemoryEditor: TToolButton;
        toolbuttonReset: TToolButton;
        toolbuttonSingleStep: TToolButton;
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
        procedure comboboxRunChange(Sender: TObject);
        procedure comboboxSlowRunChange(Sender: TObject);
        procedure cpuRunTimer(Sender: TObject);
        procedure cpuSlowRunTimer(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
        procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
        procedure FormShow(Sender: TObject);
        procedure panelFdd0Paint(Sender: TObject);
        procedure panelFdd1Paint(Sender: TObject);
        procedure panelHddPaint(Sender: TObject);
    private
        bootRomEnabled: boolean;
        runSpeedValue: integer;
        {$ifndef Windows}
        isKeyAltGr: boolean;
        {$endif}
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
    RUNSPEED_4MHZ = 6875;
    RUNSPEED_8MHZ = 12375;
    RUNSPEED_12MHZ = 17875;
    RUNSPEED_16MHZ = 23375;
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
    {$ifdef Windows}
    Constraints.MinWidth := 886;  // 880 + 6
    Constraints.MinHeight := 686; // 680 + 6
    {$else}
    Constraints.MinWidth := 806;   // 800 + 6
    Constraints.MinHeight := 663;  // 657 + 6
    {$endif}
    Constraints.MaxWidth := Constraints.MinWidth;
    Constraints.MaxHeight := Constraints.MinHeight;

    SystemMemory := TSystemMemory.Create;
    bootRomEnabled := SystemMemory.isRomFileValid;
    SystemFdc := TSystemFdc.Create(panelFdd0, panelFdd1);
    SystemHdc := TSystemHdc.Create(panelHdd);
    SystemTerminal := TSystemTerminal.Create(panelSystemTerminal, False);
    SystemRtc := TSystemRtc.Create;
    SystemInOut := TSystemInOut.Create;
    Z180Cpu := TZ180Cpu.Create;

    {$ifndef Windows}
    isKeyAltGr := False;
    {$endif}

    comboboxRun.ItemIndex := SystemSettings.ReadInteger('Emulation', 'RunSpeed', 0);
    comboboxRunChange(nil);
    comboboxSlowRun.ItemIndex := SystemSettings.ReadInteger('Emulation', 'SlowRunSpeed', 2);
    comboboxSlowRunChange(nil);
    actionResetExecute(nil);
    panelSystemTerminal.SetFocus;
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
    //setRunSpeed;
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
    //setSlowRunSpeed;
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
procedure TMainWindow.comboboxRunChange(Sender: TObject);
begin

    case (comboboxRun.ItemIndex) of
        0: runSpeedValue := RUNSPEED_4MHZ;
        1: runSpeedValue := RUNSPEED_8MHZ;
        2: runSpeedValue := RUNSPEED_12MHZ;
        3: runSpeedValue := RUNSPEED_16MHZ;
        else runSpeedValue := RUNSPEED_4MHZ;
    end;
    SystemSettings.WriteInteger('Emulation', 'RunSpeed', comboboxRun.ItemIndex);
end;

// --------------------------------------------------------------------------------
procedure TMainWindow.comboboxSlowRunChange(Sender: TObject);
begin
    case (comboboxSlowRun.ItemIndex) of
        0: cpuRun.Interval := SLOWSPEED_1OPS;
        1: cpuRun.Interval := SLOWSPEED_2OPS;
        2: cpuRun.Interval := SLOWSPEED_5OPS;
        3: cpuRun.Interval := SLOWSPEED_10OPS;
        else cpuRun.Interval := SLOWSPEED_5OPS;
    end;
    SystemSettings.WriteInteger('Emulation', 'SlowRunSpeed', comboboxSlowRun.ItemIndex);
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
