unit Memory_Settings;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, EditBtn, Types;

type

    { TMemorySettings }

    TMemorySettings = class(TForm)
        buttonReloadImage: TButton;
        checkboxReloadOnEnable: TCheckBox;
        comboboxRamSize: TComboBox;
        comboboxRomSize: TComboBox;
        editBootRomImageFile: TFileNameEdit;
        groupBootRomFile: TGroupBox;
        groupBootRomSize: TGroupBox;
        groupSystemRamSize: TGroupBox;
        panelBootRomFile: TPanel;
        panelMemorySettings: TPanel;
        procedure buttonReloadImageClick(Sender: TObject);
        procedure comboboxDrawItem(Control: TWinControl; Index: integer; ARect: TRect; State: TOwnerDrawState);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormShow(Sender: TObject);

    private
        settingsLoaded: boolean;
        oldRomSize: string;
        oldRamSize: string;
        oldImageFile: string;
        oldAdressDecode: boolean;
        oldReloadOnEnable: boolean;
        instantlyReload: boolean;

    public

    end;


implementation

{$R *.lfm}

uses UscaleDPI, System_Settings, System_Memory, Memory_Editor;

{ TMemorySettings }

// --------------------------------------------------------------------------------
procedure TMemorySettings.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    SystemSettings.saveFormState(TForm(self));
    if (oldRomSize <> comboboxRomSize.Items[comboboxRomSize.ItemIndex]) then begin
        SystemSettings.WriteString('Memory', 'RomSize', comboboxRomSize.Items[comboboxRomSize.ItemIndex]);
        SystemMemory.setBootRomSize(comboboxRomSize.Items[comboboxRomSize.ItemIndex]);
        if Assigned(MemoryEditor) then begin
            MemoryEditor.memoryChanged;
        end;
    end;
    if (oldImageFile <> editBootRomImageFile.FileName) then begin
        SystemSettings.WriteString('Memory', 'RomImageFile', editBootRomImageFile.FileName);
        SystemMemory.SetRomImageFile(editBootRomImageFile.FileName);
        if Assigned(MemoryEditor) then begin
            MemoryEditor.showMemoryData;
        end;
    end;
    if (oldRamSize <> comboboxRamSize.Items[comboboxRamSize.ItemIndex]) then begin
        SystemSettings.WriteString('Memory', 'RamSize', comboboxRamSize.Items[comboboxRamSize.ItemIndex]);
        SystemMemory.setSystemRamSize(comboboxRamSize.Items[comboboxRamSize.ItemIndex]);
        if Assigned(MemoryEditor) then begin
            MemoryEditor.memoryChanged;
        end;
    end;
    if (oldReloadOnEnable <> checkboxReloadOnEnable.Checked) then begin
        SystemSettings.WriteBoolean('Memory', 'ReloadOnEnable', checkboxReloadOnEnable.Checked);
        SystemMemory.EnableReloadImageOnEnable(checkboxReloadOnEnable.Checked);
    end;
    if (instantlyReload) then begin
        SystemMemory.LoadRomFile;
        if Assigned(MemoryEditor) then begin
            MemoryEditor.showMemoryData;
        end;
    end;
    CloseAction := caFree;
end;

// --------------------------------------------------------------------------------
procedure TMemorySettings.buttonReloadImageClick(Sender: TObject);
begin
    instantlyReload := True;
end;

procedure TMemorySettings.comboboxDrawItem(Control: TWinControl; Index: integer; ARect: TRect; State: TOwnerDrawState);
var
    offset, posX, posY: integer;
begin
    with (Control as Tcombobox) do begin
        if (Control as Tcombobox = comboboxRamSize) then begin
            offset := 48;
        end
        else begin
            offset := 32;
        end;
        posX := ARect.Right - Canvas.TextWidth(Items[Index]) - offset;
        posY := ARect.Top + ((ARect.Height - Canvas.TextHeight(Items[Index])) div 2);
        Canvas.TextRect(ARect, posX, posY, Items[Index]);
    end;
end;

// --------------------------------------------------------------------------------
procedure TMemorySettings.FormShow(Sender: TObject);
var
    ItemIndex: integer;
begin
    SystemSettings.restoreFormState(TForm(self));
    ScaleDPI(self, 96);
    self.SetAutoSize(True);
    Constraints.MinWidth := Width;
    Constraints.MaxWidth := Width;
    Constraints.MinHeight := Height;
    Constraints.MaxHeight := Height;
    editBootRomImageFile.InitialDir := GetUserDir;
    settingsLoaded := False;
    oldRomSize := Trim(SystemSettings.ReadString('Memory', 'RomSize', '8KB'));
    ItemIndex := comboboxRomSize.Items.IndexOf(oldRomSize);
    if (ItemIndex = -1) then begin
        ItemIndex := 1;
    end;
    comboboxRomSize.ItemIndex := ItemIndex;
    oldImageFile := SystemSettings.ReadString('Memory', 'RomImageFile', '');
    editBootRomImageFile.FileName := oldImageFile;
    oldRamSize := Trim(SystemSettings.ReadString('Memory', 'RamSize', '64KB'));
    ItemIndex := comboboxRamSize.Items.IndexOf(oldRamSize);
    if (ItemIndex = -1) then begin
        ItemIndex := 0;
    end;
    comboboxRamSize.ItemIndex := ItemIndex;
    oldReloadOnEnable := SystemSettings.ReadBoolean('Memory', 'ReloadOnEnable', False);
    checkboxReloadOnEnable.Checked := oldReloadOnEnable;
    oldAdressDecode := SystemSettings.ReadBoolean('Memory', 'FullAdressDecode', True);
    instantlyReload := False;
    settingsLoaded := True;
end;

// --------------------------------------------------------------------------------
end.


