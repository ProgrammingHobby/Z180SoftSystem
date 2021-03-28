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
        groupboxMemorySettings: TGroupBox;
        groupSystemRamSize: TGroupBox;
        panelBootRomFile: TPanel;
        panelMemorySettings: TPanel;
        procedure buttonReloadImageClick(Sender: TObject);
        procedure comboboxDrawItem(Control: TWinControl; Index: integer; ARect: TRect; State: TOwnerDrawState);
        procedure editBootRomImageFileChange(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormShow(Sender: TObject);

    private
        oldRomSize: integer;
        oldRamSize: integer;
        oldImageFile: string;
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
    if (oldRomSize <> comboboxRomSize.ItemIndex) then begin
        SystemSettings.WriteInteger('Memory', 'RomSize', comboboxRomSize.ItemIndex);
        SystemMemory.setBootRomSize(comboboxRomSize.ItemIndex);
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
    if (oldRamSize <> comboboxRamSize.ItemIndex) then begin
        SystemSettings.WriteInteger('Memory', 'RamSize', comboboxRamSize.ItemIndex);
        SystemMemory.setSystemRamSize(comboboxRamSize.ItemIndex);
        if Assigned(MemoryEditor) then begin
            MemoryEditor.memoryChanged;
        end;
    end;
    if (oldReloadOnEnable <> checkboxReloadOnEnable.Checked) then begin
        SystemSettings.WriteBoolean('Memory', 'ReloadOnEnable', checkboxReloadOnEnable.Checked);
        SystemMemory.setReloadImageOnEnable(checkboxReloadOnEnable.Checked);
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

// --------------------------------------------------------------------------------
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
procedure TMemorySettings.editBootRomImageFileChange(Sender: TObject);
begin
    editBootRomImageFile.SelStart := editBootRomImageFile.FileName.Length;
    editBootRomImageFile.Hint := editBootRomImageFile.FileName;
end;

// --------------------------------------------------------------------------------
procedure TMemorySettings.FormShow(Sender: TObject);
begin
    SystemSettings.restoreFormState(TForm(self));
    ScaleDPI(self, 96);
    self.SetAutoSize(True);
    Constraints.MinWidth := Width;
    Constraints.MaxWidth := Width;
    Constraints.MinHeight := Height;
    Constraints.MaxHeight := Height;
    editBootRomImageFile.InitialDir := GetUserDir;
    oldRomSize := SystemSettings.ReadInteger('Memory', 'RomSize', 0);
    comboboxRomSize.ItemIndex := oldRomSize;
    oldImageFile := SystemSettings.ReadString('Memory', 'RomImageFile', '');
    editBootRomImageFile.FileName := oldImageFile;
    oldRamSize := SystemSettings.ReadInteger('Memory', 'RamSize', 0);
    comboboxRamSize.ItemIndex := oldRamSize;
    oldReloadOnEnable := SystemSettings.ReadBoolean('Memory', 'ReloadOnEnable', False);
    checkboxReloadOnEnable.Checked := oldReloadOnEnable;
    instantlyReload := False;

    groupboxMemorySettings.SetFocus;
end;

// --------------------------------------------------------------------------------
end.


