unit Terminal_Settings;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls, Spin, SpinEx;

type

    { TTerminalSettings }

    TTerminalSettings = class(TForm)
        checkboxEnableLogging: TCheckBox;
        checkboxEnableLocalEcho: TCheckBox;
        checkboxEnableCrLf: TCheckBox;
        comboboxColorType: TComboBox;
        labelUpDownCharacterSize: TLabel;
        groupboxColorType: TGroupBox;
        groupboxCharacterSize: TGroupBox;
        groupboxLogging: TGroupBox;
        groupboxLocalEcho: TGroupBox;
        groupboxCrLf: TGroupBox;
        labelCharacterSize: TLabel;
        labelTerminalType: TLabel;
        panelLabelUpDown: TPanel;
        panelCharacterSize: TPanel;
        updownCharacterSize: TUpDown;
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormShow(Sender: TObject);
        procedure updownCharacterSizeClick(Sender: TObject; Button: TUDBtnType);
    private
        oldEnableCrLf: boolean;
        oldEnableLocalEcho: boolean;
        oldEnableLogging: boolean;
        oldColorType: integer;
        oldCharSize: integer;

    public

    end;

var
    TerminalSettings: TTerminalSettings;

implementation

{$R *.lfm}

uses UscaleDPI, System_Settings, System_Terminal;

{ TTerminalSettings }

// --------------------------------------------------------------------------------
procedure TTerminalSettings.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    SystemSettings.saveFormState(TForm(self));
    if (checkboxEnableCrLf.Checked <> oldEnableCrLf) then begin
        SystemSettings.WriteBoolean('Terminal', 'UseCRLF', checkboxEnableCrLf.Checked);
        SystemTerminal.setCrLF(checkboxEnableCrLf.Checked);
    end;
    if (checkboxEnableLocalEcho.Checked <> oldEnableLocalEcho) then begin
        SystemSettings.WriteBoolean('Terminal', 'LocalEcho', checkboxEnableLocalEcho.Checked);
        SystemTerminal.setLocalEcho(checkboxEnableLocalEcho.Checked);
    end;
    if (checkboxEnableLogging.Checked <> oldEnableLogging) then begin
        SystemSettings.WriteBoolean('Terminal', 'Loggin', checkboxEnableLogging.Checked);
        SystemTerminal.setLogging(checkboxEnableLogging.Checked);
    end;
    if (comboboxColorType.ItemIndex <> oldColorType) then begin
        SystemSettings.WriteInteger('Terminal', 'ColorType', comboboxColorType.ItemIndex);
        SystemTerminal.setColorType(comboboxColorType.ItemIndex);
    end;
    if (updownCharacterSize.Position <> oldCharSize) then begin
        SystemSettings.WriteInteger('Terminal', 'CharacterSize', updownCharacterSize.Position);
        SystemTerminal.SetCharSize(updownCharacterSize.Position);
    end;
    CloseAction := caFree;
end;

// --------------------------------------------------------------------------------
procedure TTerminalSettings.FormShow(Sender: TObject);
begin
    SystemSettings.restoreFormState(TForm(self));
    ScaleDPI(self, 96);
    self.SetAutoSize(True);
    Constraints.MinWidth := Width;
    Constraints.MaxWidth := Width;
    Constraints.MinHeight := Height;
    Constraints.MaxHeight := Height;
    panelLabelUpDown.Width := ((panelLabelUpDown.Height div 10) * 12);
    oldEnableCrLf := SystemSettings.ReadBoolean('Terminal', 'UseCRLF', False);
    checkboxEnableCrLf.Checked := oldEnableCrLf;
    oldEnableLocalEcho := SystemSettings.ReadBoolean('Terminal', 'LocalEcho', False);
    checkboxEnableLocalEcho.Checked := oldEnableLocalEcho;
    oldEnableLogging := SystemSettings.ReadBoolean('Terminal', 'Loggin', False);
    checkboxEnableLogging.Checked := oldEnableLogging;
    oldColorType := SystemSettings.ReadInteger('Terminal', 'ColorType', 0);
    comboboxColorType.ItemIndex := oldColorType;
    oldCharSize := SystemSettings.ReadInteger('Terminal', 'CharacterSize', 10);
    updownCharacterSize.Position := oldCharSize;
    labelUpDownCharacterSize.Caption := IntToStr(oldCharSize);
end;

// --------------------------------------------------------------------------------
procedure TTerminalSettings.updownCharacterSizeClick(Sender: TObject; Button: TUDBtnType);
begin
    labelUpDownCharacterSize.Caption := IntToStr(updownCharacterSize.Position);
end;

// --------------------------------------------------------------------------------
end.

