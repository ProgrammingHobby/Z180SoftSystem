unit Terminal_Settings;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

    { TTerminalSettings }

    TTerminalSettings = class(TForm)
        checkboxEnableLogging: TCheckBox;
        checkboxEnableLocalEcho: TCheckBox;
        checkboxEnableCrLf: TCheckBox;
        groupboxTerminalColor: TGroupBox;
        groupboxLogging: TGroupBox;
        groupboxLocalEcho: TGroupBox;
        groupboxCrLf: TGroupBox;
        checkboxInverseTerminalScreen: TCheckBox;
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormShow(Sender: TObject);
    private
        oldEnableCrLf: boolean;
        oldEnableLocalEcho: boolean;
        oldEnableLogging: boolean;
        oldInverseTerminal: boolean;

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
        SystemTerminal.setTerminalLogging(checkboxEnableLogging.Checked);
    end;
    if(checkboxInverseTerminalScreen.Checked <> oldInverseTerminal) then begin
        SystemSettings.WriteBoolean('Terminal', 'InverseScreen', checkboxInverseTerminalScreen.Checked);
        SystemTerminal.setInverseScreen(checkboxInverseTerminalScreen.Checked);
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
    oldEnableCrLf := SystemSettings.ReadBoolean('Terminal', 'UseCRLF', False);
    checkboxEnableCrLf.Checked := oldEnableCrLf;
    oldEnableLocalEcho := SystemSettings.ReadBoolean('Terminal', 'LocalEcho', False);
    checkboxEnableLocalEcho.Checked := oldEnableLocalEcho;
    oldEnableLogging := SystemSettings.ReadBoolean('Terminal', 'Loggin', False);
    checkboxEnableLogging.Checked := oldEnableLogging;
    oldInverseTerminal := SystemSettings.ReadBoolean('Terminal', 'InverseScreen', False);
    checkboxInverseTerminalScreen.Checked:=oldInverseTerminal;
end;

// --------------------------------------------------------------------------------
end.

