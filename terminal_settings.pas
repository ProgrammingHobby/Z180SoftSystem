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
        groupboxLogging: TGroupBox;
        groupboxLocalEcho: TGroupBox;
        groupboxCrLf: TGroupBox;
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormShow(Sender: TObject);
    private
        dialogChanges: integer;
        oldEnableCrLf: boolean;
        oldEnableLocalEcho: boolean;
        oldEnableLogging: boolean;

    public
        function getResult: integer;

    end;

var
    TerminalSettings: TTerminalSettings;

implementation

{$R *.lfm}

uses UscaleDPI, System_Settings;

{ TTerminalSettings }

// --------------------------------------------------------------------------------
procedure TTerminalSettings.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    SystemSettings.saveFormState(TForm(self));
    dialogChanges := 0;
    if (checkboxEnableCrLf.Checked <> oldEnableCrLf) then begin
        SystemSettings.WriteBoolean('Terminal', 'UseCRLF', checkboxEnableCrLf.Checked);
        dialogChanges := dialogChanges + $0001;
    end;
    if (checkboxEnableLocalEcho.Checked <> oldEnableLocalEcho) then begin
        SystemSettings.WriteBoolean('Terminal', 'LocalEcho', checkboxEnableLocalEcho.Checked);
        dialogChanges := dialogChanges + $0002;
    end;
    if (checkboxEnableLogging.Checked <> oldEnableLogging) then begin
        SystemSettings.WriteBoolean('Terminal', 'Loggin', checkboxEnableLogging.Checked);
        dialogChanges := dialogChanges + $0004;
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
end;

// --------------------------------------------------------------------------------
function TTerminalSettings.getResult: integer;
begin
    Result := dialogChanges;
end;

// --------------------------------------------------------------------------------
end.

