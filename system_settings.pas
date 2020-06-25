unit System_Settings;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, XMLConf, FileUtil, Forms;

type

    { TSystemSettings }

    TSystemSettings = class(TDataModule)
        xmlStore: TXMLConfig;
        procedure DataModuleCreate(Sender: TObject);

    private

    public
        procedure saveFormState(var Form: TForm);
        procedure restoreFormState(var Form: TForm);
        function ReadString(const Section, Key, default: string): string;
        procedure WriteString(const Section, Key, Value: string);
        function ReadInteger(const Section, Key: string; default: integer): integer;
        procedure WriteInteger(const Section, Key: string; Value: integer);
        function ReadBoolean(const Section, Key: string; default: boolean): boolean;
        procedure WriteBoolean(const Section, Key: string; Value: boolean);
    end;

var
    SystemSettings: TSystemSettings;

implementation

{$R *.lfm}

uses strutils;

// --------------------------------------------------------------------------------
procedure TSystemSettings.DataModuleCreate(Sender: TObject);
begin
    xmlStore.Filename := 'Z180EMU.xml';
end;

// --------------------------------------------------------------------------------
procedure TSystemSettings.saveFormState(var Form: TForm);
var
    FormState: string;
begin
    FormState := IntToHex(Form.Left, 6);
    FormState := FormState + IntToHex(Form.Top, 6);
    FormState := FormState + IntToHex(Form.Width, 6);
    FormState := FormState + IntToHex(Form.Height, 6);
    FormState := FormState + IntToHex(Form.RestoredLeft, 6);
    FormState := FormState + IntToHex(Form.RestoredTop, 6);
    FormState := FormState + IntToHex(Form.RestoredWidth, 6);
    FormState := FormState + IntToHex(Form.RestoredHeight, 6);
    FormState := FormState + IntToHex(integer(Form.WindowState), 6);
    xmlStore.OpenKey('Forms/' + Form.Name);
    xmlStore.SetValue('State', FormState);
    //xmlStore.Flush;
    xmlStore.CloseKey;
end;

// --------------------------------------------------------------------------------
procedure TSystemSettings.restoreFormState(var Form: TForm);
var
    LastWindowState: TWindowState;
    FormState: string;
begin
    xmlStore.OpenKey('Forms/' + Form.Name);
    FormState := xmlStore.GetValue('State', '000000000000000000000000000000000000000000000000000000');
    xmlStore.CloseKey;
    LastWindowState := TWindowState(FormState.Substring(48, 6).ToInteger);
    if LastWindowState = wsMaximized then begin
        Form.WindowState := wsNormal;
        Form.BoundsRect := Bounds(Hex2Dec(FormState.Substring(24, 6)), Hex2Dec(FormState.Substring(30, 6)),
            Hex2Dec(FormState.Substring(36, 6)), Hex2Dec(FormState.Substring(42, 6)));
        Form.WindowState := wsMaximized;
    end
    else begin
        Form.WindowState := wsNormal;
        Form.BoundsRect := Bounds(Hex2Dec(FormState.Substring(0, 6)), Hex2Dec(FormState.Substring(6, 6)),
            Hex2Dec(FormState.Substring(12, 6)), Hex2Dec(FormState.Substring(18, 6)));
    end;
end;

// --------------------------------------------------------------------------------
function TSystemSettings.ReadString(const Section, Key, default: string): string;
begin
    xmlStore.OpenKey(Section + '/' + Key);
    Result := xmlStore.GetValue('Value', default);
    xmlStore.CloseKey;
end;

// --------------------------------------------------------------------------------
procedure TSystemSettings.WriteString(const Section, Key, Value: string);
begin
    xmlStore.OpenKey(Section + '/' + Key);
    xmlStore.SetValue('Value', Value);
    //xmlStore.Flush;
    xmlStore.CloseKey;
end;

// --------------------------------------------------------------------------------
function TSystemSettings.ReadInteger(const Section, Key: string; default: integer): integer;
begin
    xmlStore.OpenKey(Section + '/' + Key);
    Result := xmlStore.GetValue('Value', default);
    xmlStore.CloseKey;
end;

// --------------------------------------------------------------------------------
procedure TSystemSettings.WriteInteger(const Section, Key: string; Value: integer);
begin
    xmlStore.OpenKey(Section + '/' + Key);
    xmlStore.SetValue('Value', Value);
    //xmlStore.Flush;
    xmlStore.CloseKey;
end;

// --------------------------------------------------------------------------------
function TSystemSettings.ReadBoolean(const Section, Key: string; default: boolean): boolean;
begin
    xmlStore.OpenKey(Section + '/' + Key);
    Result := xmlStore.GetValue('Value', default);
    xmlStore.CloseKey;
end;

// --------------------------------------------------------------------------------
procedure TSystemSettings.WriteBoolean(const Section, Key: string; Value: boolean);
begin
    xmlStore.OpenKey(Section + '/' + Key);
    xmlStore.SetValue('Value', Value);
    //xmlStore.Flush;
    xmlStore.CloseKey;
end;

// --------------------------------------------------------------------------------
end.
