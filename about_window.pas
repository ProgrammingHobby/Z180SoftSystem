unit About_Window;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

    { TAboutWindow }

    TAboutWindow = class(TForm)
        buttonClose: TButton;
        imageAbout: TImage;
        labelProgSystem: TLabel;
        labelCopyright: TLabel;
        labelVersion: TLabel;
        labelAboutAppName: TLabel;
        panelAboutImage: TPanel;
        panelClose: TPanel;
        procedure buttonCloseClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormShow(Sender: TObject);
        procedure panelAboutImageResize(Sender: TObject);
    private

    public

    end;

var
    AboutWindow: TAboutWindow;

implementation

{$R *.lfm}

uses UscaleDPI, System_Settings, Version_Info;

{ TAboutWindow }

// --------------------------------------------------------------------------------
procedure TAboutWindow.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    SystemSettings.saveFormState(TForm(self));
    CloseAction := caFree;
end;

// --------------------------------------------------------------------------------
procedure TAboutWindow.buttonCloseClick(Sender: TObject);
begin
    Close;
end;

// --------------------------------------------------------------------------------
procedure TAboutWindow.FormShow(Sender: TObject);
begin
    SystemSettings.restoreFormState(TForm(self));
    ScaleDPI(self, 96);
    labelVersion.Caption := 'Version: ' + GetFileVersion;
    labelProgSystem.Caption := GetOS + ' ' + GetLCLVersion + ' & ' + GetCompilerInfo;
    self.SetAutoSize(True);
    Constraints.MinWidth := Width;
    Constraints.MaxWidth := Width;
    Constraints.MinHeight := Height;
    Constraints.MaxHeight := Height;
end;

// --------------------------------------------------------------------------------
procedure TAboutWindow.panelAboutImageResize(Sender: TObject);
begin
    imageAbout.Left := (panelAboutImage.Width - imageAbout.Width) shr 1;
    imageAbout.Top := (panelAboutImage.Height - imageAbout.Height) shr 1;
end;

// --------------------------------------------------------------------------------
end.

