program Z180SoftSystem;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, Main_Window, Cpu_Register, System_Memory, Memory_Editor,
  System_Settings, Z180_CPU, System_Terminal, Cpu_Io_Register, System_Fdc,
  Memory_Settings, System_InOut, Fdd_Settings, Terminal_Settings, About_Window,
  Version_Info, Hdd_Settings, System_Hdc, Hardware_Info;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainWindow, MainWindow);
  Application.CreateForm(TSystemSettings, SystemSettings);
  Application.Run;
end.

