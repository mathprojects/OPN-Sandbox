program OPNSandbox;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {OPNSandboxForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TOPNSandboxForm, OPNSandboxForm);
  Application.Run;
end.
