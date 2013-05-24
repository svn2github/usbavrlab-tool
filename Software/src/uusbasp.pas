unit uUSBasp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Spin
  ;

type

  { TfUSBAsp }

  TfUSBAsp = class(TForm)
    Bevel2: TBevel;
    cbBaudrate: TComboBox;
    cbISPSpeed: TComboBox;
    cbParity: TComboBox;
    lBaudRate: TLabel;
    lDatabits: TLabel;
    lDebugPort: TLabel;
    lISPSpeed1: TLabel;
    lParity: TLabel;
    lStopbits: TLabel;
    mSerial: TMemo;
    Panel1: TPanel;
    seDatabits: TSpinEdit;
    seStopbits: TSpinEdit;
    tmrUSBASP: TTimer;
    procedure cbBaudrateChange(Sender: TObject);
    procedure cbISPSpeedSelect(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tmrUSBASPTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fUSBAsp: TfUSBAsp;

const
  FUNC_SET_ISP_SPEED = 34;
  FUNC_GET_ISP_SPEED = 35;

implementation

uses uMain,uIntfStrConsts,uBootloader,uLibUSBDevice;

{ TfUSBAsp }

procedure TfUSBAsp.cbBaudrateChange(Sender: TObject);
var
  cdc_mode : array[0..6] of byte = ($80, $25, 0, 0, 0, 0, 8);
  Addr: byte;
  Data: byte;
  res: LongInt;
  Device: TLibUSBDevice;
begin
  cdc_mode[0] := StrToIntDef(cbBaudrate.text,9600) shr 8;
  cdc_mode[1] := StrToIntDef(cbBaudrate.text,9600) and $FF;
  Device := TLibUSBDevice(fMain.tvProgrammer.Selected.Data);
  if Device.OpenDevice then
    begin
      res := Device.SendControlMsg(USB_TYPE_VENDOR or USB_RECIP_DEVICE or USB_ENDPOINT_OUT,31{Set CDC Mode},0,0,@cdc_mode,7, 5000);
      Device.CloseDevice;
      if res = 0 then
        fMain.StatusBar.SimpleText := strDebugPortModeUpdated;
    end;
end;

procedure TfUSBAsp.cbISPSpeedSelect(Sender: TObject);
var
  Data: byte;
  res: LongInt;
  Device: TLibUSBDevice;
begin
  Data := cbISPSpeed.ItemIndex+2;
  Device := TLibUSBDevice(fMain.tvProgrammer.Selected.Data);
  if Device.OpenDevice then
    begin
      res := Device.SendControlMsg(USB_TYPE_VENDOR or USB_RECIP_DEVICE or USB_ENDPOINT_IN,FUNC_SET_ISP_SPEED,Data,0,nil,0, 5000);
      device.CloseDevice;
      if res = 0 then
        fMain.StatusBar.SimpleText := strISPSpeedUpdated;
    end;
end;

procedure TfUSBAsp.FormHide(Sender: TObject);
begin
  tmrUSBASP.Enabled := false;
end;

procedure TfUSBAsp.FormShow(Sender: TObject);
begin
  tmrUSBASP.Enabled := True;
end;

procedure TfUSBAsp.tmrUSBASPTimer(Sender: TObject);
var
  res: LongInt;
  data : array[0..99] of byte;
  i : Integer;
  Device: TLibUSBDevice;
begin
  if not Assigned(fMain.tvProgrammer.Selected) then exit;
  Device := TLibUSBDevice(fMain.tvProgrammer.Selected.Data);
  if Device.OpenDevice then
    begin
      res := 100;
      while res = 100 do
        begin
          res := Device.SendControlMsg(USB_TYPE_VENDOR or USB_RECIP_DEVICE or USB_ENDPOINT_IN,32{CDC Read}, 0, 0,@data, 100, 5000);
          while res > 0 do
            begin
              mSerial.Lines.Add(char(data[res-1]));
              dec(res);
            end;
        end;
      Device.CloseDevice;
    end;
end;

initialization
  {$I uusbasp.lrs}

end.

