unit uI2CLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type

  { TfI2CLogger }

  TfI2CLogger = class(TForm)
    bI2CSend: TBitBtn;
    eI2CSend: TEdit;
    mI2CLog: TMemo;
    tmrI2C: TTimer;
    procedure bI2CSendClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tmrI2CTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fI2CLogger: TfI2CLogger;

implementation

uses uMain,uIntfStrConsts,uLibUSBDevice;

{ TfI2CLogger }

procedure TfI2CLogger.FormShow(Sender: TObject);
begin
  tmrI2C.Enabled := True;
end;

procedure TfI2CLogger.tmrI2CTimer(Sender: TObject);
const
  sda = 2;
  scl = 1;
var
  res: LongInt;
  data : array[0..255] of byte;
  i : Integer;
  send : string;
  count : integer;
  adata : byte;
  idx: Integer;
  Device: TLibUSBDevice;
begin
  if not Assigned(fMain.tvProgrammer.Selected) then exit;
  Device := TLibUSBDevice(fMain.tvProgrammer.Selected.Data);
  if Device.OpenDevice then
    begin
      res := 254;
      while res = 254 do
        begin
          res := Device.SendControlMsg(USB_TYPE_VENDOR or USB_RECIP_DEVICE or USB_ENDPOINT_IN,1{Read Buffer}, 0, 0,@data[1], 254, 5000);
          if res > 0 then
            begin
              count := res;
              data[0] := 3;
              for i := 1 to count do
                begin
                  if (data[i-1] and scl = scl) and (data[i] and scl = scl) then //Start or Stop Condition
                    begin
                      if data[i] and sda = sda then
                        send := send+'p'+#10
                      else
                        send := send+'s';
                      idx := -1;
                      adata := 0;
                    end
                  else if ((data[i-1] and scl) = 0) and ((data[i] and scl) = 1) then
                    begin
                      inc(idx);
                      if idx = 8 then //ack / nack
                        begin
                          if data[i] and sda = sda then
                            send := send+'n'
                          else
                            send := send+'a';
                          idx := -1;
                          adata := 0;
                        end
                      else
                        begin
                          adata := adata shl 1;
                          if data[i] and sda = sda then
                            adata := adata or 1;
                          if idx = 7 then
                            send := send+IntToHex(adata,2);
                        end;
                    end;
                end;
              mI2CLog.Lines.Add(send);
            end;
        end;
      Device.CloseDevice;
    end;
end;

procedure TfI2CLogger.FormHide(Sender: TObject);
begin
  tmrI2C.Enabled := False;
end;

procedure TfI2CLogger.bI2CSendClick(Sender: TObject);
var
  res: LongInt;
  data : array[0..10] of byte;
  i: Integer;
  tmp: String;
  Device: TLibUSBDevice;
begin
  tmrI2C.Enabled := False;
  Device := TLibUSBDevice(fMain.tvProgrammer.Selected.Data);
  if Device.OpenDevice then
    begin
      i := 0;
      while length(eI2CSend.Text) >= 2 do
        begin
          data[i] := StrToInt('$'+copy(eI2CSend.Text,0,2));
          eI2CSend.Text := copy(eI2CSend.Text,3,length(eI2CSend.Text));
          inc(i);
        end;
      res := Device.SendControlMsg(USB_TYPE_VENDOR or USB_RECIP_DEVICE or USB_ENDPOINT_OUT,2{Send Buffer}, 0, 0,@data[0], i, 5000);
      if res = i then
        fMain.StatusBar.SimpleText := strI2CSendComplete
      else
        fMain.StatusBar.SimpleText := Format(strI2CSendfailed,[res]);
      Device.CloseDevice;
      tmrI2C.Enabled := True;
    end;
end;

initialization
  {$I ui2clogger.lrs}

end.

