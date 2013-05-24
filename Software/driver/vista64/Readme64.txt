

                       Using LOWCDC.SYS on Vista x64


    This is the Readme file about AVR-CDC x64 patch drivers.
    This method was informed and was tested by Benjamin Kerschner.

SUMMARY
=======
    Vista x64 does not accept unauthorized kernel-mode drivers. To use
    AVR-CDC on 64bit machine, you have to turn the "Driver Signature
    Enforcement" off by pressing the F8 function key during system boot up.

    "Driver Signature Enforcement Overrider" signs the driver as a
    testdriver and activates a testmode so you can load it without a real
    signature. You can find a more detailed description on the download
    page of the overrider
        http://www.ngohq.com/home.php?page=dseo

USAGE
=====
    To install the driver:
    1. Download the "Driver Signature Enforcement Overrider".
       http://www.ngohq.com/home.php?page=dseo
    2. Start the DSEO (no installation is needed).
    3. Choose "Sign a System File", click the "next" button and sign
       lowcdc.inf.
    4. Choose "Sign a System File" again, click the "next" button and sign
       lowcdc.sys.
    5. Choose "Enable Testmode" and click the "next" button.
    6. Restart your computer.


USING lowcdc.sys FOR FREE
=========================
    The lowcdc.sys is published under an Open Source compliant license.
    See the file "License.txt" for details.


    Osamu Tamura @ Recursion Co., Ltd.

    30 March 2009

