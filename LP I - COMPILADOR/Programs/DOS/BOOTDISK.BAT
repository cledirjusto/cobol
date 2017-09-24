@echo off

set DefFloppyDrive1=A:
set DefFloppyDrive1_L=a:
set DefFloppyDrive2=B:
set DefFloppyDrive2_L=b:

set DefFloppyDrive=%1

cls

:CHECK
if not exist EBD\NUL goto NODIR

if "%1"=="/?" GOTO USAGE

if "%1"=="%DefFloppyDrive1%" goto FORMAT

if "%1"=="%DefFloppyDrive1_L%" goto FORMAT

if "%1"=="%DefFloppyDrive2%" goto FORMAT

if "%1"=="%DefFloppyDrive2_L%" goto FORMAT
 
:SET_FLOPPY
echo Especifique a letra da unidade de disquete.
echo Pressione 1 para %DefFloppyDrive1%
echo ou
echo Pressione 2 para %DefFloppyDrive2%
echo.
choice /c:12 Choose an option
if errorlevel 2 goto BDRIVE
if errorlevel 1 goto ADRIVE

:ADRIVE
set DefFloppyDrive=%DefFloppyDrive1%
goto FORMAT

:BDRIVE
set DefFloppyDrive=%DefFloppyDrive2%
goto FORMAT


:FORMAT
echo.
echo Para fazer um disco de inicializaá∆o
echo Nomeie um disco como "Disco de inicializaá∆o do Windows 98" e o insira na unid. %DefFloppyDrive%
echo Cuidado: O Programa de Instalaá∆o apagar† os arquivos existentes neste disquete.
echo.
pause
format %DefFloppyDrive% /u /v:EBD /autotest
if not errorlevel 0 goto FORMAT_ERROR

:COPY
echo.
echo Copiando arquivos em %DefFloppyDrive% ...
copy .\EBD\*.* %DefFloppyDrive% > NUL
echo.
echo Transferindo arquivos de sistemas...
sys %DefFloppyDrive% > NUL
echo.
echo O disco de inicializaá∆o est† pronto.
echo.
goto END

:FORMAT_ERROR
echo.
echo Ocorreu um erro na formataá∆o da unidade especificada.
echo Pressione S para repetir ou N para cancelar
choice /c:yn
if errorlevel 2 goto FORMAT_CANCEL
if errorlevel 1 goto FORMAT
:FORMAT_CANCEL
echo .
echo Exiting
goto END

:USAGE
echo.
echo Uso: bootdisk  [letra da unidade:]
echo        e.g: bootdisk
echo                ou
echo             bootdisk %DefFloppyDrive1%
echo.
echo Este comando deve ser executado da pasta WINDOWS\COMMAND.
echo.
goto END

:NODIR
echo.
echo A pasta EBD n∆o existe.
echo Mude para a pasta WINDOWS\COMMAND e tente novamente.
echo.
goto END

:END
set DefFloppyDrive=
set DefFloppyDrive1=
set DefFloppyDrive1_L=
set DefFloppyDrive2=
set DefFloppyDrive2_L=
