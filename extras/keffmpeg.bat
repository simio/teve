@echo off
title Koda om video

:: DVD/x264/MPEG-4  -- https://wiki.archlinux.org/index.php/FFmpeg

echo.
echo Ta tag i filmen du vill konvertera, dra den hit och tryck sedan RETUR.
echo (Detta fungerar inte i Windows Vista. Mata in filnamnet manuellt om du har Vista.)
echo.
set /p filename="> "
echo.
echo.

IF NOT EXIST %filename% goto fulfil

:meny
set mpgq=-sameq
set mkvq=3000

cls
echo.
echo Filnamn: %filename%
echo.
echo Format             Hastighet   Bitrate              Filtyp
echo ----------------------------------------------------------
echo 1. DVD MPEG-2      Snabb       (Ange)               (.mpg)
echo 2. DVD MPEG-2      Snabb       Efterlikna kvalitet  (.mpg)
echo 3. MPEG-4          Glacial     (Ange)               (.avi)
echo 4. MPEG-4          Glacial     %mkvq%kb/s             (.avi)
echo 5. x264/Matroska   Glacial     Bauta                (.mkv)
echo.
echo 0. Bort!
echo.
echo Kvalitet eller hastighet. Vad vill du ha?
set /p choice="> "
if %choice%==1 goto prempg
if %choice%==2 goto mpg
if %choice%==3 goto preavi
if %choice%==4 goto avi
if %choice%==5 goto mkv
if %choice%==0 exit
goto meny

:prempg
echo.
echo Ange bitrate med endast siffror i kilobit per sekund.
set /p quality="> "
set mpgq=-b %quality%k

:mpg
.\ffmpeg.exe -i %filename% -target film-dvd %mpgq% %filename%-dvd.mpg
goto slut

:mkv
.\ffmpeg.exe -i %filename% -acodec libmp3lame -ab 256k -vcodec libx264 -preset veryslow -crf 15 -threads 0 -x264opts frameref=15:fast_pskip=0 %filename%-x264.mkv
goto slut

:preavi
echo.
echo Ange bitrate med endast siffror i kilobit per sekund.
set /p quality="> "
set mkvq=%quality%

:avi
.\ffmpeg.exe -i %filename% -acodec copy -vcodec mpeg4 -vtag DX50 -mbd 2 -trellis 2 -flags +cbp+mv0 -pre_dia_size 4 -dia_size 4 -precmp 4 -cmp 4 -subcmp 4 -preme 2 -qns 2 -b %mkvq%k %filename%-mpeg-4.avi
goto slut

:slut
echo Avslutad.
pause
exit

:fulfil
echo Det gick inte att hitta filen. Typiskt BAT-filer!
pause
exit
