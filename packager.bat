@echo off
if %1A == A (
  echo .
  echo "Usage: packager <xoptfoil_version>"
  echo .
  goto :eof
)

set VERSION=%1
set WIN_RELEASE=Releases\Xoptfoil-%VERSION%-windows

echo .
echo Packaging Xoptfoil-JX runtime into %WIN_RELEASE%
echo .
pause
echo .

if not exist releases mkdir releases
if     exist %WIN_RELEASE% rd %WIN_RELEASE% /s /q
mkdir %WIN_RELEASE%

xcopy windows\bin\*.exe    %WIN_RELEASE%\windows\bin   /s /i /q
xcopy windows\bin\*.dll    %WIN_RELEASE%\windows\bin   /s /i /q
xcopy windows\bin\x*.py    %WIN_RELEASE%\windows\bin   /s /i /q

xcopy doc\*.pdf            %WIN_RELEASE%\doc           /s /i /q
xcopy examples             %WIN_RELEASE%\examples      /s /i /q

rem ----- zip with powershell ------------
cd releases
if     exist Xoptfoil-%VERSION%-windows.zip del  Xoptfoil-%VERSION%-windows.zip
powershell Compress-Archive Xoptfoil-%VERSION%-windows\* Xoptfoil-%VERSION%-windows.zip
cd ..