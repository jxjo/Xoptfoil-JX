@echo off
if %1A == A (
  echo .
  echo "Usage: packager_worker <xoptfoil_version>"
  echo .
  goto :eof
)

set VERSION=%1
set WIN_RELEASE=Releases\Xfoil_Worker-%VERSION%-windows

echo .
echo Packaging Xfoil_Worker runtime into %WIN_RELEASE%
echo .
pause
echo .

if not exist releases mkdir releases
if     exist %WIN_RELEASE% rd %WIN_RELEASE% /s /q
mkdir %WIN_RELEASE%

xcopy windows\bin\Xfoil_Worker.exe    %WIN_RELEASE%   /s /i /q

xcopy doc\xfoil_worker.pdf            %WIN_RELEASE%   /i /q
xcopy "examples\smooth foil"          "%WIN_RELEASE%\examples\smooth foil"      /s /i /q

rem ----- zip with powershell ------------
cd releases
if     exist Xfoil_Worker-%VERSION%-windows.zip del  Xoptfoil-%VERSION%-windows.zip
powershell Compress-Archive Xfoil_Worker-%VERSION%-windows\* -Force Xfoil_Worker-%VERSION%-windows.zip 
cd ..