
@echo off
echo 确定要删除当前目录下所有.bak、.exe类型的文件和lib目录吗？
pause


for /r %%f in (*.bak) do (
del %%f
echo 删除："%%f"
)


for /r %%a in (*.exe) do (
echo 删除exe："%%a"
DEL /F /A /Q "%%~a"
)

for /r %%a in (*.log) do (
echo 删除log："%%a"
DEL /F /A /Q "%%~a"
)


if exist lib (
echo "删除lib"
rd /s/q lib
)


for /f "delims=" %%b in ('dir /s/b/ad lib') do (
echo 删除："%%b"
rd /s/q "%%b" 
)



set a=2
:dao
set /a a=a-1
ping -n 2 -w 500 127.1>nul
cls
echo 清理成功！
echo %a%秒后自动关闭...
if %a%==0 (exit) else (goto dao)


rem BY CM,2016.06.02AM
rem update 20160614 by CM