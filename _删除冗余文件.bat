
@echo off
echo 确定要删除当前目录下所有.bak、.exe类型的文件和lib目录吗？
pause


for /r %%f in (*.bak) do (
del %%f
echo 删除："%%f"
)


if exist lib (
echo "删除lib"
rd /s/q lib
)

if exist _Lib (
echo "删除_Lib"
rd /s/q _Lib
)


for /f "delims=" %%b in ('dir /s/b/ad lib') do (
echo 删除："%%b"
rd /s/q "%%b" 
)



set a=1
:dao
set /a a=a-1
ping -n 2 -w 500 127.1>nul
cls
echo 清理成功！
echo %a%秒后自动关闭...
if %a%==0 (exit) else (goto dao)


rem BY CM,2016.06.02AM
rem update 20160614 by CM