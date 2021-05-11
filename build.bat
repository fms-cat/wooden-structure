@echo off
setlocal
cd %~dp0

call "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvarsall.bat" amd64_x86
msbuild blossom.sln /p:Configuration=Release
