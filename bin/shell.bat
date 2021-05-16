@echo off

SET PROJECT_NAME=sss
SET PROJECT_LINKER_FLAGS=user32.lib gdi32.lib winmm.lib Shlwapi.lib Opengl32.lib windowscodecs.lib Ole32.lib Shell32.lib

SET PATH=%UserProfile%\Dev\Cpp\bin;%PATH%

call global_shell.bat
