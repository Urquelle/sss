set compiler_flags=-Od -D_CRT_SECURE_NO_WARNINGS -MTd -nologo -fp:fast -fp:except- -Gm- -GR- -EHa- -Zo -Oi -WX -W4 -wd4624 -wd4530 -wd4201 -wd4100 -wd4101 -wd4189 -wd4505 -wd4127 -wd4702 -wd4310 -FC -Z7 -Isrc -Ithird_party/dyncall-1.2-release/include

set linker_flags= -libpath:third_party/dyncall-1.2-release/lib -incremental:no -nodefaultlib:LIBCMT -opt:ref
cl %compiler_flags% tests\tests.cpp -Fetests.exe /link %linker_flags%

if %ERRORLEVEL% NEQ 0 (
    echo "fehler bei der kompilierung des projekts"
    exit /b %ERRORLEVEL%
)
