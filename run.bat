::stack build
::set /p temp="Hit enter to continue"

start cmd /k stack run -- 1100 1101
start cmd /k stack run -- 1101 1100
start cmd /k stack run -- 1102 1100
set /p temp="Hit enter to close windows"

taskkill /im netchange.exe /f
taskkill /im cmd.exe /f