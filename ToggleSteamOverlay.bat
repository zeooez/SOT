@echo off
:: Steam Overlay DLL Permission Y/N Confirmation
setlocal EnableDelayedExpansion

echo Steam Overlay Permission Toggle
echo ==============================
echo.

:: Check for admin rights
net session >nul 2>&1
if %errorlevel% neq 0 (
    echo ERROR: Administrator privileges required.
    echo Please right-click and select "Run as administrator".
    pause
    exit /b 1
)

:: Define target DLLs
set "DLL_FILES=GameOverlayRenderer.dll GameOverlayRenderer64.dll SteamOverlayVulkanLayer.dll SteamOverlayVulkanLayer64.dll"

:: Search for DLLs in current directory
set "FILES_FOUND=0"
set "CURRENT_DIR=%~dp0"
set "FIRST_FILE="

echo Searching for Steam overlay DLLs...

for %%F in (%DLL_FILES%) do (
    if exist "%CURRENT_DIR%%%F" (
        echo Found: %%F
        set /a FILES_FOUND+=1
        if "!FIRST_FILE!"=="" set "FIRST_FILE=%CURRENT_DIR%%%F"
    )
)

if %FILES_FOUND% EQU 0 (
    echo ERROR: No Steam overlay DLLs found in this directory.
    echo This script must be placed in the same directory as the Steam overlay DLLs.
    pause
    exit /b 1
)

echo Found %FILES_FOUND% Steam overlay DLL files.

:: Check current status using first file
set "CURRENTLY_RESTRICTED=0"
icacls "%FIRST_FILE%" | findstr /C:"Everyone:(DENY)" /C:"*S-1-1-0:(DENY)" > nul
if !errorlevel! EQU 0 set "CURRENTLY_RESTRICTED=1"

:: Display status and action
echo.
if %CURRENTLY_RESTRICTED% EQU 1 (
    echo CURRENT STATUS: Steam Overlay is DISABLED
    echo.
    echo This script will ENABLE the Steam overlay by restoring permissions.
    set "ACTION_TO_TAKE=ENABLE"
) else (
    echo CURRENT STATUS: Steam Overlay is ENABLED
    echo.
    echo This script will DISABLE the Steam overlay by restricting permissions.
    set "ACTION_TO_TAKE=DISABLE"
)
echo.

:: Y/N Confirmation
echo Do you want to %ACTION_TO_TAKE% the Steam Overlay?
choice /C YN /M "Confirm (Y/N):"

if errorlevel 2 (
    echo.
    echo Operation cancelled by user. No changes were made.
    pause
    exit /b 0
)
if errorlevel 1 (
    echo.
    echo Proceeding with the change...
)

echo.
:: Process each DLL file
for %%F in (%DLL_FILES%) do (
    if exist "%CURRENT_DIR%%%F" (
        if %CURRENTLY_RESTRICTED% EQU 1 (
            call :RestorePermissions "%CURRENT_DIR%%%F"
        ) else (
            call :RestrictPermissions "%CURRENT_DIR%%%F"
        )
    )
)

echo.
if %CURRENTLY_RESTRICTED% EQU 1 (
    echo Steam overlay has been ENABLED.
    echo Note: You may need to restart Steam for changes to take effect.
) else (
    echo Steam overlay has been DISABLED.
)
echo.
pause
exit /b 0

:RestrictPermissions
set "FILE=%~1"
set "FILENAME=%~nx1"
echo Disabling overlay for: %FILENAME%

takeown /F "%FILE%" /A >nul
icacls "%FILE%" /deny *S-1-1-0:(RX) >nul
if !errorlevel! neq 0 (
    echo ERROR: Failed to set permissions for %FILENAME%.
) else (
    echo - Done.
)
exit /b 0

:RestorePermissions
set "FILE=%~1"
set "FILENAME=%~nx1"
echo Enabling overlay for: %FILENAME%

takeown /F "%FILE%" /A >nul
icacls "%FILE%" /remove:d *S-1-1-0 >nul
if !errorlevel! neq 0 (
    echo ERROR: Failed to restore permissions for %FILENAME%.
) else (
    echo - Done.
)
exit /b 0