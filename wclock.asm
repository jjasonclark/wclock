;Simulated Clock project
;
; By Jason Clark
;
;
; Program compiled with Masm32 the 32 bit for windows version of
; the assembler used in class.  It can be downloaded from
; http://win32asm.cjb.net/
;
;
;This programs basic design is laid out below
;
;  Initialize program
;  Start timer
;  Create application window
;  Run message loop till told to exit
;  
;  
;  Most time will be spent processing messages from the message loop
;  This program mostly processes timer events once per second and then
;  paint messages to display the time
;
; #########################################################################

    .486                      ; create 32 bit code
    .model flat, stdcall      ; 32 bit memory model
    option casemap :none      ; case sensitive

; #########################################################################

;     include files
;     ~~~~~~~~~~~~~
    include \masm32\include\windows.inc   ;Needed to bind to windows APIs
    include \masm32\include\masm32.inc 
    include \masm32\include\gdi32.inc     ;Needed for drawing APIs
    include \masm32\include\user32.inc    ;Needed for window calls
    include \masm32\include\kernel32.inc  ;Needed for timing functions

;     libraries
;     ~~~~~~~~~
    includelib \masm32\lib\masm32.lib
    includelib \masm32\lib\gdi32.lib
    includelib \masm32\lib\user32.lib
    includelib \masm32\lib\kernel32.lib

; #########################################################################

;=================
; Local prototypes
;=================
    WinMain             PROTO :DWORD,:DWORD,:DWORD,:DWORD
    WndProc             PROTO :DWORD,:DWORD,:DWORD,:DWORD
    Paint_Proc          PROTO :DWORD
    RegisterWinClass    PROTO :DWORD,:DWORD,:DWORD,:DWORD,:DWORD
    MsgLoop             PROTO
    Timer_Proc          PROTO :DWORD
    Convert_Proc        PROTO
    SaveGlobalVars      PROTO
    CreateAndShowWindow PROTO

; ##########################################################################
.data
    WindowTitle DB "WClock",0         ; Window caption
    WindowClass DB "wclock_Class", 0  ; Name of window class
    hInstance dd 0                    ; Instance to program
    hIcon dd 0                        ; Application icon
    hCursor dd 0                      ; Application cursor
    hWnd dd 0                         ; Window handle

    TimeText DB "00:00:00", 12 DUP(0) ; Text version of time
    Timer    DD 0                     ; Compressed time
    Lookup   DB "0123456789",0        ; Numbers used in converting byte to text
    format   DB "%2i:%02i:%02i", 0    ; Format of time for conversion
    font     DD ?                     ; Font used to draw the time

.code

; #########################################################################

start:

    ; set global values
    call SaveGlobalVars
    
    invoke Timer_Proc, hWnd     ; Call before display to set up
                                ; initial look of the time
    call CreateAndShowWindow    ; Show the main window
    
    ;Main loop
    ;Processes all messages until a WM_QUIT is sent.
    call MsgLoop

    ;Exit program either from a failed window creation
    ;Or from a WM_QUIT message
Exit:
    invoke ExitProcess,eax


; #########################################################################

;Saves needed global variables
;Should be called at start up
SaveGlobalVars proc
    invoke GetModuleHandle, NULL     ;Get instance handle for use
    mov hInstance, eax               ;in most win32 API calls

    invoke LoadIcon,hInstance,500    ;Using default application icon
    mov hIcon, eax
    
    invoke LoadCursor,NULL,IDC_ARROW ;Using default mouse pointer
    mov hCursor, eax
    
    ret
SaveGlobalVars endp

; #########################################################################

;Creates the main window and shows it on screen
;This procedure will jump to the exit if the window creation fails
CreateAndShowWindow proc
    ; register class name for CreateWindowEx call
    invoke RegisterWinClass,ADDR WndProc,ADDR WindowClass,
                hIcon,hCursor,COLOR_BTNFACE+1

    ;Create window;  Exit program if failed
    invoke CreateWindowEx,WS_EX_LEFT,
                ADDR WindowClass,
                ADDR WindowTitle,
                WS_OVERLAPPED or WS_SYSMENU,
                300,300,140,110,
                NULL,NULL,
                hInstance,NULL
    mov hWnd,eax
    cmp eax, 0                              ;Test for failure
    je Exit                                 ;Abort if failed

    invoke ShowWindow,hWnd, SW_SHOWNORMAL   ;Display window
    invoke UpdateWindow,hWnd                ;Force a redraw

    ret
CreateAndShowWindow endp

; #########################################################################

;Macro places parameter 2 into parameter 1
;Used to simplify the look of a struct fill out
;Will be used to push a parameter to a function call into
;a memory location.  Memory to memory assignment
m2m MACRO M1, M2
    push M2
    pop  M1
ENDM

; Fills in the window class structure needed to create the application
;window and registers this class
RegisterWinClass proc lpWndProc:DWORD, lpClassName:DWORD,
                Icon:DWORD, Cursor:DWORD, bColor:DWORD

    LOCAL wc:WNDCLASSEX

    mov wc.cbSize,         sizeof WNDCLASSEX
    mov wc.style,          CS_BYTEALIGNCLIENT or \
    CS_BYTEALIGNWINDOW
    m2m wc.lpfnWndProc,    lpWndProc
    mov wc.cbClsExtra,     NULL
    mov wc.cbWndExtra,     NULL
    m2m wc.hInstance,      hInstance
    m2m wc.hbrBackground,  bColor
    mov wc.lpszMenuName,   NULL
    m2m wc.lpszClassName,  lpClassName
    m2m wc.hIcon,          Icon
    m2m wc.hCursor,        Cursor
    m2m wc.hIconSm,        Icon

    invoke RegisterClassEx, ADDR wc
    ret

RegisterWinClass endp

; ########################################################################

;Main Loop for message handling
MsgLoop proc

    LOCAL msg:MSG

    @@StartLoop:
        invoke GetMessage,ADDR msg,NULL,0,0    ;Get any waiting message
        cmp eax, 0                             ;Check for WM_QUIT message
        je @@ExitLoop                          ;Exit loop if found
        ; Windows wants messages translated before dispatching
        invoke TranslateMessage, ADDR msg      ;Translate it
        invoke DispatchMessage,  ADDR msg      ;Send it to WndProc
        jmp @@StartLoop
    @@ExitLoop:

    mov eax, msg.wParam
    ret

MsgLoop endp

; #########################################################################

; Sets return code in eax and returns from a procedure call
return MACRO ReturnCode
    mov eax, ReturnCode
    ret
ENDM

;Main window procedure
;Processes all messages received by the message loop.
; Only need to process 5 message, all other messages are processed by
;  Windows default procedures
WndProc proc hWin   :DWORD,
             uMsg   :DWORD,
             wParam :DWORD,
             lParam :DWORD

    .if uMsg == WM_CREATE
        ;Sent when program starts
        ;Start the timer
        invoke SetTimer, hWin, 0, 1000, NULL
        mov Timer, eax
        ;Create font used to show the time
        invoke CreateFont, 50,15,0,0,FW_BOLD,0,0,0, \          ; Create font
                           ANSI_CHARSET,OUT_TT_ONLY_PRECIS, \
                           CLIP_DEFAULT_PRECIS,DEFAULT_QUALITY,FF_MODERN,0
        mov font, eax
        return 0

    .elseif uMsg == WM_PAINT
        ; Sent when the window needs to be redrawn.
        invoke Paint_Proc, hWin
        return 0

    .elseif uMsg == WM_TIMER
        ;Sent when the 1 second timer goes off
        invoke Timer_Proc, hWin
        ;No return; allow windows to also handle this message

    .elseif uMsg == WM_CLOSE
        ;Sent when window is about to close
        ;Need to stop the timer before exiting
        invoke KillTimer, hWin, Timer
        invoke DeleteObject, font      ;To prevent resource leaks
        ;No return; allow windows to also handle this message

    .elseif uMsg == WM_DESTROY
        ;Sent after the window has been closed.  We need to quit the program
        invoke PostQuitMessage, NULL
        return 0

    .endif

    ;All other processing is sent to default procedure
    invoke DefWindowProc, hWin, uMsg, wParam, lParam

    ret

WndProc endp

; #########################################################################

;Called each time the window needs to be redrawn
;This procedure will draw the clock onto the application window.
;In order to prevent flickering all drawing is done on a "back buffer"
;then the whole buffer is drawn over the window.
Paint_Proc proc hWin:DWORD

    LOCAL hDC      :DWORD         ;Hold drawing surface
    LOCAL Ps       :PAINTSTRUCT   ;Information about this call
    LOCAL hMemDC   :DWORD         ;Back buffer drawing surface
    LOCAL hBitmap  :DWORD         ;Used to have color output
    LOCAL rect     :RECT          ;Drawing area

    ;Start the paint process
    invoke BeginPaint,hWin,ADDR Ps
    mov hDC, eax

    ;Make drawing area be the whole window
    invoke GetClientRect, hWnd, addr rect

    invoke CreateCompatibleDC, hDC        ;Create back buffer
    mov hMemDC,eax

    invoke CreateCompatibleBitmap, hDC, rect.right, rect.bottom ;Create bitmap
    mov hBitmap, eax

    invoke SelectObject, hMemDC, hBitmap     ;Select bitmap to draw on
    invoke SelectObject, hMemDC, font        ;Select font to print in
    invoke SetBkMode,    hMemDC, OPAQUE      ;Remove background from text
    invoke SetTextColor, hMemDC, 0           ;Set text color to black

    invoke FillRect, hMemDC, ADDR rect, COLOR_WINDOW+1     ;Fill background
    invoke DrawText, hMemDC, ADDR TimeText, 8, ADDR rect, \;Draw text to buffer
                     DT_CENTER + DT_VCENTER + DT_SINGLELINE

    ;Copy back buffer to screen
    invoke BitBlt, hDC, 0, 0, rect.right, rect.bottom, \    
                   hMemDC, 0, 0, SRCCOPY

    invoke DeleteObject, hBitmap    ;Delete all objects used
    invoke DeleteDC,hMemDC

    ;End paint process
    invoke EndPaint,hWin,ADDR Ps

    ret

Paint_Proc endp

; ########################################################################

;Converts AX from the range of 0-23 to 1 to 12
Convert_Proc proc
    cmp ax, 12
    jle @@skip
    sub ax, 12
@@skip:
    cmp ax, 0
    jne @@end
    mov ax, 12
@@end:
    ret
Convert_Proc endp


;Called each second
;Updates current time text and then calls for the window to be repainted
Timer_Proc proc hWin:DWORD
    LOCAL time:SYSTEMTIME

    ; Get current time and save it to time
    invoke GetLocalTime, ADDR time

    ;Create output sting from time using wsprintf win32 API
    xor ax, ax           ; Push vars in reverse order because
    push ax              ; wsprintf uses ctol calling convention
    push time.wSecond
    push ax
    push time.wMinute
    push ax
    mov ax, time.wHour
    call Convert_Proc    ; Correct hour range before pushing
    push ax

    invoke wsprintf, ADDR TimeText, ADDR format ; Other params already on stack
    sub esp, 14                    ; wsprintf doesn't pop parameters from stack

    ;Force a redraw of the application window
    invoke RedrawWindow, hWin, NULL, 0, RDW_INVALIDATE    ; Redraw window

    ret
Timer_Proc endp

; ########################################################################

end start
