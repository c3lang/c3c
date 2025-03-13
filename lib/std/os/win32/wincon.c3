// console

module std::os::win32 @if(env::WIN32);

struct Win32_KEY_EVENT_RECORD 
{
	Win32_BOOL  bKeyDown;
	Win32_WORD  wRepeatCount;
	Win32_WORD  wVirtualKeyCode;
	Win32_WORD  wVirtualScanCode;
	union uChar
	{
		Win32_WCHAR unicodeChar;
		Win32_CHAR  asciiChar;
	}
	Win32_DWORD dwControlKeyState;
}

struct Win32_COORD 
{
	Win32_SHORT x;
	Win32_SHORT y;
}

struct Win32_MOUSE_EVENT_RECORD 
{
	Win32_COORD dwMousePosition;
	Win32_DWORD dwButtonState;
	Win32_DWORD dwControlKeyState;
	Win32_DWORD dwEventFlags;
}

struct Win32_WINDOW_BUFFER_SIZE_RECORD 
{
	Win32_COORD dwSize;
}

struct Win32_MENU_EVENT_RECORD 
{
	Win32_UINT dwCommandId;
}

struct Win32_FOCUS_EVENT_RECORD 
{
	Win32_BOOL bSetFocus;
}

struct Win32_INPUT_RECORD 
{
    Win32_WORD eventType;
    union event
    {
        Win32_KEY_EVENT_RECORD keyEvent;
        Win32_MOUSE_EVENT_RECORD mouseEvent;
        Win32_WINDOW_BUFFER_SIZE_RECORD windowBufferSizeEvent;
        Win32_MENU_EVENT_RECORD menuEvent;
        Win32_FOCUS_EVENT_RECORD focusEvent;
    }
}

alias Win32_PCOORD = Win32_COORD*;