//-----------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//----------------------------------------------------------------------------
(*
  The TerraBalloon to draw ToolTips for hints
*)

unit uTerraBalloon;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.Commctrl,
  Vcl.Controls,
  Vcl.Graphics;

const
  TTS_BALLOON  = $40;
  TTM_SETTITLE = (WM_USER + 32);

procedure CreateToolTips(hWnd: cardinal);
procedure AddToolTip(hwnd: DWORD; lpti: PToolInfo; IconType: integer;
  Text, Title: PChar);
procedure DeleteToolTip;

var
  hTooltip: cardinal;
  ti:     TToolInfo;
  buffer: array[0..255] of char;

//============================================================================
implementation
//============================================================================

procedure CreateToolTips(hWnd: cardinal);
begin
  hToolTip := CreateWindowEx(0, 'Tooltips_Class32', nil, TTS_ALWAYSTIP or
    TTS_BALLOON, integer(CW_USEDEFAULT), integer(CW_USEDEFAULT),
    integer(CW_USEDEFAULT), integer(CW_USEDEFAULT), hWnd, 0, hInstance, nil);
  if hToolTip <> 0 then
  begin
    SetWindowPos(hToolTip, HWND_TOPMOST, 0, 0, 0, 0,
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
    ti.cbSize := SizeOf(TToolInfo);
    ti.uFlags := TTF_SUBCLASS;
    ti.hInst  := hInstance;
  end;
end;

procedure AddToolTip(hwnd: DWORD; lpti: PToolInfo; IconType: integer;
  Text, Title: PChar);
var
  Item: THandle;
  Rect: TRect;
begin
  Item := hWnd;
  if (Item <> 0) and (GetClientRect(Item, Rect)) then
  begin
    lpti.hwnd     := Item;
    lpti.Rect     := Rect;
    lpti.lpszText := Text;
    SendMessage(hToolTip, TTM_ADDTOOL, 0, integer(lpti));
    FillChar(buffer, SizeOf(buffer), #0);
    lstrcpy(buffer, Title);
    if (IconType > 3) or (IconType < 0) then
    begin
      IconType := 0;
    end;
    SendMessage(hToolTip, TTM_SETTITLE, IconType, integer(@buffer));
  end;
end;

procedure DeleteToolTip;
begin
  SendMessage(hToolTip, TTM_DELTOOL, 0, integer(@ti));
end;

{
IconType can be:

 0 - No icon
 1 - Information
 2 - Warning
 3 - Error
}

end.
