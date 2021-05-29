use winapi::shared::minwindef::UINT;
use winapi::um::winuser;

pub fn dbg_msg(msg: UINT) -> String {
    let msg_str = match msg {
        winuser::WM_NULL => Some("WM_NULL"),
        winuser::WM_CREATE => Some("WM_CREATE"),
        winuser::WM_DESTROY => Some("WM_DESTROY"),
        winuser::WM_MOVE => Some("WM_MOVE"),
        winuser::WM_SIZE => Some("WM_SIZE"),
        winuser::WM_ACTIVATE => Some("WM_ACTIVATE"),
        winuser::WM_SETFOCUS => Some("WM_SETFOCUS"),
        winuser::WM_KILLFOCUS => Some("WM_KILLFOCUS"),
        winuser::WM_ENABLE => Some("WM_ENABLE"),
        winuser::WM_SETREDRAW => Some("WM_SETREDRAW"),
        winuser::WM_SETTEXT => Some("WM_SETTEXT"),
        winuser::WM_GETTEXT => Some("WM_GETTEXT"),
        winuser::WM_GETTEXTLENGTH => Some("WM_GETTEXTLENGTH"),
        winuser::WM_PAINT => Some("WM_PAINT"),
        winuser::WM_CLOSE => Some("WM_CLOSE"),
        winuser::WM_QUERYENDSESSION => Some("WM_QUERYENDSESSION"),
        winuser::WM_QUIT => Some("WM_QUIT"),
        winuser::WM_QUERYOPEN => Some("WM_QUERYOPEN"),
        winuser::WM_ERASEBKGND => Some("WM_ERASEBKGND"),
        winuser::WM_SYSCOLORCHANGE => Some("WM_SYSCOLORCHANGE"),
        winuser::WM_ENDSESSION => Some("WM_ENDSESSION"),
        winuser::WM_SHOWWINDOW => Some("WM_SHOWWINDOW"),
        winuser::WM_WININICHANGE => Some("WM_WININICHANGE"),
        winuser::WM_DEVMODECHANGE => Some("WM_DEVMODECHANGE"),
        winuser::WM_ACTIVATEAPP => Some("WM_ACTIVATEAPP"),
        winuser::WM_FONTCHANGE => Some("WM_FONTCHANGE"),
        winuser::WM_TIMECHANGE => Some("WM_TIMECHANGE"),
        winuser::WM_CANCELMODE => Some("WM_CANCELMODE"),
        winuser::WM_SETCURSOR => Some("WM_SETCURSOR"),
        winuser::WM_MOUSEACTIVATE => Some("WM_MOUSEACTIVATE"),
        winuser::WM_CHILDACTIVATE => Some("WM_CHILDACTIVATE"),
        winuser::WM_QUEUESYNC => Some("WM_QUEUESYNC"),
        winuser::WM_GETMINMAXINFO => Some("WM_GETMINMAXINFO"),
        winuser::WM_PAINTICON => Some("WM_PAINTICON"),
        winuser::WM_ICONERASEBKGND => Some("WM_ICONERASEBKGND"),
        winuser::WM_NEXTDLGCTL => Some("WM_NEXTDLGCTL"),
        winuser::WM_SPOOLERSTATUS => Some("WM_SPOOLERSTATUS"),
        winuser::WM_DRAWITEM => Some("WM_DRAWITEM"),
        winuser::WM_MEASUREITEM => Some("WM_MEASUREITEM"),
        winuser::WM_DELETEITEM => Some("WM_DELETEITEM"),
        winuser::WM_VKEYTOITEM => Some("WM_VKEYTOITEM"),
        winuser::WM_CHARTOITEM => Some("WM_CHARTOITEM"),
        winuser::WM_SETFONT => Some("WM_SETFONT"),
        winuser::WM_GETFONT => Some("WM_GETFONT"),
        winuser::WM_SETHOTKEY => Some("WM_SETHOTKEY"),
        winuser::WM_GETHOTKEY => Some("WM_GETHOTKEY"),
        winuser::WM_QUERYDRAGICON => Some("WM_QUERYDRAGICON"),
        winuser::WM_COMPAREITEM => Some("WM_COMPAREITEM"),
        winuser::WM_GETOBJECT => Some("WM_GETOBJECT"),
        winuser::WM_COMPACTING => Some("WM_COMPACTING"),
        winuser::WM_COMMNOTIFY => Some("WM_COMMNOTIFY"),
        winuser::WM_WINDOWPOSCHANGING => Some("WM_WINDOWPOSCHANGING"),
        winuser::WM_WINDOWPOSCHANGED => Some("WM_WINDOWPOSCHANGED"),
        winuser::WM_POWER => Some("WM_POWER"),
        winuser::WM_COPYDATA => Some("WM_COPYDATA"),
        winuser::WM_CANCELJOURNAL => Some("WM_CANCELJOURNAL"),
        winuser::WM_NOTIFY => Some("WM_NOTIFY"),
        winuser::WM_INPUTLANGCHANGEREQUEST => Some("WM_INPUTLANGCHANGEREQUEST"),
        winuser::WM_INPUTLANGCHANGE => Some("WM_INPUTLANGCHANGE"),
        winuser::WM_TCARD => Some("WM_TCARD"),
        winuser::WM_HELP => Some("WM_HELP"),
        winuser::WM_USERCHANGED => Some("WM_USERCHANGED"),
        winuser::WM_NOTIFYFORMAT => Some("WM_NOTIFYFORMAT"),
        winuser::WM_CONTEXTMENU => Some("WM_CONTEXTMENU"),
        winuser::WM_STYLECHANGING => Some("WM_STYLECHANGING"),
        winuser::WM_STYLECHANGED => Some("WM_STYLECHANGED"),
        winuser::WM_DISPLAYCHANGE => Some("WM_DISPLAYCHANGE"),
        winuser::WM_GETICON => Some("WM_GETICON"),
        winuser::WM_SETICON => Some("WM_SETICON"),
        winuser::WM_NCCREATE => Some("WM_NCCREATE"),
        winuser::WM_NCDESTROY => Some("WM_NCDESTROY"),
        winuser::WM_NCCALCSIZE => Some("WM_NCCALCSIZE"),
        winuser::WM_NCHITTEST => Some("WM_NCHITTEST"),
        winuser::WM_NCPAINT => Some("WM_NCPAINT"),
        winuser::WM_NCACTIVATE => Some("WM_NCACTIVATE"),
        winuser::WM_GETDLGCODE => Some("WM_GETDLGCODE"),
        winuser::WM_SYNCPAINT => Some("WM_SYNCPAINT"),
        winuser::WM_NCMOUSEMOVE => Some("WM_NCMOUSEMOVE"),
        winuser::WM_NCLBUTTONDOWN => Some("WM_NCLBUTTONDOWN"),
        winuser::WM_NCLBUTTONUP => Some("WM_NCLBUTTONUP"),
        winuser::WM_NCLBUTTONDBLCLK => Some("WM_NCLBUTTONDBLCLK"),
        winuser::WM_NCRBUTTONDOWN => Some("WM_NCRBUTTONDOWN"),
        winuser::WM_NCRBUTTONUP => Some("WM_NCRBUTTONUP"),
        winuser::WM_NCRBUTTONDBLCLK => Some("WM_NCRBUTTONDBLCLK"),
        winuser::WM_NCMBUTTONDOWN => Some("WM_NCMBUTTONDOWN"),
        winuser::WM_NCMBUTTONUP => Some("WM_NCMBUTTONUP"),
        winuser::WM_NCMBUTTONDBLCLK => Some("WM_NCMBUTTONDBLCLK"),
        winuser::WM_NCXBUTTONDOWN => Some("WM_NCXBUTTONDOWN"),
        winuser::WM_NCXBUTTONUP => Some("WM_NCXBUTTONUP"),
        winuser::WM_NCXBUTTONDBLCLK => Some("WM_NCXBUTTONDBLCLK"),
        winuser::SBM_SETPOS => Some("SBM_SETPOS"),
        winuser::SBM_GETPOS => Some("SBM_GETPOS"),
        winuser::SBM_SETRANGE => Some("SBM_SETRANGE"),
        winuser::SBM_GETRANGE => Some("SBM_GETRANGE"),
        winuser::SBM_ENABLE_ARROWS => Some("SBM_ENABLE_ARROWS"),
        winuser::SBM_SETRANGEREDRAW => Some("SBM_SETRANGEREDRAW"),
        winuser::SBM_SETSCROLLINFO => Some("SBM_SETSCROLLINFO"),
        winuser::SBM_GETSCROLLINFO => Some("SBM_GETSCROLLINFO"),
        winuser::SBM_GETSCROLLBARINFO => Some("SBM_GETSCROLLBARINFO"),
        winuser::BM_GETCHECK => Some("BM_GETCHECK"),
        winuser::BM_SETCHECK => Some("BM_SETCHECK"),
        winuser::BM_GETSTATE => Some("BM_GETSTATE"),
        winuser::BM_SETSTATE => Some("BM_SETSTATE"),
        winuser::BM_SETSTYLE => Some("BM_SETSTYLE"),
        winuser::BM_CLICK => Some("BM_CLICK"),
        winuser::BM_GETIMAGE => Some("BM_GETIMAGE"),
        winuser::BM_SETIMAGE => Some("BM_SETIMAGE"),
        winuser::BM_SETDONTCLICK => Some("BM_SETDONTCLICK"),
        winuser::WM_INPUT => Some("WM_INPUT"),
        winuser::WM_KEYDOWN => Some("WM_KEYDOWN | WM_KEYFIRST"),
        winuser::WM_KEYUP => Some("WM_KEYUP"),
        winuser::WM_CHAR => Some("WM_CHAR"),
        winuser::WM_DEADCHAR => Some("WM_DEADCHAR"),
        winuser::WM_SYSKEYDOWN => Some("WM_SYSKEYDOWN"),
        winuser::WM_SYSKEYUP => Some("WM_SYSKEYUP"),
        winuser::WM_SYSCHAR => Some("WM_SYSCHAR"),
        winuser::WM_SYSDEADCHAR => Some("WM_SYSDEADCHAR"),
        winuser::WM_UNICHAR => Some("WM_UNICHAR | WM_KEYLAST"),
        winuser::WM_IME_STARTCOMPOSITION => Some("WM_IME_STARTCOMPOSITION"),
        winuser::WM_IME_ENDCOMPOSITION => Some("WM_IME_ENDCOMPOSITION"),
        winuser::WM_IME_COMPOSITION => Some("WM_IME_COMPOSITION | WM_IME_KEYLAST"),
        winuser::WM_INITDIALOG => Some("WM_INITDIALOG"),
        winuser::WM_COMMAND => Some("WM_COMMAND"),
        winuser::WM_SYSCOMMAND => Some("WM_SYSCOMMAND"),
        winuser::WM_TIMER => Some("WM_TIMER"),
        winuser::WM_HSCROLL => Some("WM_HSCROLL"),
        winuser::WM_VSCROLL => Some("WM_VSCROLL"),
        winuser::WM_INITMENU => Some("WM_INITMENU"),
        winuser::WM_INITMENUPOPUP => Some("WM_INITMENUPOPUP"),
        winuser::WM_MENUSELECT => Some("WM_MENUSELECT"),
        winuser::WM_MENUCHAR => Some("WM_MENUCHAR"),
        winuser::WM_ENTERIDLE => Some("WM_ENTERIDLE"),
        winuser::WM_MENURBUTTONUP => Some("WM_MENURBUTTONUP"),
        winuser::WM_MENUDRAG => Some("WM_MENUDRAG"),
        winuser::WM_MENUGETOBJECT => Some("WM_MENUGETOBJECT"),
        winuser::WM_UNINITMENUPOPUP => Some("WM_UNINITMENUPOPUP"),
        winuser::WM_MENUCOMMAND => Some("WM_MENUCOMMAND"),
        winuser::WM_CHANGEUISTATE => Some("WM_CHANGEUISTATE"),
        winuser::WM_UPDATEUISTATE => Some("WM_UPDATEUISTATE"),
        winuser::WM_QUERYUISTATE => Some("WM_QUERYUISTATE"),
        winuser::WM_CTLCOLORMSGBOX => Some("WM_CTLCOLORMSGBOX"),
        winuser::WM_CTLCOLOREDIT => Some("WM_CTLCOLOREDIT"),
        winuser::WM_CTLCOLORLISTBOX => Some("WM_CTLCOLORLISTBOX"),
        winuser::WM_CTLCOLORBTN => Some("WM_CTLCOLORBTN"),
        winuser::WM_CTLCOLORDLG => Some("WM_CTLCOLORDLG"),
        winuser::WM_CTLCOLORSCROLLBAR => Some("WM_CTLCOLORSCROLLBAR"),
        winuser::WM_CTLCOLORSTATIC => Some("WM_CTLCOLORSTATIC"),
        winuser::WM_MOUSEMOVE => Some("WM_MOUSEMOVE | WM_MOUSEFIRST"),
        winuser::WM_LBUTTONDOWN => Some("WM_LBUTTONDOWN"),
        winuser::WM_LBUTTONUP => Some("WM_LBUTTONUP"),
        winuser::WM_LBUTTONDBLCLK => Some("WM_LBUTTONDBLCLK"),
        winuser::WM_RBUTTONDOWN => Some("WM_RBUTTONDOWN"),
        winuser::WM_RBUTTONUP => Some("WM_RBUTTONUP"),
        winuser::WM_RBUTTONDBLCLK => Some("WM_RBUTTONDBLCLK"),
        winuser::WM_MBUTTONDOWN => Some("WM_MBUTTONDOWN"),
        winuser::WM_MBUTTONUP => Some("WM_MBUTTONUP"),
        winuser::WM_MBUTTONDBLCLK => Some("WM_MBUTTONDBLCLK"),
        winuser::WM_MOUSEWHEEL => Some("WM_MOUSEWHEEL"),
        winuser::WM_XBUTTONDOWN => Some("WM_XBUTTONDOWN"),
        winuser::WM_XBUTTONUP => Some("WM_XBUTTONUP"),
        winuser::WM_XBUTTONDBLCLK => Some("WM_XBUTTONDBLCLK"),
        winuser::WM_MOUSEHWHEEL => Some("WM_MOUSEHWHEEL | WM_MOUSELAST"),
        winuser::WM_PARENTNOTIFY => Some("WM_PARENTNOTIFY"),
        winuser::WM_ENTERMENULOOP => Some("WM_ENTERMENULOOP"),
        winuser::WM_EXITMENULOOP => Some("WM_EXITMENULOOP"),
        winuser::WM_NEXTMENU => Some("WM_NEXTMENU"),
        winuser::WM_SIZING => Some("WM_SIZING"),
        winuser::WM_CAPTURECHANGED => Some("WM_CAPTURECHANGED"),
        winuser::WM_MOVING => Some("WM_MOVING"),
        winuser::WM_POWERBROADCAST => Some("WM_POWERBROADCAST"),
        winuser::WM_DEVICECHANGE => Some("WM_DEVICECHANGE"),
        winuser::WM_MDICREATE => Some("WM_MDICREATE"),
        winuser::WM_MDIDESTROY => Some("WM_MDIDESTROY"),
        winuser::WM_MDIACTIVATE => Some("WM_MDIACTIVATE"),
        winuser::WM_MDIRESTORE => Some("WM_MDIRESTORE"),
        winuser::WM_MDINEXT => Some("WM_MDINEXT"),
        winuser::WM_MDIMAXIMIZE => Some("WM_MDIMAXIMIZE"),
        winuser::WM_MDITILE => Some("WM_MDITILE"),
        winuser::WM_MDICASCADE => Some("WM_MDICASCADE"),
        winuser::WM_MDIICONARRANGE => Some("WM_MDIICONARRANGE"),
        winuser::WM_MDIGETACTIVE => Some("WM_MDIGETACTIVE"),
        winuser::WM_MDISETMENU => Some("WM_MDISETMENU"),
        winuser::WM_ENTERSIZEMOVE => Some("WM_ENTERSIZEMOVE"),
        winuser::WM_EXITSIZEMOVE => Some("WM_EXITSIZEMOVE"),
        winuser::WM_DROPFILES => Some("WM_DROPFILES"),
        winuser::WM_MDIREFRESHMENU => Some("WM_MDIREFRESHMENU"),
        winuser::WM_IME_SETCONTEXT => Some("WM_IME_SETCONTEXT"),
        winuser::WM_IME_NOTIFY => Some("WM_IME_NOTIFY"),
        winuser::WM_IME_CONTROL => Some("WM_IME_CONTROL"),
        winuser::WM_IME_COMPOSITIONFULL => Some("WM_IME_COMPOSITIONFULL"),
        winuser::WM_IME_SELECT => Some("WM_IME_SELECT"),
        winuser::WM_IME_CHAR => Some("WM_IME_CHAR"),
        winuser::WM_IME_REQUEST => Some("WM_IME_REQUEST"),
        winuser::WM_IME_KEYDOWN => Some("WM_IME_KEYDOWN"),
        winuser::WM_IME_KEYUP => Some("WM_IME_KEYUP"),
        winuser::WM_NCMOUSEHOVER => Some("WM_NCMOUSEHOVER"),
        winuser::WM_MOUSEHOVER => Some("WM_MOUSEHOVER"),
        winuser::WM_NCMOUSELEAVE => Some("WM_NCMOUSELEAVE"),
        winuser::WM_MOUSELEAVE => Some("WM_MOUSELEAVE"),
        winuser::WM_CUT => Some("WM_CUT"),
        winuser::WM_COPY => Some("WM_COPY"),
        winuser::WM_PASTE => Some("WM_PASTE"),
        winuser::WM_CLEAR => Some("WM_CLEAR"),
        winuser::WM_UNDO => Some("WM_UNDO"),
        winuser::WM_RENDERFORMAT => Some("WM_RENDERFORMAT"),
        winuser::WM_RENDERALLFORMATS => Some("WM_RENDERALLFORMATS"),
        winuser::WM_DESTROYCLIPBOARD => Some("WM_DESTROYCLIPBOARD"),
        winuser::WM_DRAWCLIPBOARD => Some("WM_DRAWCLIPBOARD"),
        winuser::WM_PAINTCLIPBOARD => Some("WM_PAINTCLIPBOARD"),
        winuser::WM_VSCROLLCLIPBOARD => Some("WM_VSCROLLCLIPBOARD"),
        winuser::WM_SIZECLIPBOARD => Some("WM_SIZECLIPBOARD"),
        winuser::WM_ASKCBFORMATNAME => Some("WM_ASKCBFORMATNAME"),
        winuser::WM_CHANGECBCHAIN => Some("WM_CHANGECBCHAIN"),
        winuser::WM_HSCROLLCLIPBOARD => Some("WM_HSCROLLCLIPBOARD"),
        winuser::WM_QUERYNEWPALETTE => Some("WM_QUERYNEWPALETTE"),
        winuser::WM_PALETTEISCHANGING => Some("WM_PALETTEISCHANGING"),
        winuser::WM_PALETTECHANGED => Some("WM_PALETTECHANGED"),
        winuser::WM_HOTKEY => Some("WM_HOTKEY"),
        winuser::WM_PRINT => Some("WM_PRINT"),
        winuser::WM_PRINTCLIENT => Some("WM_PRINTCLIENT"),
        winuser::WM_APPCOMMAND => Some("WM_APPCOMMAND"),
        winuser::WM_HANDHELDFIRST => Some("WM_HANDHELDFIRST"),
        winuser::WM_HANDHELDLAST => Some("WM_HANDHELDLAST"),
        winuser::WM_AFXFIRST => Some("WM_AFXFIRST"),
        winuser::WM_AFXLAST => Some("WM_AFXLAST"),
        winuser::WM_PENWINFIRST => Some("WM_PENWINFIRST"),
        winuser::WM_PENWINLAST => Some("WM_PENWINLAST"),
        winuser::WM_APP => Some("WM_APP"),
        _ => None,
    };

    if let Some(msg_str) = msg_str {
        format!("{}", msg_str)
    } else {
        format!("{:#X}", msg)
    }
}
