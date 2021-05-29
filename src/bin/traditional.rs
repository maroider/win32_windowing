use win32_windowing::dbg_msg;

use widestring::{U16CStr, U16CString};

use winapi::ctypes::c_int;
use winapi::shared::basetsd::LONG_PTR;
use winapi::shared::minwindef::{ATOM, DWORD, HINSTANCE, LPARAM, LRESULT, UINT, WPARAM};
use winapi::shared::windef::{HMENU, HWND};
use winapi::um::libloaderapi;
use winapi::um::winuser;

use std::any::Any;
use std::cell::Cell;
use std::mem;
use std::panic;
use std::ptr;
use std::rc::Rc;

fn main() {
    // Get the `HINSTANCE` of our program.
    let hinstance = unsafe { libloaderapi::GetModuleHandleW(ptr::null()) };

    let class_name = U16CString::from_str("My Window Class").unwrap();
    let class = register_window_class(&class_name, hinstance);

    let shared_data = Rc::new(Data {
        window_count: Cell::new(0),
    });
    let panic_proxy = Rc::new(PanicProxy::new());

    let window_name = U16CString::from_str("My Window").unwrap();
    let window = create_window(
        0,
        class,
        &window_name,
        winuser::WS_OVERLAPPEDWINDOW,
        winuser::CW_USEDEFAULT,
        winuser::CW_USEDEFAULT,
        800,
        600,
        ptr::null_mut(),
        ptr::null_mut(),
        hinstance,
        &shared_data,
        &panic_proxy,
    )
    .unwrap();

    // By using `SW_SHOWDEFAULT`, we automatically respect `nCommandShow`.
    unsafe { winuser::ShowWindow(window, winuser::SW_SHOWDEFAULT) };

    // The good old Windows message pump.
    // There's nothing special going on here.
    unsafe {
        let mut msg: winuser::MSG;
        loop {
            msg = mem::zeroed();

            if winuser::GetMessageW(&mut msg, ptr::null_mut(), 0, 0) == 0 {
                break;
            }

            winuser::TranslateMessage(&msg);
            winuser::DispatchMessageW(&msg);

            // If the window procedure panicked while messages were being dispatched, we resume unwinding here.
            if let Some(panic) = panic_proxy.take_panic() {
                panic::resume_unwind(panic)
            }
        }
    }
}

/// Register our window class.
///
/// Note that we don't need to unregister it, as Windows will do that for us once our program quits.
fn register_window_class(name: &U16CStr, hinstance: HINSTANCE) -> ATOM {
    // <https://docs.microsoft.com/en-us/windows/win32/api/winuser/ns-winuser-wndclassw>
    let window_class = winuser::WNDCLASSW {
        style: 0,
        // One interesting thing to note here is that function pointers are represented as `Option<fn()>`.
        // This is because Rust's native function pointers, `fn()`, cannot be null.
        lpfnWndProc: Some(window_proc),
        cbClsExtra: 0,
        cbWndExtra: 0,
        hInstance: hinstance,
        hIcon: ptr::null_mut(),
        hCursor: ptr::null_mut(),
        hbrBackground: ptr::null_mut(),
        lpszMenuName: ptr::null(),
        lpszClassName: name.as_ptr(),
    };

    // Register the window class with Windows. We use`RegisterClassW` rather than `RegisterClassA` so that
    // Windows knows our program is Unicode-aware.
    //
    // <https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-registerclassw>
    let atom = unsafe { winuser::RegisterClassW(&window_class) };
    // If `atom` is 0, then we should call `GetLastError` to get more detailed error information.
    // Since I can't be arsed to write the formatting code required to get a *useful* error message,
    // we'll simply panic in the error case.
    assert_ne!(atom, 0, "Window class registration failed!");
    atom
}

fn create_window(
    ex_style: DWORD,
    class: ATOM,
    window_name: &U16CStr,
    style: DWORD,
    x: c_int,
    y: c_int,
    width: c_int,
    height: c_int,
    parent: HWND,
    menu: HMENU,
    hinstance: HINSTANCE,
    shared_data: &SharedData,
    panic_proxy: &PanicProxyShared,
) -> Option<HWND> {
    let userdata = Box::into_raw(Box::new(UserData::new(shared_data, panic_proxy)));
    let hwnd = unsafe {
        winuser::CreateWindowExW(
            ex_style,
            class as _,
            window_name.as_ptr(),
            style,
            x,
            y,
            width,
            height,
            parent,
            menu,
            hinstance,
            userdata.cast(),
        )
    };

    if hwnd.is_null() {
        let last_error = get_last_error();
        if last_error != 0 {
            // `CreateWindowExW` sets the thread's last-error code to 0 upon success.
            // If the call to `CreateWindowExW` didn't succeed, then we assume that the window procedure
            // didn't get called, which means that we can't rely on the window procedure to drop our data.
            drop(unsafe { Box::from_raw(userdata) });
            // This should be a log::error!() or an error return in real code
            eprintln!("Window creation failed with: {:#X}", last_error);
        }
        None
    } else {
        Some(hwnd)
    }
}

#[allow(unused_unsafe)]
unsafe extern "system" fn window_proc(
    hwnd: HWND,
    msg: UINT,
    wparam: WPARAM,
    lparam: LPARAM,
) -> LRESULT {
    let userdata_ptr = {
        // <https://docs.microsoft.com/en-us/windows/win32/winmsg/wm-nccreate>
        if msg == winuser::WM_NCCREATE {
            // <https://docs.microsoft.com/en-us/windows/win32/api/winuser/ns-winuser-createstructw>
            let createstruct = unsafe { &*(lparam as *const winuser::CREATESTRUCTW) };
            // This is the data we passed to the `lpParam` parameter in `CreateWindowExW`
            let userdata = createstruct.lpCreateParams as LONG_PTR;
            unsafe { winuser::SetWindowLongPtrW(hwnd, winuser::GWLP_USERDATA, userdata) };
            userdata
        } else {
            unsafe { winuser::GetWindowLongPtrW(hwnd, winuser::GWLP_USERDATA) }
        }
    };
    // If `msg` isn't `WM_NCCREATE`, then there's nothing meaningful we can do with a `userdata`_ptr
    // that's 0.
    if userdata_ptr == 0 {
        return unsafe { winuser::DefWindowProcW(hwnd, msg, wparam, lparam) };
    }
    let userdata_ptr = userdata_ptr as *mut UserData;

    // Turning `userdata_ptr` into a mutable reference may feel a bit iffy to some (myself included), but
    // this should be fine give the following:
    // 1. The window procedure should only ever be called from the thread the window it is associated with
    //    was created on.
    // 2. We meticulously give `borrowchk` extra data so it can tell when we're passing the data along to an
    //    "inner" invocation of the window procedure. This is a bit cumbersome, but the alternative is to
    //    only use immutable references and something that provides interior mutability, like a `Mutex` or a
    //    `RefCell`. You can decide for yourself which alternative is the most cumbersome.
    let userdata = unsafe { &mut *userdata_ptr };
    let panic_proxy = &mut userdata.panic_proxy;
    let window_data = &mut userdata.window_data;

    window_data.recurse_depth += 1;
    let mut ret = panic_proxy
        .catch_unwind(|| message_handler(window_data, hwnd, msg, wparam, lparam))
        .unwrap_or(-1);
    window_data.recurse_depth -= 1;

    // `WM_NCDESTROY` *should* be the last message a window procedure receives before the window is destroyed
    // but as you'll see a bit further down, this isn't quite the reality.
    //
    // <https://docs.microsoft.com/en-us/windows/win32/winmsg/wm-ncdestroy>
    if msg == winuser::WM_NCDESTROY {
        window_data.destroyed = true;
        ret = 0;
    }

    if let WindowData {
        recurse_depth: 0,
        destroyed: true,
        ..
    } = window_data
    {
        // We can't drop the userdata immediately, as we may not be in the "outermost" call to the window
        // procedure when `WM_NCDESTROY` is received. This is why we track the `recurse_depth`, and only
        // drop the userdata when we're certain that we are the only ones with a `&mut UserData`.
        //
        // There's an extra potential complication here that's explained in the following article by
        // Raymond Chen: <https://devblogs.microsoft.com/oldnewthing/20050727-16/?p=34793>.
        // Because we can *potentially* receive `WM_NCDESTROY` more than once, we'd like to guard against
        // accidentally accessing already freed memory. We *could* leave behind a "tombstone" value, but
        // that would still leak a (tiny) bit of memory. Instead, we set the userdata pointer to 0, so that
        // subsequent calls to the window procedure will forward to `DefWindowProcW` and return immediately.
        unsafe {
            drop(userdata);
            winuser::SetWindowLongPtrW(hwnd, winuser::GWLP_USERDATA, 0);
            drop(Box::from_raw(userdata_ptr));
        }
    }

    ret
}

fn message_handler(
    data: &mut WindowData,
    hwnd: HWND,
    msg: UINT,
    wparam: WPARAM,
    lparam: LPARAM,
) -> LRESULT {
    println!("{}: {}", data.recurse_depth, dbg_msg(msg));

    let ret = match msg {
        winuser::WM_NCCREATE => {
            data.shared
                .window_count
                .set(data.shared.window_count.get() + 1);
            None
        }
        winuser::WM_NCDESTROY => {
            data.shared
                .window_count
                .set(data.shared.window_count.get() - 1);
            if data.shared.window_count.get() == 0 {
                unsafe { winuser::PostQuitMessage(0) };
            }
            None
        }
        _ => None,
    };

    ret.unwrap_or_else(|| unsafe {
        cir(data, || winuser::DefWindowProcW(hwnd, msg, wparam, lparam))
    })
}

/// Call indirectly recursing function.
///
/// Use this to call any function which might indirectly call back into the window procedure.
/// This is done so that borrowchk can pretend that this indirect recursion is like regular
/// recursion WRT to how mutable references behave. Passing around `&mut WindowData` without
/// this trick would be unsound.
fn cir<T>(_data: &mut WindowData, f: impl FnOnce() -> T) -> T {
    f()
}

struct UserData {
    panic_proxy: PanicProxyShared,
    window_data: WindowData,
}

impl UserData {
    fn new(shared: &SharedData, proxy: &PanicProxyShared) -> Self {
        Self {
            panic_proxy: Rc::clone(proxy),
            window_data: WindowData {
                recurse_depth: 0,
                destroyed: false,
                shared: Rc::clone(shared),
                extra: None,
            },
        }
    }
}

#[allow(dead_code)]
struct WindowData {
    recurse_depth: usize,
    destroyed: bool,
    shared: SharedData,
    extra: Option<WindowDataExtra>,
}

struct WindowDataExtra {
    //
}

struct Data {
    window_count: Cell<usize>,
}

type SharedData = Rc<Data>;

struct PanicProxy {
    panic: Cell<Option<Box<dyn Any + Send + 'static>>>,
}

impl PanicProxy {
    fn new() -> Self {
        Self {
            panic: Cell::new(None),
        }
    }

    /// Panicking across the FFI boundary is Undefined Behaviour.
    ///
    /// We avoid panicking across the FFI boundary by catching any panics with this method, and then resuming
    /// the unwind once we've gotten out of FFI-land.
    fn catch_unwind<T>(&self, f: impl FnOnce() -> T) -> Option<T> {
        let panic = self.panic.take();
        let result = if panic.is_none() {
            Some(panic::catch_unwind(panic::AssertUnwindSafe(f)))
        } else {
            None
        };

        if panic.is_none() {
            result.and_then(|result| match result {
                Ok(ok) => Some(ok),
                Err(err) => {
                    self.panic.set(Some(err));
                    None
                }
            })
        } else {
            self.panic.set(panic);
            None
        }
    }

    fn take_panic(&self) -> Option<Box<dyn Any + Send + 'static>> {
        self.panic.take()
    }
}

type PanicProxyShared = Rc<PanicProxy>;

fn get_last_error() -> DWORD {
    unsafe { winapi::um::errhandlingapi::GetLastError() }
}
