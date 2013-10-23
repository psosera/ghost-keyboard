module Program

open System
open System.Diagnostics
open System.Drawing
open System.IO
open System.Runtime.InteropServices
open System.Windows.Forms

(******************************************************************************
 * Keyboard model
 *****************************************************************************)

// The type of all keystrokes this program monitors
type Key =
   | Kb of Keys
   | Mouse1
   | Mouse2

// The global state of keystrokes maintained by the hook callbacks
let state:(Key * bool ref) list =
  let mkRef():bool ref = ref false
  (Mouse1, mkRef())::(Mouse2, mkRef())::
  List.map (fun k -> (k, mkRef()))
           [for i in Enum.GetValues(typeof<Keys>) -> i :?> Keys |> Kb]

// Helper function that looks up in values in a given associative list
let lookup (x:'a) (l:('a * 'b) list):'b option =
  match List.tryFind (fun (y, v) -> x = y) l with
  | Some (y, v) -> Some v
  | None        -> None

// Toggles the flag of the given key in the global state
let toggle (k:Key) (b:bool):unit =
  match lookup k state with
  | Some r -> r := b
  | _      -> ()

(******************************************************************************
 * GUI
 *****************************************************************************)

let bgFile   = "keyboard.png"
let locsFile = "keyboardlocs.txt"
let hlColor  = Color.FromArgb(128, 0, 0, 200)

// Retrieves the location data for the keyboard image file so we can highlight
// keystrokes as they occur.
let keyLocs:(Key * Rectangle) list =
  File.ReadAllLines locsFile
  |> Array.map
      (fun line ->
         match line.Split([|' '|]) with
         | [|code; x; y; width; height|] ->
           let rect = Rectangle(Int32.Parse(x), Int32.Parse(y),
                                Int32.Parse(width), Int32.Parse(height))
           (match code with
            | "mouse1" -> (Mouse1, rect)
            | "mouse2" -> (Mouse2, rect)
            | _        -> (Int32.Parse(code) |> enum |> Kb, rect))
         | _        -> raise (FormatException()))
  |> List.ofArray

[<DllImport("User32.dll", CharSet = CharSet.Auto, SetLastError = true)>]
extern bool ReleaseCapture()

[<DllImport("User32.dll", CharSet = CharSet.Auto, SetLastError = true)>]
extern int SendMessage(IntPtr hWnd, int msg, int wParam, int lParam)

// The Form on which we display keystrokes.
type GhostKeyboardForm() as self =
  inherit Form()
  let wmNclButtonDown = 0xA1
  let htcaption = 0x2
  let bg = Image.FromFile(bgFile)
  let brush = new SolidBrush(hlColor)
  do self.Icon           <- new Icon("ghost.ico")
     self.Size           <- bg.Size
     self.Text           <- ""
     self.DoubleBuffered <- true
     self.TopMost        <- true
     self.ControlBox     <- false
     self.MouseDown.Add(fun e ->
       do match e.Button with
          | MouseButtons.Left ->
              ReleaseCapture() |> ignore
              SendMessage(self.Handle, wmNclButtonDown, htcaption, 0) |>  ignore
          | _ -> ())
  override self.OnPaint (e:PaintEventArgs):unit =
    let transform (rect:Rectangle) (wp:single) (hp:single) =
      Rectangle(single rect.X * wp |> int , single rect.Y * hp |> int,
                single rect.Width * wp |> int, single rect.Height * hp |> int)
    let g = e.Graphics
    let (wp, hp) = (single self.ClientRectangle.Width / single bg.Size.Width,
                    single self.ClientRectangle.Height / single bg.Size.Height)
    do g.DrawImage(bg, 0, 0, (single bg.Width) * wp |> int,
                             (single bg.Height) * hp |> int)
       List.iter (fun ((k, rect):(Key * Rectangle)) ->
                    match lookup k state with
                    | Some rb  -> if !rb
                                  then g.FillRectangle(brush,
                                                       transform rect wp hp)
                                  else ()
                    | None     -> ())
                 keyLocs 

// The instance of the form we'll use in the program.
let form = new GhostKeyboardForm()

(******************************************************************************
 * The keystroke logger
 *****************************************************************************)

let whKeyboardLL  = 13
let whMouseLL     = 14
let wmKeyDown     = IntPtr 0x0100
let wmKeyUp       = IntPtr 0x0101
let wmLButtonDown = IntPtr 0x0201
let wmLButtonUp   = IntPtr 0x0202
let wmRButtonDown = IntPtr 0x0204
let wmRButtonUp   = IntPtr 0x0205

let kbHookID      = ref IntPtr.Zero
let mouseHookID   = ref IntPtr.Zero

type LowLevelInputProc = delegate of int * IntPtr * IntPtr -> IntPtr

[<DllImport("user32.dll", CharSet = CharSet.Auto, SetLastError = true)>]
extern IntPtr SetWindowsHookEx(int idHook, LowLevelInputProc lpfn,
                               IntPtr hMod, uint32 dwThreadId)

[<DllImport("user32.dll", CharSet = CharSet.Auto, SetLastError = true)>]
extern bool UnhookWindowsHookEx(IntPtr hhk)

[<DllImport("user32.dll", CharSet = CharSet.Auto, SetLastError = true)>]
extern IntPtr CallNextHookEx(IntPtr hhk, int nCode,
                             IntPtr wParam, IntPtr lParam)

[<DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)>]
extern IntPtr GetModuleHandle(string lpModuleName)

[<DllImport("user32.dll", CharSet = CharSet.Auto, SetLastError = true)>]
extern int GetKeyState(int nVirtKey)

// Install the given low-level input hook.
let setHook (kind:int) (hook:LowLevelInputProc):IntPtr =
  use curProcess = Process.GetCurrentProcess()
  use curModule  = curProcess.MainModule
  SetWindowsHookEx(kind, hook,
                   GetModuleHandle(curModule.ModuleName), uint32 0)

// Constructs a low-level input hook from an action over the parameters of the
// the hook's callback.
let createHook (action:IntPtr -> IntPtr -> bool)
               (hookId:IntPtr ref) : int -> IntPtr -> IntPtr -> IntPtr =
  fun (nCode:int) (wParam:IntPtr) (lParam:IntPtr) ->
    if nCode < 0 then () else
      if action wParam lParam then form.Invalidate() else ()
    CallNextHookEx(!hookId, nCode, wParam, lParam)

// The keyboard hook action.
let kbHook (wParam:IntPtr) (lParam:IntPtr):bool =
  let key:Keys = Marshal.ReadInt32(lParam) |> enum
  match wParam with
  | p when p = wmKeyDown -> toggle (Kb key) true; true
  | p when p = wmKeyUp   -> toggle (Kb key) false; true
  | _                    -> false

// The mouse hook action.
let mouseHook (wParam:IntPtr) (lParam:IntPtr):bool =
  match wParam with
  | p when p = wmLButtonDown -> toggle Mouse1 true; true
  | p when p = wmLButtonUp   -> toggle Mouse1 false; true
  | p when p = wmRButtonDown -> toggle Mouse2 true; true
  | p when p = wmRButtonUp   -> toggle Mouse2 false; true
  | _                        -> false

// NOTE: this is necessary to prevent the delegates from being GCed
let keyboardProc = new LowLevelInputProc(createHook kbHook kbHookID)
let mouseProc = new LowLevelInputProc(createHook mouseHook mouseHookID)

#if COMPILED
[<STAThread>]
do kbHookID := setHook whKeyboardLL keyboardProc
   mouseHookID := setHook whMouseLL mouseProc
   Application.Run(form)
   UnhookWindowsHookEx(!kbHookID) |> ignore
   UnhookWindowsHookEx(!mouseHookID) |> ignore
#endif
