#if INTERACTIVE
#r "WindowsBase.dll"
#r "PresentationCore.dll"
#r "PresentationFramework.dll"
#r "System.Xaml"
#endif
open System
open System.Windows
open System.Windows.Controls

let private main (args: string []) =
    let w = Window(Title = (sprintf "%A" args))
    (new Application()).Run(w)

#if INTERACTIVE
fsi.CommandLineArgs |> Array.toList |> List.tail |> List.toArray |> main
#else
[<EntryPoint; STAThread>]
let entryPoint args = main args
#endif
