//Copyright © 2016 Dominik Pytlewski. Licensed under Apache License 2.0. See LICENSE file for details

(*
Finds out version from most recent git tag. 
Finds assemblyinfo and nuspec files and updates relevant version fields.
NOTE: it assumes tag naming convention v[\d]+
*)

open System.Diagnostics
open System.IO
open System.Text.RegularExpressions

let captureStdout program args =
    let result = System.Text.StringBuilder()

    use proc = new Process()
    proc.StartInfo.UseShellExecute <- false
    proc.StartInfo.CreateNoWindow <- true
    proc.StartInfo.FileName <- program
    proc.StartInfo.Arguments <- args
    proc.StartInfo.RedirectStandardOutput <- true
    proc.EnableRaisingEvents <- true    
    proc.OutputDataReceived.AddHandler(DataReceivedEventHandler(fun _ x -> result.Append(x.Data) |> ignore))

    proc.Start() |> ignore
    proc.BeginOutputReadLine()
    
    proc.WaitForExit()
    match proc.ExitCode with
    | 0 -> result.ToString()
    | _ -> failwithf "process %s %s returned error exit code %i" program args proc.ExitCode

let getVersionFromlatestGitTag () = 
    //git command sequence by kilianc
    //http://stackoverflow.com/questions/1404796/how-to-get-the-latest-tag-name-in-current-branch-in-git#7979255

    let sha = captureStdout "git" "rev-list --tags --max-count=1"
    printfn "sha %s" sha

    let tag = captureStdout "git" (sprintf "describe --tags %s" sha)
    printfn "tag %s" tag

    let pattern = "^v([\d\.]+)$"
    let reVersion = Regex(pattern, RegexOptions.IgnoreCase)
    
    match reVersion.Match tag with
    |x when x.Success -> x.Groups.[1].Value
    |_ -> failwithf "tag %s doesn't match pattern %s" tag pattern
    
let rec iterateFiles dirMatcher fileMatcher rootFolder = 
    seq {
        let files = Directory.GetFiles rootFolder |> Seq.ofArray |> Seq.filter fileMatcher
        let dirs = Directory.GetDirectories rootFolder |> Seq.ofArray |> Seq.filter dirMatcher
        
        yield! files
        for d in dirs do 
            yield! iterateFiles dirMatcher fileMatcher d
    }

type Replacement = {
    From : Regex
    ChangeTo : string
}
with static member build version re subst = {From = Regex re; ChangeTo = sprintf subst version }

type FileRule = {
    FileName : Regex
    Substitutes : Replacement list
}

let rec adjust adjustments content = 
    match adjustments with
    | []  -> content
    | x::rest -> 
       adjust rest (x.From.Replace(content, x.ChangeTo))

let version = getVersionFromlatestGitTag ()
printfn "version %s" version
let replacement = Replacement.build version

let adjustments = [
    {FileRule.FileName = Regex "(?i)^assemblyinfo\.[cf]s$"; Substitutes = 
        [
            replacement """AssemblyVersion\("[\d\.]+"\)""" """AssemblyVersion("%s")""" 
            replacement """AssemblyFileVersion\("[\d\.]+"\)""" """AssemblyFileVersion("%s")"""
        ]
    }
    {FileRule.FileName = Regex "(?i)\.nuspec$"; Substitutes = 
        [
            replacement """<version>[\s\d\.]+</version>""" """<version>%s</version>"""
        ]
    }
    {FileRule.FileName = Regex "(?i)\.[cf]sproj"; Substitutes = 
        [
            replacement """<Version>[\s\d\.]+</Version>""" """<Version>%s</Version>"""
        ]
    }
]

let dirsToIgnore path = 
    [".git"; "bin"; "obj"; ".vs"] |> List.contains (Path.GetFileName(path)) |> not
    
let filterOutNonAdjustable path = 
    let fileName = (Path.GetFileName path).ToLower()
    
    adjustments
    |> List.choose (fun x -> if x.FileName.IsMatch fileName then Some x else None)
    |> List.isEmpty
    |> not

iterateFiles dirsToIgnore filterOutNonAdjustable "." 
|> Seq.iter (fun filePath -> 
    let fileName = (Path.GetFileName filePath).ToLower()

    adjustments
    |> List.tryPick (fun x -> if x.FileName.IsMatch fileName then Some x else None)
    |> function
        | None -> failwithf "bug: path %s cannot be adjusted anymore" filePath
        | Some adj -> 
            let content = File.ReadAllText filePath |> adjust adj.Substitutes 
            File.WriteAllText(filePath, content)
            printfn "changed %s" filePath )

0