// include Fake libs
#r "packages/FAKE/tools/FakeLib.dll"

open Fake
open Fake.Testing.NUnit3

// Directories
let buildDir  = "./build/"
let testDir   = "./test/"
let deployDir = "./deploy/"

// Filesets
let appReferences = 
    !! "**/*.csproj"
        ++ "**/*.fsproj"
        -- "**/*.Tests.csproj"
        -- "**/*.Tests.fsproj"

let testReferences =
    !! "**/*.Tests.csproj"
        ++ "**/*.Tests.fsproj"

// Targets
Target "Clean" (fun _ -> 
    CleanDirs [buildDir; testDir; deployDir]
)

Target "BuildApp" (fun _ ->
    appReferences
        |> MSBuildRelease buildDir "Build"
        |> Log "AppBuild-Output: "
)

Target "BuildTests" (fun _ ->
    testReferences
        |> MSBuildDebug testDir "Build"
        |> Log "TestBuild-Output: "
)

Target "Test" (fun _ ->
    !! (testDir + "/*.Tests.dll")
        |> NUnit3 id
)

// Build order
"Clean"
    ==> "BuildApp"
    ==> "BuildTests"
    ==> "Test"

// start build
RunTargetOrDefault "Test"
