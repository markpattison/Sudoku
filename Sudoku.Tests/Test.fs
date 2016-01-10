namespace Sudoku.Tests

open FsUnit
open NUnit.Framework

open Analyse

[<TestFixture>]
module Tests =
    
    let readGrid3 gridText =
        let validChars = "-123456789"
        let chars = gridText |> Seq.filter (fun c -> Seq.exists (fun validChar -> c = validChar) validChars) |> Seq.toArray
        let charToContent c = if c = '-' then None else Some (System.Int32.Parse(System.String.Concat(c)))
        Grid.New 3 (fun x y -> chars.[y * 9 + x] |> charToContent)

    let readGrid4 gridText =
        let validChars = "-1234567890ABCDEF"
        let chars = gridText |> Seq.filter (fun c -> Seq.exists (fun validChar -> c = validChar) validChars) |> Seq.toArray
        let charToContent c =
            match c with
            | '-' -> None
            | '0' -> Some 10
            | 'A' -> Some 11
            | 'B' -> Some 12
            | 'C' -> Some 13
            | 'D' -> Some 14
            | 'E' -> Some 15
            | 'F' -> Some 16
            | _ -> Some (System.Int32.Parse(System.String.Concat(c)))
        Grid.New 4 (fun x y -> chars.[y * 16 + x] |> charToContent)

    [<Test>]
    let ``test3`` ()=
        let sampleGrid =
            "6-- -7- 49-
             -5- 36- 2--
             --- --9 ---

             -19 --- 8-7
             5-- --- --9
             8-3 --- 54-

             --- 9-- ---
             --8 -15 -7-
             -92 -3- --5"

        let solutionGrid =
            "681 572 493
             957 364 218
             234 189 756

             419 253 867
             576 841 329
             823 796 541

             765 928 134
             348 615 972
             192 437 685"

        let result = readGrid3 sampleGrid |> Solve
        result |> should equal (Solution (readGrid3 solutionGrid))

    [<Test>]
    let ``test4 1`` ()=
        let sampleGrid =
            "---1 -FD- 69-- -C--
             ---- A--6 -7D- 9-81
             3B-- C--- 2--4 5AF-
             ---6 94-- FC-- -2BE

             5-94 B--F A--- ----
             ---A ---- ---- 7B0D
             1--C 4-98 B32F -6A5
             ---- --A- -D5- 4-3-

             ---- 50-- 32F- -E--
             -67F --1- C--- -3D-
             -5C2 ---3 0E67 ----
             D--- -6-A 1--- -F-9

             0--E 63B- ---- -1--
             6--- ---7 DB-- 0-92
             A--- ---- --4- ----
             C--7 28-- 51-- --E-"
        
        let solutionGrid =
            "E821 0FDB 69A5 3C47
             FC45 A236 E7DB 9081
             3BD9 CE71 2804 5AF6
             70A6 9485 FC13 D2BE

             5394 BD6F A07E 182C
             8FEA 3152 46C9 7B0D
             1D0C 4798 B32F E6A5
             276B ECA0 8D51 493F

             9A18 50C4 32FD 6E7B
             467F 8B1E C59A 23D0
             B5C2 D9F3 0E67 A418
             DE30 762A 14B8 CF59

             095E 63BD 7A82 F1C4
             6183 FA47 DBEC 0592
             A2BD 15EC 9F40 8763
             C4F7 2809 5136 BDEA"

        let result = readGrid4 sampleGrid |> Solve
        result |> should equal (Solution (readGrid4 solutionGrid))
