namespace Sudoku.Tests

open FsUnit
open NUnit.Framework

open Grid
open Analyse

[<TestFixture>]
module Tests =
    
    let readGrid3 gridText =
        let validChars = "-123456789"
        let chars = gridText |> Seq.filter (fun c -> Seq.exists (fun validChar -> c = validChar) validChars) |> Seq.toArray
        let charToContent c = if c = '-' then Unknown else Known (System.Int32.Parse(System.String.Concat(c)))
        Grid.New 3 (fun column row -> chars.[row * 9 + column] |> charToContent)

    let readGrid4 gridText =
        let validChars = "-1234567890ABCDEF"
        let chars = gridText |> Seq.filter (fun c -> Seq.exists (fun validChar -> c = validChar) validChars) |> Seq.toArray
        let charToContent c =
            match c with
            | '-' -> Unknown
            | '0' -> Known 10
            | 'A' -> Known 11
            | 'B' -> Known 12
            | 'C' -> Known 13
            | 'D' -> Known 14
            | 'E' -> Known 15
            | 'F' -> Known 16
            | _ -> Known (System.Int32.Parse(System.String.Concat(c)))
        Grid.New 4 (fun column row -> chars.[row * 16 + column] |> charToContent)

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
    let ``testHard`` ()=
        // "i" 06/09/2016
        let sampleGrid =
            "-5- --2 4--
             4-3 6-- --7
             2-- --3 ---
             
             3-- -8- -9-
             --- --- ---
             -4- -6- --1
             
             --- 1-- --9
             6-- --9 8-5
             --2 3-- -6-"

        let solutionGrid =
            "156 792 483
             483 615 927
             279 843 516
             
             361 584 792
             725 931 648
             948 267 351
             
             834 156 279
             617 429 835
             592 378 164"

        let result = readGrid3 sampleGrid |> Solve
        result |> should equal (Solution (readGrid3 solutionGrid))

    [<Test>]
    let ``testExtreme`` ()=
        // Arto Inkala, 2010
        let sampleGrid =
            "--5 3-- ---
             8-- --- -2-
             -7- -1- 5--
             
             4-- --5 3--
             -1- -7- --6
             --3 2-- -8-
             
             -6- 5-- --9
             --4 --- -3-
             --- --9 7--"

        let result = readGrid3 sampleGrid |> Solve
        result |> isSolution |> should equal true

    [<Test>]
    let ``test4 1`` ()=
        let sampleGrid =
            "---- 4--- ---- ---8
             -B6D ---- 2--- -4--
             --37 CB1- --65 ---2
             1C-A ---3 -8-- -567

             ---- ---- 8CB- 5-A-
             --F- -6-- -403 E--D
             E-7- ---- --2D 48C-
             -A1- 3CD- EF-- ---B

             968- 7A-- --C0 --5-
             -10F D8-- 3-5E ----
             7--5 ---- -9A- --3-
             ---- ---C -6-- D--9

             -0-- -392 B--- A---
             ---- ---7 C-E9 3-D4
             89B- 5--E AD-F 07--
             C-5- ---- ---4 ----"
        
        let solutionGrid =
            "25E9 4F6D 107A C3B8
             0B6D 9578 23FC 14EA
             F837 CB1A 4E65 9D02
             1C4A E203 98DB F567

             D496 27E0 8CB1 5FA3
             B2FC A681 5403 E97D
             E370 B9F5 6A2D 48C1
             5A18 3CD4 EF97 602B

             9684 7A3F D1C0 2B5E
             A10F D829 3B5E 764C
             7DC5 6E4B F9A2 8130
             3E2B 015C 7648 DAF9

             40DE F392 B716 AC85
             6FA1 80B7 C5E9 32D4
             89B2 54CE AD3F 0716
             C753 1DA6 0284 BE9F"

        let result = readGrid4 sampleGrid |> Solve
        result |> should equal (Solution (readGrid4 solutionGrid))
    
    [<Test>]
    let ``test4 2`` ()=
        let sampleGrid =
            "-372 4--- -B1- E-50
             4F59 --A- --D0 --1-
             --06 ---- ---- -2--
             E1-- ---D 27F3 -C--

             ---- --04 -278 5-6-
             -5-- BE71 -4C- A-0-
             --8A 2--- 56-E --7C
             2--- ---- A--9 D---

             394- A-CB ---- 6--F
             ---- ---- 4--5 1---
             1--- 87-- ---6 ---9
             -2-F 5--- --0B -A--

             ---- 7-2A E19D 8FC-
             ---7 D460 ---2 3BA-
             A-3- E8-- ---- -0--
             ---8 C1-- ---4 --DE"
        
        let solutionGrid =
            "C372 49F8 6B1A ED50
             4F59 3CA2 8ED0 B617
             D806 1BE7 954C F23A
             E1AB 065D 27F3 4C98

             BCFD 9A04 1278 5E63
             6593 BE71 D4CF A802
             048A 2FD3 56BE 917C
             271E 658C A039 D4FB

             3940 A2CB 7D81 65EF
             8B6C F039 4AE5 172D
             1AD5 874E CF26 03B9
             72EF 5D16 390B CA84

             50B4 732A E19D 8FC6
             9EC7 D460 F852 3BA1
             AD31 E89F BC67 2045
             F628 C1B5 03A4 79DE"

        let result = readGrid4 sampleGrid |> Solve
        result |> should equal (Solution (readGrid4 solutionGrid))
    
    [<Test>]
    let ``test4 3`` ()=
        let sampleGrid =
            "-F5- ---6 D--- ---E
             --3- ---- ---- -C-7
             -BAC 801- 975E ----
             ---8 9--- ---- 6A--

             3-41 A5-- -98- -F-C
             86D- 2E4- -B-- -70-
             29-7 ---- ---- --1B
             -5B- 71-9 ---4 -2AD

             54-D F20- C-E- ----
             -123 --8- -A7- -4-5
             EC-- ---- ---- 7---
             ---- -C-- 629D 01E8

             ---- D-EF 3-67 5---
             ---0 -6-- ---- ---A
             A--- 173- 2--- -8-4
             4--F ---- 8--- 9E7-"
        
        let solutionGrid =
            "7F54 C326 D0A1 B98E
             0D39 EFBA 4826 1C57
             6BAC 801D 975E F342
             12E8 9475 BFC3 6AD0

             3041 A5DB 7982 EF6C
             86DA 2E4C 1BF5 3709
             29F7 0863 AEDC 451B
             C5BE 71F9 0634 82AD

             548D F207 C1EB A693
             9123 6D8E FA70 C4B5
             EC06 B9A1 5348 7D2F
             FA7B 3C54 629D 01E8

             B892 DAEF 3467 50C1
             D7C0 4698 E51F 2B3A
             AE65 1730 2CB9 D8F4
             431F 5BC2 8D0A 9E76"

        let result = readGrid4 sampleGrid |> Solve
        result |> should equal (Solution (readGrid4 solutionGrid))
    
    [<Test>]
    let ``test4 4`` ()=
        let sampleGrid =
            "--B- --89 -6F1 ---D
             EC-- -5-1 72-3 A9--
             --F8 ---- ---- --B2
             ---9 072E 8--- ---C

             -DC- E-F- 9--B 4---
             5--- ---- --4- --91
             --0- 1A-C ---- E523
             F84- 5--D 3-70 -C--

             ---- 7C-F 21-- 0--E
             -3-- -96B 50C- -A-4
             --DA ---- --6- -F3-
             ---- ---- -A-- ----

             -2-- -6-- ---C ---5
             -6A- -EC5 4-87 D--B
             -1-- 28-- --5- ----
             --9- ---- ---A -3--"
        
        let solutionGrid =
            "2AB3 C489 E6F1 705D
             EC6D F5B1 7203 A948
             07F8 6D3A C495 1EB2
             1459 072E 8BAD 36FC

             ADC2 E3F6 951B 4870
             5E36 8B70 AC42 FD91
             9B07 1A4C F8D6 E523
             F841 529D 3E70 BC6A

             6985 7CAF 2134 0BDE
             731E D96B 50CF 2A84
             C0DA 41E2 B768 5F39
             BF24 3058 DAE9 61C7

             82EF A6D4 13BC 9705
             36A0 9EC5 4F87 D21B
             D17B 2803 695E C4AF
             459C BF17 0D2A 83E6"

        let result = readGrid4 sampleGrid |> Solve
        result |> should equal (Solution (readGrid4 solutionGrid))
    
    [<Test>]
    let ``test4 5`` ()=
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
