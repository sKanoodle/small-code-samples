open System.Text.RegularExpressions

let toInt = function
    | 'I' -> 1
    | 'V' -> 5
    | 'X' -> 10
    | 'L' -> 50
    | 'C' -> 100
    | 'D' -> 500
    | 'M' -> 1000

let rec ConvertFromRomanToArabic = function
    | [] -> 0
    | curr::next::rest when toInt curr < toInt next -> (toInt next - toInt curr) + ConvertFromRomanToArabic rest
    | curr::rest -> toInt curr + ConvertFromRomanToArabic rest

let ConvertFromRomanToArabicWithErrorChecking (roman:char list) =
    if Regex("(M*)?(CM|CD|D?C{1,3})?(XC|XL|L?X{1,3})?(IX|IV|V?I{1,3})?").Match(System.String.Concat(roman)).Length = roman.Length |> not
    then None else ConvertFromRomanToArabic roman |> Some

let toRoman = function
    | i when i >= 1000 -> ("M", 1000)
    | i when i >= 900 -> ("CM", 900)
    | i when i >= 500 -> ("D", 500)
    | i when i >= 400 -> ("CD", 400)
    | i when i >= 100 -> ("C", 100)
    | i when i >= 90 -> ("XC", 90)
    | i when i >= 50 -> ("L", 50)
    | i when i >= 40 -> ("XL", 40)
    | i when i >= 10 -> ("X", 10)
    | i when i >= 9 -> ("IX", 9)
    | i when i >= 5 -> ("V", 5)
    | i when i >= 4 -> ("IV", 4)
    | i when i >= 1 -> ("I", 1)

let rec ConvertFromArabicToRoman = function
    | 0 -> ""
    | arabic -> 
        let s, value = toRoman arabic
        s + ConvertFromArabicToRoman (arabic - value)

let testList f list =
    let printInputTuple t =
        printf "%-25O" t
        t
    list |> List.iter (printInputTuple >> f >> printfn "%b")

let ``test roman to arabic`` =
    testList (fun (r, a) -> Seq.toList r |> ConvertFromRomanToArabic |> (=) a)

let ``test roman to arabic with error checking`` =
    testList (fun (r, a) -> Seq.toList r |> ConvertFromRomanToArabicWithErrorChecking |> (=) a)

let ``test arabic to roman`` =
    testList (fun (r, a) -> a |> ConvertFromArabicToRoman |> (=) r)

[<EntryPoint>]
let main argv =
    let list = [("I", 1); ("II", 2); ("IV", 4); ("V", 5); ("IX", 9); ("XLII", 42); ("XCIX", 99); ("MMXIII", 2013)]
    let erroneousList = [("I", Some 1); ("MMXIII", Some 2013); ("MM XIII", None); ("DNFSA", None); (" ", None); ("", Some 0); ("VIIII", None); ("XM", None); ("MDLM", None)]
    printfn "roman -> arabic"; ``test roman to arabic`` list
    printfn "\nroman -> arabic"; ``test roman to arabic with error checking`` erroneousList
    printfn "\narabic -> roman"; ``test arabic to roman`` list
    System.Console.ReadKey() |> ignore
    0 // Integer-Exitcode zurückgeben
