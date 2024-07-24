
//Q1.a
let yashsalary = [75000;48000; 120000; 190000; 300113; 92000; 36000]
let myfilteredList = List.filter (fun x -> x > 100000) yashsalary
printfn "Q1.a.Answer; Filtered Hign Salary: %A" myfilteredList


//Q1.b
let mysalaries = [75000; 48000; 120000; 190000; 300113; 92000; 36000]

let Taxcalc (x: int) =
    let xmyFloat = float x
    match x with
    | x when x <= 49020 -> xmyFloat * 0.15 
    | x when x > 49020 && x <= 98040 -> xmyFloat * 0.205 
    | x when x > 98040 && x <= 151978 -> xmyFloat * 0.26 
    | x when x > 151978 && x <= 216511 -> xmyFloat * 0.29 
    | _ -> xmyFloat * 0.33 

let Salariestax = mysalaries |> List.map Taxcalc
let salaryAfterTax = List.zip mysalaries Salariestax |> List.map (fun (salary, tax) -> salary + int tax) 

printfn "Q1.b.Answer; Taxes for each salary: %A" Salariestax
printfn "Q1.b.Answer; Salaries after adding tax: %A" salaryAfterTax






//Q1.c
let newsalary = [75000;48000; 120000; 190000; 300113; 92000; 36000]

let filteredSalaries = List.filter (fun x -> x < 49020) newsalary

let AddSalary = List.map (fun x -> x + 20000) filteredSalaries

printfn "Q1.c.Answer; Value After Adding 20000 to Salaries: %A" AddSalary


//Q1.d

let dsalary = [75000;48000; 120000; 190000; 300113; 92000; 36000]

let filterlessSalaries = List.filter (fun x -> x > 50000 && x < 100000) dsalary

let additionOfFilteredNumbers = List.fold (+) 0 filterlessSalaries

printfn "Q1.d.Answer;Filtered salary between 50000 and 100000 are: %A" filterlessSalaries
printfn "Q1.d.Answer;The sum of all all filtered numbers is %d" additionOfFilteredNumbers



//Q.2

let multiple (n: int) = 
    let rec countMultiple (current: int) (acc: int) =         
        if current <= 0 then acc
        else
            countMultiple (current - 3) (acc + current)
    countMultiple n 0

let res = multiple 27

printfn "Q2.Answer: The sum of all multiples of 3 up to 27 is %d" res




