namespace Fquirrel
    module Parser =

        open AST
        open Template
        open FParsec
        open System.Web

        let (?) o n : 'T =  
            match n with
            | "" -> unbox(false)
            | _ ->let prop = o.GetType().GetProperty(n)
                  downcast prop.GetValue(o, null)


        let rec Template foo data =
            let applyCond (p :parsedTemplate) : string =
                match p with
                | If cb when data?(cb.expression) -> Template cb.body data
                | If cb -> 
                        let e = List.tryPick (function | Else e -> Some(e) |_ -> None) cb.body
                        match e with
                        | Some x when x.expression = "" -> Template e.Value.body data
                        | Some x when data?(x.expression) -> Template e.Value.body data
                        | _ -> ""
                | _ -> ""

            foo |>             
                    (List.map(fun(p) ->
                            match p with
                            | Expr s -> HttpUtility.HtmlEncode(data?(s))
                            | Literal s -> s 
                            | Html s -> data?(s)
                            | If cb -> applyCond p
                            | Else cb -> applyCond p
                    )
                    >> List.fold( fun acc x -> acc + x) "")

        let parse tmpl = 
          let r = run template tmpl
          match r with
          | Success(result, _, _) -> Template(result)
          | Failure(msg, _, _)  -> raise (System.Exception(msg))