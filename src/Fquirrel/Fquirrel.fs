namespace Fquirrel
    module Parser =


        open Template
        open FParsec
        open System.Web

        let (?) o n : 'T =  
            printfn "Requested property name for get'%s'" n
            let prop = o.GetType().GetProperty(n)
            downcast prop.GetValue(o, null)


        let Template foo data =
            foo |>             
                    (List.map(fun(p) ->
                            match p with
                            | Expr s -> HttpUtility.HtmlEncode(data?(s))
                            | Literal s -> s 
                            | Html s -> data?(s)
                    )
                    >> List.fold( fun acc x -> acc + x) "")
                    


        
        

       
        let parse tmpl = 
          let r = run template tmpl
          match r with
          | Success(result, _, _) -> Template(result)
          | Failure(msg, _, _)  -> raise (System.Exception(msg))