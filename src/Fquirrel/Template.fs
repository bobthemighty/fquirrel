namespace Fquirrel

module Template =
    open System
    open System.IO
    open FParsec


    [<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
    type parsedTemplate =   Expr of string
                            | Literal of string
                            | Html of string
     

    type UserState = unit
    type Parser<'t> = Parser<'t, UserState>
    


    let ws = spaces
    let str s = pstring s

    let identifier : Parser<_> =
        let isIdentifierFirstChar c = isLetter c || c = '_'
        let isIdentifierChar c = isLetter c || isDigit c || c = '_'

        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws // skips trailing whitepace



    let betweenStrings s1 s2 p = str s1 >>. p .>> str s2
    
    let expr = (identifier |> betweenStrings "${" "}") |>> Expr
    let normalChar : Parser<_> = satisfy(fun c -> c <> '$' && c <> '{')
        
    let literal : Parser<_> = 
        (many1Chars normalChar) |>> Literal

    let htmlFrag : Parser<_> =
        (identifier |> betweenStrings "{{html " "}}") |>> Html

    let template =     many1 (htmlFrag <|> expr <|> literal)
