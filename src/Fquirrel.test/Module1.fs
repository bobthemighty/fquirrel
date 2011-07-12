// Learn more about F# at http://fsharp.net
module Fquirrel.test

open NUnit.Framework
open FsUnit

open Fquirrel.Template
open Fquirrel.Parser

type Customer = {name: string}
    
[<TestFixture>]
type ``Given a literal fragment`` ()=
    let template = "Hello world!"

    [<Test>] member test.
     ``The literal parser yields a function returning the literal text`` ()=
         (parse template) () |> should equal "Hello world!"
    

[<TestFixture>]
type ``Given a template that contains a variable`` ()=
    let template = "Hello ${name}! welcome to a working example."

    [<Test>] member test.
     ``the variable is correctly output.`` ()=
       (parse template) {name= "Bob"} |> should equal "Hello Bob! welcome to a working example."
        


(*
[<TestFixture>]
type ``Given a template that contains literals and variables`` ()=
    let template = "Hello ${name}!"

    [<Test>] member test.
     ``when I evaluate the template for html, I receive a function.`` ()=
        printf "%A" (parse template)

*)