// Learn more about F# at http://fsharp.net
module Fquirrel.test

open NUnit.Framework
open FsUnit

open Fquirrel.Template
open Fquirrel.Parser

type Customer = {name: string; htmlText : string}
    
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
       (parse template) {name= "Bob"; htmlText = ""} 
        |> should equal "Hello Bob! welcome to a working example."
        

[<TestFixture>]
type ``Given a template that contains unescaped html`` ()=
    let template = "Hello {{html htmlText}}"

    [<Test>] member test.
     ``the variable is correctly output.`` ()=
       (parse template) {name= "Bob"; htmlText = "<p>O HAI!</p>"} 
        |> should equal "Hello <p>O HAI!</p>"

[<TestFixture>]
type ``Given a template that uses html in a normal variable`` ()=
     let template = "Hello ${htmlText}"

     [<Test>] member test.
      ``the variable is html encoded`` ()=
        (parse template) {name = ""; htmlText = "<p> Badgers & Things</p>"} 
        |> should equal "Hello &lt;p&gt; Badgers &amp; Things&lt;/p&gt;"