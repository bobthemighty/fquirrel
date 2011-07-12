// Learn more about F# at http://fsharp.net
module Fquirrel.test

open NUnit.Framework
open FsUnit

open Fquirrel.Template
open Fquirrel.Parser

type Customer = {name: string; htmlText : string; isSpecial : bool}
    
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
       (parse template) {name= "Bob"; htmlText = ""; isSpecial = false} 
        |> should equal "Hello Bob! welcome to a working example."
        

[<TestFixture>]
type ``Given a template that contains unescaped html`` ()=
    let template = "Hello {{html htmlText}}"

    [<Test>] member test.
     ``the variable is correctly output.`` ()=
       (parse template) {name= "Bob"; htmlText = "<p>O HAI!</p>"; isSpecial = false} 
        |> should equal "Hello <p>O HAI!</p>"

[<TestFixture>]
type ``Given a template that uses html in a normal variable`` ()=
     let template = "Hello ${htmlText}"

     [<Test>] member test.
      ``the variable is html encoded`` ()=
        (parse template) {name = ""; htmlText = "<p> Badgers & Things</p>"; isSpecial = false} 
        |> should equal "Hello &lt;p&gt; Badgers &amp; Things&lt;/p&gt;"


[<TestFixture>]
type ``Given a template that contains an if block`` ()=
    let template = "Hello <b>${name}</b>\
{{if isSpecial}}\
 <blink>You are very special!</blink>\
{{/if}}"

    [<Test>] member test.
     ``when a customer is special, the block is output.`` ()=
       (parse template) {name= "Bob"; htmlText = "<p>O HAI!</p>"; isSpecial = true} 
        |> should equal "Hello <b>Bob</b><blink>You are very special!</blink>"
    
    [<Test>] member test.
     ``when a customer is not special, the block is ignored.`` ()=
       (parse template) {name= "Bob"; htmlText = "<p>O HAI!</p>"; isSpecial = false} 
        |> should equal "Hello <b>Bob</b>"


[<TestFixture>]
type ``Given an if block that contains variables`` ()=
    let template = "Hello <b>${name}</b>\
{{if isSpecial}}\
 <blink>${name} is very special!</blink>\
{{/if}}"

    [<Test>] member test.
     ``when a customer is special, the block is output.`` ()=
       (parse template) {name= "Bob"; htmlText = "<p>O HAI!</p>"; isSpecial = true} 
        |> should equal "Hello <b>Bob</b><blink>Bob is very special!</blink>"
    
    [<Test>] member test.
     ``when a customer is not special, the block is ignored.`` ()=
       (parse template) {name= "Bob"; htmlText = "<p>O HAI!</p>"; isSpecial = false} 
        |> should equal "Hello <b>Bob</b>"