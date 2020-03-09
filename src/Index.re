"e*R"
->Lexer.tokenize("+-*/^_", "!")
->Parser.parse(Grammar.weight)
->Evaluator.evaluate(
    Function.global_scope,
    [|"f(x) = 2x! * 5"|]
    ->Belt.Array.map(Function.func_exp_of_string)
    ->Belt.Array.keepMap(x => Belt.Option.map(x, x => Function.UserFunc(x))),
  )
// ->Belt.List.toArray
// ->Debugging.deep_log;

// let cls = Belt.Map.make(~id=(module Closure));
// open Belt.Map;
// cls->set("x", "2")->toArray->Js.Console.log;
// cls->toArray->Js.Console.log;

let is_foo = Regex.Make.is_function([|"cos", "sin", "tan"|]);
// [|"cos2", "sin", "x"|]->Belt.Array.map(is_foo)->Debugging.deep_log;
ReactDOMRe.renderToElementWithId(<Ui />, "root");
/*
 let _ =
   switch (Function.func_exp_of_string("foo(x)= (x^2)!")) {
   | Some(f) => f.exp->Debugging.deep_log
   };*/