"5 ^ 2!"
->Lexer.tokenize("+-*/^_", "!")
->Parser.parse(Grammar.weight)
->Debugging.deep_log;

module Closure =
  Belt.Id.MakeComparable({
    type t = string;
    let cmp: (string, string) => int = Pervasives.compare;
  });

// let cls = Belt.Map.make(~id=(module Closure));
// open Belt.Map;
// cls->set("x", "2")->toArray->Js.Console.log;
// cls->toArray->Js.Console.log;

let is_foo = Regex.Make.is_function([|"cos", "sin", "tan"|]);
[|"cos2", "sin", "x"|]->Belt.Array.map(is_foo)->Debugging.deep_log;

/* ReactDOMRe.renderToElementWithId(<Ui/>, "root"*/