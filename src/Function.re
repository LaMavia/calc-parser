open Tree;

module Closure =
  Belt.Id.MakeComparable({
    type t = string;
    let cmp: (t, t) => int = Pervasives.compare;
  });

type func_exp = {
  name: string,
  var: string,
  exp: node,
};

type func_static = {
  name: string,
  eval: float => float,
};

type func =
  | UserFunc(func_exp)
  | StaticFunc(func_static);

let static_functions =
  [|
    // cos
    {name: "cos", eval: cos},
    {name: "cosh", eval: cosh},
    // sin
    {name: "sin", eval: sin},
    {name: "sinh", eval: sinh},
    // tan
    {name: "tan", eval: tan},
    {name: "tanh", eval: tanh},
    {name: "log", eval: log},
    {
      name: "!",
      eval: {
        let rec f = n =>
          if (n < 2.0) {
            1.0;
          } else {
            n *. f(n -. 1.0);
          };
        f;
      },
    },
  |]
  |> Array.map(x => StaticFunc(x));

type operator = {
  name: string,
  eval: (float, float) => float,
  identity: float,
};

let operators = [|
  {name: "+", identity: 0.0, eval: (+.)},
  {name: "-", identity: 0.0, eval: (-.)},
  {name: "*", identity: 1.0, eval: ( *. )},
  {name: "/", identity: 1.0, eval: (/.)},
  {name: "^", identity: 1.0, eval: ( ** )},
  {name: "_", identity: 1.0, eval: (a, b) => b ** (1.0 /. a)},
|];

let eval_op = (op, f) =>
  switch (Belt.Array.getBy(operators, ({name}) => name == op)) {
  | Some(def) => f(def)
  | None =>
    raise(
      Js.Exn.raiseReferenceError(
        {j|Operator ($op) is not defined in the current scope|j},
      ),
    )
  };

let global_scope = {
  Belt.Map.(
    make(~id=(module Closure))
    ->set("e", Js.Math._E)
    ->set("p", Js.Math._PI)
    ->set("R", 8.31)
  );
};

/**Converts a function definition (string) to a function expression
 * ```reason
 * Regex.func_exp_of_string("foo(x)= (x^2)!")
 * == {
 *  name: "foo",
 *  var: "x",
 *  exp: "(x^2)!"
 * }
 * ``` */
let func_exp_of_string = f =>
  Belt.(
    Js.Re.exec_(Regex.re_function_exp, f)
    ->Option.map(r =>
        r
        ->Js.Re.captures
        ->Array.map(x => x |> Js.Nullable.toOption |> Option.getExn)
        ->Array.sliceToEnd(1)
      )
    ->Option.map(r =>
        switch (r) {
        | [|n, v, e|] => {
            name: n,
            var: v,
            exp:
              e
              ->Lexer.tokenize(Static.infixes, Static.suffixes)
              ->Parser.parse(Grammar.weight),
          }
        | _ =>
          raise(
            Js.Exn.raiseError(
              {j|Error converting "$f" to a function expression|j},
            ),
          )
        }
      )
  );