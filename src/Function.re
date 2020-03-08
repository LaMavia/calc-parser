open Tree;

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
};

let operators = [|
  {name: "+", eval: (+.)},
  {name: "-", eval: (-.)},
  {name: "*", eval: ( *. )},
  {name: "/", eval: (/.)},
  {name: "^", eval: ( ** )},
  {name: "_", eval: (a, b) => b ** (1.0 /. a)},
|];

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