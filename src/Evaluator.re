module Closure =
  Belt.Id.MakeComparable({
    type t = string;
    let cmp: (t, t) => int = Pervasives.compare;
  });

let evaluate =
    (
      tree,
      user_scope: Belt.Map.t(string, float, _),
      functions: array(Function.func),
    ) => {
  open Tree;
  open Function;

  let functions = Belt.Array.concat(functions, Function.static_functions);

  let rec calc = (node, scope) => {
    switch (node) {
    | Number(n) => Js.Float.fromString(n)
    | Variable(v) =>
      switch (Belt.Map.get(scope, v)) {
      | Some(v) => v
      | None =>
        raise(
          Js.Exn.raiseReferenceError(
            {j|Variable $v is not defined in the current scope|j},
          ),
        )
      }

    | N0(n) => calc(n, scope)

    | N1(Function(f) | Suffix(f), n) =>
      let f_def =
        Belt.Array.getBy(functions, (StaticFunc({name}) | UserFunc({name})) =>
          name == f
        );
      switch (f_def) {
      | Some(UserFunc({var, exp})) =>
        let f_scope = scope->Belt.Map.set(var, calc(n, scope));
        calc(exp, f_scope);
      | Some(StaticFunc({eval})) => eval(calc(n, scope))
      | None =>
        raise(Js.Exn.raiseReferenceError({j|Function $f not found|j}))
      };

    | N2(Infix(op), a, b) =>
      switch (Belt.Array.getBy(Function.operators, ({name}) => name == op)) {
      | Some({eval: f}) => f(calc(a, scope), calc(b, scope))
      | None =>
        raise(
          Js.Exn.raiseReferenceError(
            {j|Operator ($op) is not defined in the current scope|j},
          ),
        )
      }

    | _ =>
      raise(
        Js.Exn.raiseEvalError("Compilation error: empty node encountered"),
      )
    };
  };

  calc(tree, user_scope);
};