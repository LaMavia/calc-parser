open Grammar;
let tokenize = (str, infixes, suffixes) => {
  open Regex;
  let chars =
    str
    ->Js.String2.splitByRe(re_space_any)
    ->Belt.Array.keepMap(x =>
        switch (x) {
        | Some(x) when x != "" => Some(x)
        | _ => None
        }
      );

  let is_infix = Make.is_op(infixes);
  let is_suffix = Make.is_op(suffixes);

  let peak = chars => List.nth(chars, 0);
  let prev = tokens => Belt.List.get(tokens, 0);

  let rec loop = (chars, current, t) => {
    switch (chars, current) {
    | ([], None) => List.rev(t)
    | ([], Some(a)) => loop([], None, [a, ...t])
    | _ =>
      let head = List.hd(chars);
      let tail = List.tl(chars);
      let next = loop(tail);

      switch (head, current, prev(t)) {
      // state: None
      | ("(", None, _) => next(None, [Bracket(Open), ...t])
      | (")", None, _) => next(None, [Bracket(Close), ...t])
      | (a, None, _) when is_num(a) => next(Some(Number(a)), t)
      | (a, None, _) when is_name(a) => next(Some(Variable(a)), t)
      // | (a, None, None | Some(Bracket(_))) when is_sign(a) =>
      //   next(Some(Sign(a)), t)
      | (a, None, _) when is_infix(a) =>
        next(None, [Operator(Infix(a)), ...t])
      | (a, None, _) when is_suffix(a) =>
        next(None, [Operator(Suffix(a)), ...t])

      // state: Number
      | (a, Some(Number(n)), _) when is_num(a) =>
        next(Some(Number(n ++ a)), t)
      | (a, Some(Number(n)), _) when is_name(a) =>
        next(Some(Variable(a)), [Operator(Infix("*")), Number(n), ...t])

      // state: Function
      | (a, Some(Operator(Function(f))), _) when is_num(a) =>
        next(Some(Number(a)), [Operator(Function(f)), ...t])
      | (a, Some(Operator(Function(f))), _) when is_name(a) =>
        next(Some(Operator(Function(f ++ a))), t)

      // state: Variable
      | (a, Some(Variable(_) as v), _) when is_num(a) =>
        next(None, [Number(a), Operator(Infix("^")), v, ...t])
      | (a, Some(Variable(v)), _) when is_name(a) =>
        next(Some(Operator(Function(v ++ a))), t)
      | ("(", Some(Variable(v)), _) =>
        next(None, [Bracket(Open), Operator(Function(v)), ...t])

      | ("(", Some(tkn), _) => next(None, [Bracket(Open), tkn, ...t])
      | (")", Some(tkn), _) => next(None, [Bracket(Close), tkn, ...t])

      // State: Number / Variable / Function && next = Operator(Suffix/Infix)
      | (a, Some((Number(_) | Variable(_) | Operator(Function(_))) as b), _)
          when is_infix(a) =>
        next(None, [Operator(Infix(a)), b, ...t])
      | (a, Some((Number(_) | Variable(_) | Operator(Function(_))) as b), _)
          when is_suffix(a) =>
        next(None, [Operator(Suffix(a)), b, ...t])

      // Error
      | (a, b, c) =>
        raise(Js.Exn.raiseError({j|Unmatched state ($a ; $b ; $c)|j}))
      };
    };
  };

  loop(chars->Belt.List.fromArray, None, []);
};