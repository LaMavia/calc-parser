open Grammar;
let tokenize = (str, infixes, suffixes) => {
  open Regex;
  let chars =
    str
    ->Js.String2.splitByRe(re_space_any)
    ->Belt.Array.keepMap(x =>
        switch (x) {
        | Some(x) => Some(x)
        | _ => None
        }
      );

  let is_infix = Make.is_op(infixes);
  let is_suffix = Make.is_op(suffixes);

  let rec loop = (chars, current, tokens) =>
    switch (chars, current) {
    | ([], None) => List.rev(tokens)
    | ([], Some(a)) => loop([], None, [a, ...tokens])
    | _ =>
      let head = List.hd(chars);
      let tail = List.tl(chars);
      let next = loop(tail);
      switch (head, current, tokens) {
      // state: None
      | ("(", None, t) => next(None, [Bracket(Open), ...t])
      | (")", None, t) => next(None, [Bracket(Close), ...t])
      | (a, None, t) when is_num(a) => next(Some(Number(a)), t)
      | (a, None, t) when is_name(a) =>
        next(Some(Operator(Function(a))), t)
      | (a, None, t) when is_infix(a) =>
        next(None, [Operator(Infix(a)), ...t])
      | (a, None, t) when is_suffix(a) =>
        next(None, [Operator(Suffix(a)), ...t])

      // state: Number
      | (a, Some(Number(n)), t) when is_num(a) =>
        next(Some(Number(n ++ a)), t)
      | (a, Some(Number(n)), t) when is_name(a) =>
        next(Some(Operator(Function(a))), [Number(n), ...t])
      | (a, Some(Number(n)), t) when is_infix(a) =>
        next(None, [Operator(Infix(a)), Number(n), ...t])
      | (a, Some(Number(n)), t) when is_suffix(a) =>
        next(None, [Operator(Suffix(a)), Number(n), ...t])

      // state: Function
      | (a, Some(Operator(Function(f))), t) when is_num(a) =>
        next(Some(Number(a)), [Operator(Function(f)), ...t])
      | (a, Some(Operator(Function(f))), t) when is_name(a) =>
        next(Some(Operator(Function(f ++ a))), t)
      | (a, Some(Operator(Function(f))), t) when is_infix(a) =>
        next(None, [Operator(Infix(a)), Operator(Function(f)), ...t])
      | (a, Some(Operator(Function(f))), t) when is_suffix(a) =>
        next(None, [Operator(Suffix(a)), Operator(Function(f)), ...t])

      | ("(", Some(tkn), t) => next(None, [Bracket(Open), tkn, ...t])
      | (")", Some(tkn), t) => next(None, [Bracket(Close), tkn, ...t])

      // Error
      | (a, b, c) =>
        raise(Js.Exn.raiseError({j|Unmatched state ($a, $b, $c)|j}))
      };
    };

  loop(chars->Belt.List.fromArray, None, []);
};