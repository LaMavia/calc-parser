type operator =
  | Suffix(string)
  | Infix(string)
  | Function(string);

type bracket =
  | Open
  | Close;

type token =
  | Operator(operator)
  | Number(string)
  | Variable(string)
  | Bracket(bracket);

let weight = op =>
  switch (op) {
  | Infix("+" | "-") => 0
  | Infix("*" | "/") => 1
  | Infix("^" | "_") => 2
  | _ => 3
  };