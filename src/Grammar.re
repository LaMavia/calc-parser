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
  | Bracket(bracket);