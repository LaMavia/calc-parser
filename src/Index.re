let weight = op =>
  Grammar.(
    switch (op) {
    | Infix("+" | "-") => 0
    | Infix("*" | "/") => 1
    | Infix("^" | "_") => 2
    | _ => 3
    }
  );

Lexer.tokenize("1.75 + 2.3 * (cos(5 + 3!)) ^ 2", "+-*/^_", "!")
->Parser.parse(weight)
->Debugging.deep_log;