let weight = op =>
  Grammar.(
    switch (op) {
    | Infix("+" | "-") => 0
    | Infix("*" | "/") => 1
    | Infix("^" | "_") => 2
    | _ => 3
    }
  );

Lexer.tokenize("2_2", "+-*/^_", "!")
->Parser.parse(weight)
->Debugging.deep_log;