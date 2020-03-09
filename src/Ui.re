type action =
  | Input(string)
  | Output(string)
  | Funcs(string);

[@react.component]
let make = () => {
  let ((inp, outp, funcs), dispatch) =
    React.useReducer(
      ((old_in, old_out, old_funcs), action) => {
        switch (action) {
        | Input(inp) => (inp, old_out, old_funcs)
        | Output(outp) => (old_in, outp, old_funcs)
        | Funcs(funcs) => (old_in, old_out, funcs)
        }
      },
      ("", "", ""),
    );

  let onChange = e => {
    e->ReactEvent.Form.target##value->Input->dispatch;
  };
  let funcsOnChange = e => {
    e->ReactEvent.Form.currentTarget##value->Funcs->dispatch;
  };

  <form
    onSubmit={e => {
      e->ReactEvent.Form.preventDefault;
      inp
      ->Lexer.tokenize("+-*/^_", "!")
      ->Parser.parse(Grammar.weight)
      ->Evaluator.evaluate(
          Function.global_scope,
          funcs
          ->Js.String2.splitByRe(Regex.re_eol)
          ->Belt.Array.keepMap(s =>
              switch (s) {
              | Some(s) when s != "" => Some(s)
              | _ => None
              }
            )
          ->Belt.Array.map(Function.func_exp_of_string)
          ->Belt.Array.keepMap(x =>
              Belt.Option.map(x, x => Function.UserFunc(x))
            ),
        )
      ->Js.String.make
      ->Output
      ->dispatch;
    }}>
    <p className="output"> outp->Helpers.str </p>
    <input value=inp type_="text" name="inp" id="inp" onChange />
    <textarea name="fs" id="fs" cols=30 rows=10 onChange=funcsOnChange />
    <input type_="submit" value="calc" />
  </form>;
};