type action =
  | Input(string)
  | Output(string);

[@react.component]
let make = () => {
  let ((inp, outp), dispatch) =
    React.useReducer(
      ((old_in, old_out), action) => {
        switch (action) {
        | Input(inp) => (inp, old_out)
        | Output(outp) => (old_in, outp)
        }
      },
      ("", ""),
    );

  let onChange = e => {
    e->ReactEvent.Form.target##value->Input->dispatch;
  };
  <form
    onSubmit={e => {
      e->ReactEvent.Form.preventDefault;
      inp
      ->Lexer.tokenize("+-*/^_", "!")
      ->Parser.parse(Grammar.weight)
      ->Js.Console.log;
    }}>
    <p className="output"> outp->Helpers.str </p>
    <input value=inp type_="text" name="inp" id="inp" onChange />
    <input type_="submit" value="calc" />
  </form>;
};