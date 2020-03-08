open Js.Re;
let re_num = [%bs.re "/[\d\.]+/"];
let re_name = [%bs.re "/[a-zA-Z]/i"];
let re_space_any = [%bs.re "/\s*/"];

let is_num = test_(re_num);
let is_name = test_(re_name);

let escape_regex = str =>
  str->Js.String2.replaceByRe([%bs.re "/[.*+?^${}()|[\]\\-]/g"], "\\$&");

module Make = {
  let is_op = operators => {
    let re =
      (
        "["
        ++ operators
           ->Js.String2.replaceByRe([%bs.re "/\s+/gi"], "")
           ->escape_regex
        ++ "]"
      )
      ->fromString;

    test_(re);
  };

  let is_function = (functions: array(string)) => {
    let re =
      "("
      ++ (
        functions->Belt.Array.map(f => f->escape_regex)
        |> Js.Array.joinWith("|")
      )
      ++ ")"
      |> fromString;
    test_(re);
  };
};