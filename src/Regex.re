open Js.Re;
let re_num = [%bs.re "/[\d\.]+/"];
let re_name = [%bs.re "/[a-zA-Z]/i"];
let re_space_any = [%bs.re "/\s*/"];

let is_num = test_(re_num);
let is_name = test_(re_name);

module Make = {
  let is_op = operators => {
    let re =
      (
        "["
        ++ operators
           ->Js.String2.replaceByRe([%bs.re "/\s+/gi"], "")
           ->Js.String2.replaceByRe(
               [%bs.re "/[.*+?^${}()|[\]\\-]/g"],
               "\\$&",
             )
        ++ "]"
      )
      ->Js.Re.fromString;

    test_(re);
  };
};