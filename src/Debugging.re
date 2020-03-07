[@bs.module "util"] external inspect: ('a, Js.t({..})) => 'b = "inspect";
let deep_log = x =>
  Js.Console.log(inspect(x, {"depth": 50, "colors": true}));