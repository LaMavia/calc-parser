type node =
  | Empty
  | Number(string)
  | N0(node)
  | N1(Grammar.operator, node) // Suffix / Function
  | N2(Grammar.operator, node, node) /* Infi*/;

let weight_of_node = node => {
  switch (node) {
  | Empty
  | Number(_) => 0
  | N0(_) => 1
  | N1(_) => 2
  | N2(_) => 3
  };
};

let insert = (tree, v) => {
  switch (tree) {
  | N0(_) => N0(v)
  | N1(op, _) => N1(op, v)
  | N2(op, Empty, b) => N2(op, v, b)
  | N2(op, a, _) => N2(op, a, v)
  | Empty => v
  | a =>
    raise(
      Js.Exn.raiseError({j|Unmatched type of tree while inserting { $a }|j}),
    )
  };
};