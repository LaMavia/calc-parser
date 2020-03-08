let str = React.string;
let useState = initial => {
  React.useReducer((_ignored, newState) => newState, initial);
};