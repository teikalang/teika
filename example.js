const f_1 = (x_0, x_1) =>
  function* (x_2) {
    return x_1;
  };
const f_2 = (x_0) =>
  function* (x_1) {
    return f_1(x_0, x_1);
  };
const main = function* () {
  const x_1 = f_2(x_0);
  return x_1;
};
