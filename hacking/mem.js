const g_clos = (x, y) => () => console.log(x.length, y.length);
const h_clos = (y) => () => console.log(y.length);
const f = () => {
  const x = new Int8Array(1024 * 1024 * 1024).fill(1);
  const y = new Int8Array(1024 * 1024 * 1024).fill(1);
  const g = g_clos(x, y);
  const h = h_clos(y);
  return h;
};
