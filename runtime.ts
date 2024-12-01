enum Instr {
  HALT = 0,
  LET = 1,
  END_LET = 2,
  HOIST = 3,
  FIX = 4,
  APPLY = 5,
  LAMBDA = 6,
  RET = 7,
  VAR = 8,
}

const eval_bytecode = (code: Uint8Array) => {
  let pc = 0;
  let stack = new Uint32Array(1024);
  while (1) {
    const instr = code[pc++];
    switch (instr) {
      case Instr.HALT:
        return;
      case Instr.LET:

      default:
        break;
    }
  }
};
