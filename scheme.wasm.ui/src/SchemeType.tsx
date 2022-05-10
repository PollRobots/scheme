export enum SchemeType {
  Empty = 0,
  Nil = 1,
  Boolean = 2,
  Cons = 3,
  I64 = 4,
  F64 = 5,
  Symbol = 6,
  Str = 7,
  Char = 8,
  Env = 9,
  Special = 10,
  Builtin = 11,
  Lambda = 12,
  Error = 13,
  Values = 14,
  Vector = 15,
  Bytevector = 16,
  Cont = 17,
  BigInt = 18,
  Except = 19,
  ContProc = 20,
  SyntaxRules = 21,
  Rational = 22,
  Complex = 23,
  Record = 24,
  RecordMeta = 25,
  RecordMethod = 26,
  CaseLambda = 27,
  Port = 28,
  Eof = 29,
  MaxHeap = 30,
  Mask = 0x1f,
}
