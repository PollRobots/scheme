(%define %inc (%var) (local.set %var (i32.add (local.get %var) (i32.const 1))))

(%define %inc64 (%var) (local.set %var (i64.add (local.get %var) (i64.const 1))))

(%define %dec (%var) (local.set %var (i32.sub (local.get %var) (i32.const 1))))

(%define %dec64 (%var) (local.set %var (i64.sub (local.get %var) (i64.const 1))))

(%define %plus-eq (%var %amt) (local.set %var (i32.add (local.get %var) (i32.const %amt))))

(%define %plus-eq64 (%var %amt) (local.set %var (i64.add (local.get %var) (i64.const %amt))))
