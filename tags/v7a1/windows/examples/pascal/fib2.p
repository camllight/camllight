program fibonacci;
var fib: array [0 .. 100] of integer;
var n: integer;
var i: integer;
begin
  read(n);
  fib[0] := 1;
  fib[1] := 1;
  i := 2;
  while i <= n do begin fib[i] := fib[i - 1] + fib[i - 2]; i := i + 1 end;
  write(fib[n])
end
