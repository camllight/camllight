program fibonacci;
var n: integer;
function fib(n: integer): integer;
  begin
    if n < 2 then fib := 1 else fib := fib(n - 1) + fib(n - 2)
  end;
begin
  read(n);
  write(fib(n))
end
