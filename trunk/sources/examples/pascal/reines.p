program huitReines;

var col: array [1..8] of integer;
var nsol: integer;
var ligne: integer;
var colonne: integer;
var l: integer;

function conflit(c:integer, l:integer):boolean;
  var dl: integer;
  var k: integer;
  begin
    conflit := false;
    k := 1;
    while k <= c do begin
      dl := col[k] - l;
      if dl < 0 then dl := - dl;
      if dl * (dl - c - 1 + k) = 0 then conflit := true;
      k := k + 1
    end
  end;

begin
  nsol := 0;
  l := 1;
  while l <= 8 do begin
    col[1] := l; colonne := 1; ligne := 1;
    while colonne <> 0 do begin
      while ligne <= 8 do begin
        if conflit(colonne, ligne) then
          ligne := ligne + 1
        else begin
          col[colonne + 1] := ligne;
          colonne := colonne + 1;
          if colonne <= 7 then
            ligne := 1
          else begin
            nsol := nsol + 1;
            ligne := 9
          end
        end
      end;
      while (ligne > 8) and (colonne <> 0) do begin
        colonne := colonne - 1;
        ligne := col[colonne + 1] + 1
      end
    end;
    l := l + 1
  end;
  write(nsol)
end
