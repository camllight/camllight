let print_digit nat ofs =
  let nhigh = make_nat 1 and nlow = make_nat 1 and trash = make_nat 1 in
  blit_nat nhigh 0 nat ofs 1;
  shift_right_nat nhigh 0 1 nlow 0 16;
  shift_right_nat nlow 0 1 trash 0 16;
  print_string(format_int "%04X" (nth_digit_nat nhigh 0));
  print_string(format_int "%04X" (nth_digit_nat nlow 0))
;;

let print_nat n =
  for i = length_nat n - 1 downto 0 do
    print_string "|";
    print_digit n i
  done;
  print_string "|"
;;

  
