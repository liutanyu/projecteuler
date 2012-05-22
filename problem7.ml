let rec problem7 a b c = 
  let square x = x * x in
  match a, b with
    |_, 10001 -> a-1
    |_, _ when (a mod c <> 0) && (a > (square c)) -> problem7 a b (c+1)
    |_, _ when (a < (square c)) -> problem7 (a+1) (b+1) 2
    |_, _ -> problem7 (a+1) b 2
;;
print_int (problem7 2 0 2);;
