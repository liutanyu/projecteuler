let rec problem5 (a:int) (b:int) = 
  match a with
  |a when (b mod a == 0) && (a < 21) -> problem5 (a+1) b
  |a when a == 21 -> b
  |_ -> problem5 1 (b+1)
;;
print_int (problem5 1 1)

