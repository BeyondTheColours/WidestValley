let widest_valley l =
  let rec aux l start_height count best =
    match l with
    |[] -> best
    |h::t ->
        if h > start_height && count = 0 then
          aux t h count best
        else if h = start_height then
          if count > best then
            aux t h 0 count
          else
            aux t h 0 best
        else
          max (aux t start_height (count+1) best) (aux t h 0 0)
  in aux l 0 0 0
;;
