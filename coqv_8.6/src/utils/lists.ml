let find_pos elem lst = 
    let rec find_pos_rec elem lst indx = 
        match lst with
        | [] -> -1
        | e::es -> if e = elem then indx else find_pos_rec elem es (indx+1) in
    find_pos_rec elem lst 0