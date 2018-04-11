let action f opt = 
    match opt with
    | None -> ()
    | Some v -> f v