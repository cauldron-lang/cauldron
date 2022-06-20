module StringList = struct
  let join str_list separator =
    List.fold_left (fun acc str -> acc ^ separator ^ str) "" str_list
end