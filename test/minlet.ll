external print = "print"
let id =
      fun x ->
        let y = x in
        y
let main = print (id 0)
