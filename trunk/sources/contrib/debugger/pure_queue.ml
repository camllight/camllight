type 'a Queue = {Front : 'a list; Rear : 'a list};;

let empty = {Front = []; Rear = []};;

let is_empty =
  function
    {Front = []; Rear = []} ->
      true
  | _ ->
      false;;

let size queue = list_length queue.Front + list_length queue.Rear;;

let insert queue x = {Front = queue.Front; Rear = x :: queue.Rear};;

let insert_front queue x = {Front = x :: queue.Front; Rear = queue.Rear};;

let remove =
  function
    {Front = x::front; Rear = rear} ->
       x,
       {Front = front; Rear = rear}
  | {Front = []; Rear = rear} ->
          match rev rear with
              [] ->
		raise Empty
            | x::front' ->
		x,
		{Front = front'; Rear = []};;
