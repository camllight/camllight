type tampon = { mutable val: int; mutable nbits: int };;
let tampon = { val = 0; nbits = 0 };;
let initialise () = tampon.val <- 0; tampon.nbits <- 0;;
let écrire_bit sortie bit =
  tampon.val <- tampon.val lor (bit lsl tampon.nbits);
  tampon.nbits <- tampon.nbits + 1;
  if tampon.nbits >= 8 then begin
    output_char sortie (char_of_int tampon.val);
    tampon.val <- 0;
    tampon.nbits <- 0
  end;;

let finir sortie =
  if tampon.nbits > 0 then
    output_char sortie (char_of_int tampon.val);;
let lire_bit entrée =
  if tampon.nbits <= 0 then begin
    tampon.val <- int_of_char(input_char entrée);
    tampon.nbits <- 8
  end;
  let res = tampon.val land 1 in
  tampon.val <- tampon.val lsr 1;
  tampon.nbits <- tampon.nbits - 1;
  res;;
