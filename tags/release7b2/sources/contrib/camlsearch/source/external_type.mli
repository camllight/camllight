type GLOBAL_NAME =
  {Module_name : string;
   Local_name : string};;
type EXTERNAL_TYPE =
    ET_frozen_variable of int
  | ET_subst_variable of int
  | ET_function of EXTERNAL_TYPE * EXTERNAL_TYPE
  | ET_product of EXTERNAL_TYPE list
  | ET_constant of GLOBAL_NAME * EXTERNAL_TYPE list
  | ET_unit;;
