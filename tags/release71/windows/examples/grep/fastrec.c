/* La version C de la fonction determ__reconnait */

#include "mlvalues.h"

value reconnait(automate, chaine)
        value automate, chaine;
{
  value etat_courant, transition;
  int i, longueur;

  etat_courant = automate;
  longueur = string_length(chaine);
  for (i = 0; i < longueur; i++) {
    transition = Field(Field(etat_courant, 0), Byte_u(chaine, i));
    if (Tag_val(transition) == 1) return Val_false;
    etat_courant = Field(transition, 0);
  }
  return Field(etat_courant, 1);
}
