# Le programme principal
        read                    # lecture de l'argument (dans r 1)
        jmp     fact, ra        # calcul de la factorielle
        write                   # �criture du r�sultat (r 1)
        stop                            

# La fonction factorielle(N)
# L'argument N est dans r 1. Le r�sultat est mis dans r 1.
fact:   braz    r 1, fact_0     # N = 0 ?
        sub     sp, 8, sp       # r�serve deux places dans la pile
        store   sp, 0, ra       # sauvegarde de l'adresse de retour
        store   sp, 4, r 1      # et de la valeur de N
        sub     r 1, 1, r 1
        jmp     fact, ra        # appel r�cursif sur N-1
        load    sp, 4, r 2      # r�cup�ration de la valeur de N
        mult    r 1, r 2, r 1   # calcul de N * fact(N-1)
        load    sp, 0, ra       # r�cup�ration de l'adresse de retour
        add     sp, 8, sp       # et de la place en pile
        jmp     ra, r 0         # retour � l'appelant
fact_0: add     r 0, 1, r 1     # mettre 1 dans r1
        jmp     ra, r 0         # retour � l'appelant
