CAMLC=camlc -g

test: test.out
	./test.out

TESTFILES=test.zo \
test_ints.zo test_nats.zo test_big_ints.zo test_ratios.zo test_nums.zo test_arith.zo end_test.zo

test.out: $(TESTFILES) ../nums.zo ../libnums.a
	$(CAMLC) -o test.out -custom -I .. nums.zo $(TESTFILES) ../libnums.a

.SUFFIXES: .ml .zo

.ml.zo:
	$(CAMLC) -I .. -c $<

clean:
	rm -f test.out *.zo *.zi topnum

end_test.zo: test.zo 
test_big_ints.zo: test.zo ../int_misc.zi ../nat.zi ../big_int.zi 
test_nats.zo: test.zo ../int_misc.zi ../nat.zi 
test_nums.zo: ../ratio.zi ../num.zi test.zo ../int_misc.zi ../nat.zi \
    ../big_int.zi 
test_ratios.zo: ../ratio.zi test.zo ../int_misc.zi ../nat.zi ../big_int.zi 
