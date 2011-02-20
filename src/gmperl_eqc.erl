-module(gmperl_eqc).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").


%% TODO:
%% Different bases
%% Ensure some tests are done with small integers
%% Constructor tests
%% Work out coverage?

mpz() ->
	?LET(I, largeint(), begin
							MPZ = gmperl_nifs:mpz_init(),
							IntStr = integer_to_list(I),
							gmperl_nifs:mpz_set_str(MPZ, IntStr, 10),
							MPZ
						end).

prop_add_assoc() ->
	?FORALL({I1, I2, I3},
			{mpz(), mpz(), mpz()},
			begin
				Res1 = gmperl_nifs:mpz_init(),
				Res2 = gmperl_nifs:mpz_init(),
				ok = gmperl_nifs:mpz_add(Res1, I1, I2),
				ok = gmperl_nifs:mpz_add(Res1, Res1, I3),
				ok = gmperl_nifs:mpz_add(Res2, I2, I3),
				ok = gmperl_nifs:mpz_add(Res2, I1, Res2),
				gmperl_nifs:mpz_cmp(Res1, Res2) == 0
			end).

prop_add_comm() ->
	?FORALL({I1, I2},
			{mpz(), mpz()},
			begin
				Res1 = gmperl_nifs:mpz_init(),
				Res2 = gmperl_nifs:mpz_init(),
				ok = gmperl_nifs:mpz_add(Res1, I1, I2),
				ok = gmperl_nifs:mpz_add(Res2, I2, I1),
				gmperl_nifs:mpz_cmp(Res1, Res2) == 0
			end).

prop_mul_comm() ->
	?FORALL({I1, I2},
			{mpz(), mpz()},
			begin
				Res1 = gmperl_nifs:mpz_init(),
				Res2 = gmperl_nifs:mpz_init(),
				ok = gmperl_nifs:mpz_mul(Res1, I1, I2),
				ok = gmperl_nifs:mpz_mul(Res2, I2, I1),
				gmperl_nifs:mpz_cmp(Res1, Res2) == 0
			end).

prop_mul_assoc() ->
	?FORALL({I1, I2, I3},
			{mpz(), mpz(), mpz()},
			begin
				Res1 = gmperl_nifs:mpz_init(),
				Res2 = gmperl_nifs:mpz_init(),
				ok = gmperl_nifs:mpz_mul(Res1, I1, I2),
				ok = gmperl_nifs:mpz_mul(Res1, Res1, I3),
				ok = gmperl_nifs:mpz_mul(Res2, I2, I3),
				ok = gmperl_nifs:mpz_mul(Res2, I1, Res2),
				gmperl_nifs:mpz_cmp(Res1, Res2) == 0
			end).

%% x * (y + z) = (x * y) + (x * z)
prop_distrib() ->
		?FORALL({I1, I2, I3},
			{mpz(), mpz(), mpz()},
			begin
				Res1 = gmperl_nifs:mpz_init(),
				Res2 = gmperl_nifs:mpz_init(),
				Res3 = gmperl_nifs:mpz_init(),
				ok = gmperl_nifs:mpz_add(Res1, I2, I3),
				ok = gmperl_nifs:mpz_mul(Res1, I1, Res1),

				ok = gmperl_nifs:mpz_mul(Res2, I1, I2),
				ok = gmperl_nifs:mpz_mul(Res3, I1, I3),
				ok = gmperl_nifs:mpz_add(Res3, Res2, Res3),
				gmperl_nifs:mpz_cmp(Res1, Res3) == 0
			end).

prop_add_ident_1() ->
	?FORALL(I1, mpz(),
			begin
				Zero = gmperl_nifs:mpz_init(),
				gmperl_nifs:mpz_set_si(Zero, 0),
				Res1 = gmperl_nifs:mpz_init(),
				ok = gmperl_nifs:mpz_add(Res1, I1, Zero),
				gmperl_nifs:mpz_cmp(Res1, I1) == 0
			end).

prop_add_ident_2() ->
	?FORALL(I1, mpz(),
			begin
				Zero = gmperl_nifs:mpz_init(),
				gmperl_nifs:mpz_set_si(Zero, 0),
				Res1 = gmperl_nifs:mpz_init(),
				ok = gmperl_nifs:mpz_mul(Res1, I1, Zero),
				gmperl_nifs:mpz_cmp(Res1, Zero) == 0
			end).

prop_mul_ident() ->
	?FORALL(I1, mpz(),
			begin
				One = gmperl_nifs:mpz_init(),
				gmperl_nifs:mpz_set_si(One, 1),
				Res1 = gmperl_nifs:mpz_init(),
				ok = gmperl_nifs:mpz_mul(Res1, I1, One),
				gmperl_nifs:mpz_cmp(Res1, I1) == 0
			end).

prop_cmp_trans() ->
	?FORALL({I1, I2, I3},
			{mpz(), mpz(), mpz()},
			?IMPLIES(gmperl_nifs:mpz_cmp(I1, I2) =< 0 andalso
					 gmperl_nifs:mpz_cmp(I2, I3) =< 0,
					 begin
						 gmperl_nifs:mpz_cmp(I1, I3) =< 0
					 end)).

prop_lt_irreflexive() ->
	?FORALL(I1, mpz(),
			begin
				gmperl_nifs:mpz_cmp(I1, I1) =:= 0
			end).

prop_lt_equiv() ->
	?FORALL({I1, I2},
			{mpz(), mpz()},
			?IMPLIES(gmperl_nifs:mpz_cmp(I1, I2) =< 0 andalso
					 gmperl_nifs:mpz_cmp(I2, I1) =< 0,
					 begin
						 gmperl_nifs:mpz_cmp(I1, I2) =:= 0
					 end)).
