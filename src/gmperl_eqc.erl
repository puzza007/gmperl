-module(gmperl_eqc).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

mpz() ->
	?LET(I, int(), begin
					   MPZ = gmperl_nifs:mpz_init(),
					   gmperl_nifs:mpz_set_si(MPZ, I),
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

prop_mul_assoc() ->
	?FORALL({I1, I2},
			{mpz(), mpz()},
			begin
				Res1 = gmperl_nifs:mpz_init(),
				Res2 = gmperl_nifs:mpz_init(),
				ok = gmperl_nifs:mpz_mul(Res1, I1, I2),
				ok = gmperl_nifs:mpz_mul(Res2, I2, I1),
				gmperl_nifs:mpz_cmp(Res1, Res2) == 0
			end).
