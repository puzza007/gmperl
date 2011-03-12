-module(gmperl_eqc).

-ifdef(EQC).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(QC_OUT(P),
        eqc:on_output(fun(Str, Args) -> io:format(user, Str, Args) end, P)).

qc(P) ->
    ?assert(eqc:quickcheck(?QC_OUT(P))).

%% TODO:
%% Different bases
%% Ensure some tests are done with small integers
%% Constructor tests
%% Work out coverage?

largempz() ->
	?LET(I, largeint(), begin
							MPZ = gmperl_nifs:mpz_init(),
							IntStr = integer_to_list(I),
							gmperl_nifs:mpz_set_str(MPZ, IntStr, 10),
							MPZ
						end).

mpz() ->
	?LET(I, int(), begin
					   MPZ = gmperl_nifs:mpz_init(),
					   IntStr = integer_to_list(I),
					   gmperl_nifs:mpz_set_str(MPZ, IntStr, 10),
					   MPZ
				   end).

prop_set_str_badarg() ->
	?FORALL(X, eqc_gen:oneof([binary(), bitstring(), bool(), char(), int(),
							  largeint(), nat(), real()]),
			  begin
				  MPZ = gmperl_nifs:mpz_init(),
				  case catch gmperl_nifs:mpz_set_str(MPZ, X, 10) of
					  {'EXIT', {badarg, _}} -> true
				  end
			  end).

prop_set_str_badarg_test() ->
	qc(prop_set_str_badarg()).

prop_add_assoc() ->
	?FORALL({X, Y, Z},
			{largempz(), largempz(), largempz()},
			begin
				Res1 = gmperl_nifs:mpz_init(),
				Res2 = gmperl_nifs:mpz_init(),
				ok = gmperl_nifs:mpz_add(Res1, X, Y),
				ok = gmperl_nifs:mpz_add(Res1, Res1, Z),
				ok = gmperl_nifs:mpz_add(Res2, Y, Z),
				ok = gmperl_nifs:mpz_add(Res2, X, Res2),
				gmperl_nifs:mpz_cmp(Res1, Res2) == 0
			end).

prop_add_assoc_test() ->
	qc(prop_add_assoc()).

prop_add_comm() ->
	?FORALL({X, Y},
			{largempz(), largempz()},
			begin
				Res1 = gmperl_nifs:mpz_init(),
				Res2 = gmperl_nifs:mpz_init(),
				ok = gmperl_nifs:mpz_add(Res1, X, Y),
				ok = gmperl_nifs:mpz_add(Res2, Y, X),
				gmperl_nifs:mpz_cmp(Res1, Res2) == 0
			end).

prop_add_comm_test() ->
	qc(prop_add_comm()).

prop_mul_comm() ->
	?FORALL({X, Y},
			{largempz(), largempz()},
			begin
				Res1 = gmperl_nifs:mpz_init(),
				Res2 = gmperl_nifs:mpz_init(),
				ok = gmperl_nifs:mpz_mul(Res1, X, Y),
				ok = gmperl_nifs:mpz_mul(Res2, Y, X),
				gmperl_nifs:mpz_cmp(Res1, Res2) == 0
			end).

prop_mul_comm_test() ->
	qc(prop_mul_comm()).

prop_mul_assoc() ->
	?FORALL({X, Y, Z},
			{largempz(), largempz(), largempz()},
			begin
				Res1 = gmperl_nifs:mpz_init(),
				Res2 = gmperl_nifs:mpz_init(),
				ok = gmperl_nifs:mpz_mul(Res1, X, Y),
				ok = gmperl_nifs:mpz_mul(Res1, Res1, Z),
				ok = gmperl_nifs:mpz_mul(Res2, Y, Z),
				ok = gmperl_nifs:mpz_mul(Res2, X, Res2),
				gmperl_nifs:mpz_cmp(Res1, Res2) == 0
			end).

prop_mul_assoc_test() ->
	qc(prop_mul_assoc()).

%% x * (y + z) = (x * y) + (x * z)
prop_distrib() ->
		?FORALL({X, Y, Z},
			{largempz(), largempz(), largempz()},
			begin
				Res1 = gmperl_nifs:mpz_init(),
				Res2 = gmperl_nifs:mpz_init(),
				Res3 = gmperl_nifs:mpz_init(),
				ok = gmperl_nifs:mpz_add(Res1, Y, Z),
				ok = gmperl_nifs:mpz_mul(Res1, X, Res1),

				ok = gmperl_nifs:mpz_mul(Res2, X, Y),
				ok = gmperl_nifs:mpz_mul(Res3, X, Z),
				ok = gmperl_nifs:mpz_add(Res3, Res2, Res3),
				gmperl_nifs:mpz_cmp(Res1, Res3) == 0
			end).

prop_distrib_test() ->
	qc(prop_distrib()).

prop_add_ident_1() ->
	?FORALL(X, largempz(),
			begin
				Zero = gmperl_nifs:mpz_init(),
				gmperl_nifs:mpz_set_si(Zero, 0),
				Res1 = gmperl_nifs:mpz_init(),
				ok = gmperl_nifs:mpz_add(Res1, X, Zero),
				gmperl_nifs:mpz_cmp(Res1, X) == 0
			end).

prop_add_ident_1_test() ->
	qc(prop_add_ident_1()).

prop_add_ident_2() ->
	?FORALL(X, largempz(),
			begin
				Zero = gmperl_nifs:mpz_init(),
				gmperl_nifs:mpz_set_si(Zero, 0),
				Res1 = gmperl_nifs:mpz_init(),
				ok = gmperl_nifs:mpz_mul(Res1, X, Zero),
				gmperl_nifs:mpz_cmp(Res1, Zero) == 0
			end).

prop_add_ident_2_test() ->
	qc(prop_add_ident_2()).

prop_mul_ident() ->
	?FORALL(X, largempz(),
			begin
				One = gmperl_nifs:mpz_init(),
				gmperl_nifs:mpz_set_si(One, 1),
				Res1 = gmperl_nifs:mpz_init(),
				ok = gmperl_nifs:mpz_mul(Res1, X, One),
				gmperl_nifs:mpz_cmp(Res1, X) == 0
			end).

prop_mul_ident_test() ->
	qc(prop_mul_ident()).

prop_cmp_trans() ->
	?FORALL({X, Y, Z},
			{largempz(), largempz(), largempz()},
			?IMPLIES(gmperl_nifs:mpz_cmp(X, Y) =< 0 andalso
					 gmperl_nifs:mpz_cmp(Y, Z) =< 0,
					 begin
						 gmperl_nifs:mpz_cmp(X, Z) =< 0
					 end)).

prop_cmp_trans_test() ->
	qc(prop_cmp_trans()).

prop_lt_irreflexive() ->
	?FORALL(X, largempz(),
			begin
				gmperl_nifs:mpz_cmp(X, X) =:= 0
			end).

prop_lt_irreflexive_test() ->
	qc(prop_lt_irreflexive()).

%% prop_lt_equiv() ->
%% 	?FORALL({X, Y},
%% 			{mpz(), mpz()},
%% 			?IMPLIES(gmperl_nifs:mpz_cmp(X, Y) =< 0 andalso
%% 					 gmperl_nifs:mpz_cmp(Y, X) =< 0,
%% 					 gmperl_nifs:mpz_cmp(X, Y) =:= 0)).

%% prop_lt_equiv_test() ->
%% 	qc(prop_lt_equiv()).

prop_mult_ineq() ->
	Zero = gmperl_nifs:mpz_init(),
	gmperl_nifs:mpz_set_si(Zero, 0),
	?FORALL({X, Y, Z},
			?SUCHTHAT({X, Y, Z},
					  {largempz(), largempz(), largempz()},
					  gmperl_nifs:mpz_cmp(Zero, Z) < 0 andalso
					  gmperl_nifs:mpz_cmp(X, Y) < 0),
			begin
				M1 = gmperl_nifs:mpz_init(),
				M2 = gmperl_nifs:mpz_init(),
				ok =  gmperl_nifs:mpz_mul(M1, X, Z),
				ok =  gmperl_nifs:mpz_mul(M2, Y, Z),
				gmperl_nifs:mpz_cmp(M1, M2) < 0
			end).

prop_mult_ineq_test() ->
	qc(prop_mult_ineq()).

-endif.
