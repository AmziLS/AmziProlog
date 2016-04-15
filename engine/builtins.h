/****************************************************************************
*
* builtins.h -- Built-in predicates
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
****************************************************************************/

#ifndef BUILTINS_H
#define BUILTINS_H

class BuiltIns;

typedef TF (BuiltIns::*pBIP)();

class BuiltIns
{
private:
   LEngine *m_peng;
   void MakeBIPred(STRptr, ARITY, pBIP, bool, STRptr, STRptr);

   time_t StartTime;

public:
   BuiltIns(LEngine *peng) { m_peng = peng; }
   ~BuiltIns() {};
   void Init();

   // Implemented in BuiltIns class

   TF p_gc(void);
   TF p_srand(void);
   TF p_abort(void);
   TF p_commandl(void);
   TF p_timer(void);
   TF p_time(void);
   TF p_time4(void);
   TF p_date(void);
   TF p_openlog(void);
   TF p_closelog(void);
   TF p_writelog(void);
   TF p_nllog(void);
   TF p_plmtraceon(void);
   TF p_plmtraceoff(void);
   TF p_pro_heap(void);
   TF p_pro_stack(void);
   TF p_pro_local(void);
   TF p_pro_trail(void);
   TF p_highwater(void);
   TF p_syscom(void);
   TF p_opsys(void);
   TF p_buildenv(void);
   TF p_userZname(void);

   TF p_address(void);                            // ray
   //TF p_mode(void);
   TF p_version(void);
   TF p_version_build(void);
   TF p_version_build3(void);
   TF p_flagZvalue(void);
   TF p_set_flag_value(void);
   TF p_prologZflag(void);
   TF p_is_unicode(void);
   TF p_faultyZtower(void);
   TF p_amzi_directory(void);
   TF p_debugZpause(void);

   // Atom Table & Atom Predicates

   TF p_moduleZ(void);
   TF p_end_moduleZ(void);
   TF p_currentZmodule(void);
   TF p_loading_module(void);
   TF p_moduleZindex(void);
   TF p_getZmodix(void);
   TF p_remove_module(void);

   TF p_atom_length(void);
   TF p_atomlist_concat(void);
   TF p_name(void);
   TF p_number_chars(void);
   TF p_atom_chars(void);
   TF p_atom_codes(void);
   TF p_atom_uplow(void);
   TF p_number_codes(void);
   TF p_term_codes(void);
   TF p_op(void);

   // Dynamic Database Predicates

   TF p_assertZ(void);
   TF p_debug_data(void);
   TF p_clauseZdb(void);
   TF p_retractZdb(void);

   TF p_getZpred(void);
   TF p_predicateZproperty(void);
   TF p_predicateZenginfo(void);
   TF p_getZop(void);

   TF p_abolish(void);
   TF p_iscode(void);
   TF p_isZcode(void);

   //TF p_pro_db(void);
   TF p_definedZ(void);
   TF p_setZdiscontiguous(void);

   TF p_import_1(void);
   TF p_importZ2(void);
   TF p_importZmod(void);
   TF p_exportZ(void);
   TF p_setZmeta(void);
   TF p_isZmeta(void);
   TF p_setZsorted(void);
   TF p_setZindexed(void);

   // String and Thing Predicates

   TF p_string_tokens(void);
   TF p_string_tokens3(void);
   TF p_str_list(void);
   TF p_string(void);
   TF p_string_length(void);
   TF p_substring(void);
   TF p_subZstring(void);
   TF p_strcat(void);
   TF p_stringlist_concat(void);
   TF p_string_split(void);
   TF p_string_icomp(void);
   TF p_read_term(void);
   TF p_read_string(void);
   TF p_string_term(void);
   TF p_is_string_term(void);
   TF p_string_termq(void);
   TF p_string_termq3(void);
   TF p_string_trim(void);
   TF p_nonblank_string(void);
   TF p_string_atom(void);
   TF p_tilt_slashes(void);

   TF p_dumpthings(void);

   // Arithmetic Predicates

   TF p_fixed_list(void);
   TF p_arith_plus(void);
   TF p_cntr(void);
   TF p_for(void);
   TF p_numeq(void);
   TF p_almost_equal(void);
   TF p_gt(void);
   TF p_is(void);
   TF p_lt(void);
   TF p_is_integer(void);
   TF p_is_odd(void);
   TF p_nth(void);
   TF p_newReal(void);
   //TF p_realDescr(void);
	TF p_divrem(void);
	TF p_divmodu(void);
	TF p_divmods(void);
	// Primes
   TF p_makePrimes(void);
   TF p_Primes(void);
   TF p_nthPrime(void);

   // Number Types
   TF p_integer(void);
   //TF p_long(void);
   //TF p_short(void);
   TF p_fixed_real(void);
   TF p_long_real(void);
   TF p_real(void);
   TF p_single(void);
   TF p_double(void);
   TF p_float(void);
   TF p_real_components(void);

	TF p_epsilon(void);

   // Term Classification
   TF p_arg(void);
   TF p_atom(void);
   // bigdig TF p_atomic(void);
   //TF p_dbref(void);
   TF p_functor(void);
   TF p_fraction(void);
   TF p_char(void);
   //TF p_lexord(void);
   TF p_lessZ(void);
   TF p_copy_term(void);
   TF p_memberv(void);
   TF p_not_strong_unify(void);
   TF p_number(void);
   TF p_strong_unify(void);
   TF p_univ(void);
   TF p_varsof(void);
   TF p_gensym(void);
   TF p_is_cyclic(void);
   TF p_ground(void);
//#ifdef REAL_H
   TF p_truncate(void);
   TF p_int_real(void);
   TF p_float_real(void);
   TF p_real_list(void);
//#endif

   // Stashing Predicates

   TF p_get_term(void);
   TF p_peek_term(void);
   TF p_poke_term(void);
   TF p_reserve_heap(void);
   TF p_stash_term(void);
   TF p_stash_free(void);

   // Term Writing Predicates

   TF p_varlist(void);
   TF p_write(void);
   //TF p_write1(void);
#ifdef LANDFILL
   TF p_landZfill(void);   // writes a term to the landfill
   TF p_logZterm(void);
#endif

   // Term Reading Predicates

   //TF p_read(void);

   // Stream I/O Predicates

   TF p_streamList(void);
   TF p_stream_props(void);
   TF p_cur_streams(void);
   TF p_cur_user(void);
   TF p_cur_input(void);
   TF p_cur_output(void);
   TF p_set_input(void);
   TF p_set_output(void);
   TF p_set_streampos(void);
   TF p_at_eos(void);
   //TF p_sopen(void);
   //TF p_sclose(void);
   //TF p_fclose(void);
   TF p_fflush(void);
   //TF p_fopen(void);
   //TF p_fleopen(void);
   TF p_open(void);
   TF p_close(void);
   //TF p_see(void);
   //TF p_tell(void);
   TF p_fread(void);
   TF p_fseek(void);
   TF p_fwrite(void);
   TF p_flewrite(void);
   TF p_geto(void);
   TF p_getcodeo(void);

   TF p_ungeto(void);
   TF p_handle_name(void);
   //TF p_stream_type(void);
   TF p_putc(void);

   TF p_keyZb(void);
   TF p_read_binary(void);
   TF p_write_binary(void);

	//TF p_stream_attrs(void);

   // Load & Code Predicates

   TF p_loadZfile(void);
   TF p_loadZmemory(void);
   TF p_loadZops(void);
   TF p_unload(void);
   TF p_is_loaded(void);
   TF p_loadlsx(void);
   TF p_ensure_loaded(void);

   TF p_cut_env(void);
   TF p_cut_debug64_env(void);
   TF p_get_env(void);
   TF p_get_env1(void);
   TF p_saveZstack(void);

   TF p_cuttag(void);
   TF p_cutZtag(void);
   TF p_deZtag(void);
   TF p_reZtag(void);
   TF p_tag(void);

//   TF p_spy(void);
   TF p_macHisto(void);

   // Error Handling Predicates

   TF p_err_read(void);     // just a stub for old times sake
   TF p_err_abort(void);
   TF p_err_fatal(void);
   TF p_errZexec(void);

   TF p_debugZstack(void);

//ocho   TF p_licenseZaction(void);
   TF p_licenseZinfo(void);
#ifdef ARULESXLRT
   TF p_registerZruntime(void);
   TF p_enterZlicensekey(void);
#endif

   TF p_get_async_action(void);
   TF p_set_event(void);

   // Debugging Predicates
#ifdef LANDFILL   
   TF p_dumpdb(void);
   TF p_dumpZcontrol(void);
   TF p_dumpZhxl(void);
   TF p_debugZbreak(void);
   TF p_macNames(void);
#ifdef BUG_LEAK
   TF p_leakZreport(void);
#endif
#endif
};

#endif //BUILTINS_H


