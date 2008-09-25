/* Generated from glf.scm by the Chicken compiler
   2006-11-17 10:38
   Version 2, Build 3 - linux-unix-gnu-x86 - [ dload ptables ]
   command line: glf.scm -output-file glf.cpp -dynamic -feature chicken-compile-shared -quiet
   used units: library eval extras tinyclos
   default installation home: /usr/share/chicken
   default heap size: 0
   default nursery (stack) size: 64000
*/

#include "chicken.h"

C_extern void swig_glf_init(C_word,C_word,C_word) C_noret;

static C_PTABLE_ENTRY *create_ptable(void);
C_noret_decl(C_library_toplevel)
C_externimport void C_ccall C_library_toplevel(C_word c,C_word d,C_word k) C_noret;
C_noret_decl(C_eval_toplevel)
C_externimport void C_ccall C_eval_toplevel(C_word c,C_word d,C_word k) C_noret;
C_noret_decl(C_extras_toplevel)
C_externimport void C_ccall C_extras_toplevel(C_word c,C_word d,C_word k) C_noret;
C_noret_decl(C_tinyclos_toplevel)
C_externimport void C_ccall C_tinyclos_toplevel(C_word c,C_word d,C_word k) C_noret;

static C_TLS C_word lf[28];


C_noret_decl(C_toplevel)
C_externexport void C_ccall C_toplevel(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_36)
static void C_ccall f_36(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_39)
static void C_ccall f_39(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_42)
static void C_ccall f_42(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_45)
static void C_ccall f_45(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_50)
static void C_ccall f_50(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_82)
static void C_ccall f_82(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_92)
static void C_ccall f_92(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5) C_noret;
C_noret_decl(f_162)
static void C_fcall f_162(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_105)
static void C_ccall f_105(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_133)
static void C_fcall f_133(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_108)
static void C_ccall f_108(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_123)
static void C_ccall f_123(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_131)
static void C_ccall f_131(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_127)
static void C_ccall f_127(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_113)
static void C_ccall f_113(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_121)
static void C_ccall f_121(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_86)
static void C_ccall f_86(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_52)
static void C_ccall f_52(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4) C_noret;
C_noret_decl(f_69)
static void C_ccall f_69(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_75)
static void C_ccall f_75(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_60)
static void C_ccall f_60(C_word c,C_word t0,C_word t1) C_noret;

C_noret_decl(trf_162)
static void C_fcall trf_162(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_162(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_162(t0,t1,t2);}

C_noret_decl(trf_133)
static void C_fcall trf_133(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_133(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_133(t0,t1,t2);}

C_noret_decl(tr5)
static void C_fcall tr5(C_proc5 k) C_regparm C_noret;
C_regparm static void C_fcall tr5(C_proc5 k){
C_word t4=C_pick(0);
C_word t3=C_pick(1);
C_word t2=C_pick(2);
C_word t1=C_pick(3);
C_word t0=C_pick(4);
C_adjust_stack(-5);
(k)(5,t0,t1,t2,t3,t4);}

C_noret_decl(tr3)
static void C_fcall tr3(C_proc3 k) C_regparm C_noret;
C_regparm static void C_fcall tr3(C_proc3 k){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
(k)(3,t0,t1,t2);}

C_noret_decl(tr4)
static void C_fcall tr4(C_proc4 k) C_regparm C_noret;
C_regparm static void C_fcall tr4(C_proc4 k){
C_word t3=C_pick(0);
C_word t2=C_pick(1);
C_word t1=C_pick(2);
C_word t0=C_pick(3);
C_adjust_stack(-4);
(k)(4,t0,t1,t2,t3);}

C_noret_decl(tr6)
static void C_fcall tr6(C_proc6 k) C_regparm C_noret;
C_regparm static void C_fcall tr6(C_proc6 k){
C_word t5=C_pick(0);
C_word t4=C_pick(1);
C_word t3=C_pick(2);
C_word t2=C_pick(3);
C_word t1=C_pick(4);
C_word t0=C_pick(5);
C_adjust_stack(-6);
(k)(6,t0,t1,t2,t3,t4,t5);}

C_noret_decl(tr2)
static void C_fcall tr2(C_proc2 k) C_regparm C_noret;
C_regparm static void C_fcall tr2(C_proc2 k){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
(k)(2,t0,t1);}

/* toplevel */
static C_TLS int toplevel_initialized=0;
C_main_entry_point
C_noret_decl(toplevel_trampoline)
static void C_fcall toplevel_trampoline(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall toplevel_trampoline(void *dummy){
C_toplevel(2,C_SCHEME_UNDEFINED,C_restore);}

void C_ccall C_toplevel(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
if(toplevel_initialized) C_kontinue(t1,C_SCHEME_UNDEFINED);
else C_toplevel_entry(C_text("toplevel"));
C_resize_stack(64000);
C_check_nursery_minimum(3);
if(!C_demand(3)){
C_save(t1);
C_reclaim((void*)toplevel_trampoline,NULL);}
toplevel_initialized=1;
if(!C_demand_2(153)){
C_save(t1);
C_rereclaim2(153*sizeof(C_word), 1);
t1=C_restore;}
a=C_alloc(3);
C_initialize_lf(lf,28);
lf[1]=C_static_lambda_info(C_heaptop,13,"swig_glf_init");
lf[4]=C_h_intern(&lf[4],9,"slot-set!");
lf[5]=C_h_intern(&lf[5],9,"swig-this");
lf[6]=C_h_intern(&lf[6],8,"slot-ref");
lf[7]=C_h_intern(&lf[7],9,"instance\077");
lf[8]=C_static_lambda_info(C_heaptop,40,"(swig-initialize obj2 initargs3 create4)");
lf[9]=C_h_intern(&lf[9],20,"<swig-metaclass-glf>");
lf[10]=C_h_intern(&lf[10],25,"compute-getter-and-setter");
lf[11]=C_h_intern(&lf[11],5,"<top>");
lf[12]=C_h_intern(&lf[12],13,":swig-virtual");
lf[13]=C_static_lambda_info(C_heaptop,10,"(a112 o18)");
lf[14]=C_static_lambda_info(C_heaptop,16,"(a122 o19 new20)");
lf[15]=C_h_intern(&lf[15],9,":swig-set");
lf[16]=C_static_lambda_info(C_heaptop,18,"(search-set lst16)");
lf[17]=C_h_intern(&lf[17],9,":swig-get");
lf[18]=C_static_lambda_info(C_heaptop,18,"(search-get lst13)");
lf[19]=C_static_lambda_info(C_heaptop,47,"(a91 call-next-method6 class7 slot8 allocator9)");
lf[20]=C_h_intern(&lf[20],26,"\010tinyclosadd-global-method");
lf[21]=C_h_intern(&lf[21],7,"<class>");
lf[22]=C_h_intern(&lf[22],4,"void");
lf[23]=C_h_intern(&lf[23],4,"make");
lf[24]=C_h_intern(&lf[24],4,"name");
lf[25]=C_h_intern(&lf[25],13,"direct-supers");
lf[26]=C_h_intern(&lf[26],12,"direct-slots");
lf[27]=C_static_lambda_info(C_heaptop,10,"(toplevel)");
C_register_lf2(lf,28,create_ptable());
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_36,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
C_library_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* k34 */
static void C_ccall f_36(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_36,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_39,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_eval_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* k37 in k34 */
static void C_ccall f_39(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_39,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_42,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_extras_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* k40 in k37 in k34 */
static void C_ccall f_42(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_42,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_45,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_tinyclos_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* k43 in k40 in k37 in k34 */
static void C_ccall f_45(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_45,2,t0,t1);}
t2=C_mutate(&lf[0],(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)swig_glf_init,a[2]=lf[1],tmp=(C_word)a,a+=3,tmp));
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_50,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace("glf.scm: 8    swig-init");
t4=lf[0];
((C_proc2)C_retrieve_proc(t4))(2,t4,t3);}

/* k48 in k43 in k40 in k37 in k34 */
static void C_ccall f_50(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word ab[12],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_50,2,t0,t1);}
t2=C_mutate(&lf[2],t1);
t3=C_mutate(&lf[3],(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_52,a[2]=lf[8],tmp=(C_word)a,a+=3,tmp));
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_82,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t5=(C_word)C_a_i_list(&a,1,C_retrieve(lf[21]));
t6=(C_word)C_a_i_list(&a,1,lf[22]);
C_trace("glf.scm: 22   make");
t7=C_retrieve(lf[23]);
((C_proc9)C_retrieve_proc(t7))(9,t7,t4,C_retrieve(lf[21]),lf[24],lf[9],lf[25],t5,lf[26],t6);}

/* k80 in k48 in k43 in k40 in k37 in k34 */
static void C_ccall f_82(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[15],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_82,2,t0,t1);}
t2=C_mutate((C_word*)lf[9]+1,t1);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_86,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t4=(C_word)C_a_i_list(&a,3,C_retrieve(lf[9]),C_retrieve(lf[11]),C_retrieve(lf[11]));
t5=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_92,a[2]=lf[19],tmp=(C_word)a,a+=3,tmp);
C_trace("glf.scm: 24   ##tinyclos#add-global-method");
t6=C_retrieve(lf[20]);
((C_proc6)C_retrieve_proc(t6))(6,t6,t3,*((C_word*)lf[10]+1),lf[10],t4,t5);}

/* a91 in k80 in k48 in k43 in k40 in k37 in k34 */
static void C_ccall f_92(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5){
C_word tmp;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word ab[10],*a=ab;
if(c!=6) C_bad_argc_2(c,6,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr6,(void*)f_92,6,t0,t1,t2,t3,t4,t5);}
if(C_truep((C_word)C_i_memq(lf[12],t4))){
t6=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_105,a[2]=t4,a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t7=C_SCHEME_UNDEFINED;
t8=(*a=C_VECTOR_TYPE|1,a[1]=t7,tmp=(C_word)a,a+=2,tmp);
t9=C_set_block_item(t8,0,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_162,a[2]=t8,a[3]=lf[18],tmp=(C_word)a,a+=4,tmp));
t10=((C_word*)t8)[1];
f_162(t10,t6,t4);}
else{
C_trace("glf.scm: 26   call-next-method");
t6=t2;
((C_proc2)C_retrieve_proc(t6))(2,t6,t1);}}

/* search-get in a91 in k80 in k48 in k43 in k40 in k37 in k34 */
static void C_fcall f_162(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word *a;
loop:
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_162,NULL,3,t0,t1,t2);}
if(C_truep((C_word)C_i_nullp(t2))){
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_FALSE);}
else{
t3=(C_word)C_i_car(t2);
t4=(C_word)C_eqp(t3,lf[17]);
if(C_truep(t4)){
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,(C_word)C_i_cadr(t2));}
else{
t5=(C_word)C_i_cdr(t2);
C_trace("glf.scm: 32   search-get");
t7=t1;
t8=t5;
t1=t7;
t2=t8;
goto loop;}}}

/* k103 in a91 in k80 in k48 in k43 in k40 in k37 in k34 */
static void C_ccall f_105(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[10],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_105,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_108,a[2]=((C_word*)t0)[3],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=C_SCHEME_UNDEFINED;
t4=(*a=C_VECTOR_TYPE|1,a[1]=t3,tmp=(C_word)a,a+=2,tmp);
t5=C_set_block_item(t4,0,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_133,a[2]=t4,a[3]=lf[16],tmp=(C_word)a,a+=4,tmp));
t6=((C_word*)t4)[1];
f_133(t6,t2,((C_word*)t0)[2]);}

/* search-set in k103 in a91 in k80 in k48 in k43 in k40 in k37 in k34 */
static void C_fcall f_133(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word *a;
loop:
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_133,NULL,3,t0,t1,t2);}
if(C_truep((C_word)C_i_nullp(t2))){
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_FALSE);}
else{
t3=(C_word)C_i_car(t2);
t4=(C_word)C_eqp(t3,lf[15]);
if(C_truep(t4)){
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,(C_word)C_i_cadr(t2));}
else{
t5=(C_word)C_i_cdr(t2);
C_trace("glf.scm: 38   search-set");
t7=t1;
t8=t5;
t1=t7;
t2=t8;
goto loop;}}}

/* k106 in k103 in a91 in k80 in k48 in k43 in k40 in k37 in k34 */
static void C_ccall f_108(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[8],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_108,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_113,a[2]=((C_word*)t0)[3],a[3]=lf[13],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_123,a[2]=t1,a[3]=lf[14],tmp=(C_word)a,a+=4,tmp);
C_trace("glf.scm: 39   values");
C_values(4,0,((C_word*)t0)[2],t2,t3);}

/* a122 in k106 in k103 in a91 in k80 in k48 in k43 in k40 in k37 in k34 */
static void C_ccall f_123(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word ab[9],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_123,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_127,a[2]=t3,a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t5=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_131,a[2]=t3,a[3]=t4,a[4]=((C_word*)t0)[2],tmp=(C_word)a,a+=5,tmp);
C_trace("glf.scm: 41   slot-ref");
t6=C_retrieve(lf[6]);
((C_proc4)C_retrieve_proc(t6))(4,t6,t5,t2,lf[5]);}

/* k129 in a122 in k106 in k103 in a91 in k80 in k48 in k43 in k40 in k37 in k34 */
static void C_ccall f_131(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("glf.scm: 41   setter");
t2=((C_word*)t0)[4];
((C_proc4)C_retrieve_proc(t2))(4,t2,((C_word*)t0)[3],t1,((C_word*)t0)[2]);}

/* k125 in a122 in k106 in k103 in a91 in k80 in k48 in k43 in k40 in k37 in k34 */
static void C_ccall f_127(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=((C_word*)t0)[3];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,((C_word*)t0)[2]);}

/* a112 in k106 in k103 in a91 in k80 in k48 in k43 in k40 in k37 in k34 */
static void C_ccall f_113(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word ab[4],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_113,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_121,a[2]=t1,a[3]=((C_word*)t0)[2],tmp=(C_word)a,a+=4,tmp);
C_trace("glf.scm: 40   slot-ref");
t4=C_retrieve(lf[6]);
((C_proc4)C_retrieve_proc(t4))(4,t4,t3,t2,lf[5]);}

/* k119 in a112 in k106 in k103 in a91 in k80 in k48 in k43 in k40 in k37 in k34 */
static void C_ccall f_121(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("glf.scm: 40   getter");
t2=((C_word*)t0)[3];
((C_proc3)C_retrieve_proc(t2))(3,t2,((C_word*)t0)[2],t1);}

/* k84 in k80 in k48 in k43 in k40 in k37 in k34 */
static void C_ccall f_86(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
t2=C_mutate((C_word*)lf[10]+1,t1);
t3=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_UNDEFINED);}

/* swig-initialize in k48 in k43 in k40 in k37 in k34 */
static void C_ccall f_52(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4){
C_word tmp;
C_word t5;
C_word t6;
C_word t7;
C_word ab[7],*a=ab;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr5,(void*)f_52,5,t0,t1,t2,t3,t4);}
t5=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_60,a[2]=t2,a[3]=t1,tmp=(C_word)a,a+=4,tmp);
if(C_truep((C_word)C_i_memq(lf[5],t3))){
t6=t5;
f_60(2,t6,(C_word)C_i_cadr(t3));}
else{
t6=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_69,a[2]=t5,tmp=(C_word)a,a+=3,tmp);
C_apply(4,0,t6,t4,t3);}}

/* k67 in swig-initialize in k48 in k43 in k40 in k37 in k34 */
static void C_ccall f_69(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_69,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_75,a[2]=t1,a[3]=((C_word*)t0)[2],tmp=(C_word)a,a+=4,tmp);
C_trace("glf.scm: 18   instance?");
t3=C_retrieve(lf[7]);
((C_proc3)C_retrieve_proc(t3))(3,t3,t2,t1);}

/* k73 in k67 in swig-initialize in k48 in k43 in k40 in k37 in k34 */
static void C_ccall f_75(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
if(C_truep(t1)){
C_trace("glf.scm: 19   slot-ref");
t2=C_retrieve(lf[6]);
((C_proc4)C_retrieve_proc(t2))(4,t2,((C_word*)t0)[3],((C_word*)t0)[2],lf[5]);}
else{
t2=((C_word*)t0)[3];
f_60(2,t2,((C_word*)t0)[2]);}}

/* k58 in swig-initialize in k48 in k43 in k40 in k37 in k34 */
static void C_ccall f_60(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("glf.scm: 14   slot-set!");
t2=C_retrieve(lf[4]);
((C_proc5)C_retrieve_proc(t2))(5,t2,((C_word*)t0)[3],((C_word*)t0)[2],lf[5],t1);}

#ifdef C_ENABLE_PTABLES
static C_PTABLE_ENTRY ptable[23] = {
{"toplevelglf.scm",(void*)C_toplevel},
{"f_36glf.scm",(void*)f_36},
{"f_39glf.scm",(void*)f_39},
{"f_42glf.scm",(void*)f_42},
{"f_45glf.scm",(void*)f_45},
{"f_50glf.scm",(void*)f_50},
{"f_82glf.scm",(void*)f_82},
{"f_92glf.scm",(void*)f_92},
{"f_162glf.scm",(void*)f_162},
{"f_105glf.scm",(void*)f_105},
{"f_133glf.scm",(void*)f_133},
{"f_108glf.scm",(void*)f_108},
{"f_123glf.scm",(void*)f_123},
{"f_131glf.scm",(void*)f_131},
{"f_127glf.scm",(void*)f_127},
{"f_113glf.scm",(void*)f_113},
{"f_121glf.scm",(void*)f_121},
{"f_86glf.scm",(void*)f_86},
{"f_52glf.scm",(void*)f_52},
{"f_69glf.scm",(void*)f_69},
{"f_75glf.scm",(void*)f_75},
{"f_60glf.scm",(void*)f_60},
{NULL,NULL}};
#endif

static C_PTABLE_ENTRY *create_ptable(void){
#ifdef C_ENABLE_PTABLES
return ptable;
#else
return NULL;
#endif
}
/* end of file */
