SHELL=/bin/csh

.SUFFIXES: .cos 

include ../Path.incl
include $(SCIDIR)/Makefile.incl

ALLC=	abs_sign_2_C.cosf \
	abs_sign_C.cosf \
	basic_C.cosf \
	basic_gain_C.cosf \
	basic_if_C.cosf \
	basic_lss2_C.cosf \
	basic_lss_C.cosf \
	const2_C.cosf \
	const3_C.cosf \
	const_C.cosf \
	if_matrix_mult_C.cosf \
	inlined_transpose_C.cosf \
	int2_C.cosf \
	int_C.cosf \
	int_div2_C.cosf \
	int_div_C.cosf \
	int_eye_C.cosf \
	mswitch_C.cosf \
	switch2_C.cosf \
	switch3_C.cosf \
	switch_C.cosf \
	transpose_C.cosf \
	with_cblock_C.cosf \
	geneauto2_C.cosf \
	kalman_C.cosf \
	ifthen_C.cosf

ALLP=	abs_sign_2_P.cosf \
	abs_sign_P.cosf \
	basic_P.cosf \
	basic_gain_P.cosf \
	basic_if_P.cosf \
	basic_lss2_P.cosf \
	basic_lss_P.cosf \
	const2_P.cosf \
	const3_P.cosf \
	const_P.cosf \
	if_matrix_mult_P.cosf \
	inlined_transpose_P.cosf \
	int2_P.cosf \
	int_P.cosf \
	int_div2_P.cosf \
	int_div_P.cosf \
	int_eye_P.cosf \
	mswitch_P.cosf \
	switch2_P.cosf \
	switch3_P.cosf \
	switch_P.cosf \
	transpose_P.cosf \
	with_cblock_P.cosf \
	geneauto2_P.cosf \
	kalman_P.cosf \
	ifthen_P.cosf

all:: $(ALLC) $(ALLP)

clean::
	$(RM) $(ALLC) $(ALLP)

# special target 

%_P.cosf: %.cos
	@echo "compile $<"
	@$(SCIDIR)/bin/nsp -nw -f generate_code.sce -e "compile_super('$<');quit"

%_C.cosf: %.cos
	@echo "compile $<"
	@$(SCIDIR)/bin/nsp -nw -f generate_code.sce -e "compile_super('$<',target='C');quit"

