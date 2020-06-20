## Process this file with automake to produce Makefile.in

#
# Makefile source for OCR fonts
#
# This file (Makefile.am) is released to the public domain by its author,
# Matthew Skala, but be aware that the generated Makefile may be subject
# to claims by the Free Software Foundation or others.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# Matthew Skala
# http://ansuz.sooke.bc.ca/
# mskala@ansuz.sooke.bc.ca
#

############################################################################

# BASIC AUTOTOOLS STUFF

ACLOCAL_AMFLAGS = -I m4
@SET_MAKE@
@INC_AMINCLUDE@

mvp:=$(if $(VPATH),$(VPATH),.)

empty:=
space:=$(empty) $(empty)
comma:=,
percent=%

############################################################################

# SILENT BUILD BLINKENLIGHTS

TSU_V_REDIR=$(if $(AM_V_at),>& /dev/null)

if COND_COLOUR
  ECHO_ANSI=echo -e
  ansi_default=\e[0m
  ansi_red=\e[31m
  ansi_green=\e[32m
  ansi_yellow=\e[33m
  ansi_blue=\e[34m
  ansi_magenta=\e[35m
  ansi_cyan=\e[36m
  ansi_pink=\e[35;1m
  ansi_white=\e[37;1m
  ansi_cleareol=\e[K
  ansi_clearescaped=\033\[K
  ansi_home=\e[1G
  am__@andt@v_CC_0=@echo -e "$(ansi_default)$(ansi_cleareol)  CC    " $@;
  am__@andt@v_CCLD_0=@echo -e "$(ansi_default)$(ansi_cleareol)  CCLD  " $@;
  am__@andt@v_GEN_0=@echo -e "$(ansi_default)$(ansi_cleareol)  GEN   " $@;
else
  ECHO_ANSI=echo
  ansi_default=
  ansi_red=
  ansi_green=
  ansi_yellow=
  ansi_blue=
  ansi_magenta=
  ansi_cyan=
  ansi_pink=
  ansi_white=
  ansi_cleareol=
  ansi_clearescaped=
  ansi_home=
endif

TSU_V_CP =$(TSU_V_CP_$(V))
TSU_V_CP_=$(TSU_V_CP_$(AM_DEFAULT_VERBOSITY))
TSU_V_CP_0=@$(ECHO_ANSI) \
  "$(ansi_cleareol)  $(ansi_default)CP$(ansi_default)    " $@;

TSU_V_MT1=$(TSU_V_MT1_$(V))
TSU_V_MT1_=$(TSU_V_MT1_$(AM_DEFAULT_VERBOSITY))
TSU_V_MT1_0=$(ECHO_ANSI) \
  "$(ansi_cleareol)  $(ansi_blue)MT1$(ansi_default)   " $@;

TSU_V_NAPERL=$(TSU_V_NAPERL_$(V))
TSU_V_NAPERL_=$(TSU_V_NAPERL_$(AM_DEFAULT_VERBOSITY))
TSU_V_NAPERL_0=$(ECHO_ANSI) \
  "$(ansi_cleareol)  $(ansi_magenta)PERL$(ansi_default)  " $@;

TSU_V_OTF=$(TSU_V_OTF_$(V))
TSU_V_OTF_=$(TSU_V_OTF_$(AM_DEFAULT_VERBOSITY))
TSU_V_OTF_0=$(ECHO_ANSI) \
  "$(ansi_cleareol)  $(ansi_cyan)OTF$(ansi_default)   " $@;

TSU_V_RMO=$(TSU_V_RMO_$(V))
TSU_V_RMO_=$(TSU_V_RMO_$(AM_DEFAULT_VERBOSITY))
TSU_V_RMO_0=$(ECHO_ANSI) \
  "$(ansi_cleareol)  $(ansi_cyan)RMO$(ansi_default)   " $@;

TSU_V_TEX=$(TSU_V_TEX_$(V))
TSU_V_TEX_=$(TSU_V_TEX_$(AM_DEFAULT_VERBOSITY))
TSU_V_TEX_0=@$(ECHO_ANSI) \
  "$(ansi_cleareol)  $(ansi_green)TEX$(ansi_default)   " $@;

TSU_V_TEXL=$(TSU_V_TEXL_$(V))
TSU_V_TEXL_=$(TSU_V_TEXL_$(AM_DEFAULT_VERBOSITY))
TSU_V_TEXL_0=$(ECHO_ANSI) \
  "$(ansi_cleareol)  $(ansi_green)TEX$(ansi_default)   " "$@($$I)";

############################################################################

# CONDITIONAL OPTIONS

if COND_DOCS
    MAYBE_DOCS=ocr.pdf
endif
if COND_EXPECT
  expect_start= $(if $(AM_V_at),$(EXPECT) -c \
    'log_user 0 ; spawn -noecho sh -c ")
  expect_end= $(if $(AM_V_at), " ; \
    expect -timeout -1 \
    -re "\n! " {exit 1} \
    -re "PROGRESS:(\[^\n\]*)\n" \
      {puts -nonewline \
         "$(ansi_clearescaped)$$expect_out(1,string)\r"; \
       flush stdout;exp_continue} \
    eof' )
else
  expect_end=$(TSU_V_REDIR)
endif

############################################################################

# FILENAMES FOR AUTOTOOLS

# we need a "built source" to make jobserver mode work, for stupid reasons
BUILT_SOURCES=Makefile

# QVFG and FPEVCGF are DIST and SCRIPTS in ROT13, to keep Automake
# from recognizing them

STANDALONE_QVFG:=fntbase.mp
EXTRA_DIST:= \
  $(if $(filter $(distdir),$(top_distdir)),$(STANDALONE_QVFG)) \
  $(wildcard $(mvp)/*.mp) \
  ocr.tex ocra.fea ocrb.fea

STANDALONE_FPEVCGF:=mp2pf
dist_noinst_SCRIPTS:= \
  $(if $(filter $(distdir),$(top_distdir)),$(STANDALONE_FPEVCGF)) \
  ocra-no.pe ocra-yes.pe ocrb-no.pe ocrb-yes.pe ocrbnames.pe rmo.pe

dist_pdf_DATA = $(MAYBE_DOCS)

dist_otf_DATA = OCRA.otf \
  OCRB.otf OCRBE.otf OCRBF.otf OCRBL.otf OCRBS.otf OCRBX.otf
dist_ttf_DATA = OCRA.ttf \
  OCRB.ttf OCRBE.ttf OCRBF.ttf OCRBL.ttf OCRBS.ttf OCRBX.ttf
dist_type1_DATA =  OCRA.pfb \
  OCRB.pfb OCRBE.pfb OCRBF.pfb OCRBL.pfb OCRBS.pfb OCRBX.pfb
dist_afm_DATA = $(patsubst %.pfb,%.afm,$(dist_type1_DATA))

############################################################################

# MAKE CLEAN

MOSTLYCLEANFILES = \
  *.afm *.otf *.pfb *.rmo *.ttf \
  ocr.aux ocr.log ocr.out ocr.pdf

DISTCLEANFILES = \
  aminclude.am ffaetest.pe

############################################################################

# POSTSCRIPT FONTS

%.pfb: %.mp \
  mp2pf @T1ASM@ \
  fntbase.mp
	$(AM_V_at)$(TSU_V_MT1) \
	$(MKDIR_P) $*.tmp ; \
	$(LN_S) -f $(realpath $(mvp))/*.mp $*.tmp/ ; \
	$(LN_S) -f $(realpath .)/*.mp $*.tmp/ ; \
	(cd $*.tmp ; $(expect_start)$(MPOST) $*.mp$(expect_end) ) ; \
	if $(GREP) -q '^! ' $*.tmp/$*.log ; then exit 1 ; fi ; \
	$(TSU_V_NAPERL) \
	$(PERL) -CSD $(mvp)/mp2pf $*.tmp/$* $(TSU_V_REDIR) ; \
	(cd $*.tmp ; $(realpath @T1ASM@) -b $*.p $*.pfb ) ; \
	if test -r $*.tmp/$*.pfb \
	  && $(GREP) -q 'output files\? written' $*.tmp/$*.log; then \
	  mv -f $*.tmp/$*.pfb . ; \
	else \
	  echo "ERROR BUILDING" $@ ; \
	  exit 1 ; \
	fi ; \
	if test "x$(KEEPTMP)" = "x" ; then \
	  rm -rf $*.tmp ; \
	fi

msk-ocra10.pfb: msk-ocra10.mp msk-ocra.mp

msk-ocrb10.pfb: msk-ocrb10.mp msk-ocrbdef.mp msk-ocrbmac.mp

msk-ocrb10efg.pfb: msk-ocrb10efg.mp msk-ocrbdef.mp msk-ocrbmac.mp

msk-ocrb10ebg.pfb: msk-ocrb10ebg.mp msk-ocrbdef.mp msk-ocrbmac.mp

msk-ocrb10ffg.pfb: msk-ocrb10ffg.mp msk-ocrbdef.mp msk-ocrbmac.mp

msk-ocrb10fbg.pfb: msk-ocrb10fbg.mp msk-ocrbdef.mp msk-ocrbmac.mp

msk-ocrb10g.pfb: msk-ocrb10g.mp msk-ocrbdef.mp msk-ocrbmac.mp

msk-ocrb10l.pfb: msk-ocrb10l.mp msk-ocrbdef.mp msk-ocrbmac.mp

msk-ocrb10s.pfb: msk-ocrb10s.mp msk-ocrbdef.mp msk-ocrbmac.mp

msk-ocrb10xfg.pfb: msk-ocrb10xfg.mp msk-ocrbdef.mp msk-ocrbmac.mp

msk-ocrb10xbg.pfb: msk-ocrb10xbg.mp msk-ocrbdef.mp msk-ocrbmac.mp

%.rmo: %.pfb rmo.pe
	$(AM_V_at)$(TSU_V_RMO)$(FONTFORGE) $(mvp)/rmo.pe $*.pfb $(TSU_V_REDIR)
	$(AM_V_at)touch $*.rmo

msk-ocra10.rmo: msk-ocra10.pfb

msk-ocrb10.rmo: msk-ocrb10.pfb

msk-ocrb10ebg.rmo: msk-ocrb10ebg.pfb

msk-ocrb10efg.rmo: msk-ocrb10efg.pfb

msk-ocrb10fbg.rmo: msk-ocrb10fbg.pfb

msk-ocrb10ffg.rmo: msk-ocrb10ffg.pfb

msk-ocrb10l.rmo: msk-ocrb10l.pfb

msk-ocrb10s.rmo: msk-ocrb10s.pfb

msk-ocrb10xbg.rmo: msk-ocrb10xbg.pfb

msk-ocrb10xfg.rmo: msk-ocrb10xfg.pfb

############################################################################

# FONTS

OCRA.pfb OCRA.afm OCRA.ttf OCRA.otf: msk-ocra10.rmo \
  ocra-@ae_arg_okay@.pe ocra.fea
	$(AM_V_at)$(TSU_V_OTF)$(FONTFORGE) \
	  $(mvp)/ocra-@ae_arg_okay@.pe \
	  $(mvp)/ocra.fea $(TSU_V_REDIR)

OCRB.pfb OCRB.afm OCRB.ttf OCRB.otf: \
  msk-ocrb10.rmo ocrb-@ae_arg_okay@.pe ocrbnames.pe ocrb.fea
	$(AM_V_at)$(TSU_V_OTF)$(FONTFORGE) \
	  $(mvp)/ocrb-@ae_arg_okay@.pe '' '' "Regular" 0 \
	  $(mvp)/ocrb.fea $(TSU_V_REDIR)

OCRBE.pfb OCRBE.afm OCRBE.ttf OCRBE.otf: \
  msk-ocrb10efg.rmo msk-ocrb10ebg.rmo \
  ocrb-@ae_arg_okay@.pe ocrbnames.pe ocrb.fea
	$(AM_V_at)$(TSU_V_OTF)$(FONTFORGE) \
	  $(mvp)/ocrb-@ae_arg_okay@.pe 'e' 'E' "Inverted Sharp" 1 \
	  $(mvp)/ocrb.fea $(TSU_V_REDIR)

OCRBF.pfb OCRBF.afm OCRBF.ttf OCRBF.otf: \
  msk-ocrb10ffg.rmo msk-ocrb10fbg.rmo \
  ocrb-@ae_arg_okay@.pe ocrbnames.pe ocrb.fea
	$(AM_V_at)$(TSU_V_OTF)$(FONTFORGE) \
	  $(mvp)/ocrb-@ae_arg_okay@.pe 'f' 'F' "Outline" 1 \
	  $(mvp)/ocrb.fea $(TSU_V_REDIR)

OCRBG.pfb OCRBG.afm OCRBG.ttf OCRBG.otf: \
  msk-ocrb10g.rmo ocrb-@ae_arg_okay@.pe ocrbnames.pe ocrb.fea
	$(AM_V_at)$(TSU_V_OTF)$(FONTFORGE) \
	  $(mvp)/ocrb-@ae_arg_okay@.pe 'g' 'G' "Slanted Outline" 0 \
	  $(mvp)/ocrb.fea $(TSU_V_REDIR)

OCRBL.pfb OCRBL.afm OCRBL.ttf OCRBL.otf: \
  msk-ocrb10l.rmo ocrb-@ae_arg_okay@.pe ocrbnames.pe ocrb.fea
	$(AM_V_at)$(TSU_V_OTF)$(FONTFORGE) \
	  $(mvp)/ocrb-@ae_arg_okay@.pe 'l' 'L' "Slanted" 0 \
	  $(mvp)/ocrb.fea $(TSU_V_REDIR)

OCRBS.pfb OCRBS.afm OCRBS.ttf OCRBS.otf: \
  msk-ocrb10s.rmo ocrb-@ae_arg_okay@.pe ocrbnames.pe ocrb.fea
	$(AM_V_at)$(TSU_V_OTF)$(FONTFORGE) \
	  $(mvp)/ocrb-@ae_arg_okay@.pe 's' 'S' "Sharp" 0 \
	  $(mvp)/ocrb.fea $(TSU_V_REDIR)

OCRBX.pfb OCRBX.afm OCRBX.ttf OCRBX.otf: \
  msk-ocrb10xfg.rmo msk-ocrb10xbg.rmo \
  ocrb-@ae_arg_okay@.pe ocrbnames.pe ocrb.fea
	$(AM_V_at)$(TSU_V_OTF)$(FONTFORGE) \
	  $(mvp)/ocrb-@ae_arg_okay@.pe 'x' 'X' "Inverted" 1 \
	  $(mvp)/ocrb.fea $(TSU_V_REDIR)

############################################################################

# PDF DOCUMENTATION

ocr.pdf: OCRA.otf OCRB.otf ocr.tex
	$(AM_V_at)$(TSU_V_TEX)$(expect_start)$(XELATEX) $(mvp)/ocr$(expect_end)

############################################################################

# TESTING

TESTS = $(dist_otf_DATA) $(dist_ttf_DATA) $(dist_type1_DATA)
LOG_COMPILER = fontlint

############################################################################

# PARASITE BUILD

if COND_PARASITE

fntbase.mp: ../mp/fntbase.mp
	$(TSU_V_CP) cp $< $@

m4/ax_add_am_macro.m4: ../m4/ax_add_am_macro.m4
	$(TSU_V_CP) cp $< $@

m4/ax_am_jobserver.m4: ../m4/ax_am_jobserver.m4
	$(TSU_V_CP) cp $< $@

m4/ax_am_macros.m4: ../m4/ax_am_macros.m4
	$(TSU_V_CP) cp $< $@

m4/ax_append_to_file.m4: ../m4/ax_append_to_file.m4
	$(TSU_V_CP) cp $< $@

m4/ax_check_gnu_make.m4: ../m4/ax_check_gnu_make.m4
	$(TSU_V_CP) cp $< $@

m4/ax_count_cpus.m4: ../m4/ax_count_cpus.m4
	$(TSU_V_CP) cp $< $@

m4/ax_file_escapes.m4: ../m4/ax_file_escapes.m4
	$(TSU_V_CP) cp $< $@

m4/ax_print_to_file.m4: ../m4/ax_print_to_file.m4
	$(TSU_V_CP) cp $< $@

mp2pf: ../tools/mp2pf
	$(TSU_V_CP) cp $< $@

endif
