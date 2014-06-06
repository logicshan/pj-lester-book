#################################################################
#								#
#	Imatexplementing Functional Languages			#
#       (c) 1991 Simon Peyton Jones & David Lester.		#
#								#
#	Version V1.10 						#
#	See the release history in Release-history		#
#								#
#################################################################


.SUFFIXES:	.fig .src .tex .m .dvi .mail .ind .idx .lhs .hs .o

# The "LANGUAGE" variable tells which version of the code to select when
# making the typeset book

LANGUAGE = H

STUDENT_SRCS = student.src title.src preface.src language.src \
	template.src tim.src gm.src pargm.src lambda.src \
	utils.src coreprog.src biblio.src \
	CodeGen.hs

TUTOR_SRCS = README-TUTOR tutor.src title-tutor.src preface-tutor.src \
	template-tutor.src tim-tutor.src gm-tutor.src \
	pargm-tutor.src lambda-tutor.src language-tutor.src

FIGURES = overview.fig map_acc.fig foldl.fig gc-ind.fig \
	gm_arg.fig     gm_let.fig      gm_slide.fig\
	gm_exa.fig     gm_lrec.fig   gm_upd.fig\
	gm_y1.fig       gm_y2.fig       gm_alter.fig\
	gm_stack.fig	gm_ex.fig 	gm_v1.fig	gm_v2.fig \
	gm_v3.fig	\
	pgm_1x1.fig	pgm_1x2.fig	pgm_1x3.fig 	pgm_1x4.fig \
	depend.fig	gc.fig

TOOLS_SRCS = verbatim.l make-version add-index-entries subsume-index-entries

OTHER_SRCS = README Makefile fpstyle.sty \
	 $(FIGURES) getting.src $(TOOLS_SRCS) installation.src \
	Release-history ERRATA Contributions.src

SOURCES = $(TUTOR_SRCS) $(STUDENT_SRCS) $(OTHER_SRCS)

LATEX = latex
HC_OPTS = -H20M		# Needed for some of the compilations on an Alpha
HC = ghc-0.26 $(HC_OPTS)

.PRECIOUS: $(SOURCES)

CFLAGS = -w

# Default version number is 1
VERSION=1



# Default target is the first...
default: student.dvi	

clean:
	rm *.tex *.aux *~ *.m *.dvi *.log *.x *.ilg *.idx

#################################################
#						#
#	Making the books			#
#						#
#################################################

student.dvi:	verbatim fpstyle.sty student.main.ind \
		$(FIGURES:.fig=.tex) $(STUDENT_SRCS:.src=.tex)

student-xrefs.aux: $(STUDENT_SRCS:.src=.aux)
	grep -h newlabel $(STUDENT_SRCS:.src=.aux) > student-xrefs.aux

tutor.dvi:	fpstyle.sty $(TUTOR_SRCS:.src=.tex) student-xrefs.aux

installation.dvi: verbatim fpstyle.sty installation.tex getting.tex


# getting.tex is \input'ed by preface and installation
#
preface.tex: getting.tex
installation.tex: getting.tex

touch-aux:
	touch $(STUDENT_SRCS:.src=.aux)


#################################################
#						#
#	Making Miranda sources			#
#						#
#################################################

mira:	$(STUDENT_SRCS:.src=.m) $(TUTOR_SRCS:.src=.m)

language.m: language.src language-tutor.src
	cat language.src language-tutor.src | ./make-version 3 M > $@


template1.m: template.src template-tutor.src
	cat template.src template-tutor.src | ./make-version 1 M > $@

template2.m: template.src template-tutor.src
	cat template.src template-tutor.src | ./make-version 2 M > $@

template3.m: template.src template-tutor.src
	cat template.src template-tutor.src | ./make-version 3 M > $@

template4.m: template.src template-tutor.src
	cat template.src template-tutor.src | ./make-version 4 M > $@

template5.m: template.src template-tutor.src
	cat template.src template-tutor.src | ./make-version 5 M > $@

tim.m:	tim.src tim-tutor.src
	cat tim.src tim-tutor.src | ./make-version $(VERSION) M >$@

tim1.m: tim.src tim-tutor.src
	cat tim.src tim-tutor.src | ./make-version 1 M >$@

tim2.m: tim.src tim-tutor.src
	cat tim.src tim-tutor.src | ./make-version 2 M >$@

tim3.m: tim.src tim-tutor.src
	cat tim.src tim-tutor.src | ./make-version 3 M >$@

tim4.m: tim.src tim-tutor.src
	cat tim.src tim-tutor.src | ./make-version 4 M >$@

tim5.m: tim.src tim-tutor.src

	cat tim.src tim-tutor.src | ./make-version 5 M >$@

gm1.m:	gm.src gm-tutor.src
	cat gm.src gm-tutor.src | ./make-version 1 M > $@

gm2.m:	gm.src gm-tutor.src
	cat gm.src gm-tutor.src | ./make-version 2 M > $@

gm3.m:	gm.src gm-tutor.src
	cat gm.src gm-tutor.src | ./make-version 3 M > $@

gm4.m:	gm.src gm-tutor.src
	cat gm.src gm-tutor.src | ./make-version 4 M > $@

gm5.m:	gm.src gm-tutor.src
	cat gm.src gm-tutor.src | ./make-version 5 M > $@

gm6.m:	gm.src gm-tutor.src
	cat gm.src gm-tutor.src | ./make-version 6 M > $@

gm7.m:	gm.src gm-tutor.src
	cat gm.src gm-tutor.src | ./make-version 7 M > $@

pargm1.m: pargm.src pargm-tutor.src
	cat pargm.src pargm-tutor.src | ./make-version 1 M > $@

pargm2.m: pargm.src pargm-tutor.src
	cat pargm.src pargm-tutor.src | ./make-version 2 M > $@

pargm3.m: pargm.src pargm-tutor.src
	cat pargm.src pargm-tutor.src | ./make-version 3 M n> $@

pargm4.m: pargm.src pargm-tutor.src
	cat pargm.src pargm-tutor.src | ./make-version 4 M > $@

lambda1.m: lambda.src
	cat lambda.src | ./make-version 1 M > $@

lambda2.m: lambda.src lambda-tutor.src
	cat lambda.src lambda-tutor.src | ./make-version 2 M > $@

lambda3.m: lambda.src lambda-tutor.src
	cat lambda.src lambda-tutor.src | ./make-version 3 M > $@

lambda4.m: lambda.src lambda-tutor.src
	cat lambda.src lambda-tutor.src | ./make-version 4 M > $@

lambda5.m: lambda.src lambda-tutor.src
	cat lambda.src lambda-tutor.src | ./make-version 5 M > $@

lambda6.m: lambda.src lambda-tutor.src
	cat lambda.src lambda-tutor.src | ./make-version 6 M > $@



.src.m: 
	cat $< | ./make-version $(VERSION) M >$@



clean_mira:
	rm -f $(STUDENT_SRCS:.src=.x) $(TUTOR_SRCS:.src=.x)


#################################################
#						#
#	Making Gofer sources			#
#						#
#################################################

language.lhs: language.src language-tutor.src
	cat language.src language-tutor.src | ./make-version 3 G > $@


template1.lhs: template.src template-tutor.src
	cat template.src template-tutor.src | ./make-version 1 G > $@

template2.lhs: template.src template-tutor.src
	cat template.src template-tutor.src | ./make-version 2 G > $@

template3.lhs: template.src template-tutor.src
	cat template.src template-tutor.src | ./make-version 3 G > $@

template4.lhs: template.src template-tutor.src
	cat template.src template-tutor.src | ./make-version 4 G > $@

template5.lhs: template.src template-tutor.src
	cat template.src template-tutor.src | ./make-version 5 G > $@

tim.lhs:	tim.src tim-tutor.src
	cat tim.src tim-tutor.src | ./make-version $(VERSION) G >$@

tim1.lhs: tim.src tim-tutor.src
	cat tim.src tim-tutor.src | ./make-version 1 G >$@

tim2.lhs: tim.src tim-tutor.src
	cat tim.src tim-tutor.src | ./make-version 2 G >$@

tim3.lhs: tim.src tim-tutor.src
	cat tim.src tim-tutor.src | ./make-version 3 G >$@

tim4.lhs: tim.src tim-tutor.src
	cat tim.src tim-tutor.src | ./make-version 4 G >$@

tim5.lhs: tim.src tim-tutor.src
	cat tim.src tim-tutor.src | ./make-version 5 G >$@

gm1.lhs:	gm.src gm-tutor.src
	cat gm.src gm-tutor.src | ./make-version 1 G > $@

gm2.lhs:	gm.src gm-tutor.src
	cat gm.src gm-tutor.src | ./make-version 2 G > $@

gm3.lhs:	gm.src gm-tutor.src
	cat gm.src gm-tutor.src | ./make-version 3 G > $@

gm4.lhs:	gm.src gm-tutor.src
	cat gm.src gm-tutor.src | ./make-version 4 G > $@

gm5.lhs:	gm.src gm-tutor.src
	cat gm.src gm-tutor.src | ./make-version 5 G > $@

gm6.lhs:	gm.src gm-tutor.src
	cat gm.src gm-tutor.src | ./make-version 6 G > $@

gm7.lhs:	gm.src gm-tutor.src
	cat gm.src gm-tutor.src | ./make-version 7 G > $@

pargm1.lhs: pargm.src pargm-tutor.src
	cat pargm.src pargm-tutor.src | ./make-version 1 G > $@

pargm2.lhs: pargm.src pargm-tutor.src
	cat pargm.src pargm-tutor.src | ./make-version 2 G > $@

pargm3.lhs: pargm.src pargm-tutor.src
	cat pargm.src pargm-tutor.src | ./make-version 3 G n> $@

pargm4.lhs: pargm.src pargm-tutor.src
	cat pargm.src pargm-tutor.src | ./make-version 4 G > $@

lambda1.lhs: lambda.src
	cat lambda.src | ./make-version 1 G > $@

lambda2.lhs: lambda.src lambda-tutor.src
	cat lambda.src lambda-tutor.src | ./make-version 2 G > $@

lambda3.lhs: lambda.src lambda-tutor.src
	cat lambda.src lambda-tutor.src | ./make-version 3 G > $@

lambda4.lhs: lambda.src lambda-tutor.src
	cat lambda.src lambda-tutor.src | ./make-version 4 G > $@

lambda5.lhs: lambda.src lambda-tutor.src
	cat lambda.src lambda-tutor.src | ./make-version 5 G > $@

lambda6.lhs: lambda.src lambda-tutor.src
	cat lambda.src lambda-tutor.src | ./make-version 6 G > $@



.src.lhs: 
	cat $< | ./make-version $(VERSION) G >$@


#################################################
#						#
#	Making Haskell sources			#
#						#
#################################################

Language.lhs: language.src language-tutor.src
	cat language.src language-tutor.src | ./make-version 3 H > $@


Utils.lhs: utils.src
	cat utils.src | ./make-version 3 H > $@


Template1.lhs: template.src template-tutor.src
	cat template.src template-tutor.src | ./make-version 1 H > $@

Template2.lhs: template.src template-tutor.src
	cat template.src template-tutor.src | ./make-version 2 H > $@

Template3.lhs: template.src template-tutor.src
	cat template.src template-tutor.src | ./make-version 3 H > $@

Template4.lhs: template.src template-tutor.src
	cat template.src template-tutor.src | ./make-version 4 H > $@

Template5.lhs: template.src template-tutor.src
	cat template.src template-tutor.src | ./make-version 5 H > $@

Tim.lhs:	tim.src tim-tutor.src
	cat tim.src tim-tutor.src | ./make-version $(VERSION) H >$@

Tim1.lhs: tim.src tim-tutor.src
	cat tim.src tim-tutor.src | ./make-version 1 H >$@

Tim2.lhs: tim.src tim-tutor.src
	cat tim.src tim-tutor.src | ./make-version 2 H >$@

Tim3.lhs: tim.src tim-tutor.src
	cat tim.src tim-tutor.src | ./make-version 3 H >$@

Tim4.lhs: tim.src tim-tutor.src
	cat tim.src tim-tutor.src | ./make-version 4 H >$@

Tim5.lhs: tim.src tim-tutor.src
	cat tim.src tim-tutor.src | ./make-version 5 H >$@

Gm1.lhs:	gm.src gm-tutor.src
	cat gm.src gm-tutor.src | ./make-version 1 H > $@

Gm2.lhs:	gm.src gm-tutor.src
	cat gm.src gm-tutor.src | ./make-version 2 H > $@
	cp GM.hi GM2.hi

Gm3.lhs:	gm.src gm-tutor.src
	cat gm.src gm-tutor.src | ./make-version 3 H > $@
	cp GM.hi GM3.hi

Gm4.lhs:	gm.src gm-tutor.src
	cat gm.src gm-tutor.src | ./make-version 4 H > $@
	cp GM.hi GM4.hi

Gm5.lhs:	gm.src gm-tutor.src
	cat gm.src gm-tutor.src | ./make-version 5 H > $@
	cp GM.hi GM5.hi

Gm6.lhs:	gm.src gm-tutor.src
	cat gm.src gm-tutor.src | ./make-version 6 H > $@
	cp GM.hi GM6.hi

Gm7.lhs:	gm.src gm-tutor.src
	cat gm.src gm-tutor.src | ./make-version 7 H > $@
	cp GM.hi GM7.hi

Pargm1.lhs: pargm.src pargm-tutor.src
	cat pargm.src pargm-tutor.src | ./make-version 1 H > $@

Pargm2.lhs: pargm.src pargm-tutor.src
	cat pargm.src pargm-tutor.src | ./make-version 2 H > $@

Pargm3.lhs: pargm.src pargm-tutor.src
	cat pargm.src pargm-tutor.src | ./make-version 3 H n> $@

Pargm4.lhs: pargm.src pargm-tutor.src
	cat pargm.src pargm-tutor.src | ./make-version 4 H > $@

Lambda1.lhs: lambda.src
	cat lambda.src | ./make-version 1 H > $@

Lambda2.lhs: lambda.src lambda-tutor.src
	cat lambda.src lambda-tutor.src | ./make-version 2 H > $@

Lambda3.lhs: lambda.src lambda-tutor.src
	cat lambda.src lambda-tutor.src | ./make-version 3 H > $@

Lambda4.lhs: lambda.src lambda-tutor.src
	cat lambda.src lambda-tutor.src | ./make-version 4 H > $@

Lambda5.lhs: lambda.src lambda-tutor.src
	cat lambda.src lambda-tutor.src | ./make-version 5 H > $@

Lambda6.lhs: lambda.src lambda-tutor.src
	cat lambda.src lambda-tutor.src | ./make-version 6 H > $@



#################################################
#						#
#	Making Haskell executables		#
#						#
#################################################


Template1.o: Template1.lhs Language.hi Utils.hi
	$(HC) -c $<
	cp Template.hi Template1.hi

Template2.o: Template2.lhs Language.hi Utils.hi
	$(HC) -c $
	cp Template.hi Template2.hi

Template3.o: Template3.lhs Language.hi Utils.hi
	$(HC) -c $<
	cp Template.hi Template3.hi

Template4.o: Template4.lhs Language.hi Utils.hi
	$(HC) -c $<
	cp Template.hi Template4.hi

Template5.o: Template5.lhs Language.hi Utils.hi
	$(HC) -c $<
	cp Template.hi Template5.hi

Gm1.o: Gm1.lhs Language.hi Utils.hi
	$(HC) -c $<
	cp GM.hi GM1.hi

Gm2.o: Gm2.lhs Language.hi Utils.hi
	$(HC) -c $<
	cp GM.hi GM2.hi

Gm3.o: Gm3.lhs Language.hi Utils.hi
	$(HC) -c $<
	cp GM.hi GM3.hi

Gm4.o: Gm4.lhs Language.hi Utils.hi
	$(HC) -c $<
	cp GM.hi GM4.hi

Gm5.o: Gm5.lhs Language.hi Utils.hi
	$(HC) -c $<
	cp GM.hi GM5.hi

Gm6.o: Gm6.lhs Language.hi Utils.hi
	$(HC) -c $<
	cp GM.hi GM6.hi

Gm7.o: Gm7.lhs Language.hi Utils.hi
	$(HC) -c $<
	cp GM.hi GM7.hi

Lambda1.o: Lambda1.lhs Language.hi Utils.hi
	$(HC) -c $<
	cp Lambda.hi Lambda1.hi

Lambda2.o: Lambda2.lhs Language.hi Utils.hi
	$(HC) -c $<
	cp Lambda.hi Lambda2.hi

Lambda3.o: Lambda3.lhs Language.hi Utils.hi
	$(HC) -c $<
	cp Lambda.hi Lambda3.hi

Lambda4.o: Lambda4.lhs Language.hi Utils.hi
	$(HC) -c $<
	cp Lambda.hi Lambda4.hi

Lambda5.o: Lambda5.lhs Language.hi Utils.hi
	$(HC) -c $<
	cp Lambda.hi Lambda5.hi

Lambda6.o: Lambda6.lhs Language.hi Utils.hi
	$(HC) -c $<
	cp Lambda.hi Lambda6.hi

Tim1.o: Tim1.lhs Language.hi Utils.hi
	$(HC) -c $<
	cp Tim.hi Tim1.hi

Tim2.o: Tim2.lhs Language.hi Utils.hi
	$(HC) -c $<
	cp Tim.hi Tim2.hi

Tim3.o: Tim3.lhs Language.hi Utils.hi
	$(HC) -c $<
	cp Tim.hi Tim3.hi

Tim4.o: Tim4.lhs Language.hi Utils.hi
	$(HC) -c $<
	cp Tim.hi Tim4.hi

Tim5.o: Tim5.lhs Language.hi Utils.hi
	$(HC) -c $<
	cp Tim.hi Tim5.hi

Pargm1.o: Pargm1.lhs Language.hi Utils.hi
	$(HC) -c $<
	cp ParGM.hi ParGM1.hi

Pargm2.o: Pargm2.lhs Language.hi Utils.hi
	$(HC) -c $<
	cp ParGM.hi ParGM2.hi

Pargm3.o: Pargm3.lhs Language.hi Utils.hi
	$(HC) -c $<
	cp ParGM.hi ParGM3.hi

Pargm4.o: Pargm4.lhs Language.hi Utils.hi
	$(HC) -c $<
	cp ParGM.hi ParGM4.hi

Utils.o: Utils.lhs
	$(HC) -c $<

Language.o: Language.lhs Utils.hi
	$(HC) -c $<

Template.o: Template.hs Template.hi Language.hi Utils.hi
	$(HC) -c $<

ParGM.o: ParGM.hs ParGM.hi Language.hi Utils.hi
	$(HC) -c $<

GM.o: GM.hs GM.hi Language.hi Utils.hi
	$(HC) -c $<

Lambda.o: Lambda.hs Lambda.hi Language.hi Utils.hi
	$(HC) -c $<

Tim.o: Tim.hs Tim.hi Language.hi Utils.hi
	$(HC) -c $<

Parse.o: Parse.hs Language.hi Utils.hi
	$(HC) -c $<

parse:	Parse.o Utils.o Language.o
	$(HC) -o $@ Parse.o Utils.o Language.o

CodeGen.o: CodeGen.hs GM.hi Language.hi Utils.hi
	$(HC) -c $<

cg: Gm7.o CodeGen.o Utils.o Language.o
	$(HC) -o $@ Gm7.o CodeGen.o Utils.o Language.o

template1: Template1.o Template.o Utils.o Language.o
	$(HC) -o $@ Template1.o Template.o Utils.o Language.o

template2: Template2.o Template.o Utils.o Language.o
	$(HC) -o $@ Template2.o Template.o Utils.o Language.o

template3: Template3.o Template.o Utils.o Language.o
	$(HC) -o $@ Template3.o Template.o Utils.o Language.o

template4: Template4.o Template.o Utils.o Language.o
	$(HC) -o $@ Template4.o Template.o Utils.o Language.o

template5: Template5.o Template.o Utils.o Language.o
	$(HC) -o $@ Template5.o Template.o Utils.o Language.o

gm1: Gm1.o GM.o Utils.o Language.o
	$(HC) -o $@ Gm1.o GM.o Utils.o Language.o

gm2: Gm2.o GM.o Utils.o Language.o
	$(HC) -o $@ Gm2.o GM.o Utils.o Language.o

gm3: Gm3.o GM.o Utils.o Language.o
	$(HC) -o $@ Gm3.o GM.o Utils.o Language.o

gm4: Gm4.o GM.o Utils.o Language.o
	$(HC) -o $@ Gm4.o GM.o Utils.o Language.o

gm5: Gm5.o GM.o Utils.o Language.o
	$(HC) -o $@ Gm5.o GM.o Utils.o Language.o

gm6: Gm6.o GM.o Utils.o Language.o
	$(HC) -o $@ Gm6.o GM.o Utils.o Language.o

gm7: Gm7.o GM.o Utils.o Language.o
	$(HC) -o $@ Gm7.o GM.o Utils.o Language.o

tim1: Tim1.o Tim.o Utils.o Language.o
	$(HC) -o $@ Tim1.o Tim.o Utils.o Language.o

tim2: Tim2.o Tim.o Utils.o Language.o
	$(HC) -o $@ Tim2.o Tim.o Utils.o Language.o

tim3: Tim3.o Tim.o Utils.o Language.o
	$(HC) -o $@ Tim3.o Tim.o Utils.o Language.o

tim4: Tim4.o Tim.o Utils.o Language.o
	$(HC) -o $@ Tim4.o Tim.o Utils.o Language.o

tim5: Tim5.o Tim.o Utils.o Language.o
	$(HC) -o $@ Tim5.o Tim.o Utils.o Language.o

lambda1: Lambda1.o Lambda.o Utils.o Language.o
	$(HC) -o $@ Lambda1.o Lambda.o Utils.o Language.o

lambda2: Lambda2.o Lambda.o Utils.o Language.o
	$(HC) -o $@ Lambda2.o Lambda.o Utils.o Language.o

lambda3: Lambda3.o Lambda.o Utils.o Language.o
	$(HC) -o $@ Lambda3.o Lambda.o Utils.o Language.o

lambda4: Lambda4.o Lambda.o Utils.o Language.o
	$(HC) -o $@ Lambda4.o Lambda.o Utils.o Language.o

lambda5: Lambda5.o Lambda.o Utils.o Language.o
	$(HC) -o $@ Lambda5.o Lambda.o Utils.o Language.o

lambda6: Lambda6.o Lambda.o Utils.o Language.o
	$(HC) -o $@ Lambda6.o Lambda.o Utils.o Language.o

pargm1: Pargm1.o ParGM.o Utils.o Language.o
	$(HC) -o $@ Pargm1.o ParGM.o Utils.o Language.o

pargm2: Pargm2.o ParGM.o Utils.o Language.o
	$(HC) -o $@ Pargm2.o ParGM.o Utils.o Language.o

pargm3: Pargm3.o ParGM.o Utils.o Language.o
	$(HC) -o $@ Pargm3.o ParGM.o Utils.o Language.o

pargm4: Pargm4.o ParGM.o Utils.o Language.o
	$(HC) -o $@ Pargm4.o ParGM.o Utils.o Language.o

.lhs.hi:
	$(HC) -c $<

.hs.hi:
	$(HC) -c $<



#################################################
#						#
#	Latex mumbo jumbo			#
#						#
#################################################

# All .tex files depend on the verbatim program
#
$(STUDENT_SRCS:.src=.tex) $(TUTOR_SRCS:.src=.tex) : verbatim


.src.tex:
	@echo "Making $@ from $<"
	@expand < $< | ./make-version LATEX $(LANGUAGE) | perl add-index-entries | ./verbatim > $@

.fig.tex:
	fig2dev -L latex $< $@

.fig.ps:
	fig2dev -L ps $< $@

.tex.dvi:
	$(LATEX) $<

# Making the index.  We are trying to make xxx.ind, but to avoid updating
# it when it hasn't changed, we make xxx.nid first, then compare with xxx.ind,
# and only update if there is a change.
#
# There are the following steps:
#
#	1. Sort the index and subsume redundant entries, creating
#		student.idx1
#	2. Create the main index: student.main.ind  (first grep/cmp)
#	3. Create the other index: student.prog.ind (second grep/cmp)
#	4. Clean up

student.prog.ind student.main.ind:	student.idx
	sort student.idx | perl subsume-index-entries > student.idx1
	-grep -v "indexentry{@@" < student.idx1 | \
		makeindex -i -t student.main.ilg > student.main.nid
	-cmp -s student.main.nid student.main.ind || \
		mv student.main.nid student.main.ind
	-grep "indexentry{@@" < student.idx1 | \
		sed "s/indexentry{@@/indexentry{/" | \
		makeindex -i -t student.prog.ilg > student.prog.nid
	-cmp -s student.prog.nid student.prog.ind || \
		mv student.prog.nid student.prog.ind
	rm -f student.idx1 student.main.nid student.prog.nid

clean_latex:
	rm -f 	$(STUDENT_SRCS:.src=.tex) $(TUTOR_SRCS:.src=.tex) \
	*.dvi *.log *.aux *.toc


#################################################
#						#
#	Making the "verbatim" program		#
#						#
#################################################

verbatim:       verbatim.l
	lex verbatim.l
	cc lex.yy.c -ll -o verbatim
	rm lex.yy.c




