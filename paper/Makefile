LATEX=pdflatex -interaction=batchmode
NAME=index
FINALPDF=$(NAME).pdf
MAIN_TEX_SOURCE=$(NAME).tex
AUX_SOURCE=$(NAME).aux
# INDEX_SOURCE=$(NAME).toc
SOURCE=*.tex
TEMPFILES=*.aux *.out *.log *.toc *.nav *.snm  *~

# output color:
#COLOR="\033["
WHITE="\033[38;1m"
GREEN="\033[32;1m"
CLOSE="\033[0m"

# prefixes:
WORK=$(GREEN)"::"$(CLOSE)
INFO=$(GREEN)" >"$(CLOSE)


all: $(FINALPDF)
	@echo  $(INFO) $(WHITE)"Compiled"$(CLOSE)

$(FINALPDF): $(AUX_SOURCE) $(SOURCE)
	@echo -n $(WORK) $(WHITE)"Compiling" $@ $(CLOSE); $(LATEX) $(MAIN_TEX_SOURCE) 2>&1 > /dev/null
	@echo  "\t"$(GREEN)[$(CLOSE)$(WHITE)"Done"$(CLOSE)$(GREEN)]$(CLOSE)

$(AUX_SOURCE): $(MAIN_TEX_SOURCE) # $(SOURCE)
	@echo -n $(WORK) $(WHITE)"Compiling" $@ $(CLOSE); $(LATEX) $(MAIN_TEX_SOURCE) 2>&1 > /dev/null
	@echo  "\t"$(GREEN)[$(CLOSE)$(WHITE)"Done"$(CLOSE)$(GREEN)]$(CLOSE)

clean:
	@echo  $(WORK) $(WHITE)"Clean useless files"$(CLOSE)
	@rm -f $(FINALPDF) $(TEMPFILES)

erase:
	@echo  $(WORK) $(WHITE)"Erase useless & aux files"$(CLOSE)
	@rm -f $(FINALPDF) $(TEMPFILES) $(AUX_SOURCE)

open: $(FINALPDF)
	@echo  $(INFO) $(WHITE)"Apro $(FINALPDF)"$(CLOSE)
	@xdg-open $(FINALPDF)
