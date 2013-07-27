CC= gfortran
CFLAGS= -c
# CFLAGS= -c -O0 -g	# for debug
LDFLAGS=
SOURCE=wnmf.f90 pgma_io.f90 read.f90 createA.f90 factor.f90
OBJECTS= $(SOURCE:.f90=.o)
EXECUTABLE=PGM
TMPFILES= *~ a.out *.o

CC_CONVERT= g++
CONVERT_PATH=convert/
SOURCE_CONVERT=pgmb_to_pgma.cpp
CONVERT=bin2ascii

# output color:
#COLOR="\033["
WHITE="\033[38;1m"
GREEN="\033[32;1m"
CLOSE="\033[0m"

# prefixes:
WORK=$(GREEN)"::"$(CLOSE)
INFO=$(GREEN)" >"$(CLOSE)

all: $(EXECUTABLE)
	@echo -e $(INFO)$(CLOSE) $(WHITE)"Compiled"$(CLOSE)
#	@echo -e  $(GREEN)[$(CLOSE)$(WHITE)"Done"$(CLOSE)$(GREEN)]$(CLOSE)

$(EXECUTABLE): $(OBJECTS)
	@echo -en $(WORK) $(WHITE)"Linking..."$(CLOSE)"\t"; $(CC) $(LDFLAGS) $(OBJECTS) -o $@
	@echo -en  "\t"$(GREEN)[$(CLOSE)$(WHITE)"Done"$(CLOSE)$(GREEN)]$(CLOSE)

%.o: %.f90
	@echo -en $(WORK) $(WHITE)"Compiling" $@ $(CLOSE); $(CC) $(CFLAGS) $< -o $@
	@echo -e "\t"$(GREEN)[$(CLOSE)$(WHITE)"Done"$(CLOSE)$(GREEN)]$(CLOSE)
clean:
	@echo -e $(WORK) $(WHITE)"Clean useless files"$(CLOSE)
	@rm -f $(TMPFILES) $(OBJECTS) $(EXECUTABLE)
cclean:
	@echo -e $(WORK) $(WHITE)"Clean all useless files"$(CLOSE)
	@rm -f $(CONVERT) $(TMPFILES) $(OBJECTS) $(EXECUTABLE)

run: $(EXECUTABLE)
	@echo -e $(INFO) $(WHITE)"Run" $< $(CLOSE)
	@./$<

whole: $(CONVERT) $(EXECUTABLE)
	@echo -e $(INFO)$(CLOSE) $(WHITE)"Compiled"$(CLOSE)
$(CONVERT): $(CONVERT_PATH)$(SOURCE_CONVERT)
	@echo -en $(WORK) $(WHITE)"Compiling" $@ $(CLOSE); $(CC_CONVERT) $< -o $@
	@echo -e "\t"$(GREEN)[$(CLOSE)$(WHITE)"Done"$(CLOSE)$(GREEN)]$(CLOSE)
