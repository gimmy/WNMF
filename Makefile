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

CONVERT_script=ascii_pgm_convert
FACE_DIR=faces
FACES=faces/*.pgm
TMPDIR=tmp
LINK=http://www.cl.cam.ac.uk/Research/DTG/attarchive/pub/data/att_faces.zip

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
	@echo -e $(WORK) $(WHITE)"Clean all useless files and directories"$(CLOSE)
	@rm -f $(CONVERT) $(TMPFILES) $(OBJECTS) $(EXECUTABLE)
	@rm -rf $(TMPDIR) $(FACE_DIR)

run: $(EXECUTABLE)
	@echo -e $(INFO) $(WHITE)"Run" $< $(CLOSE)
	@./$<

whole: $(FACES) $(EXECUTABLE)
	@echo -e $(INFO)$(CLOSE) $(WHITE)"Ready"$(CLOSE)

$(CONVERT): $(CONVERT_PATH)$(SOURCE_CONVERT)
	@echo -en $(WORK) $(WHITE)"Compiling" $@ $(CLOSE); $(CC_CONVERT) $< -o $@
	@echo -e "\t"$(GREEN)[$(CLOSE)$(WHITE)"Done"$(CLOSE)$(GREEN)]$(CLOSE)

faces: $(FACES)
	@echo -e $(INFO)$(CLOSE) $(WHITE)"Faces are in "$(FACE_DIR)"/ directory" $(CLOSE)

$(FACES): $(CONVERT)
	@mkdir $(FACE_DIR) $(TMPDIR)
	@echo -en $(INFO) $(WHITE)"Download Faces...\n"$(CLOSE); 
	@curl -L# $(LINK) > $(TMPDIR)/att_faces.zip
	@echo -en $(INFO) $(WHITE)"Extract Faces...\n"$(CLOSE); unzip -q $(TMPDIR)/att_faces.zip -d $(TMPDIR)
	@echo -en $(WORK) $(WHITE)"Converting Faces " $(CLOSE); ./$(CONVERT_script) $(TMPDIR) $(FACE_DIR)
	@echo -e  "\t"$(GREEN)[$(CLOSE)$(WHITE)"Done"$(CLOSE)$(GREEN)]$(CLOSE)
	@echo -en $(INFO) $(WHITE)"Clean up" $(CLOSE); rm -r $(TMPDIR)
	@echo -e "\t\t"$(GREEN)[$(CLOSE)$(WHITE)"Done"$(CLOSE)$(GREEN)]$(CLOSE)

