FLATFILEDIR       = .

# Target flat files
FLATFILENAME3d    = postxconfig-NT3d.txt
FLATFILENAME2d    = postxconfig-NT2d.txt

# Source Post XML files
AVAILXMLFILENAME  = post_avblflds.xml
CTRLFILENAME2d    = postcntrl2d.xml
CTRLFILENAME3d    = postcntrl3d.xml

# Post flat file generator
PERLXML           = PostXMLPreprocessor.pl

# CMD with param
DTCPERLXML3d      = /usr/bin/perl $(PERLXML) $(CTRLFILENAME3d) $(AVAILXMLFILENAME) $(FLATFILENAME3d)
DTCPERLXML2d      = /usr/bin/perl $(PERLXML) $(CTRLFILENAME2d) $(AVAILXMLFILENAME) $(FLATFILENAME2d) 

# Targets
all: $(FLATFILENAME3d) $(FLATFILENAME2d)

$(FLATFILENAME3d): $(AVAILXMLFILENAME) $(CTRLFILENAME3d)
	$(DTCPERLXML3d)

$(FLATFILENAME2d): $(AVAILXMLFILENAME) $(CTRLFILENAME2d)
	$(DTCPERLXML2d)

clean:
	/bin/rm -f $(FLATFILENAME3d) $(FLATFILENAME2d)
