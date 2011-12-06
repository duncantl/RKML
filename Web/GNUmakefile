PACKAGE=RKML

include ../../AdminTools/GNUmakefile

TARGET_DIR=www.omegahat.org:OmegaWeb/$(PACKAGE)

SVG_FILES=pairs_link axes_tips axes_hrefs occ_anim

ship:
	scp $(SVG_FILES:%=../tests/%.svg) examples.html $(TARGET_DIR)/tests
	scp ../inst/CSS/RSVGPlot.css $(TARGET_DIR)

update:
	scp examples.html $(TARGET_DIR)/tests

changes:
	scp ../Changes $(TARGET_DIR)/

fixExamples:
	for i $(SVG_FILES:%=../tests/%.svg); do \
	   ssh eeyore  OmegaWeb/SVGAnnotation/tests/fix OmegaWeb/SVGAnnotation/tests/$$i
	fi


