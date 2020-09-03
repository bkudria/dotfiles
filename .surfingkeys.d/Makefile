files := utils theme settings mappings sites
default:
	rm ../.surfingkeys.js
	cat $(foreach file, $(files),$(file).js) > ../.surfingkeys.js
