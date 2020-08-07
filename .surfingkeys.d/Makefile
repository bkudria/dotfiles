files := utils theme settings mappings sites engines
default:
	rm ../.surfingkeys.js
	cat $(foreach file, $(files),$(file).js) > ../.surfingkeys.js
