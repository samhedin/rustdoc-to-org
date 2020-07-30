# Used for development
standard:
	cd debug_files ;\
	pandoc enum.Option.html  --lua-filter filter.lua -t native -o filterednative ;\
	pandoc -f native filterednative -o option.org

trait:
	cd debug_files ;\
	pandoc trait.AsRef.html --lua-filter filter.lua -t native -o filterednative ;\
	pandoc -f native filterednative -o AsRef.org

code:
	cd debug_files ;\
	pandoc shared.rs.html --lua-filter filter.lua -t native -o filterednative ;\
	pandoc -f native filterednative -o shared.rs.org

constant:
	cd debug_files ;\
	pandoc constant.ARCH.html --lua-filter filter.lua -t native -o filterednative ;\
	pandoc constant.ARCH.html -t json | rustdoc-to-org-exe | pandoc -f json -t native -o filterednative ;\
	pandoc -f native filterednative -o constant.arch.org
