# Used for development
refresh:
	pandoc option.html -t json | stack run | pandoc -f json -t native -o filterednative
	pandoc -f native filterednative -o option.org

refresh_trait:
	pandoc trait.AsRef.html -t json | stack run | pandoc -f json -t native -o filterednative
	pandoc -f native filterednative -o AsRef.org

native:
	pandoc option.html -t native -o native
	pandoc -f native native -o optionnative.org

install:
	stack install

run_installed:
	pandoc option.html --filter rustoc-to-org-exe -o option.org

refresh_all:
	pandoc option.html -t json | stack run | pandoc -f json -t native -o filterednative
	pandoc -f native filterednative -o option.org
	pandoc trait.AsRef.html -t json | stack run | pandoc -f json -t native -o filterednative
	pandoc -f native filterednative -o AsRef.org
