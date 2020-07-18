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