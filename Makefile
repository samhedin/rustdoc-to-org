# Used for development
refresh:
	pandoc option.html -t json | stack run | pandoc -f json -t native -o filterednative
	pandoc -f native filterednative -o option.org