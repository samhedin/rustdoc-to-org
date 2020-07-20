# Used for development
refresh:
	pandoc enum.Option.html -t json | stack run | pandoc -f json -t native -o filterednative
	pandoc -f native filterednative -o option.org

refresh_trait:
	pandoc trait.AsRef.html -t json | stack run | pandoc -f json -t native -o filterednative
	pandoc -f native filterednative -o AsRef.org