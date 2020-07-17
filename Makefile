refresh:
	pandoc option.html -t json | stack run | pandoc -f json -t native -o filterednative
	pandoc -f native filterednative -o option.org

out:
	pandoc ../option.html -t json | stack run | pandoc -f json -o ../option.org

filterednative:
	pandoc ../option.html -t json | stack run | pandoc -f json -t native -o ../filterednative
