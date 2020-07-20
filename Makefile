# Used for development
refresh:
	pandoc enum.Option.html -t json | stack run | pandoc -f json -t native -o filterednative
	pandoc -f native filterednative -o option.org

refr:
	pandoc enum.Option.html -t json | stack run | pandoc -f json -t native -o filterednative
	pandoc -f native filterednative -o option.org

refresh_trait:
	pandoc trait.AsRef.html -t json | stack run | pandoc -f json -t native -o filterednative
	pandoc -f native filterednative -o AsRef.org

native:
	pandoc option.html -t native -o native
	pandoc -f native native -o optionnative.org

batch_convert:
	sh batch_convert.sh "/home/sam/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/share/doc/rust/html/std" "/home/sam/.emacs.d/private/rustdoc/"
