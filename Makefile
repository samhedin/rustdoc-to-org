# Used for development
refresh:
	cd pandoc_filter; \
	stack install; \
	cd .. ;\
	pandoc enum.Option.html -t json | rustdoc-to-org-exe | pandoc -f json -t native -o filterednative
	pandoc -f native filterednative -o option.org

refresh_trait:
	cd pandoc_filter; \
	stack install; \
	cd .. ;\
	pandoc trait.AsRef.html -t json | rustdoc-to-org-exe | pandoc -f json -t native -o filterednative
	pandoc -f native filterednative -o AsRef.org
