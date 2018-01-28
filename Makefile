all: presentation.md
	pandoc -f markdown+grid_tables -t beamer presentation.md -o presentation.pdf
