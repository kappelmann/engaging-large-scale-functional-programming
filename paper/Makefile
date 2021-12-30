FILE := engaging_fp_education
OUT  := build

.PHONY: pdf
pdf:
	latexmk -interaction=nonstopmode -outdir=$(OUT) -pdf -shell-escape -halt-on-error $(FILE)

.PHONY: watch
watch:
	latexmk -interaction=nonstopmode -outdir=$(OUT) -pdf -shell-escape -pvc -halt-on-error $(FILE)

.PHONY: clean
clean:
	rm -rf $(filter-out $(OUT)/$(FILE).pdf, $(wildcard $(OUT)/*))

.PHONY: purge
purge:
	rm -rf $(OUT)

.PHONY: start
start:
	gnome-terminal -x "bash" -c "make watch" &
