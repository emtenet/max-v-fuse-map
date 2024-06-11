
OPT=--optimize

INDEX=/home/local/static/max-v/index.js

${INDEX}: src/Main.elm depends .SILENT
	echo "BUILD index.js"
	elm make ${OPT} --output=${.TARGET} ${.ALLSRC:[1]}

depends: .SILENT
	echo "DEPEND index.js"
	find src -name "*.elm" | sed -e "s|^|${INDEX}: |" > ${.TARGET}
	find src -type d | sed -e "s|^|depends: |" >> ${.TARGET}

.dinclude "depends"

