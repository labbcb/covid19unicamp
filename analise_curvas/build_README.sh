sed -i 's/\\(/\$/g' modelo_derivadas.md
sed -i 's/\\)/\$/g' modelo_derivadas.md
sed -i 's/\\\[/\$\$/g' modelo_derivadas.md
sed -i 's/\\\]/\$\$/g' modelo_derivadas.md
python -m readme2tex --output README.md modelo_derivadas.md

# substitute 
# https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/None
# by
# https://raw.githubusercontent.com/labbcb/covid19unicamp/master/analise_curvas