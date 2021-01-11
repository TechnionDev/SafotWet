esc=$(printf '\033')
RED='[0;31m'
GREEN='[0;32m'
ORANGE='[0;33m'
NC='[0m'

fswatch -0 ./mlist_ex5.sml | while read -d "" event 
do 
    clear
    cat mlisp.sml hw3_q3.sml mlist_ex5.sml > tmp.sml
    sml tmp.sml | grep -A 50 --color=always here \
		| sed -e "s/true/${esc}${GREEN}&${esc}${NC}/g" \
		| sed -e "s/bool/${esc}${ORANGE}&${esc}${NC}/g" \
		| sed -e "s/false/${esc}${RED}&${esc}${NC}/g"

done
