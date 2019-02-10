amixer set Master toggle;
if 
[[ -n $(amixer get Master | grep 'Left:' | grep -o "\[off\]") ]] ;
  then volnoti-show -m -v;
else 
amixer get Master | egrep -o "([0-9]+)%" | egrep -o "[0-9]+" | xargs -0 volnoti-show;
fi
