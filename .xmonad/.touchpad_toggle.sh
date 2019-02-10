if 
[[ -n $(xinput list 'SynPS/2 Synaptics TouchPad' | grep -o "disabled") ]] ; 
  then xinput set-prop 11 'Device Enabled' 1;
else
xinput set-prop 11 'Device Enabled' 0;
fi
