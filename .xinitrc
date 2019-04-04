# DBUS
if test -z "$DBUS_SESSION_BUS_ADDRESS" ; then
   eval `dbus-launch --sh-syntax --exit-with-session`
fi

#X
nitrogen --restore &
conky | while read -r; do xsetroot -name "$REPLY"; done & 

xrandr -s 3840x2160 &
xrb -merge .Xresources &

#StartX
exec xmonad
#exec subtle
#exec dwm 
#exec ck-launch-session dbus-launch --sh-syntax --exit-with-session awesome
