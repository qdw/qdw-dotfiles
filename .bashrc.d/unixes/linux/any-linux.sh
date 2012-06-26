##########################
########################## Settings and functions that work on any Linux distro
##########################

# gsm FILE1 ...: play .gsm files.
gsm() { tcat $@ > /dev/audio ;}
