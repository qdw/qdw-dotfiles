source ~/.bashrc
########################## MacPorts' PATH-munging (commented out; see below).
#
# I'm commenting this out, because I believe PATH should be initialized in
# ~/.bashrc, to a static value, without variable interpolation). That way,
# each new shell starts with a proper PATH, subshells (of subshells of
# subshells...) don't keep adding to PATH recursively, and any changes
# you make in this file take effect as soon as you start a new shell;
# they don't require you to log in again.
# # Your previous /Users/quinn/.bash_profile file was backed up as /Users/quinn/.bash_profile.macports-saved_2009-09-28_at_14:49:29
##
# MacPorts Installer addition on 2009-09-28_at_14:49:29: adding an appropriate PATH variable for use with MacPorts.
# export PATH=/opt/local/bin:/opt/local/sbin:$PATH
# Finished adapting your PATH environment variable for use with MacPorts.
