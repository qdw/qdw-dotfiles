########     dependencies:
########         none
########     third-party dependencies:
########          perlbrew (installed in ~/perl5, the default dir for that)
########          Module::Install)

# Make Module::Install auto-follow dependencies. cpanm sets this by default,
# but sometimes you can't use cpanm (e.g., you're developing a Catalyst app,
# which means using MakeMaker directly).
export PERL_MM_USE_DEFAULT=1

function pb {
    ~/perl5/perlbrew/bin/perlbrew "$@"
}

function pw {
    # shellcheck disable=SC2034
    local MANNAME="find a Perl library in PERL5LIB"
    check-args 1 1 'MyModule::Name | MyModule/Name.pm' "$@" || return 1
    perldoc -l "$1"
}

function inc {
    echo -n "perl = "; which perl
    echo
    perl -e 'print (join "\n", @INC); print "\n";'
}

function lwhich {
    for DIR in $(inc); do
        if [[ -x "$DIR/$1" ]]; then
            echo "$DIR/$1"
            return 0
        fi
    done
    return 2
}

function perlbrew-activate {
    # shellcheck disable=SC1091
    source "$HOME/perl5/perlbrew/etc/bashrc"
}
