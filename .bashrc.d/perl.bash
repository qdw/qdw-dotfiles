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
