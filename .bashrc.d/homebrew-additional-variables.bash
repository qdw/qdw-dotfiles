## HOMEBREW_API_AUTO_UPDATE_SECS
# Check Homebrew’s API for new formulae or cask data every
# $HOMEBREW_API_AUTO_UPDATE_SECS seconds. Alternatively, disable
# API auto-update checks entirely with $HOMEBREW_NO_AUTO_UPDATE.

# Default: 450.

## HOMEBREW_API_DOMAIN
# Use this URL as the download mirror for Homebrew JSON API. If
# metadata files at that URL are temporarily unavailable, the
# default API domain will be used as a fallback mirror.

# Default: https://formulae.brew.sh/api.

## HOMEBREW_ARTIFACT_DOMAIN
# Prefix all download URLs, including those for bottles, with this
# value. For example, export
# HOMEBREW_ARTIFACT_DOMAIN=http://localhost:8080 will cause a
# formula with the URL https://example.com/foo.tar.gz to instead
# download from
# http://localhost:8080/https://example.com/foo.tar.gz. Bottle
# URLs however, have their domain replaced with this prefix. This
# results in e.g.
# https://ghcr.io/v2/homebrew/core/gettext/manifests/0.21 to
# instead be downloaded from
# http://localhost:8080/v2/homebrew/core/gettext/manifests/0.21.
# If the value already contains a /v2 path (e.g. an OCI registry
# proxying GitHub Packages under a repository prefix such as
# https://mirror.example.com/v2/ghcr-io), the v2 path is not
# duplicated, resulting in e.g.
# https://mirror.example.com/v2/ghcr-io/homebrew/core/gettext/manifests/0.21.

## HOMEBREW_ARTIFACT_DOMAIN_NO_FALLBACK
# When $HOMEBREW_ARTIFACT_DOMAIN and
# $HOMEBREW_ARTIFACT_DOMAIN_NO_FALLBACK are both set, if the
# request to $HOMEBREW_ARTIFACT_DOMAIN fails then Homebrew will
# error rather than trying any other/default URLs.

## HOMEBREW_AUTO_UPDATE_QUIET
# If set, the auto-update run before commands like brew install,
# brew upgrade or brew tap will not show information about new,
# outdated or deleted formulae and casks.

## HOMEBREW_AUTO_UPDATE_SECS
# Run brew update once every $HOMEBREW_AUTO_UPDATE_SECS seconds
# before some commands, e.g. brew install, brew upgrade or brew
# tap. Alternatively, disable auto-update entirely with
# $HOMEBREW_NO_AUTO_UPDATE.

# Default: 86400 (24 hours), 3600 (1 hour) if a developer command
# has been run or 300 (5 minutes) if $HOMEBREW_NO_INSTALL_FROM_API
# is set.

## HOMEBREW_AVOID_NESTED_SANDBOXING
# If set, skip Homebrew’s sandbox when it is itself running inside
# another sandbox, for an unprivileged user outside the default
# prefix. This trades Homebrew’s build-time network and filesystem
# denial for trust in the outer sandbox. Homebrew errors out if
# the prefix or group makes skipping unsupported.

## HOMEBREW_BAT
# If set, use bat for the brew cat command. Set $BAT_CONFIG_PATH
# to use a custom configuration file and $BAT_THEME to select a
# theme.

## HOMEBREW_BOTTLE_DOMAIN
# Use this URL as the download mirror for bottles and their
# manifests. If a bottle or manifest is unavailable at the mirror,
# the default bottle domain will be used as a fallback. Prefer
# $HOMEBREW_ARTIFACT_DOMAIN for a mirror that transparently
# proxies all Homebrew downloads. For example, export
# HOMEBREW_BOTTLE_DOMAIN=http://localhost:8080 will cause all
# bottles to download from the prefix http://localhost:8080/.

# Default: https://ghcr.io/v2/homebrew/core.

## HOMEBREW_BREW_GIT_REMOTE
# Use this URL as the Homebrew/brew git(1) remote.

# Default: https://github.com/Homebrew/brew.

## HOMEBREW_BROWSER
# Use this as the browser when opening project homepages.

# Default: $BROWSER or the OS’s default browser.

## HOMEBREW_BUNDLE_CLEANUP_NO_BREW
# If set, brew bundle cleanup will not clean up formula
# dependencies.

## HOMEBREW_BUNDLE_CLEANUP_NO_CARGO
# If set, brew bundle cleanup will not clean up Cargo packages.

## HOMEBREW_BUNDLE_CLEANUP_NO_CASK
# If set, brew bundle cleanup will not clean up cask dependencies.

## HOMEBREW_BUNDLE_CLEANUP_NO_FLATPAK
# If set, brew bundle cleanup will not clean up Flatpak packages.

## HOMEBREW_BUNDLE_CLEANUP_NO_GO
# If set, brew bundle cleanup will not clean up Go packages.

## HOMEBREW_BUNDLE_CLEANUP_NO_KREW
# If set, brew bundle cleanup will not clean up Krew plugins.

## HOMEBREW_BUNDLE_CLEANUP_NO_MAS
# If set, brew bundle cleanup will not clean up Mac App Store
# dependencies.

## HOMEBREW_BUNDLE_CLEANUP_NO_NPM
# If set, brew bundle cleanup will not clean up npm packages.

## HOMEBREW_BUNDLE_CLEANUP_NO_TAP
# If set, brew bundle cleanup will not clean up tap dependencies.

## HOMEBREW_BUNDLE_CLEANUP_NO_UV
# If set, brew bundle cleanup will not clean up uv tools.

## HOMEBREW_BUNDLE_CLEANUP_NO_VSCODE
# If set, brew bundle cleanup will not clean up VSCode (and
# forks/variants) extensions.

## HOMEBREW_BUNDLE_CLEANUP_NO_WINGET
# If set, brew bundle cleanup will not clean up WinGet packages.

## HOMEBREW_BUNDLE_DUMP_NO_BREW
# If set, brew bundle dump will not dump formula dependencies.

## HOMEBREW_BUNDLE_DUMP_NO_CARGO
# If set, brew bundle dump will not dump Cargo packages.

## HOMEBREW_BUNDLE_DUMP_NO_CASK
# If set, brew bundle dump will not dump cask dependencies.

## HOMEBREW_BUNDLE_DUMP_NO_FLATPAK
# If set, brew bundle dump will not dump Flatpak packages.

## HOMEBREW_BUNDLE_DUMP_NO_GO
# If set, brew bundle dump will not dump Go packages.

## HOMEBREW_BUNDLE_DUMP_NO_KREW
# If set, brew bundle dump will not dump Krew plugins.

## HOMEBREW_BUNDLE_DUMP_NO_MAS
# If set, brew bundle dump will not dump Mac App Store
# dependencies.

## HOMEBREW_BUNDLE_DUMP_NO_NPM
# If set, brew bundle dump will not dump npm packages.

## HOMEBREW_BUNDLE_DUMP_NO_TAP
# If set, brew bundle dump will not dump tap dependencies.

## HOMEBREW_BUNDLE_DUMP_NO_UV
# If set, brew bundle dump will not dump uv tools.

## HOMEBREW_BUNDLE_DUMP_NO_VSCODE
# If set, brew bundle dump will not dump VSCode (and
# forks/variants) extensions.

## HOMEBREW_BUNDLE_DUMP_NO_WINGET
# If set, brew bundle dump will not dump WinGet packages.

## HOMEBREW_BUNDLE_NO_DESCRIBE
# If set, do not enable bundle description comments from
# $HOMEBREW_BUNDLE_DESCRIBE or the default. This does not disable
# an explicit --describe.

## HOMEBREW_BUNDLE_SECRETS
# If set, do not enable the default secret scrubbing. This does
# not disable an explicit --no-secrets.

## HOMEBREW_BUNDLE_USER_CACHE
# If set, use this directory as the bundle(1) user cache.

## HOMEBREW_CACHE
# Use this directory as the download cache.

# Default: macOS: ~/Library/Caches/Homebrew, Linux:
# $XDG_CACHE_HOME/Homebrew or ~/.cache/Homebrew.

## HOMEBREW_CASK_OPTS
# Append these options to all cask commands. All --*dir options,
# --language, --require-sha and --no-binaries are supported. For
# example, you might add something like the following to your
# ~/.profile, ~/.bash_profile, or ~/.zshenv:

# export HOMEBREW_CASK_OPTS="--appdir=${HOME}/Applications
# --fontdir=/Library/Fonts"

## HOMEBREW_CLEANUP_MAX_AGE_DAYS
# Cleanup all cached files older than this many days.

# Default: 120.

## HOMEBREW_CLEANUP_PERIODIC_FULL_DAYS
# If set, brew install, brew upgrade and brew reinstall will
# cleanup all formulae when this number of days has passed.

# Default: 30.

## HOMEBREW_COLOR
# If set, force colour output on non-TTY outputs.

## HOMEBREW_CORE_GIT_REMOTE
# Use this URL as the Homebrew/homebrew-core git(1) remote.

# Default: https://github.com/Homebrew/homebrew-core.

## HOMEBREW_CURLRC
# If set to an absolute path (i.e. beginning with /), pass it with
# --config when invoking curl(1). If set but not a valid path, do
# not pass --disable, which disables the use of .curlrc.

## HOMEBREW_CURL_PATH
# Linux only: Set this value to a new enough curl executable for
# Homebrew to use.

# Default: curl.

## HOMEBREW_CURL_RETRIES
# Pass the given retry count to --retry when invoking curl(1).

# Default: 3.

## HOMEBREW_CURL_VERBOSE
# If set, pass --verbose when invoking curl(1).

## HOMEBREW_DEBUG
# If set, always assume --debug when running commands.

## HOMEBREW_DEVELOPER
# If set, tweak behaviour to be more relevant for Homebrew
# developers (active or budding) by e.g. turning warnings into
# errors.

## HOMEBREW_DISABLE_DEBREW
# If set, the interactive formula debugger available via --debug
# will be disabled.

## HOMEBREW_DISABLE_LOAD_FORMULA
# If set, refuse to load formulae. This is useful when formulae
# are not trusted (such as in pull requests).

## HOMEBREW_DISPLAY
# Use this X11 display when opening a page in a browser, for
# example with brew home. Primarily useful on Linux.

# Default: $DISPLAY.

## HOMEBREW_DISPLAY_INSTALL_TIMES
# If set, print install times for each formula at the end of the
# run.

## HOMEBREW_DOCKER_REGISTRY_BASIC_AUTH_TOKEN
# Use this base64 encoded username and password for authenticating
# with a Docker registry proxying GitHub Packages. If set to none,
# no authentication header will be sent. This can be used, if
# remote $HOMEBREW_ARTIFACT_DOMAIN does not support any
# authentication. If $HOMEBREW_DOCKER_REGISTRY_TOKEN is set, it
# will be used instead.

## HOMEBREW_DOCKER_REGISTRY_TOKEN
# Use this bearer token for authenticating with a Docker registry
# proxying GitHub Packages. Preferred over
# $HOMEBREW_DOCKER_REGISTRY_BASIC_AUTH_TOKEN.

## HOMEBREW_DOWNLOAD_CONCURRENCY
# Homebrew will download in parallel using this many concurrent
# connections. The default, auto, will use twice the number of
# available CPU cores (what our benchmarks showed to produce the
# best performance). If set to 1, Homebrew will download in
# serial.

# Default: auto.

## HOMEBREW_EDITOR
# Use this editor when editing a single formula, or several
# formulae in the same directory.

# Note: brew edit will open all of Homebrew as discontinuous files
# and directories. Visual Studio Code can handle this correctly in
# project mode, but many editors will do strange things in this
# case.

# Default: $EDITOR or $VISUAL.

## HOMEBREW_ENV_SYNC_STRICT
# If set, brew *env-sync will only sync the exact installed
# versions of formulae.

## HOMEBREW_FAIL_LOG_LINES
# Output this many lines of output on formula system failures.

# Default: 15.

## HOMEBREW_FORBIDDEN_CASKS
# A space-separated list of casks. Homebrew will refuse to install
# a cask if it or any of its dependencies is on this list.

## HOMEBREW_FORBIDDEN_FORMULAE
# A space-separated list of formulae. Homebrew will refuse to
# install a formula or cask if it or any of its dependencies is on
# this list.

## HOMEBREW_FORBIDDEN_LICENSES
# A space-separated list of SPDX licence identifiers. Homebrew
# will refuse to install a formula if it or any of its
# dependencies has a licence on this list.

## HOMEBREW_FORBIDDEN_OWNER
# The person who has set any $HOMEBREW_FORBIDDEN_* variables.

# Default: you.

## HOMEBREW_FORBIDDEN_OWNER_CONTACT
# How to contact the $HOMEBREW_FORBIDDEN_OWNER, if set and
# necessary.

## HOMEBREW_FORBIDDEN_TAPS
# A space-separated list of taps. Homebrew will refuse to install
# a formula if it or any of its dependencies is in a tap on this
# list. Each entry is a user/repository name (which matches only
# taps using the default GitHub remote) or a remote URL (required
# to match taps with a custom remote).

## HOMEBREW_FORBID_PACKAGES_FROM_PATHS
# If set, Homebrew will refuse to read formulae or casks provided
# from file paths, e.g. brew install ./package.rb.

# Default: true unless $HOMEBREW_DEVELOPER is set.

## HOMEBREW_FORCE_API_AUTO_UPDATE
# If set, update the Homebrew API formula or cask data even if
# $HOMEBREW_NO_AUTO_UPDATE is set.

## HOMEBREW_FORCE_BREWED_CA_CERTIFICATES
# If set, always use a Homebrew-installed ca-certificates rather
# than the system version.

## HOMEBREW_FORCE_BREWED_CURL
# If set, always use a Homebrew-installed curl(1) rather than the
# system version. Automatically set if the system version of curl
# is too old.

## HOMEBREW_FORCE_BREWED_GIT
# If set, always use a Homebrew-installed git(1) rather than the
# system version. Automatically set if the system version of git
# is too old.

## HOMEBREW_FORCE_VENDOR_RUBY
# If set, always use Homebrew’s vendored, relocatable Ruby version
# even if the system version of Ruby is new enough.

## HOMEBREW_FORMULA_BUILD_NETWORK
# If set, controls network access to the sandbox for formulae
# builds. Overrides any controls set through DSL usage inside
# formulae. Must be allow or deny. If no value is set through this
# environment variable or DSL usage, the default behaviour is
# allow.

## HOMEBREW_FORMULA_POSTINSTALL_NETWORK
# If set, controls network access to the sandbox for formulae
# postinstall. Overrides any controls set through DSL usage inside
# formulae. Must be allow or deny. If no value is set through this
# environment variable or DSL usage, the default behaviour is
# allow.

## HOMEBREW_FORMULA_TEST_NETWORK
# If set, controls network access to the sandbox for formulae
# test. Overrides any controls set through DSL usage inside
# formulae. Must be allow or deny. If no value is set through this
# environment variable or DSL usage, the default behaviour is
# allow.

## HOMEBREW_GITHUB_API_TOKEN
# Use this personal access token for the GitHub API, for features
# such as brew search. You can create one at
# <https://github.com/settings/tokens>. If set, GitHub will allow
# you a greater number of API requests. For more information, see:
# <https://docs.github.com/en/rest/overview/rate-limits-for-the-rest-api>

# Note: Homebrew doesn’t require permissions for any of the
# scopes, but some developer commands may require additional
# permissions.

## HOMEBREW_GITHUB_PACKAGES_TOKEN
# Use this GitHub personal access token when accessing the GitHub
# Packages Registry (where bottles may be stored).

## HOMEBREW_GITHUB_PACKAGES_USER
# Use this username when accessing the GitHub Packages Registry
# (where bottles may be stored).

## HOMEBREW_GIT_COMMITTER_EMAIL
# Set the Git committer email to this value.

## HOMEBREW_GIT_COMMITTER_NAME
# Set the Git committer name to this value.

## HOMEBREW_GIT_EMAIL
# Set the Git author name and, if $HOMEBREW_GIT_COMMITTER_EMAIL is
# unset, committer email to this value.

## HOMEBREW_GIT_NAME
# Set the Git author name and, if $HOMEBREW_GIT_COMMITTER_NAME is
# unset, committer name to this value.

## HOMEBREW_GIT_PATH
# Linux only: Set this value to a new enough git executable for
# Homebrew to use.

# Default: git.

## HOMEBREW_INSTALL_BADGE
# Print this text before the installation summary of each
# successful build.

# Default: The “Beer Mug” emoji.

## HOMEBREW_LIVECHECK_AUTOBUMP
# If set, brew livecheck will include data for packages that are
# autobumped by BrewTestBot.

## HOMEBREW_LIVECHECK_WATCHLIST
# Consult this file for the list of formulae to check by default
# when no formula argument is passed to brew livecheck.

# Default: ${XDG_CONFIG_HOME}/homebrew/livecheck_watchlist.txt if
# $XDG_CONFIG_HOME is set or ~/.homebrew/livecheck_watchlist.txt
# otherwise.

## HOMEBREW_LOCK_CONTEXT
# If set, Homebrew will add this output as additional context for
# locking errors. This is useful when running brew in the
# background.

## HOMEBREW_LOGS
# Use this directory to store log files.

# Default: macOS: ~/Library/Logs/Homebrew, Linux:
# ${XDG_CACHE_HOME}/Homebrew/Logs or ~/.cache/Homebrew/Logs.

## HOMEBREW_MAKE_JOBS
# Use this value as the number of parallel jobs to run when
# building with make(1).

# Default: The number of available CPU cores.

## HOMEBREW_NO_ANALYTICS
# If set, do not send analytics. Google Analytics were destroyed.
# For more information, see: <https://docs.brew.sh/Analytics>

## HOMEBREW_NO_ASK
# If set, do not enable default ask mode. This does not disable an
# explicit --ask.

## HOMEBREW_NO_AUTOREMOVE
# If set, calls to brew cleanup and brew uninstall will not
# automatically remove unused formula dependents.

## HOMEBREW_NO_AUTO_UPDATE
# If set, do not automatically update before running some
# commands, e.g. brew install, brew upgrade or brew tap.
# Preferably, run this less often by setting
# $HOMEBREW_AUTO_UPDATE_SECS to a value higher than the default.
# Note that setting this and e.g. tapping new taps may result in a
# broken configuration. Please ensure you always run brew update
# before reporting any issues.

## HOMEBREW_NO_BOOTSNAP
# If set, do not use Bootsnap to speed up repeated brew calls.

## HOMEBREW_NO_CLEANUP_FORMULAE
# A comma-separated list of formulae. Homebrew will refuse to
# clean up or autoremove a formula if it appears on this list.

## HOMEBREW_NO_COLOR
# If set, do not print text with colour added.

# Default: $NO_COLOR.

## HOMEBREW_NO_EMOJI
# If set, do not print $HOMEBREW_INSTALL_BADGE on a successful
# build.

## HOMEBREW_NO_ENV_HINTS
# If set, do not print any hints about changing Homebrew’s
# behaviour with environment variables.

## HOMEBREW_NO_GITHUB_API
# If set, do not use the GitHub API, e.g. for searches or fetching
# relevant issues after a failed install.

## HOMEBREW_NO_INSECURE_REDIRECT
# If set, forbid redirects from secure HTTPS to insecure HTTP.

# Note: while ensuring your downloads are fully secure, this is
# likely to cause sources for certain formulae hosted by
# SourceForge, GNU or GNOME to fail to download.

## HOMEBREW_NO_INSTALLED_DEPENDENTS_CHECK
# If set, do not check for broken linkage of dependents or
# outdated dependents after installing, upgrading or reinstalling
# formulae. This will result in fewer dependents (and their
# dependencies) being upgraded or reinstalled but may result in
# more breakage from running brew install formula or brew upgrade
# formula.

## HOMEBREW_NO_INSTALL_CLEANUP
# If set, brew install, brew upgrade and brew reinstall will never
# automatically cleanup installed/upgraded/reinstalled formulae or
# all formulae every $HOMEBREW_CLEANUP_PERIODIC_FULL_DAYS days.
# Alternatively, $HOMEBREW_NO_CLEANUP_FORMULAE allows specifying
# specific formulae to not clean up.

## HOMEBREW_NO_INSTALL_FROM_API
# If set, do not install formulae and casks in homebrew/core and
# homebrew/cask taps using Homebrew’s API and instead use (large,
# slow) local checkouts of these repositories.

## HOMEBREW_NO_INSTALL_UPGRADE
# If set, brew install formula|cask will not upgrade formula|cask
# if it is installed but outdated.

## HOMEBREW_NO_PATH_SHADOW_CHECK
# If set, brew info and brew install will not warn when a
# formula’s executables are shadowed by other commands earlier on
# $PATH.

## HOMEBREW_NO_RELOCATE_BUILD_PREFIX
# If set, do not relocate bottles built for a different prefix at
# install time. Homebrew will build from source instead.

## HOMEBREW_NO_SUDO
# If set, do not run commands with sudo(8). Automatically enabled
# when sudo is missing, reports a recognised inability to elevate
# privileges or explicitly denies access. A password requirement
# or an inconclusive check preserves normal sudo behaviour.

## HOMEBREW_NO_UPDATE_REPORT_NEW
# If set, brew update will not show the list of newly added
# formulae/casks.

## HOMEBREW_NO_UPGRADE_AUTO_UPDATES_CASKS
# If set, brew upgrade will not automatically upgrade casks with
# auto_updates true. Does not affect --greedy or
# --greedy-auto-updates upgrades.

## HOMEBREW_NO_UPGRADE_QUIT_CASKS
# If set, brew upgrade will not quit running applications for
# casks during upgrades.

## HOMEBREW_NO_VERIFY_ATTESTATIONS
# If set, Homebrew will not verify cryptographic attestations of
# build provenance for bottles from homebrew/core or supported
# third-party taps.

## HOMEBREW_PIP_INDEX_URL
# If set, brew install formula will use this URL to download PyPI
# package resources.

# Default: https://pypi.org/simple.

## HOMEBREW_SIMULATE_MACOS_ON_LINUX
# If set, running Homebrew on Linux will simulate certain macOS
# code paths. This is useful when auditing macOS formulae while on
# Linux.

## HOMEBREW_SKIP_OR_LATER_BOTTLES
# If set along with $HOMEBREW_DEVELOPER, do not use bottles from
# older versions of macOS. This is useful in development on new
# macOS versions.

## HOMEBREW_SORBET_RECURSIVE
# If set along with $HOMEBREW_SORBET_RUNTIME, enable recursive
# typechecking using Sorbet. Automatically enabled when running
# brew tests.

## HOMEBREW_SORBET_RUNTIME
# If set, enable runtime typechecking using Sorbet. Set by default
# when running brew test, brew test-bot or brew tests.

## HOMEBREW_SSH_CONFIG_PATH
# If set, Homebrew will use the given config file instead of
# ~/.ssh/config when fetching Git repositories over SSH.

# Default: ~/.ssh/config

## HOMEBREW_SUDO_THROUGH_SUDO_USER
# If set, Homebrew will use the $SUDO_USER environment variable to
# define the user to sudo(8) through when running sudo(8).

## HOMEBREW_SVN
# Use this as the svn(1) binary.

# Default: A Homebrew-built Subversion (if installed), or the
# system-provided binary.

## HOMEBREW_SYSTEM_ENV_TAKES_PRIORITY
# If set in Homebrew’s system-wide environment file
# (/etc/homebrew/brew.env), the system-wide environment file will
# be loaded last to override any prefix or user settings.

## HOMEBREW_TEMP
# Use this path as the temporary directory for building packages.
# Changing this may be needed if your system temporary directory
# and Homebrew prefix are on different volumes, as macOS has
# trouble moving symlinks across volumes when the target does not
# yet exist. This issue typically occurs when using FileVault or
# custom SSD configurations.

# Default: macOS: /private/tmp, Linux: /var/tmp.

## HOMEBREW_UPDATE_TO_TAG
# If set, always use the latest stable tag (even if developer
# commands have been run).

## HOMEBREW_UPGRADE_GREEDY
# If set, pass --greedy to all cask upgrade commands.

## HOMEBREW_UPGRADE_GREEDY_CASKS
# A space-separated list of casks. Homebrew will act as if
# --greedy was passed when upgrading any cask on this list.

## HOMEBREW_VERBOSE
# If set, always assume --verbose when running commands.

## HOMEBREW_VERBOSE_USING_DOTS
# If set, verbose output will print a . no more than once a
# minute. This can be useful to avoid long-running Homebrew
# commands being killed due to no output.

## HOMEBREW_VERIFY_ATTESTATIONS
# If set, Homebrew will use the gh tool to verify cryptographic
# attestations of build provenance for bottles from homebrew/core
# or supported third-party taps.
