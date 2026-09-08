#!/bin/sh
# Keep all work inside functions, with the entry point at the end, so a partial
# download cannot execute the setup body before the complete body is available.

bootstrap_plan() {
    cat <<'PLAN'
Bootstrap checklist (proposed; setup steps are not implemented yet)

  [ready]   Launcher, macOS preflight, and a separate log for each run
  [pending] Basic machine information and workstation/server profile
  [pending] Apple Account sign-in, verification, and an iCloud sync check
  [pending] macOS updates and resuming after a restart
  [pending] Package manager decision: Nix, Homebrew, or both
  [pending] Development tools, runtimes, and shell setup
  [pending] Dotfile links with backups and safe reruns
  [pending] Mac apps, services, permissions, and individual sign-ins
  [pending] Mac preferences, keyboard, and window management
  [pending] Optional remote access for a server/workstation
  [pending] App Store installations on a physical Mac
  [pending] Final verification and a list of unfinished manual steps

No package installation, account sign-in, or settings changes run in this version.
PLAN
}

bootstrap_help() {
    cat <<'HELP'
Usage: sh bootstrap.sh [--plan | --help | --version]

With no arguments, check this Mac and save a report. This first version prepares
the feedback loop; it does not install packages or apply the old install script.

  --plan       Print the proposed checklist without writing files
  --help       Print this help without writing files
  --version    Print the launcher version

Optional: DOTFILES_BOOTSTRAP_STATE_DIR changes where reports are saved.
Default:  ~/Library/Logs/dotfiles-bootstrap
HELP
}

bootstrap_log() {
    printf '%s\n' "$*"
    printf '%s\n' "$*" >> "$BOOTSTRAP_LOG"
}

bootstrap_finish() {
    BOOTSTRAP_EXIT=$?
    trap - 0
    if [ "$BOOTSTRAP_EXIT" -ne 0 ]; then
        printf '\nPreflight failed (exit %s). No installation was attempted.\n' "$BOOTSTRAP_EXIT" >&2
        printf 'Result: preflight failed (exit %s)\n' "$BOOTSTRAP_EXIT" >> "$BOOTSTRAP_LOG"
    fi
    exit "$BOOTSTRAP_EXIT"
}

bootstrap_main() {
    set -eu
    BOOTSTRAP_VERSION=0.1.0
    if [ "$#" -gt 1 ]; then
        bootstrap_help >&2
        return 2
    fi
    case "${1-}" in
        --help|-h) bootstrap_help; return 0 ;;
        --plan) bootstrap_plan; return 0 ;;
        --version) printf '%s\n' "$BOOTSTRAP_VERSION"; return 0 ;;
        '') ;;
        *) printf 'Unknown option: %s\n' "$1" >&2; bootstrap_help >&2; return 2 ;;
    esac

    if [ "$(uname -s)" != Darwin ]; then
        printf 'This bootstrap currently targets macOS. No changes made.\n' >&2
        return 1
    fi
    if [ "$(id -u)" -eq 0 ]; then
        printf 'Run this as your normal Mac user, without sudo. No changes made.\n' >&2
        return 1
    fi
    for BOOTSTRAP_COMMAND in sw_vers curl tar mktemp date df awk; do
        if ! command -v "$BOOTSTRAP_COMMAND" >/dev/null 2>&1; then
            printf 'Required system command is unavailable: %s\n' "$BOOTSTRAP_COMMAND" >&2
            return 1
        fi
    done

    BOOTSTRAP_STATE=${DOTFILES_BOOTSTRAP_STATE_DIR:-"$HOME/Library/Logs/dotfiles-bootstrap"}
    case "$BOOTSTRAP_STATE" in
        /*) ;;
        *) printf 'DOTFILES_BOOTSTRAP_STATE_DIR must be an absolute path.\n' >&2; return 2 ;;
    esac
    umask 077
    mkdir -p "$BOOTSTRAP_STATE/runs"
    BOOTSTRAP_RUN=$(mktemp -d "$BOOTSTRAP_STATE/runs/$(date -u +%Y%m%dT%H%M%SZ)-XXXXXX")
    BOOTSTRAP_LOG=$BOOTSTRAP_RUN/run.log
    : > "$BOOTSTRAP_LOG"
    trap bootstrap_finish 0
    trap 'exit 130' INT
    trap 'exit 143' TERM

    bootstrap_log "Dotfiles bootstrap $BOOTSTRAP_VERSION"
    bootstrap_log "macOS: $(sw_vers -productVersion) ($(sw_vers -buildVersion))"
    bootstrap_log "Architecture: $(uname -m)"
    bootstrap_log "Local user: $(id -un)"
    bootstrap_log "Run: $BOOTSTRAP_RUN"
    bootstrap_log ''

    BOOTSTRAP_OS_VERSION=$(sw_vers -productVersion)
    BOOTSTRAP_OS_MAJOR=${BOOTSTRAP_OS_VERSION%%.*}
    if [ "$BOOTSTRAP_OS_MAJOR" -lt 15 ]; then
        bootstrap_log 'Apple Account VM testing requires macOS 15 or later in the guest and host.'
        return 1
    fi
    bootstrap_log "Available disk space: $(df -h "$BOOTSTRAP_RUN" | awk 'NR == 2 { print $4 }')"
    if /usr/bin/xcode-select -p >/dev/null 2>&1; then
        bootstrap_log 'Apple developer tools: a developer directory is configured'
    else
        bootstrap_log 'Apple developer tools: not configured (installation pending)'
    fi
    if command -v brew >/dev/null 2>&1 || [ -x /opt/homebrew/bin/brew ] || [ -x /usr/local/bin/brew ]; then
        bootstrap_log 'Homebrew: present (not modified)'
    else
        bootstrap_log 'Homebrew: not detected (package manager decision pending)'
    fi
    if command -v nix >/dev/null 2>&1 || [ -x /nix/var/nix/profiles/default/bin/nix ]; then
        bootstrap_log 'Nix: present (not modified)'
    else
        bootstrap_log 'Nix: not detected (package manager decision pending)'
    fi
    bootstrap_log 'Apple Account: not checked; sign-in and sync verification are pending'
    bootstrap_log ''
    bootstrap_plan > "$BOOTSTRAP_RUN/checklist.txt"
    cat "$BOOTSTRAP_RUN/checklist.txt"
    cat "$BOOTSTRAP_RUN/checklist.txt" >> "$BOOTSTRAP_LOG"
    bootstrap_log ''
    bootstrap_log 'Result: launcher and preflight passed. Machine setup is still pending.'
    bootstrap_log 'Rerun the same download command after the next update to test the latest launcher.'
    bootstrap_log "Log: $BOOTSTRAP_LOG"
}

bootstrap_main "$@"
