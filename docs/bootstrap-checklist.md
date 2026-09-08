# Bootstrap checklist

This is a proposed scope for discussion, based on the current dotfiles, Brewfile,
and bootstrap notes. Only the launcher and preflight are implemented. Package
manager choice and the exact application list are deliberately undecided.

## Ready now

- [x] Run from a single downloaded shell script on a fresh Mac, without Git,
      Homebrew, Nix, or Command Line Tools as prerequisites.
- [x] Check macOS version, architecture, current user, and available disk space.
- [x] Report existing developer tools and package managers without invoking
      installers or changing their configuration.
- [x] Save private, separate reports for each run. Allow safe repeated runs.
- [x] Provide `--plan`, `--help`, and `--version`.

## Proposed setup stages

- [ ] **Machine profile:** local identity, computer name, workstation versus
      server/remote workstation, and any machine-specific choices.
- [ ] **Apple Account:** native sign-in and verification; handle cancellation,
      already-signed-in state, and an unexpected account; choose iCloud services;
      verify a known test file syncs. Real account testing uses an ordinary,
      separate Apple Account. Account credentials and verification codes do not
      belong in this repository or logs.
- [ ] **macOS updates:** check, install at the chosen point, and resume after
      restarting without replaying completed destructive actions.
- [ ] **Package managers:** decide whether Nix, Homebrew, or both own each class
      of software. Decide how packages and versions are declared and updated.
- [ ] **Developer tools:** required Apple Command Line Tools, Git and GitHub
      authentication, languages/runtimes, editors, and container tooling.
- [ ] **Dotfiles:** inventory the existing links; back up conflicting files;
      avoid deleting existing configuration; support reruns and rollback.
- [ ] **Apps and services:** review the existing Brewfile as candidate input,
      decide the actual list, and separate installation from account sign-in and
      Accessibility/Input Monitoring or other permissions.
- [ ] **Preferences:** shell defaults, keyboard, window management, Finder/Dock,
      and other agreed macOS settings. Verify effective values where possible.
- [ ] **Optional remote access:** Screen Sharing and Remote Login for the chosen
      machine profile, with explicit allowed users.
- [ ] **App Store:** handle purchases and installations on a physical Mac;
      report unsupported VM checks as untested, never as passed.
- [ ] **Verification:** usable fresh terminal, commands available, links resolve,
      apps launch, services run, and outstanding manual steps are visible.

## Acceptance runs to add as we implement stages

- [ ] Clean install in a fresh clone, including Apple Account sign-in.
- [ ] Second run on the same machine: no duplicate settings or damaged links.
- [ ] Cancel sign-in or interrupt an installation, then resume.
- [ ] Fail a download or prerequisite check and show the actionable failure.
- [ ] Detect a conflicting file, wrong account, or unsupported VM feature.
- [ ] Final native macOS pass for App Store and hardware/account-specific flows.

The baseline should remain stopped and signed out. Test only its clones. A VM
reset restores local disk state; the test account's cloud data persists.

## Decisions for our next iteration

1. Nix, Homebrew, or both—and what belongs to each?
2. Which apps and development tools are essential on every machine?
3. Which iCloud features should be enabled and verified?
4. What changes between a personal workstation and a remote/server Mac?
5. Which existing dotfiles need cleanup before they become install defaults?
