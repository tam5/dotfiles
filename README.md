# Dotfiles

## Bootstrap development

Run this **inside the test Mac's Terminal**, as its normal user:

```sh
curl -fsSL https://raw.githubusercontent.com/tam5/dotfiles/bootstrap-launcher/bootstrap.sh | sh
```

This development URL follows the `bootstrap-launcher` branch. Rerun the same
command after an update to exercise the new version. For automation, enable
`pipefail` in the calling shell so a failed download also fails the whole pipeline:

```sh
/bin/bash -o pipefail -c 'curl -fsSL https://raw.githubusercontent.com/tam5/dotfiles/bootstrap-launcher/bootstrap.sh | /bin/sh'
```

Version 0.1.0 checks the Mac and writes a log and checklist to
`~/Library/Logs/dotfiles-bootstrap/runs/`. It does **not** install packages, sign
into accounts, change preferences, or run the older `install` script. Successful
exit means the launcher and preflight passed, not that the Mac is configured.

Print the proposed plan without writing files:

```sh
curl -fsSL https://raw.githubusercontent.com/tam5/dotfiles/bootstrap-launcher/bootstrap.sh | sh -s -- --plan
```

See [the bootstrap checklist](docs/bootstrap-checklist.md) for proposed stages and
decisions. Package manager choice is still open. The existing `Brewfile` and
`install` are reference material for this work, not the new installer's behavior.

## Iterate locally in Tart

Enable Remote Login for the VM's user once in macOS Sharing settings. To run the
local script without publishing a commit, use the helper below. It resolves the
VM address, copies just `bootstrap.sh`, runs it, and retrieves reports into an
ignored local directory. SSH may ask for the VM user's password. It does not
store passwords, reset the VM, install a guest agent, or change the baseline.

```sh
./dev/test-bootstrap dotfiles-test test
```

The default helper arguments are `dotfiles-test` and `test`. Set
`DOTFILES_TEST_RESULTS_DIR` to change the host-side report directory.

Once the test VM is stopped, it can be replaced with a fresh clone:

```sh
tart delete dotfiles-test
tart clone dotfiles-baseline dotfiles-test
tart run dotfiles-test
```

Keep the baseline stopped and use clones for all tests. If Remote Login is not
part of the saved baseline, enable it in each fresh clone before using the helper.
