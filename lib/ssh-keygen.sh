#!/user/bin/env sh
#
# https://docs.github.com/en/github/authenticating-to-github

echo "Generating a new SSH key for GitHub..."

ssh-keygen -t ed25519 -C "$1" -f ~/.ssh/id_ed25519_github

eval "$(ssh-agent -s)"

touch ~/.ssh/config
echo "Host *\n AddKeysToAgent yes\n UseKeychain yes\n IdentityFile ~/.ssh/id_ed25519_github" | tee ~/.ssh/config

ssh-add --apple-use-keychain ~/.ssh/id_ed25519_github

echo "run 'pbcopy < ~/.ssh/id_ed25519_github.pub' and paste that into GitHub"

