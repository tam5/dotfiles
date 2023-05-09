ku() {
  local context

  context=$(kubectl config get-contexts -o name | fzf) &&
      kubectl config use-context $context
}
