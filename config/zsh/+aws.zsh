au() {
  local profile

  profile=$(aws configure list-profiles | fzf) &&
      export AWS_PROFILE=$profile
}
