#!/usr/bin/env bash

# /
# |-------------------------------------------------------------------------------
# | Kubernetes
# |-------------------------------------------------------------------------------
# | 
# | 
# | 
# /

alias k="kubectl"

ku() {
  local context

  context=$(kubectl config get-contexts -o name | fzf) &&
      kubectl config use-context $context
}

klog() {
  local pod

  pod=$(kubectl get pods --all-namespaces --no-headers | fzf) &&
      print -z "$(echo $pod | awk '{print "kubectl logs -n " $1 " " $2}')"
}

kex() {
  local pod

  pod=$(kubectl get pods --all-namespaces --no-headers | fzf) &&
      print -z "$(echo $pod | awk '{print "kubectl -n " $1 " exec -it " $2 " bash"}')"
}
