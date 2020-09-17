# Nix Config

My Nix user-environment configuration

Your average nix user-environment is managed imperatively, by using
the `nix-env` command-line tool. This repository is an example of not
doing that, using my `userEnv` function from my
[`nix-extra`](https://github.com/lambdadog/nix-extra) repository.

This package allows you to build your user-environment declaratively,
and choose whether or not you want to be able to continue using
`nix-env` to manage it imperatively using the `static` argument.

Additionally, I configure emacs using my `emacsWithConfig` function,
and strive to configure all of my configuration-heavy packages using
wrappers like this. This is in contrast to the approach of managing
your home folder using nix used by tools like
[home-manager](https://github.com/nix-community/home-manager).

I hope you find my config useful for inspiration on how you can manage
your own!

## Installation

To install my user environment (though I'm not sure why you'd want
to), use the `./switch-to-env` script provided.

## Uninstalling

To revert to your previous user environment, run the command

```sh
nix-env --rollback
```
