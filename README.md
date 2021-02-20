Z-Example
=========

[![Linux Build Status](https://github.com/ZHaskell/z-example/workflows/ubuntu-ci/badge.svg)](https://github.com/ZHaskell/z-example/actions)
[![MacOS Build Status](https://github.com/ZHaskell/z-example/workflows/osx-ci/badge.svg)](https://github.com/ZHaskell-Z/z-example/actions)

This package provides several examples using [ZHaskell](https://github.com/ZHaskell) libraries, demonstrating usability and performance.


## :fish_cake: Simple-File-Sync :fish_cake:

Automaticly run `rsync` if change happens. This will helps if you do development
on remote machines, but use local file editors.


## :fish_cake: LSP-Network :fish_cake:

A small tool that help you communicate with [language server](https://microsoft.github.io/language-server-protocol/)
through network(tcp) instead of stdio.

Source code: [LSP-Network](./Network/LanguageServer)

**N.B. You must keep your server projects synchronously with your local projects,
including directory structure.**

> :star: You can use [docker volumes](https://docs.docker.com/storage/volumes/)
> on server to keep the same project structure as your locals.
> And a [SimpleFileSync](#simple-file-sync) to sync your projects.

### Quickstart

On server:

```sh
z-lsp-network-server --host 0.0.0.0 --port 3001
```

On client, set your lsp-client plugin:

```sh
z-lsp-network-client --host <your-server-ip> --port 3001 --projects lsp-network-client.yaml
```

### TODO

- Enable TLS.
- Enable password proteced connection.


## :fish_cake: Other :fish_cake:

* wc, simple word counter with same performance with system `wc`.
* guess-std-type, provide infos on how stdout is connected.
