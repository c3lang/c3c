#!/usr/bin/env sh

curl -s https://syscalls.mebeim.net/db/x86/64/x64/latest/table.json | jq -rf gen_linux_syscall.jq > syscalls_x86_64.c3
