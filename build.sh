#!/bin/bash -eux

tree-sitter generate --abi 14
tree-sitter build -o libtree-sitter-greger.dylib
