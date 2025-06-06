/**
 * @file greger.el parser
 * @author Andreas Jansson <andreas.s.t.jansson@gmail.com>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "greger",

  rules: {
    // TODO: add the actual grammar rules
    source_file: $ => "hello"
  }
});
