/**
 * @file Greger grammar for tree-sitter
 * @author Andreas Jansson
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "greger",

  extras: $ => [
    $.html_comment,
    /[ \t]/,
  ],

  conflicts: $ => [
    [$.section_content],
  ],

  externals: $ => [
    $.tool_content,
    $.html_comment,
  ],

  rules: {
    source_file: $ => repeat($._item),

    _item: $ => choice(
      $.section,
      $.text_line,
      $._newline,
    ),

    _newline: _ => '\n',

    section: $ => seq(
      $.section_header,
      $.section_content,
    ),

    section_header: $ => choice(
      seq(/##[ \t]*/, 'USER', /[ \t]*:[ \t]*/, $._newline),
      seq(/##[ \t]*/, 'ASSISTANT', /[ \t]*:[ \t]*/, $._newline),
      seq(/##[ \t]*/, 'SYSTEM', /[ \t]*:[ \t]*/, $._newline),
      seq(/##[ \t]*/, 'THINKING', /[ \t]*:[ \t]*/, $._newline),
      seq(/##[ \t]*/, 'TOOL USE', /[ \t]*:[ \t]*/, $._newline),
      seq(/##[ \t]*/, 'TOOL RESULT', /[ \t]*:[ \t]*/, $._newline),
      seq(/##[ \t]*/, 'SERVER TOOL USE', /[ \t]*:[ \t]*/, $._newline),
      seq(/##[ \t]*/, 'SERVER TOOL RESULT', /[ \t]*:[ \t]*/, $._newline),
      seq(/##[ \t]*/, 'CITATIONS', /[ \t]*:[ \t]*/, $._newline),
    ),

    section_content: $ => repeat1(choice(
      $.tool_use_metadata,
      $.tool_param,
      $.safe_shell_commands,
      $.code_block,
      $.cite_tag,
      $.text_line,
      $._newline,
    )),

    tool_use_metadata: $ => choice(
      seq('Name:', /[^\n]*/, $._newline),
      seq('ID:', /[^\n]*/, $._newline),
    ),

    tool_param: $ => seq(
      seq(/###[ \t]*/, /[^\n]*/, $._newline),
      optional($._newline),
      $.tool_content,
      optional($._newline),
    ),

    safe_shell_commands: $ => seq(
      '<safe-shell-commands>',
      optional($._newline),
      repeat(seq(/[^\n<]*/, $._newline)),
      '</safe-shell-commands>',
    ),

    code_block: $ => choice(
      $.triple_backtick_block,
      $.single_backtick_inline,
    ),

    triple_backtick_block: $ => seq(
      '```',
      optional(/[^\n]*/),
      $._newline,
      repeat(choice(
        /[^`\n]+/,
        /`[^`]/,
        /``[^`]/,
        $._newline,
      )),
      '```',
    ),

    single_backtick_inline: $ => seq(
      '`',
      /[^`\n]+/,
      '`',
    ),

    cite_tag: $ => seq(
      '<cite>',
      repeat(choice(
        /[^<\n]+/,
        /<[^/]/,
      )),
      '</cite>',
    ),

    text_line: $ => /[^#\n`<]+/,
  }
});
