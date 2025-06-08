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
  ],

  externals: $ => [
    $.tool_content,
    $.html_comment,
  ],

  rules: {
    source_file: $ => repeat($._item),

    _item: $ => choice(
      $.user_section,
      $.assistant_section,
      $.system_section,
      $.thinking_section,
      $.tool_use_section,
      $.tool_result_section,
      $.server_tool_use_section,
      $.server_tool_result_section,
      $.citations_section,
      $._whitespace,
    ),

    _whitespace: $ => /\s+/,

    user_section: $ => seq(
      token(seq('##', /\s+/, 'USER', /\s*/, ':')),
      optional($._section_content),
    ),

    assistant_section: $ => seq(
      token(seq('##', /\s+/, 'ASSISTANT', /\s*/, ':')),
      optional($._section_content),
    ),

    system_section: $ => seq(
      token(seq('##', /\s+/, 'SYSTEM', /\s*/, ':')),
      optional($._section_content),
    ),

    thinking_section: $ => seq(
      token(seq('##', /\s+/, 'THINKING', /\s*/, ':')),
      optional($._section_content),
    ),

    tool_use_section: $ => seq(
      token(seq('##', /\s+/, 'TOOL', /\s+/, 'USE', /\s*/, ':')),
      optional($._tool_section_content),
    ),

    tool_result_section: $ => seq(
      token(seq('##', /\s+/, 'TOOL', /\s+/, 'RESULT', /\s*/, ':')),
      optional($._tool_section_content),
    ),

    server_tool_use_section: $ => seq(
      token(seq('##', /\s+/, 'SERVER', /\s+/, 'TOOL', /\s+/, 'USE', /\s*/, ':')),
      optional($._tool_section_content),
    ),

    server_tool_result_section: $ => seq(
      token(seq('##', /\s+/, 'SERVER', /\s+/, 'TOOL', /\s+/, 'RESULT', /\s*/, ':')),
      optional($._tool_section_content),
    ),

    citations_section: $ => seq(
      token(seq('##', /\s+/, 'CITATIONS', /\s*/, ':')),
      optional($._citations_content),
    ),

    _section_content: $ => repeat1(choice(
      $.text_line,
      $.code_block,
      $.cite_tag,
      $.safe_shell_commands,
      $._empty_line,
    )),

    _tool_section_content: $ => repeat1(choice(
      $.tool_use_metadata,
      $.tool_param,
      $.tool_content,
      $.text_line,
      $._empty_line,
    )),

    _citations_content: $ => repeat1(choice(
      $.citation_entry,
      $.text_line,
      $._empty_line,
    )),

    _empty_line: $ => /\n/,

    text_line: $ => seq(
      /[^#`<\n]+/,
      optional(/\n/),
    ),

    code_block: $ => choice(
      $.triple_backtick_block,
      $.single_backtick_inline,
    ),

    triple_backtick_block: $ => seq(
      token('```'),
      optional(/[^\n]*/),
      /\n/,
      repeat(choice(
        /[^`\n]+/,
        /\n/,
        /`[^`]/,
        /``[^`]/,
      )),
      token('```'),
      optional(/\n/),
    ),

    single_backtick_inline: $ => seq(
      '`',
      /[^`\n]+/,
      '`',
    ),

    cite_tag: $ => seq(
      '<cite>',
      repeat(/[^<\n]+/),
      '</cite>',
    ),

    tool_use_metadata: $ => choice(
      seq(token('Name:'), /[^\n]*/, optional(/\n/)),
      seq(token('ID:'), /[^\n]*/, optional(/\n/)),
    ),

    tool_param: $ => seq(
      token('###'),
      /[^\n]*/,
      /\n/,
      optional(/\n/),
      $.tool_content,
    ),

    citation_entry: $ => seq(
      token('###'),
      /[^\n]*/,
      /\n/,
      repeat(seq(
        /[^\n#]+/,
        /\n/,
      )),
    ),

    safe_shell_commands: $ => seq(
      '<safe-shell-commands>',
      repeat(choice(
        /[^<\n]+/,
        /\n/,
      )),
      '</safe-shell-commands>',
    ),
  }
});
