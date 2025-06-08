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
    /[ \t\r\n]/,
  ],

  externals: $ => [
    $.tool_content,
    $.html_comment,
  ],

  rules: {
    source_file: $ => repeat($._item),

    _item: $ => choice(
      $.section,
    ),

    section: $ => seq(
      $.section_header,
      repeat($._section_item),
    ),

    section_header: $ => choice(
      seq('##', 'USER', ':'),
      seq('##', 'ASSISTANT', ':'),
      seq('##', 'SYSTEM', ':'),
      seq('##', 'THINKING', ':'),
      seq('##', 'TOOL', 'USE', ':'),
      seq('##', 'TOOL', 'RESULT', ':'),
      seq('##', 'SERVER', 'TOOL', 'USE', ':'),
      seq('##', 'SERVER', 'TOOL', 'RESULT', ':'),
      seq('##', 'CITATIONS', ':'),
    ),

    _section_item: $ => choice(
      $.text_content,
      $.code_block,
      $.cite_tag,
      $.tool_use_metadata,
      $.tool_param,
      $.citation_entry,
      $.safe_shell_commands,
    ),

    text_content: $ => /[^#`<]+/,

    code_block: $ => choice(
      $.triple_backtick_block,
      $.single_backtick_inline,
    ),

    triple_backtick_block: $ => seq(
      '```',
      optional(/[^\r\n]*/),
      repeat(choice(
        /[^`\r\n]+/,
        /\r?\n/,
        /`[^`]/,
        /``[^`]/,
      )),
      '```',
    ),

    single_backtick_inline: $ => seq(
      '`',
      /[^`\r\n]+/,
      '`',
    ),

    cite_tag: $ => seq(
      '<cite>',
      repeat(/[^<\r\n]+/),
      '</cite>',
    ),

    tool_use_metadata: $ => choice(
      seq('Name:', /[^\r\n]*/),
      seq('ID:', /[^\r\n]*/),
    ),

    tool_param: $ => seq(
      '###', /[^\r\n]*/,
      $.tool_content,
    ),

    citation_entry: $ => seq(
      '###', /[^\r\n]*/,
      repeat(/[^\r\n#]+/),
    ),

    safe_shell_commands: $ => seq(
      '<safe-shell-commands>',
      repeat(/[^\r\n<]+/),
      '</safe-shell-commands>',
    ),
  }
});
