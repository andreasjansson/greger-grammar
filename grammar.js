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

  externals: $ => [
    $.tool_content,
    $.html_comment,
  ],

  rules: {
    source_file: $ => repeat($._item),

    _item: $ => choice(
      $.section,
      $._newline,
    ),

    _newline: _ => '\n',

    section: $ => prec.left(seq(
      $.section_header,
      repeat($._section_item),
    )),

    section_header: $ => choice(
      seq('##', /[ \t]*/, 'USER', /[ \t]*/, ':', /[ \t]*/, '\n'),
      seq('##', /[ \t]*/, 'ASSISTANT', /[ \t]*/, ':', /[ \t]*/, '\n'),
      seq('##', /[ \t]*/, 'SYSTEM', /[ \t]*/, ':', /[ \t]*/, '\n'),
      seq('##', /[ \t]*/, 'THINKING', /[ \t]*/, ':', /[ \t]*/, '\n'),
      seq('##', /[ \t]*/, 'TOOL', /[ \t]+/, 'USE', /[ \t]*/, ':', /[ \t]*/, '\n'),
      seq('##', /[ \t]*/, 'TOOL', /[ \t]+/, 'RESULT', /[ \t]*/, ':', /[ \t]*/, '\n'),
      seq('##', /[ \t]*/, 'SERVER', /[ \t]+/, 'TOOL', /[ \t]+/, 'USE', /[ \t]*/, ':', /[ \t]*/, '\n'),
      seq('##', /[ \t]*/, 'SERVER', /[ \t]+/, 'TOOL', /[ \t]+/, 'RESULT', /[ \t]*/, ':', /[ \t]*/, '\n'),
      seq('##', /[ \t]*/, 'CITATIONS', /[ \t]*/, ':', /[ \t]*/, '\n'),
    ),

    _section_item: $ => choice(
      $.text_line,
      $.code_block,
      $.cite_tag,
      $.tool_use_metadata,
      $.tool_param,
      $.citation_entry,
      $.safe_shell_commands,
      $._newline,
    ),

    text_line: $ => seq(
      /[^#\n`<]+/,
      '\n'
    ),

    code_block: $ => choice(
      $.triple_backtick_block,
      $.single_backtick_inline,
    ),

    triple_backtick_block: $ => seq(
      '```',
      optional(/[^\n]*/),
      '\n',
      repeat(choice(
        /[^`\n]+/,
        /`[^`]/,
        /``[^`]/,
        '\n',
      )),
      '```',
      optional('\n'),
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
      seq('Name:', /[^\n]*/, '\n'),
      seq('ID:', /[^\n]*/, '\n'),
    ),

    tool_param: $ => seq(
      '###', /[ \t]*/, /[^\n]*/, '\n',
      optional('\n'),
      $.tool_content,
      optional('\n'),
    ),

    citation_entry: $ => seq(
      '###', /[ \t]*/, /[^\n]*/, '\n',
      repeat(choice(
        seq(/[^\n#]+/, '\n'),
        '\n',
      )),
    ),

    safe_shell_commands: $ => seq(
      '<safe-shell-commands>',
      optional('\n'),
      repeat(seq(/[^\n<]*/, '\n')),
      '</safe-shell-commands>',
      optional('\n'),
    ),
  }
});
