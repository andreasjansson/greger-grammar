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
    /[ \t\n]/,
  ],

  externals: $ => [
    $.tool_content,
    $.html_comment,
    $.error_sentinel,
  ],

  rules: {
    source_file: $ => repeat($._block),

    _block: $ => choice(
      $.user,
      $.assistant,
      $.system,
      $.thinking,
      $.tool_use,
      $.tool_result,
      $.server_tool_use,
      $.server_tool_result,
      $.citations,
    ),

    user: $ => seq(
      '##',
      'USER',
      ':',
      $.text,
    ),

    assistant: $ => seq(
      '##',
      'ASSISTANT',
      ':',
      $.text,
    ),

    system: $ => seq(
      '##',
      'SYSTEM',
      ':',
      $.text,
    ),

    thinking: $ => seq(
      '##',
      'THINKING',
      ':',
      $.text,
    ),

    tool_use: $ => seq(
      '##',
      'TOOL',
      'USE',
      ':',
      /\n/,
      optional(/\n/),
      repeat(choice(
        $.name,
        $.id,
        $.tool_param,
      )),
    ),

    tool_result: $ => seq(
      '##',
      'TOOL',
      'RESULT',
      ':',
      /\n/,
      optional(/\n/),
      repeat(choice(
        $.id,
        $.content,
      )),
    ),

    server_tool_use: $ => seq(
      '##',
      'SERVER',
      'TOOL',
      'USE',
      ':',
      /\n/,
      optional(/\n/),
      repeat(choice(
        $.name,
        $.id,
        $.tool_param,
      )),
    ),

    server_tool_result: $ => seq(
      '##',
      'SERVER',
      'TOOL',
      'RESULT',
      ':',
      /\n/,
      optional(/\n/),
      repeat(choice(
        $.id,
        $.content,
      )),
    ),

    citations: $ => seq(
      '##',
      'CITATIONS',
      ':',
      optional($.text),
      repeat($.citation_entry),
    ),

    name: $ => token(seq('Name:', /[^\n]*/, /\n/)),
    id: $ => token(seq('ID:', /[^\n]*/, /\n/)),

    tool_param: $ => seq(
      '###',
      /[ ]*/,
      alias($.param_name, $.name),
      /\n/,
      optional(/\n/),
      alias($.tool_content, $.value),
    ),

    param_name: $ => /[^\n]+/,

    citation_entry: $ => seq(
      '###',
      /[ ]*/,
      alias($.citation_url, $.url),
      /\n/,
      optional(/\n/),
      optional(alias($.citation_title, $.title)),
      optional(alias($.citation_text, $.cited_text)),
      optional(alias($.citation_encrypted_index, $.encrypted_index)),
    ),

    citation_url: $ => /[^\n]*/,

    citation_title: $ => token(seq(
      'Title:',
      /[ ]+/,
      /[^\n]*/,
      /\n/,
    )),

    citation_text: $ => token(seq(
      'Cited text:',
      /[ ]+/,
      /[^\n]*/,
      /\n/,
    )),

    citation_encrypted_index: $ => token(seq(
      'Encrypted index:',
      /[ ]+/,
      /[^\n]*/,
      /\n/,
    )),

    text: $ => prec.right(repeat1(choice(
      $.code_block,
      $.cite_tag,
      $.safe_shell_commands,
      $._text_content,
      /\n/,
    ))),

    _text_content: $ => token(prec(-1, /[^#<`\n]+/)),

    content: $ => alias($.tool_content, 'content'),

    code_block: $ => choice(
      $.triple_backtick_block,
      $.single_backtick_inline,
    ),

    triple_backtick_block: $ => seq(
      '```',
      optional(/[^\n]*/),
      /\n/,
      repeat(choice(
        /[^`\n]+/,
        /\n/,
        /`[^`]/,
        /``[^`]/,
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
      /[^<]*/,
      '</cite>',
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
