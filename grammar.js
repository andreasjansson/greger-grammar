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
    /<!--[^>]*-->/,
    /[ \t\n]/,
  ],

  externals: $ => [
    $.tool_start_tag,
    $.tool_end_tag,
    $.tool_content,
  ],



  inline: $ => [
    $.content_blocks,
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
      $.content_blocks,
    ),

    assistant: $ => seq(
      '##',
      'ASSISTANT',
      ':',
      $.content_blocks,
    ),

    system: $ => seq(
      '##',
      'SYSTEM',
      ':',
      $.content_blocks,
    ),

    thinking: $ => seq(
      '##',
      'THINKING',
      ':',
      $.content_blocks,
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
      $.id,
      optional(/\n/),
      $.content,
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
      $.id,
      optional(/\n/),
      $.content,
    ),

    citations: $ => seq(
      '##',
      'CITATIONS',
      ':',
      optional(alias($.citations_text, $.text)),
      repeat($.citation_entry),
    ),

    citations_text: $ => prec.right(repeat1(choice(
      $.cite_tag,
      $.safe_shell_commands,
      $._text_content,
    ))),

    name: $ => seq(
      'Name:',
      field('value', /[^\n]+/),
      /\n/
    ),

    id: $ => seq(
      'ID:',
      field('value', /[^\n]+/),
      /\n/
    ),

    tool_param: $ => seq(
      '###',
      /[ ]*/,
      alias($.param_name, $.name),
      /\n/,
      optional(/\n/),
      alias($.tool_tag, $.value),
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

    citation_title: $ => seq(
      'Title:',
      /[ ]+/,
      field('value', /[^\n]+/),
      /\n/,
    ),

    citation_text: $ => seq(
      'Cited text:',
      /[ ]+/,
      field('value', /[^\n]+/),
      /\n/,
    ),

    citation_encrypted_index: $ => seq(
      'Encrypted index:',
      /[ ]+/,
      field('value', /[^\n]+/),
      /\n/,
    ),

    content_blocks: $ => repeat1(choice(
      $.text,
      $.code_block,
      $.inline_code,
    )),

    text: $ => prec.right(repeat1(choice(
      $.cite_tag,
      $.safe_shell_commands,
      $._text_content,
      /\n/,
    ))),

    _text_content: $ => token(prec(-1, /[^#<`\n]+/)),

    content: $ => alias($.tool_tag, 'content'),

    code_block: $ => seq(
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

    inline_code: $ => seq(
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
