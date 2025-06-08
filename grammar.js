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

    // Use aliases to shorten section names
    user: $ => alias($.user_section, 'user'),
    assistant: $ => alias($.assistant_section, 'assistant'),
    system: $ => alias($.system_section, 'system'),
    thinking: $ => alias($.thinking_section, 'thinking'),
    tool_use: $ => alias($.tool_use_section, 'tool_use'),
    tool_result: $ => alias($.tool_result_section, 'tool_result'),
    server_tool_use: $ => alias($.server_tool_use_section, 'server_tool_use'),
    server_tool_result: $ => alias($.server_tool_result_section, 'server_tool_result'),
    citations: $ => alias($.citations_section, 'citations'),

    user_section: $ => seq(
      '##',
      'USER',
      ':',
      /\n/,
      optional(alias($._section_content, $.text)),
    ),

    assistant_section: $ => seq(
      '##',
      'ASSISTANT',
      ':',
      /\n/,
      optional(alias($._section_content, $.text)),
    ),

    system_section: $ => seq(
      '##',
      'SYSTEM',
      ':',
      /\n/,
      optional(alias($._section_content, $.text)),
    ),

    thinking_section: $ => seq(
      '##',
      'THINKING',
      ':',
      /\n/,
      optional(alias($._section_content, $.text)),
    ),

    tool_use_section: $ => seq(
      '##',
      'TOOL',
      'USE',
      ':',
      /\n/,
      optional(/\n/),
      repeat(choice(
        alias($.tool_name, $.name),
        alias($.tool_id, $.id),
        $.tool_param,
      )),
    ),

    tool_result_section: $ => seq(
      '##',
      'TOOL',
      'RESULT',
      ':',
      /\n/,
      optional(/\n/),
      repeat(choice(
        alias($.tool_id, $.id),
        alias($.tool_content, $.content),
      )),
    ),

    server_tool_use_section: $ => seq(
      '##',
      'SERVER',
      'TOOL',
      'USE',
      ':',
      /\n/,
      optional(/\n/),
      repeat(choice(
        alias($.tool_name, $.name),
        alias($.tool_id, $.id),
        $.tool_param,
      )),
    ),

    server_tool_result_section: $ => seq(
      '##',
      'SERVER',
      'TOOL',
      'RESULT',
      ':',
      /\n/,
      optional(/\n/),
      repeat(choice(
        alias($.tool_id, $.id),
        alias($.tool_content, $.content),
      )),
    ),

    citations_section: $ => seq(
      '##',
      'CITATIONS',
      ':',
      /\n/,
      optional(/\n/),
      optional(alias($._section_content, $.text)),
      repeat($.citation_entry),
    ),

    _section_content: $ => repeat1(choice(
      $.code_block,
      $.cite_tag,
      $.safe_shell_commands,
      /[^#<`\n]+/,
      /\n/,
    )),

    tool_name: $ => token(seq('Name:', /[^\n]*/, /\n/)),
    tool_id: $ => token(seq('ID:', /[^\n]*/, /\n/)),

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

    text: $ => repeat1(choice(
      $.code_block,
      $.cite_tag,
      $.safe_shell_commands,
      /[^#<`\n]+/,
      /\n/,
    )),

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
