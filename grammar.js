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
      $.user_section,
      $.assistant_section,
      $.system_section,
      $.thinking_section,
      $.tool_use_section,
      $.tool_result_section,
      $.server_tool_use_section,
      $.server_tool_result_section,
      $.citations_section,
    ),

    user_section: $ => seq(
      seq('##', 'USER', ':'),
      repeat($._user_content),
    ),

    assistant_section: $ => seq(
      seq('##', 'ASSISTANT', ':'),
      repeat($._assistant_content),
    ),

    system_section: $ => seq(
      seq('##', 'SYSTEM', ':'),
      repeat($._system_content),
    ),

    thinking_section: $ => seq(
      seq('##', 'THINKING', ':'),
      repeat($._thinking_content),
    ),

    tool_use_section: $ => seq(
      seq('##', 'TOOL', 'USE', ':'),
      repeat($._tool_use_content),
    ),

    tool_result_section: $ => seq(
      seq('##', 'TOOL', 'RESULT', ':'),
      repeat($._tool_result_content),
    ),

    server_tool_use_section: $ => seq(
      seq('##', 'SERVER', 'TOOL', 'USE', ':'),
      repeat($._server_tool_use_content),
    ),

    server_tool_result_section: $ => seq(
      seq('##', 'SERVER', 'TOOL', 'RESULT', ':'),
      repeat($._server_tool_result_content),
    ),

    citations_section: $ => seq(
      seq('##', 'CITATIONS', ':'),
      repeat($._citations_content),
    ),

    _user_content: $ => choice(
      $.text_content,
      $.code_block,
      $.cite_tag,
    ),

    _assistant_content: $ => choice(
      $.text_content,
      $.code_block,
      $.cite_tag,
    ),

    _system_content: $ => choice(
      $.text_content,
      $.code_block,
      $.safe_shell_commands,
    ),

    _thinking_content: $ => choice(
      $.text_content,
      $.code_block,
    ),

    _tool_use_content: $ => choice(
      $.tool_use_metadata,
      $.tool_param,
      $.text_content,
      /\r?\n/,
    ),

    _tool_result_content: $ => choice(
      $.tool_use_metadata,
      $.tool_content,
      $.text_content,
    ),

    _server_tool_use_content: $ => choice(
      $.tool_use_metadata,
      $.tool_param,
      $.text_content,
    ),

    _server_tool_result_content: $ => choice(
      $.tool_use_metadata,
      $.tool_content,
      $.text_content,
    ),

    _citations_content: $ => choice(
      $.citation_entry,
      $.text_content,
    ),

    text_content: $ => prec(-1, /[^#`<\n]+/),

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

    tool_use_metadata: $ => prec(2, choice(
      seq(token('Name:'), /[^\r\n]*/),
      seq(token('ID:'), /[^\r\n]*/),
    )),

    tool_param: $ => prec(3, seq(
      token('###'), /[^\r\n]*/,
      $.tool_content,
    )),

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
