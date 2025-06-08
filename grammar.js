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
      /\n+/,
    ),

    user_section: $ => seq(
      '##',
      'USER',
      ':',
      optional($._section_content),
    ),

    assistant_section: $ => seq(
      '##',
      'ASSISTANT',
      ':',
      optional($._section_content),
    ),

    system_section: $ => seq(
      '##',
      'SYSTEM',
      ':',
      optional($._section_content),
    ),

    thinking_section: $ => seq(
      '##',
      'THINKING',
      ':',
      optional($._section_content),
    ),

    tool_use_section: $ => seq(
      '##',
      'TOOL',
      'USE',
      ':',
      optional($._tool_section_content),
    ),

    tool_result_section: $ => seq(
      '##',
      'TOOL',
      'RESULT',
      ':',
      optional($._tool_section_content),
    ),

    server_tool_use_section: $ => seq(
      '##',
      'SERVER',
      'TOOL',
      'USE',
      ':',
      optional($._tool_section_content),
    ),

    server_tool_result_section: $ => seq(
      '##',
      'SERVER',
      'TOOL',
      'RESULT',
      ':',
      optional($._tool_section_content),
    ),

    citations_section: $ => seq(
      '##',
      'CITATIONS',
      ':',
      optional($._citations_content),
    ),

    _section_content: $ => repeat1($._content_item),

    _tool_section_content: $ => repeat1($._tool_content_item),

    _citations_content: $ => repeat1($._citation_content_item),

    _content_item: $ => choice(
      $.text_content,
      $.code_block,
      $.cite_tag,
      $.safe_shell_commands,
      /\n/,
    ),

    _tool_content_item: $ => choice(
      $.tool_use_metadata,
      $.tool_param,
      $.tool_content,
      $.text_content,
      /\n/,
    ),

    _citation_content_item: $ => choice(
      $.citation_entry,
      $.text_content,
      /\n/,
    ),

    text_content: $ => prec(-1, /[^#`<\n]+/),

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

    tool_use_metadata: $ => choice(
      seq('Name:', /[^\n]*/),
      seq('ID:', /[^\n]*/),
    ),

    tool_param: $ => seq(
      '###',
      /[^\n]*/,
      /\n+/,
      $.tool_content,
    ),

    citation_entry: $ => seq(
      '###',
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
