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

  conflicts: $ => [
    [$.text_block],
  ],

  rules: {
    source_file: $ => repeat($._block),

    _block: $ => choice(
      $.user_section,
      $.assistant_section,
      $.system_section,
      $.thinking_section,
      $.tool_use_section,
      $.tool_result_section,
      $.server_tool_use_section,
      $.server_tool_result_section,
      $.citations_section,
      $.text_block,
    ),

    user_section: $ => prec.right(seq(
      '##',
      'USER',
      ':',
      repeat($._section_content),
    )),

    assistant_section: $ => prec.right(seq(
      '##',
      'ASSISTANT',
      ':',
      repeat($._section_content),
    )),

    system_section: $ => prec.right(seq(
      '##',
      'SYSTEM',
      ':',
      repeat($._section_content),
    )),

    thinking_section: $ => prec.right(seq(
      '##',
      'THINKING',
      ':',
      repeat($._section_content),
    )),

    tool_use_section: $ => prec.right(seq(
      '##',
      'TOOL',
      'USE',
      ':',
      repeat($._tool_section_content),
    )),

    tool_result_section: $ => prec.right(seq(
      '##',
      'TOOL',
      'RESULT',
      ':',
      repeat($._tool_section_content),
    )),

    server_tool_use_section: $ => prec.right(seq(
      '##',
      'SERVER',
      'TOOL',
      'USE',
      ':',
      repeat($._tool_section_content),
    )),

    server_tool_result_section: $ => prec.right(seq(
      '##',
      'SERVER',
      'TOOL',
      'RESULT',
      ':',
      repeat($._tool_section_content),
    )),

    citations_section: $ => prec.right(seq(
      '##',
      'CITATIONS',
      ':',
      repeat($._citations_content),
    )),

    _section_content: $ => prec(-1, choice(
      $.text_block,
      $.code_block,
      $.cite_tag,
      $.safe_shell_commands,
    )),

    _tool_section_content: $ => prec(-1, choice(
      $.tool_use_metadata,
      $.tool_param,
      $.tool_content,
      $.text_block,
    )),

    _citations_content: $ => prec(-1, choice(
      $.citation_entry,
      $.text_block,
    )),

    text_block: $ => repeat1(choice(
      /[^\n#`<]+/,
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

    citation_entry: $ => prec.right(seq(
      '###',
      $.citation_url,
      /\n/,
      repeat(choice(
        $.citation_title,
        $.citation_text,
        $.citation_encrypted_index,
        seq(/[^\n#T]/, /[^\n]*/, /\n/)  // Other lines that are not metadata or section headers
      )),
    )),

    citation_url: $ => /[^\n]*/,

    citation_title: $ => seq(
      'Title:',
      ' ',
      $.citation_title_text,
      /\n/,
    ),

    citation_text: $ => seq(
      'Cited text:',
      ' ',
      $.citation_text_content,
      /\n/,
    ),

    citation_encrypted_index: $ => seq(
      'Encrypted index:',
      ' ',
      $.citation_encrypted_index_content,
      /\n/,
    ),

    citation_title_text: $ => /[^\n]*/,
    citation_text_content: $ => /[^\n]*/,
    citation_encrypted_index_content: $ => /[^\n]*/,

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
