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
    source_file: $ => repeat($._section_or_content),

    _section_or_content: $ => choice(
      $.user_section,
      $.assistant_section,
      $.system_section,
      $.thinking_section,
      $.tool_use_section,
      $.tool_result_section,
      $.server_tool_use_section,
      $.server_tool_result_section,
      $.citations_section,
      $.content_line,
      $.empty_line,
    ),

    user_section: $ => seq(
      $.section_header_user,
      repeat($._section_content),
    ),

    assistant_section: $ => seq(
      $.section_header_assistant,
      repeat($._section_content),
    ),

    system_section: $ => seq(
      $.section_header_system,
      repeat($._section_content),
    ),

    thinking_section: $ => seq(
      $.section_header_thinking,
      repeat($._section_content),
    ),

    tool_use_section: $ => seq(
      $.section_header_tool_use,
      repeat($._tool_section_content),
    ),

    tool_result_section: $ => seq(
      $.section_header_tool_result,
      repeat($._tool_section_content),
    ),

    server_tool_use_section: $ => seq(
      $.section_header_server_tool_use,
      repeat($._tool_section_content),
    ),

    server_tool_result_section: $ => seq(
      $.section_header_server_tool_result,
      repeat($._tool_section_content),
    ),

    citations_section: $ => seq(
      $.section_header_citations,
      repeat($._citations_content),
    ),

    section_header_user: $ => seq('##', 'USER', ':', /\n/),
    section_header_assistant: $ => seq('##', 'ASSISTANT', ':', /\n/),
    section_header_system: $ => seq('##', 'SYSTEM', ':', /\n/),
    section_header_thinking: $ => seq('##', 'THINKING', ':', /\n/),
    section_header_tool_use: $ => seq('##', 'TOOL', 'USE', ':', /\n/),
    section_header_tool_result: $ => seq('##', 'TOOL', 'RESULT', ':', /\n/),
    section_header_server_tool_use: $ => seq('##', 'SERVER', 'TOOL', 'USE', ':', /\n/),
    section_header_server_tool_result: $ => seq('##', 'SERVER', 'TOOL', 'RESULT', ':', /\n/),
    section_header_citations: $ => seq('##', 'CITATIONS', ':', /\n/),

    _section_content: $ => prec.dynamic(-1, choice(
      $.content_line,
      $.empty_line,
      $.code_block,
      $.cite_tag,
      $.safe_shell_commands,
    )),

    _tool_section_content: $ => prec.dynamic(-1, choice(
      $.tool_use_metadata,
      $.tool_param,
      $.tool_content,
      $.content_line,
      $.empty_line,
    )),

    _citations_content: $ => prec.dynamic(-1, choice(
      $.citation_entry,
      $.content_line,
      $.empty_line,
    )),

    content_line: $ => seq(/[^#`<\n\r]+/, /\n/),
    empty_line: $ => /\n/,

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
      optional(/\n/),
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
      seq('Name:', /[^\n]*/, /\n/),
      seq('ID:', /[^\n]*/, /\n/),
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
